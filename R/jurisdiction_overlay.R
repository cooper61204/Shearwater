library(sf)
library(dplyr)
library(lubridate)

# ------------------------------------------------------------------------------
# overlay_eez_abnj()
# Determine whether points or segments fall within EEZs or areas beyond
# national jurisdiction (ABNJ / high seas).
#
# Inputs:
#   track_data  — sf point object with bird fixes, must have CRS set
#   eez_layer   — sf polygon layer of EEZ boundaries (from read_management_layers())
#                 expected columns: iso3, sovereign, eez_name
#
# Output:
#   track_data with two new columns appended:
#     jurisdiction   — character: the eez_name if inside an EEZ, "ABNJ" otherwise
#     iso3           — character: ISO-3 country code if inside EEZ, NA otherwise
# ------------------------------------------------------------------------------
overlay_eez_abnj <- function(track_data, eez_layer) {

  assert_crs(track_data)
  assert_crs(eez_layer)

  # Reproject track to match EEZ layer CRS
  track_proj <- sf::st_transform(track_data, sf::st_crs(eez_layer))

  # Spatial join — left join keeps all track points
  joined <- sf::st_join(track_proj, eez_layer[, c("iso3", "sovereign", "eez_name")],
                        join = sf::st_within, left = TRUE)

  # Points with no EEZ match are ABNJ (high seas)
  joined <- joined |>
    dplyr::mutate(
      jurisdiction = dplyr::if_else(is.na(.data$eez_name), "ABNJ", .data$eez_name),
      iso3         = dplyr::if_else(is.na(.data$iso3), NA_character_, .data$iso3)
    )

  # Return in original CRS
  sf::st_transform(joined, sf::st_crs(track_data))
}

# ------------------------------------------------------------------------------
# calc_time_in_jurisdictions()
# Summarize how much time (hours) birds spend in each jurisdictional area.
#
# Inputs:
#   track_data   — output of overlay_eez_abnj(), must have columns:
#                    bird_id, Date (character "YYYY-MM-DD"),
#                    Time (character military time "HH:MM:SS"),
#                    jurisdiction, iso3
#   return_raw   — if TRUE, return the row-per-fix table with step durations
#                  instead of the summary (useful for debugging)
#
# Output (default):
#   tibble with one row per bird × jurisdiction:
#     bird_id, jurisdiction, iso3, n_fixes, total_hours
# ------------------------------------------------------------------------------
calc_time_in_jurisdictions <- function(track_data, return_raw = FALSE) {

  assert_required_cols(track_data, c("bird_id", "Date", "Time", "jurisdiction"))

  # Drop geometry for tabular summary
  tbl <- sf::st_drop_geometry(track_data)

  # Combine Date + Time strings into a single POSIXct datetime
  tbl <- tbl |>
    dplyr::mutate(
      time_clean = dplyr::case_when(
        nchar(.data$Time) == 6 ~ paste0(
          substr(.data$Time, 1, 2), ":",
          substr(.data$Time, 3, 4), ":",
          substr(.data$Time, 5, 6)
        ),
        TRUE ~ .data$Time  # already "HH:MM:SS"
      ),
      datetime = as.POSIXct(
        paste(.data$Date, .data$time_clean),
        format = "%m/%d/%Y %H:%M:%S",
        tz = "UTC"
      )
    )

  # Warn if any datetimes failed to parse
  if (any(is.na(tbl$datetime))) {
    warning("Some Date/Time combinations failed to parse, check for malformed rows.")
  }

  # Sort by bird and datetime so step durations are meaningful
  tbl <- tbl |>
    dplyr::arrange(.data$bird_id, .data$datetime) |>
    dplyr::group_by(.data$bird_id) |>
    dplyr::mutate(
      step_duration_h = as.numeric(
        difftime(dplyr::lead(.data$datetime), .data$datetime, units = "hours")
      )
    ) |>
    dplyr::ungroup()

  if (return_raw) return(tbl)

  # Summarize by bird × jurisdiction
  tbl |>
    dplyr::group_by(.data$bird_id, .data$jurisdiction, .data$iso3) |>
    dplyr::summarise(
      n_fixes     = dplyr::n(),
      total_hours = sum(.data$step_duration_h, na.rm = TRUE),
      .groups     = "drop"
    ) |>
    dplyr::arrange(.data$bird_id, dplyr::desc(.data$total_hours))
}

# ------------------------------------------------------------------------------
# overlay_mpas()
# Measure overlap between seabird tracks or UDs and marine protected areas.
#
# Inputs:
#   track_data  — sf point object (track) OR sf polygon object (UD contour)
#   mpa_layer   — sf polygon layer of MPAs (from read_management_layers())
#                 expected columns: mpa_id, mpa_name, iucn_cat, status
#
# Output:
#   If track points:   track_data with new columns mpa_id, mpa_name, iucn_cat
#                      (NA where point falls outside any MPA)
#   If UD polygon:     tibble with mpa_id, mpa_name, overlap_km2, pct_ud_in_mpa
# ------------------------------------------------------------------------------
overlay_mpas <- function(track_data, mpa_layer) {

  assert_crs(track_data)
  assert_crs(mpa_layer)

  mpa_proj <- sf::st_transform(mpa_layer, sf::st_crs(track_data))

  geom_type <- unique(sf::st_geometry_type(track_data))

  # Point track: label each fix with the MPA it falls in (if any)
  if (all(geom_type %in% c("POINT", "MULTIPOINT"))) {

    joined <- sf::st_join(
      track_data,
      mpa_proj[, c("mpa_id", "mpa_name", "iucn_cat", "status")],
      join = sf::st_within,
      left = TRUE
    ) |>
      dplyr::mutate(
        in_mpa = !is.na(.data$mpa_id)
      )

    return(joined)
  }

  # UD polygon: compute area overlap with each MPA
  if (all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {

    # Use an equal-area projection for accurate km^2 calculations
    ea_crs <- "+proj=moll +datum=WGS84"
    ud_ea  <- sf::st_transform(track_data, ea_crs)
    mpa_ea <- sf::st_transform(mpa_proj,   ea_crs)

    ud_area_m2 <- as.numeric(sf::st_area(sf::st_union(ud_ea)))

    intersection <- sf::st_intersection(ud_ea, mpa_ea) |>
      dplyr::mutate(
        overlap_km2    = as.numeric(sf::st_area(.data$geometry)) / 1e6,
        pct_ud_in_mpa  = (.data$overlap_km2 * 1e6 / ud_area_m2) * 100
      ) |>
      sf::st_drop_geometry() |>
      dplyr::select("mpa_id", "mpa_name", "iucn_cat", "status",
                    "overlap_km2", "pct_ud_in_mpa") |>
      dplyr::arrange(dplyr::desc(.data$overlap_km2))

    return(intersection)
  }

  stop("track_data must contain POINT or POLYGON geometries.")
}

# ------------------------------------------------------------------------------
# calc_transboundary_movements()
# Identify trips or individuals that cross jurisdictional boundaries.
#
# Inputs:
#   track_data  — output of overlay_eez_abnj(), must have columns:
#                   bird_id, trip_id, Time (POSIXct), jurisdiction
#
# Output:
#   tibble with one row per trip:
#     bird_id, trip_id, n_jurisdictions, jurisdictions_visited (collapsed string),
#     is_transboundary (logical), crossed_into_abnj (logical)
# ------------------------------------------------------------------------------
calc_transboundary_movements <- function(track_data) {

  assert_required_cols(track_data,
                       c("bird_id", "trip_id", "Time", "jurisdiction"))

  tbl <- sf::st_drop_geometry(track_data)

  trip_summary <- tbl |>
    dplyr::group_by(.data$bird_id, .data$trip_id) |>
    dplyr::summarise(
      jurisdictions_visited = paste(unique(.data$jurisdiction), collapse = " | "),
      n_jurisdictions       = dplyr::n_distinct(.data$jurisdiction),
      crossed_into_abnj     = any(.data$jurisdiction == "ABNJ"),
      .groups               = "drop"
    ) |>
    dplyr::mutate(
      is_transboundary = .data$n_jurisdictions > 1
    ) |>
    dplyr::arrange(.data$bird_id, .data$trip_id)

  trip_summary
}

# ------------------------------------------------------------------------------
# overlay_priority_areas()
# Measure overlap between seabird tracks/UDs and candidate conservation
# priority polygons.
#
# Inputs:
#   track_data      — sf point or polygon object
#   priority_layer  — sf polygon layer (from read_management_layers())
#                     expected columns: area_id, area_name, priority_tier
#
# Output:
#   If track points: track_data with new columns area_id, area_name, priority_tier
#   If UD polygon:   tibble with area_id, area_name, priority_tier,
#                    overlap_km2, pct_ud_in_area
# ------------------------------------------------------------------------------
overlay_priority_areas <- function(track_data, priority_layer) {

  assert_crs(track_data)
  assert_crs(priority_layer)

  priority_proj <- sf::st_transform(priority_layer, sf::st_crs(track_data))

  geom_type <- unique(sf::st_geometry_type(track_data))

  # Point track
  if (all(geom_type %in% c("POINT", "MULTIPOINT"))) {

    joined <- sf::st_join(
      track_data,
      priority_proj[, c("area_id", "area_name", "priority_tier")],
      join = sf::st_within,
      left = TRUE
    ) |>
      dplyr::mutate(
        in_priority_area = !is.na(.data$area_id)
      )

    return(joined)
  }

  # UD polygon
  if (all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {

    ea_crs    <- "+proj=moll +datum=WGS84"
    ud_ea     <- sf::st_transform(track_data,    ea_crs)
    layer_ea  <- sf::st_transform(priority_proj, ea_crs)

    ud_area_m2 <- as.numeric(sf::st_area(sf::st_union(ud_ea)))

    intersection <- sf::st_intersection(ud_ea, layer_ea) |>
      dplyr::mutate(
        overlap_km2      = as.numeric(sf::st_area(.data$geometry)) / 1e6,
        pct_ud_in_area   = (.data$overlap_km2 * 1e6 / ud_area_m2) * 100
      ) |>
      sf::st_drop_geometry() |>
      dplyr::select("area_id", "area_name", "priority_tier",
                    "overlap_km2", "pct_ud_in_area") |>
      dplyr::arrange(dplyr::desc(.data$overlap_km2))

    return(intersection)
  }

  stop("track_data must contain POINT or POLYGON geometries.")
}
