# =============================================================================
# jurisdiction_overlay.R
# Read and manage spatial jurisdiction and conservation layers
# =============================================================================


#' Read Management Layers (EEZ, ABNJ, MPA, Conservation Priority)
#'
#' Imports spatial management layers from disk, validates their CRS,
#' and returns them as a named list of sf objects. Supports multiple
#' input formats (GeoPackage, GeoJSON, Shapefile) and automatically
#' reprojects to a common CRS if needed.
#'
#' @param eez_path             Character. Path to EEZ (Exclusive Economic Zone) layer.
#'                             Accepts .gpkg, .geojson, .shp, or directory.
#' @param abnj_path            Character. Optional path to ABNJ (Area Beyond National
#'                             Jurisdiction) layer. If NULL, skipped.
#' @param mpa_path             Character. Optional path to MPA (Marine Protected Area)
#'                             layer. If NULL, skipped.
#' @param conservation_path    Character. Optional path to conservation-priority layer.
#'                             If NULL, skipped.
#' @param target_crs           Numeric or character. Target CRS to reproject all layers to.
#'                             Default 4326 (WGS84). Pass NULL to skip reprojection.
#' @param layer_name           Character. For multi-layer files (e.g., .gpkg),
#'                             specify which layer to read. Default NULL (read first/default).
#' @param simplify             Logical. If TRUE, simplify geometries to reduce file size.
#'                             Default FALSE.
#' @param simplify_tolerance   Numeric. Tolerance for simplification in degrees
#'                             (if simplify = TRUE). Default 0.01.
#' @param validate             Logical. If TRUE, run basic sf validity checks.
#'                             Default TRUE.
#'
#' @return A named list of sf objects:
#'   - \code{eez}:             EEZ layer (required).
#'   - \code{abnj}:            ABNJ layer (if provided).
#'   - \code{mpa}:             MPA layer (if provided).
#'   - \code{conservation}:    Conservation-priority layer (if provided).
#'
#'   Each object has attributes:
#'   - \code{source_path}:     Original file path.
#'   - \code{crs}:             Coordinate reference system.
#'   - \code{n_features}:      Number of features.
#'   - \code{read_date}:       Timestamp when layer was read.
#'
#' @details
#'
#' Column naming: If a layer has a column typically used for jurisdiction names
#' (e.g., \code{GEONAME}, \code{TERRITORY1}, \code{name}), it will be preserved
#' as-is. No renaming is applied unless explicitly requested.
#'
#' Reprojection: All layers are reprojected to \code{target_crs} (default WGS84).
#' This ensures spatial operations across layers work correctly.
#'
#' Simplification: For large polygon layers, setting \code{simplify = TRUE} can
#' significantly reduce file size in memory and on disk. Use with caution as
#' it may introduce small artifacts along boundaries.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read EEZ only
#' layers <- read_management_layers(
#'   eez_path = "data/eez_boundaries.gpkg"
#' )
#'
#' # Read EEZ and MPA, reproject to Web Mercator
#' layers <- read_management_layers(
#'   eez_path = "data/eez_boundaries.gpkg",
#'   mpa_path = "data/mpas.shp",
#'   target_crs = 3857
#' )
#'
#' # Read all layers with simplification
#' layers <- read_management_layers(
#'   eez_path = "data/eez_boundaries.gpkg",
#'   abnj_path = "data/abnj.geojson",
#'   mpa_path = "data/mpas.gpkg",
#'   conservation_path = "data/conservation_priority.gpkg",
#'   simplify = TRUE,
#'   simplify_tolerance = 0.05
#' )
#' }
read_management_layers <- function(
    eez_path,
    abnj_path = NULL,
    mpa_path = NULL,
    conservation_path = NULL,
    target_crs = 4326,
    layer_name = NULL,
    simplify = FALSE,
    simplify_tolerance = 0.01,
    validate = TRUE) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for read_management_layers().", call. = FALSE)
  }

  # --- Helper function to read a single layer ---
  read_single_layer <- function(path, layer_nm = NULL, label = "Layer") {
    if (is.null(path)) {
      return(NULL)
    }

    if (!file.exists(path)) {
      stop(paste0(label, " file not found: ", path), call. = FALSE)
    }

    # Determine file type and read accordingly
    ext <- tolower(tools::file_ext(path))

    layer_data <- tryCatch(
      {
        if (ext %in% c("gpkg", "geopackage")) {
          sf::st_read(path, layer = layer_nm, quiet = TRUE)
        } else if (ext == "geojson") {
          sf::st_read(path, quiet = TRUE)
        } else if (ext == "shp") {
          # For shapefiles, sf::st_read reads the .shp and handles the supporting files
          sf::st_read(path, quiet = TRUE)
        } else if (ext %in% c("gml", "kml")) {
          sf::st_read(path, quiet = TRUE)
        } else {
          stop(paste0(
            label, ": unsupported file format '.", ext, "'. ",
            "Supported: .gpkg, .geojson, .shp, .gml, .kml"
          ), call. = FALSE)
        }
      },
      error = function(e) {
        stop(paste0(label, " failed to read: ", conditionMessage(e)), call. = FALSE)
      }
    )

    layer_data
  }

  # --- Helper function to validate and process a layer ---
  process_layer <- function(data, label = "Layer", path = NULL) {
    if (is.null(data)) {
      return(NULL)
    }

    # Validate CRS
    tryCatch(
      assert_crs(data),
      error = function(e) {
        stop(paste0(label, ": ", conditionMessage(e)), call. = FALSE)
      }
    )

    # Reproject if needed
    if (!is.null(target_crs)) {
      current_crs <- sf::st_crs(data)
      target_crs_obj <- sf::st_crs(target_crs)

      if (current_crs != target_crs_obj) {
        data <- sf::st_transform(data, target_crs_obj)
      }
    }

    # Simplify if requested
    if (simplify) {
      data <- sf::st_simplify(data, dTolerance = simplify_tolerance)
    }

    # Validate spatial validity if requested
    if (validate) {
      invalid_geoms <- !sf::st_is_valid(data)
      if (any(invalid_geoms)) {
        n_invalid <- sum(invalid_geoms)
        warning(paste0(
          label, ": ", n_invalid, " invalid geometry/geometries detected. ",
          "Consider running sf::st_make_valid()."
        ))
      }
    }

    # Attach metadata
    attr(data, "source_path") <- path
    attr(data, "crs") <- sf::st_crs(data)
    attr(data, "n_features") <- nrow(data)
    attr(data, "read_date") <- Sys.time()

    data
  }

  # --- Main: Read all provided layers ---
  message("Reading management layers...")

  eez <- read_single_layer(eez_path, layer_name, "EEZ")
  eez <- process_layer(eez, "EEZ", eez_path)

  abnj <- read_single_layer(abnj_path, layer_name, "ABNJ")
  abnj <- process_layer(abnj, "ABNJ", abnj_path)

  mpa <- read_single_layer(mpa_path, layer_name, "MPA")
  mpa <- process_layer(mpa, "MPA", mpa_path)

  conservation <- read_single_layer(conservation_path, layer_name, "Conservation")
  conservation <- process_layer(conservation, "Conservation", conservation_path)

  # --- Assemble and return result ---
  result <- list(
    eez = eez,
    abnj = abnj,
    mpa = mpa,
    conservation = conservation
  )

  # Remove NULL entries
  result <- result[!sapply(result, is.null)]

  # Add class and metadata
  class(result) <- c("management_layers", "list")
  attr(result, "target_crs") <- target_crs
  attr(result, "read_date") <- Sys.time()
  attr(result, "n_layers") <- length(result)

  message("Successfully read ", length(result), " management layer(s).")

  result
}


#' Print Management Layers Object
#'
#' @param x A management_layers object.
#' @param ... Additional arguments (unused).
#'
#' @export
print.management_layers <- function(x, ...) {
  cat("=== Management Layers ===\n")
  cat("Target CRS:", attr(x, "target_crs"), "\n")
  cat("Layers loaded:", attr(x, "n_layers"), "\n\n")

  for (layer_name in names(x)) {
    layer <- x[[layer_name]]
    cat(toupper(layer_name), ":\n")
    cat("  Features:", nrow(layer), "\n")
    cat("  Geometry type:", sf::st_geometry_type(layer)[1], "\n")
    cat("  CRS:", sf::st_crs(layer)$epsg, "\n")
    cat("  Columns:", paste(names(layer)[-ncol(layer)], collapse = ", "), "\n\n")
  }

  invisible(x)
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
