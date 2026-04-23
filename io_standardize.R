#' raw <- read_gps_data("data/bird_tracks_2014.csv")
#'
#' # Auto-detect common column names
#' std <- standardize_gps_columns(raw)
#'
#' # Supply explicit column map for a non-standard file
#' std <- standardize_gps_columns(
#'   raw,
#'   col_map = c(
#'     bird_id   = "BirdCode",
#'     timestamp = "UTC_DateTime",
#'     lon       = "Lon_DD",
#'     lat       = "Lat_DD",
#'     colony    = "Site",
#'     device_id = "GLS_ID"
#'   )
#' )
#' }
standardize_gps_columns <- function(raw_data,
                                    col_map        = NULL,
                                    add_missing_cols = TRUE) {

  # ── 1. Resolve column map ────────────────────────────────────────────────
  if (is.null(col_map)) {
    col_map <- .default_col_map(names(raw_data))
  }

  .validate_col_map(col_map)

  # ── 2. Rename matched columns ────────────────────────────────────────────
  src_names <- names(raw_data)
  for (std_name in names(col_map)) {
    src_name <- col_map[[std_name]]
    if (src_name %in% src_names) {
      names(raw_data)[names(raw_data) == src_name] <- std_name
    } else {
      if (add_missing_cols) {
        warning(
          "Source column '", src_name, "' (mapped to '", std_name,
          "') not found in data. Adding as NA column."
        )
        raw_data[[std_name]] <- NA
      } else {
        stop(
          "Source column '", src_name, "' (mapped to '", std_name,
          "') not found in data."
        )
      }
    }
  }

  # ── 3. Parse timestamp to POSIXct UTC if not already ────────────────────
  if ("timestamp" %in% names(raw_data) && !inherits(raw_data$timestamp, "POSIXct")) {
    raw_data$timestamp <- .parse_timestamp(raw_data$timestamp)
  }

  # ── 4. Coerce lon/lat to numeric ─────────────────────────────────────────
  for (coord_col in c("lon", "lat")) {
    if (coord_col %in% names(raw_data)) {
      raw_data[[coord_col]] <- suppressWarnings(as.numeric(raw_data[[coord_col]]))
    }
  }

  # ── 5. Initialise downstream columns if absent ───────────────────────────
  # These are filled by later modules; we seed them here so the schema is
  # always complete from the import stage onward.
  schema_extras <- c("trip_id", "phase", "quality_flag")
  for (col in schema_extras) {
    if (!col %in% names(raw_data)) {
      raw_data[[col]] <- NA_character_
    }
  }

  # ── 6. Validate via Person 1 helper (if available) ───────────────────────
  if (exists("validate_gps_data", mode = "function")) {
    raw_data <- validate_gps_data(raw_data)
  } else {
    message(
      "validate_gps_data() not found — skipping schema validation. ",
      "Ensure Person 1's utils_schema.R is loaded."
    )
  }

  tibble::as_tibble(raw_data)
}


# ── Internal helpers (not exported) ─────────────────────────────────────────

#' Build a default column map by fuzzy-matching common aliases
#' @noRd
.default_col_map <- function(src_names) {

  # Standard name -> vector of common source aliases (case-insensitive)
  aliases <- list(
    bird_id   = c("bird_id", "birdid", "id", "individual", "bird", "ring",
                  "band_id", "animal_id", "ind_id"),
    timestamp = c("timestamp", "datetime", "date_time", "date", "time",
                  "utc", "utc_datetime", "gmt", "obs_time", "fix_time"),
    lon       = c("lon", "longitude", "long", "x", "lon_dd", "lng"),
    lat       = c("lat", "latitude", "y", "lat_dd"),
    colony    = c("colony", "site", "colony_name", "breeding_colony",
                  "deploy_site"),
    device_id = c("device_id", "deviceid", "tag_id", "tagid", "gls_id",
                  "logger_id", "ptt", "argos_id", "transmitter")
  )

  src_lower <- tolower(src_names)
  col_map   <- character(0)

  for (std_name in names(aliases)) {
    matched <- src_names[src_lower %in% aliases[[std_name]]]
    if (length(matched) > 0) {
      col_map[[std_name]] <- matched[[1]]   # take first match
    } else {
      # Will be handled by add_missing_cols logic in caller
      col_map[[std_name]] <- std_name
    }
  }

  col_map
}


#' Validate the user-supplied column map
#' @noRd
.validate_col_map <- function(col_map) {
  required <- c("bird_id", "timestamp", "lon", "lat")
  missing  <- setdiff(required, names(col_map))
  if (length(missing) > 0) {
    stop(
      "col_map must include mappings for: ",
      paste(required, collapse = ", "),
      ".\nMissing: ", paste(missing, collapse = ", ")
    )
  }
  invisible(TRUE)
}


#' Attempt to parse a character/numeric vector as POSIXct UTC
#' @noRd
.parse_timestamp <- function(x) {
  # Try ISO 8601 first, then common date-time patterns
  formats <- c(
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%dT%H:%M:%S",
    "%d/%m/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M:%S",
    "%Y-%m-%d"
  )

  parsed <- as.POSIXct(NA)
  for (fmt in formats) {
    attempt <- as.POSIXct(x, format = fmt, tz = "UTC")
    if (sum(!is.na(attempt)) > sum(!is.na(parsed))) {
      parsed <- attempt
    }
  }

  n_failed <- sum(is.na(parsed)) - sum(is.na(x))
  if (n_failed > 0) {
    warning(n_failed, " timestamp(s) could not be parsed and were set to NA.")
  }

  parsed
}
