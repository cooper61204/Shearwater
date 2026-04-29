# =============================================================================
# utils_schema.R
# Schema validation and coercion for track data
# =============================================================================


#' Validate GPS Track Data
#'
#' Performs comprehensive validation on raw or standardized track data,
#' checking required columns, coordinate ranges, timestamp integrity,
#' duplicates, and structural issues. Returns validated data or a
#' structured list of errors and warnings.
#'
#' @param data           A data frame or tibble (raw or standardized track data).
#' @param strict         Logical. If TRUE, treat warnings as errors. Default FALSE.
#' @param allow_na       Logical. If TRUE, allow NA values in coordinates/timestamps.
#'                       Default FALSE.
#'
#' @return A list with class "validated_gps_data" containing:
#'   - \code{data}:     The validated data frame (if no critical errors).
#'   - \code{valid}:    Logical indicating whether validation passed.
#'   - \code{errors}:   Character vector of critical errors (if any).
#'   - \code{warnings}: Character vector of warnings (if any).
#' @export
#'
#' @examples
#' df <- data.frame(
#'   track_id = c(1, 1, 2),
#'   timestamp = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:01:00",
#'                            "2024-01-01 11:00:00"), tz = "UTC"),
#'   latitude = c(37.7749, 37.7750, 37.7800),
#'   longitude = c(-122.4194, -122.4195, -122.4100)
#' )
#' result <- validate_gps_data(df)
#' print(result)
validate_gps_data <- function(data, strict = FALSE, allow_na = FALSE) {
  errors <- character(0)
  warnings <- character(0)

  # --- Check basic structure ---
  if (!is.data.frame(data)) {
    errors <- c(errors, "`data` must be a data frame or tibble.")
    return(structure(
      list(data = NULL, valid = FALSE, errors = errors, warnings = warnings),
      class = "validated_gps_data"
    ))
  }

  if (nrow(data) == 0) {
    warnings <- c(warnings, "Data frame is empty.")
  }

  # --- Check required columns ---
  required_cols <- c("track_id", "timestamp", "latitude", "longitude")
  tryCatch(
    assert_required_cols(data, required_cols),
    error = function(e) {
      errors <<- c(errors, conditionMessage(e))
    }
  )

  if (length(errors) > 0) {
    return(structure(
      list(data = NULL, valid = FALSE, errors = errors, warnings = warnings),
      class = "validated_gps_data"
    ))
  }

  # --- Validate timestamp column ---
  tryCatch(
    assert_datetime_tz(data, "timestamp"),
    error = function(e) {
      errors <<- c(errors, conditionMessage(e))
    }
  )

  if (length(errors) > 0) {
    return(structure(
      list(data = NULL, valid = FALSE, errors = errors, warnings = warnings),
      class = "validated_gps_data"
    ))
  }

  # --- Check coordinate ranges ---
  lat_vals <- data$latitude[!is.na(data$latitude)]
  lon_vals <- data$longitude[!is.na(data$longitude)]

  if (length(lat_vals) > 0) {
    if (any(lat_vals < -90 | lat_vals > 90, na.rm = TRUE)) {
      errors <- c(errors, "Latitude values outside [-90, 90] range.")
    }
  }

  if (length(lon_vals) > 0) {
    if (any(lon_vals < -180 | lon_vals > 180, na.rm = TRUE)) {
      errors <- c(errors, "Longitude values outside [-180, 180] range.")
    }
  }

  # --- Check for NA values in required fields ---
  na_counts <- colSums(is.na(data[, required_cols]))
  if (!allow_na && any(na_counts > 0)) {
    na_summary <- paste0(
      names(na_counts[na_counts > 0]),
      " (",
      na_counts[na_counts > 0],
      " NA)",
      collapse = "; "
    )
    errors <- c(errors, paste0("NA values found in required columns: ", na_summary))
  } else if (any(na_counts > 0)) {
    na_summary <- paste0(
      names(na_counts[na_counts > 0]),
      " (",
      na_counts[na_counts > 0],
      " NA)",
      collapse = "; "
    )
    warnings <- c(warnings, paste0("NA values found: ", na_summary))
  }

  # --- Check for duplicate rows ---
  dup_indices <- duplicated(data[, required_cols])
  if (any(dup_indices)) {
    n_dups <- sum(dup_indices)
    warnings <- c(warnings, paste0(n_dups, " duplicate row(s) detected."))
  }

  # --- Check timestamp ordering within tracks ---
  for (tid in unique(data$track_id)) {
    track_subset <- data[data$track_id == tid, ]
    ts <- track_subset$timestamp
    if (!all(is.na(ts)) && !all(diff(ts) >= 0, na.rm = TRUE)) {
      warnings <- c(warnings, paste0("Track ", tid, ": timestamps not in ascending order."))
    }
  }

  # --- Check for extreme time gaps (e.g., > 24 hours) ---
  for (tid in unique(data$track_id)) {
    track_subset <- data[data$track_id == tid, ]
    ts <- track_subset$timestamp
    if (length(ts) > 1 && !all(is.na(ts))) {
      diffs <- diff(ts)
      max_gap <- max(diffs, na.rm = TRUE)
      if (max_gap > 86400) {  # 24 hours in seconds
        gap_hours <- as.numeric(max_gap) / 3600
        warnings <- c(warnings, paste0(
          "Track ", tid, ": large time gap detected (",
          round(gap_hours, 1), " hours)."
        ))
      }
    }
  }

  # --- Check data types ---
  if (!inherits(data$track_id, c("numeric", "integer", "character"))) {
    warnings <- c(warnings, "track_id has unexpected type; consider integer or character.")
  }

  # --- Determine validity ---
  valid <- length(errors) == 0
  if (strict && length(warnings) > 0) {
    errors <- c(errors, paste0("Strict mode: treating warnings as errors."))
    valid <- FALSE
  }

  structure(
    list(data = if (valid) data else NULL, valid = valid, errors = errors, warnings = warnings),
    class = "validated_gps_data"
  )
}

#' @export
print.validated_gps_data <- function(x, ...) {
  cat("=== GPS Data Validation Result ===\n")
  cat("Valid:", x$valid, "\n")
  if (x$valid && !is.null(x$data)) {
    cat("Rows:", nrow(x$data), "\n")
    cat("Tracks:", length(unique(x$data$track_id)), "\n")
  }
  if (length(x$errors) > 0) {
    cat("\nErrors:\n")
    cat(paste0("  - ", x$errors, "\n"), sep = "")
  }
  if (length(x$warnings) > 0) {
    cat("\nWarnings:\n")
    cat(paste0("  - ", x$warnings, "\n"), sep = "")
  }
  invisible(x)
}


#' Coerce Data into Standard Track Table Format
#'
#' Converts a data frame or tibble into the package's standard internal
#' track-table format. Renames columns as needed, orders columns consistently,
#' and optionally validates the result.
#'
#' The standard format is:
#'   \code{track_id, timestamp, latitude, longitude, [optional columns]}
#'
#' Column mapping is flexible: if the input has columns like \code{id}, \code{time},
#' \code{lat}, \code{lon}, they will be renamed to the standard names.
#'
#' @param data        A data frame or tibble to coerce.
#' @param col_map     Optional named list/vector mapping input column names to
#'                    standard names. E.g. \code{list(time = "timestamp", lat = "latitude")}.
#'                    Defaults to common abbreviations.
#' @param validate    Logical. If TRUE, run \code{validate_gps_data()} on the
#'                    result. Default TRUE.
#' @param as_tibble   Logical. If TRUE, return as tibble; if FALSE, as data.frame.
#'                    Default TRUE.
#'
#' @return A coerced data frame or tibble in standard track format (or
#'         a \code{validated_gps_data} object if \code{validate = TRUE}).
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id = c(1, 1, 2),
#'   time = as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 10:01:00",
#'                       "2024-01-01 11:00:00"), tz = "UTC"),
#'   lat = c(37.7749, 37.7750, 37.7800),
#'   lon = c(-122.4194, -122.4195, -122.4100)
#' )
#' std <- coerce_track_tbl(df, validate = FALSE)
#' head(std)
coerce_track_tbl <- function(data, col_map = NULL, validate = TRUE, as_tibble = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  # --- Define default column mappings ---
  default_map <- list(
    id = "track_id",
    tid = "track_id",
    track = "track_id",
    time = "timestamp",
    ts = "timestamp",
    datetime = "timestamp",
    lat = "latitude",
    y = "latitude",
    lon = "longitude",
    lng = "longitude",
    x = "longitude"
  )

  # --- Merge user-provided mappings with defaults ---
  if (!is.null(col_map)) {
    if (!is.list(col_map)) {
      col_map <- as.list(col_map)
    }
    default_map <- c(default_map, col_map)
  }

  # --- Rename columns ---
  col_names <- names(data)
  for (old_name in intersect(names(default_map), col_names)) {
    new_name <- default_map[[old_name]]
    names(data)[names(data) == old_name] <- new_name
  }

  # --- Reorder columns: standard cols first, then others ---
  standard_cols <- c("track_id", "timestamp", "latitude", "longitude")
  standard_present <- intersect(standard_cols, names(data))
  other_cols <- setdiff(names(data), standard_present)

  col_order <- c(standard_present, other_cols)
  data <- data[, col_order, drop = FALSE]

  # --- Convert to tibble if requested ---
  if (as_tibble) {
    if (requireNamespace("tibble", quietly = TRUE)) {
      data <- tibble::as_tibble(data)
    } else {
      warning("Package 'tibble' not available; returning as data.frame.")
    }
  }

  # --- Validate if requested ---
  if (validate) {
    return(validate_gps_data(data))
  }

  data
}
