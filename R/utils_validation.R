# =============================================================================
# utils_validation.R
# Infrastructure validation utilities
# =============================================================================


#' Assert Required Columns Exist
#'
#' Checks that all required columns are present in a data frame before
#' processing begins. Raises an informative error if any are missing.
#'
#' @param data   A data frame or tibble to check.
#' @param cols   A character vector of required column names.
#'
#' @return Invisibly returns TRUE if all columns are present.
#' @export
#'
#' @examples
#' df <- data.frame(x = 1, y = 2)
#' assert_required_cols(df, c("x", "y"))        # passes
#' assert_required_cols(df, c("x", "z"))        # errors
assert_required_cols <- function(data, cols) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.character(cols) || length(cols) == 0) {
    stop("`cols` must be a non-empty character vector.", call. = FALSE)
  }

  missing_cols <- setdiff(cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Required column(s) missing from data: ",
      paste(missing_cols, collapse = ", "),
      ".\nPresent columns: ",
      paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Assert Coordinate Reference System (CRS)
#'
#' Verifies that a spatial object has a CRS assigned and, optionally, that it
#' matches an expected CRS. Useful for catching mismatched projections before
#' spatial operations.
#'
#' @param x            An `sf` or `Spatial*` object.
#' @param expected_crs Optional. A CRS to compare against. Accepts anything
#'                     that \code{sf::st_crs()} can parse: an EPSG integer
#'                     (e.g. \code{4326}), a PROJ string, or an \code{crs}
#'                     object.
#'
#' @return Invisibly returns TRUE if all checks pass.
#' @export
#'
#' @examples
#' library(sf)
#' pt <- st_sf(geometry = st_sfc(st_point(c(0, 0)), crs = 4326))
#' assert_crs(pt)               # passes – CRS is set
#' assert_crs(pt, 4326)         # passes – CRS matches
#' assert_crs(pt, 32632)        # errors – CRS mismatch
assert_crs <- function(x, expected_crs = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for assert_crs().", call. = FALSE)
  }

  # Accept both sf and legacy Spatial* objects
  if (inherits(x, "Spatial")) {
    x <- sf::st_as_sf(x)
  }

  if (!inherits(x, "sf") && !inherits(x, "sfc")) {
    stop("`x` must be an sf or sfc object (or a Spatial* object).", call. = FALSE)
  }

  actual_crs <- sf::st_crs(x)

  if (is.na(actual_crs)) {
    stop(
      "Spatial object has no CRS assigned. ",
      "Set one with sf::st_set_crs() or sf::st_transform().",
      call. = FALSE
    )
  }

  if (!is.null(expected_crs)) {
    expected <- sf::st_crs(expected_crs)
    if (actual_crs != expected) {
      stop(
        "CRS mismatch.\n",
        "  Actual  : ", actual_crs$input, "\n",
        "  Expected: ", expected$input, "\n",
        "Use sf::st_transform() to reproject if needed.",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}


#' Assert Datetime Column is Parseable and Timezone-Aware
#'
#' Ensures that a timestamp column in a data frame can be parsed as
#' \code{POSIXct} and that every non-NA value carries an explicit, consistent
#' timezone (i.e. no naive/UTC-assumed datetimes).
#'
#' @param data          A data frame or tibble.
#' @param timestamp_col A string giving the name of the datetime column.
#'
#' @return Invisibly returns TRUE if all checks pass.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   ts = as.POSIXct("2024-01-01 12:00:00", tz = "America/Los_Angeles")
#' )
#' assert_datetime_tz(df, "ts")    # passes
#'
#' df_naive <- data.frame(ts = as.POSIXct("2024-01-01 12:00:00"))
#' assert_datetime_tz(df_naive, "ts")  # errors – no explicit tz
assert_datetime_tz <- function(data, timestamp_col) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.character(timestamp_col) || length(timestamp_col) != 1) {
    stop("`timestamp_col` must be a single string.", call. = FALSE)
  }

  # Ensure the column exists
  assert_required_cols(data, timestamp_col)

  col_data <- data[[timestamp_col]]

  # --- Parseability check ---
  # Accept POSIXct/POSIXlt directly; attempt coercion for character/numeric
  if (inherits(col_data, c("POSIXct", "POSIXlt"))) {
    parsed <- as.POSIXct(col_data)
  } else if (is.character(col_data) || is.numeric(col_data)) {
    parsed <- tryCatch(
      as.POSIXct(col_data),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    if (is.null(parsed) || all(is.na(parsed) & !is.na(col_data))) {
      stop(
        "Column '", timestamp_col, "' could not be parsed as datetime. ",
        "Check format and consider using as.POSIXct() with an explicit format string.",
        call. = FALSE
      )
    }
  } else {
    stop(
      "Column '", timestamp_col, "' has unsupported type: ", class(col_data)[1], ". ",
      "Expected POSIXct, POSIXlt, character, or numeric.",
      call. = FALSE
    )
  }

  # --- Timezone-awareness check ---
  tz_val <- attr(parsed, "tzone")

  # An empty string ("") means the system local timezone was assumed – treat
  # this as ambiguous/naive since it is not explicitly declared in the data.
  if (is.null(tz_val) || identical(tz_val, "")) {
    stop(
      "Column '", timestamp_col, "' has no explicit timezone. ",
      "Coerce with e.g. lubridate::with_tz() or force_tz(), ",
      "or use as.POSIXct(..., tz = 'UTC').",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
