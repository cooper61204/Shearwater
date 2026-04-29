#' Assert that two spatial objects share the same CRS
#'
#' Checks whether two sf objects have the same coordinate reference system.
#'
#' @param x An sf object.
#' @param y An sf object.
#'
#' @return Invisible TRUE if CRS matches.
#' @export
assert_crs <- function(x, y) {
  if (!inherits(x, "sf") || !inherits(y, "sf")) {
    stop("Both x and y must be sf objects.")
  }

  if (is.na(sf::st_crs(x))) {
    stop("x has no CRS.")
  }

  if (is.na(sf::st_crs(y))) {
    stop("y has no CRS.")
  }

  if (sf::st_crs(x) != sf::st_crs(y)) {
    stop("CRS mismatch between x and y.")
  }

  invisible(TRUE)
}

#' Label observations as day or night
#'
#' Adds a diel period label based on hour of day.
#'
#' @param data A data frame.
#' @param datetime_col Character string. Name of the datetime column.
#' @param day_start Numeric. Hour at which day begins.
#' @param night_start Numeric. Hour at which night begins.
#'
#' @return The input data frame with a new diel_period column.
#' @export
label_day_night_period <- function(data,
                                   datetime_col = "timestamp",
                                   day_start = 6,
                                   night_start = 18) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  if (!datetime_col %in% names(data)) {
    stop("datetime_col not found in data.")
  }

  dt <- as.POSIXct(data[[datetime_col]], tz = "UTC")

  if (all(is.na(dt))) {
    stop("Could not parse datetime column as POSIXct.")
  }

  hrs <- as.integer(format(dt, "%H"))

  data$diel_period <- ifelse(
    hrs >= day_start & hrs < night_start,
    "day",
    "night"
  )

  data
}
