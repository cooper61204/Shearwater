#' Plot raw or cleaned bird tracks on a map
#'
#' Creates a map of GPS tracks for one or more individuals. Useful for quick
#' visual QA immediately after import or cleaning.
#' std <- standardize_gps_columns(read_gps_data("tracks.csv"))
#' plot_tracks(std, colony_coords = c(lon = -175.3, lat = -19.8))
#' }
plot_tracks <- function(track_data,
                        color_by      = "bird_id",
                        colony_coords = NULL,
                        title         = "Bird Tracks") {

  .check_track_cols(track_data, c("lon", "lat", color_by))

  p <- ggplot2::ggplot(track_data,
         ggplot2::aes(x = .data[["lon"]],
                      y = .data[["lat"]],
                      color = .data[[color_by]],
                      group = .data[[color_by]])) +
    ggplot2::geom_path(alpha = 0.6, linewidth = 0.4) +
    ggplot2::geom_point(size = 0.8, alpha = 0.4) +
    ggplot2::coord_quickmap() +
    ggplot2::labs(title = title, x = "Longitude", y = "Latitude",
                  color = color_by) +
    ggplot2::theme_minimal(base_size = 11)

  if (!is.null(colony_coords)) {
    colony_df <- data.frame(lon = colony_coords[["lon"]],
                            lat = colony_coords[["lat"]])
    p <- p +
      ggplot2::geom_point(data = colony_df,
                          ggplot2::aes(x = lon, y = lat),
                          inherit.aes = FALSE,
                          shape = 23, size = 4, fill = "red", color = "black") +
      ggplot2::annotate("text", x = colony_coords[["lon"]],
                        y = colony_coords[["lat"]] + 0.3,
                        label = "Colony", size = 3)
  }

  p
}


#' Map one or more trips with colony context
#'
#' Plots a subset of trips from a trip-segmented track table. Highlights
#' departure and return points and marks the colony.
#'
#' @param trip_data A track tibble containing a \code{trip_id} column
#'   (output of Person 4's \code{segment_trips()}).
#' @param trip_ids Character or integer vector of trip IDs to plot. If
#'   \code{NULL} (default), all trips are shown.
#' @param colony_coords Named numeric vector with \code{lon} and \code{lat}.
#' @param color_by Character. Column to color-code trips by. Default:
#'   \code{"trip_id"}.
#' @param title Character. Plot title.
#'
#' @return A \code{ggplot2} plot object.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_trip_map(segmented_tracks,
#'              trip_ids      = c(1, 2, 3),
#'              colony_coords = c(lon = -175.3, lat = -19.8))
#' }
plot_trip_map <- function(trip_data,
                          trip_ids      = NULL,
                          colony_coords = NULL,
                          color_by      = "trip_id",
                          title         = "Trip Map") {

  .check_track_cols(trip_data, c("lon", "lat", "trip_id"))

  if (!is.null(trip_ids)) {
    trip_data <- trip_data[trip_data$trip_id %in% trip_ids, ]
    if (nrow(trip_data) == 0) {
      stop("No rows found for the supplied trip_ids.")
    }
  }

  p <- ggplot2::ggplot(trip_data,
         ggplot2::aes(x = .data[["lon"]],
                      y = .data[["lat"]],
                      color = as.factor(.data[[color_by]]),
                      group = as.factor(.data[[color_by]]))) +
    ggplot2::geom_path(linewidth = 0.5, alpha = 0.8) +
    ggplot2::geom_point(size = 1, alpha = 0.5) +
    ggplot2::coord_quickmap() +
    ggplot2::labs(title = title, x = "Longitude", y = "Latitude",
                  color = color_by) +
    ggplot2::theme_minimal(base_size = 11)

  if (!is.null(colony_coords)) {
    colony_df <- data.frame(lon = colony_coords[["lon"]],
                            lat = colony_coords[["lat"]])
    p <- p +
      ggplot2::geom_point(data = colony_df,
                          ggplot2::aes(x = lon, y = lat),
                          inherit.aes = FALSE,
                          shape = 23, size = 5, fill = "red", color = "black") +
      ggplot2::annotate("text", x = colony_coords[["lon"]],
                        y = colony_coords[["lat"]] + 0.3,
                        label = "Colony", size = 3)
  }

  p
}


#' Plot spatial density of seabird use
#'
#' Creates a 2-D density (heatmap-style) plot showing where birds spend the
#' most time across the study area. Useful as a quick alternative to a full
#' kernel UD (Person 6) during early QA.
#'
#' @param track_data A standardized or cleaned track tibble.
#' @param bins Integer. Number of 2-D histogram bins per axis. Default: 100.
#' @param title Character. Plot title.
#'
#' @return A \code{ggplot2} plot object.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_density_map(cleaned_tracks)
#' }
plot_density_map <- function(track_data,
                             bins  = 100,
                             title = "Seabird Use Density") {

  .check_track_cols(track_data, c("lon", "lat"))

  ggplot2::ggplot(track_data,
                  ggplot2::aes(x = .data[["lon"]], y = .data[["lat"]])) +
    ggplot2::stat_bin_2d(bins = bins,
                         ggplot2::aes(fill = ggplot2::after_stat(count))) +
    ggplot2::scale_fill_viridis_c(option = "plasma", name = "Fix count") +
    ggplot2::coord_quickmap() +
    ggplot2::labs(title = title, x = "Longitude", y = "Latitude") +
    ggplot2::theme_minimal(base_size = 11)
}


# ── Internal helpers ─────────────────────────────────────────────────────────

#' Check that required columns are present
#' @noRd
.check_track_cols <- function(data, required_cols) {
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("track_data is missing required columns: ",
         paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}
