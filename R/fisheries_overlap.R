#' Join track locations to a fisheries grid or polygon layer
#'
#' Spatially matches bird locations or utilization data to fisheries cells
#' or polygons.
#'
#' @param track_data An sf object of bird track points or polygons.
#' @param fisheries_data An sf object of fisheries cells or polygons.
#' @param join_type Character string. One of "intersects", "within", or "nearest".
#'
#' @return An sf object containing joined track-fisheries data.
#' @export
join_tracks_to_fishing_grid <- function(track_data,
                                        fisheries_data,
                                        join_type = c("intersects", "within", "nearest")) {
  join_type <- match.arg(join_type)

  if (!inherits(track_data, "sf")) {
    stop("track_data must be an sf object.")
  }

  if (!inherits(fisheries_data, "sf")) {
    stop("fisheries_data must be an sf object.")
  }

  assert_crs(track_data, fisheries_data)

  if (join_type == "intersects") {
    out <- sf::st_join(track_data, fisheries_data, join = sf::st_intersects, left = FALSE)
  } else if (join_type == "within") {
    out <- sf::st_join(track_data, fisheries_data, join = sf::st_within, left = FALSE)
  } else {
    nearest_idx <- sf::st_nearest_feature(track_data, fisheries_data)
    out <- cbind(track_data, sf::st_drop_geometry(fisheries_data[nearest_idx, , drop = FALSE]))
  }

  out
}
