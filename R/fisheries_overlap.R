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

#' @param joined_data An sf object or data frame produced by
#'   join_tracks_to_fishing_grid().
#' @param track_id_col Character string. Name of the bird or track ID column.
#' @param effort_col Character string. Name of the fisheries effort column.
#' @param gear_col Character string. Name of the gear column.
#' @param cell_id_col Optional character string. Name of the spatial cell ID column.
#'
#' @return A data frame of overlap metrics.
#' @export
calc_fisheries_overlap <- function(joined_data,
                                   track_id_col = "track_id",
                                   effort_col = "effort_std",
                                   gear_col = "gear",
                                   cell_id_col = NULL) {
  if (!is.data.frame(joined_data)) {
    stop("joined_data must be a data frame or sf object.")
  }

  if (!track_id_col %in% names(joined_data)) {
    stop("track_id_col not found in joined_data.")
  }

  if (!effort_col %in% names(joined_data)) {
    stop("effort_col not found in joined_data.")
  }

  if (!gear_col %in% names(joined_data)) {
    stop("gear_col not found in joined_data.")
  }

  dat <- if (inherits(joined_data, "sf")) sf::st_drop_geometry(joined_data) else joined_data

  group_cols <- c(track_id_col, gear_col)
  if (!is.null(cell_id_col)) {
    if (!cell_id_col %in% names(dat)) {
      stop("cell_id_col not found in joined_data.")
    }
    group_cols <- c(group_cols, cell_id_col)
  }

  split_dat <- split(dat, dat[group_cols], drop = TRUE)

  out <- lapply(split_dat, function(df) {
    vals <- suppressWarnings(as.numeric(df[[effort_col]]))
    data.frame(
      n_overlap_records = sum(!is.na(vals)),
      total_overlap = sum(vals, na.rm = TRUE),
      mean_overlap = mean(vals, na.rm = TRUE),
      max_overlap = max(vals, na.rm = TRUE)
    )
  })

  out <- do.call(rbind, out)
  keys <- unique(dat[group_cols])
  rownames(out) <- NULL

  cbind(keys, out)
}
