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

#' Calculate a fisheries risk index
#'
#' Creates a fisheries risk score based on overlap intensity and gear-specific weighting.
#'
#' @param overlap_data A data frame of overlap metrics.
#' @param overlap_col Character string. Name of the overlap metric column.
#' @param gear_col Character string. Name of the gear column.
#' @param gear_weights Either a named numeric vector or a data frame with gear weights.
#' @param scale_01 Logical. If TRUE, rescales the risk index to [0, 1].
#'
#' @return A data frame with a risk index column.
#' @export
calc_risk_index <- function(overlap_data,
                            overlap_col = "total_overlap",
                            gear_col = "gear",
                            gear_weights,
                            scale_01 = TRUE) {
  if (!is.data.frame(overlap_data)) {
    stop("overlap_data must be a data frame.")
  }

  if (!overlap_col %in% names(overlap_data)) {
    stop("overlap_col not found in overlap_data.")
  }

  if (!gear_col %in% names(overlap_data)) {
    stop("gear_col not found in overlap_data.")
  }

  if (missing(gear_weights)) {
    stop("gear_weights must be provided.")
  }

  weights_df <- NULL

  if (is.numeric(gear_weights) && !is.null(names(gear_weights))) {
    weights_df <- data.frame(
      gear_tmp = names(gear_weights),
      gear_weight = as.numeric(gear_weights),
      stringsAsFactors = FALSE
    )
    names(weights_df)[1] <- gear_col
  } else if (is.data.frame(gear_weights)) {
    if (!(gear_col %in% names(gear_weights))) {
      stop("gear_weights data frame must contain the gear column.")
    }
    if (!("gear_weight" %in% names(gear_weights))) {
      stop("gear_weights data frame must contain a gear_weight column.")
    }
    weights_df <- gear_weights
  } else {
    stop("gear_weights must be a named numeric vector or data frame.")
  }

  out <- merge(overlap_data, weights_df, by = gear_col, all.x = TRUE)
  out$gear_weight[is.na(out$gear_weight)] <- 1

  out$risk_index <- suppressWarnings(as.numeric(out[[overlap_col]])) * out$gear_weight

  if (scale_01) {
    rng <- range(out$risk_index, na.rm = TRUE)
    if (is.finite(rng[1]) && is.finite(rng[2]) && rng[1] != rng[2]) {
      out$risk_index_scaled <- (out$risk_index - rng[1]) / (rng[2] - rng[1])
    } else {
      out$risk_index_scaled <- 0
    }
  }

  out
}

#' Calculate diel overlap between bird activity and fisheries effort
#'
#' Measures overlap by time of day to assess diel coincidence between birds
#' and fisheries.
#'
#' @param joined_data A data frame or sf object containing already joined
#'   bird-fisheries data.
#' @param track_id_col Character string. Name of the bird or track ID column.
#' @param track_diel_col Character string. Name of the bird diel label column.
#' @param fisheries_diel_col Character string. Name of the fisheries diel label column.
#' @param effort_col Character string. Name of the fisheries effort column.
#'
#' @return A data frame summarizing diel overlap.
#' @export
calc_diel_overlap <- function(joined_data,
                              track_id_col = "track_id",
                              track_diel_col = "diel_period.x",
                              fisheries_diel_col = "diel_period.y",
                              effort_col = "effort_std") {
  if (!is.data.frame(joined_data)) {
    stop("joined_data must be a data frame or sf object.")
  }

  dat <- if (inherits(joined_data, "sf")) sf::st_drop_geometry(joined_data) else joined_data

  needed <- c(track_id_col, track_diel_col, fisheries_diel_col, effort_col)
  missing_cols <- needed[!needed %in% names(dat)]

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  dat <- dat[dat[[track_diel_col]] == dat[[fisheries_diel_col]], , drop = FALSE]

  split_dat <- split(dat, list(dat[[track_id_col]], dat[[track_diel_col]]), drop = TRUE)

  out <- lapply(split_dat, function(df) {
    vals <- suppressWarnings(as.numeric(df[[effort_col]]))
    data.frame(
      n_overlap_records = sum(!is.na(vals)),
      diel_overlap = sum(vals, na.rm = TRUE),
      mean_diel_overlap = mean(vals, na.rm = TRUE)
    )
  })

  out <- do.call(rbind, out)
  keys <- unique(dat[c(track_id_col, track_diel_col)])
  rownames(out) <- NULL
  names(keys)[2] <- "diel_period"

  cbind(keys, out)
}
