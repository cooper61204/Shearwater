#' Regularize GPS tracks to a common time interval
#'
#' Resamples each individual's track to a uniform time step for comparability
#' across birds. The output is a regular time grid per bird with NA coordinates
#' wherever no fix fell within the tolerance window around each grid point.
#'
#' @param df A data.frame with tracking data
#' @param id_col Column name for bird ID
#' @param date_col Column name for date
#' @param time_col Column name for time
#' @param lat_col Column name for latitude
#' @param lon_col Column name for longitude
#' @param interval_minutes Target time interval in minutes for the regularized grid
#' @param tolerance_minutes Half-window in minutes around each grid point within
#'   which an observed fix is considered a match. Defaults to half the interval.
#'
#' @return A data.frame with one row per bird per regular time step, containing:
#'   \itemize{
#'     \item The id column
#'     \item \code{datetime_regular} – the regular grid timestamp (POSIXct UTC)
#'     \item Latitude and longitude (NA where no fix matched the grid point)
#'     \item \code{is_observed} – TRUE if a real fix was matched, FALSE if grid point is empty
#'   }
#' @export
regularize_tracks <- function(
    df,
    id_col           = "ID",
    date_col         = "Date",
    time_col         = "Time",
    lat_col          = "Latitude",
    lon_col          = "Longitude",
    interval_minutes = 30,
    tolerance_minutes = NULL
) {

  # ---- 1. Validate columns ----
  required <- c(id_col, date_col, time_col, lat_col, lon_col)
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(paste("Missing columns:", paste(missing, collapse = ", ")))
  }

  if (is.null(tolerance_minutes)) {
    tolerance_minutes <- interval_minutes / 2
  }

  # ---- 2. Parse datetime ----
  df$..datetime <- as.POSIXct(
    paste(df[[date_col]], df[[time_col]]),
    format = "%m/%d/%Y %H:%M:%S",
    tz     = "UTC"
  )

  if (all(is.na(df$..datetime))) {
    stop("datetime parsing failed — check date_col and time_col formats")
  }

  lat <- suppressWarnings(as.numeric(df[[lat_col]]))
  lon <- suppressWarnings(as.numeric(df[[lon_col]]))

  tolerance_secs <- tolerance_minutes * 60
  interval_secs  <- interval_minutes  * 60

  # ---- 3. Regularize per individual ----
  ids     <- unique(df[[id_col]])
  results <- vector("list", length(ids))

  for (i in seq_along(ids)) {

    bird    <- ids[i]
    idx     <- df[[id_col]] == bird
    bird_dt <- df$..datetime[idx]
    bird_lat <- lat[idx]
    bird_lon <- lon[idx]

    # Build regular grid spanning this bird's track
    t_start <- trunc(min(bird_dt, na.rm = TRUE), units = "mins")
    t_end   <- max(bird_dt, na.rm = TRUE)
    grid    <- seq(t_start, t_end, by = interval_secs)

    # Match each grid point to the nearest observed fix within tolerance
    grid_lat <- rep(NA_real_, length(grid))
    grid_lon <- rep(NA_real_, length(grid))
    observed <- rep(FALSE,    length(grid))

    for (j in seq_along(grid)) {
      diffs <- abs(as.numeric(difftime(bird_dt, grid[j], units = "secs")))
      nearest <- which.min(diffs)
      if (length(nearest) > 0 && diffs[nearest] <= tolerance_secs) {
        grid_lat[j] <- bird_lat[nearest]
        grid_lon[j] <- bird_lon[nearest]
        observed[j] <- TRUE
      }
    }

    results[[i]] <- data.frame(
      id               = bird,
      datetime_regular = grid,
      lat              = grid_lat,
      lon              = grid_lon,
      is_observed      = observed,
      stringsAsFactors = FALSE
    )
    names(results[[i]])[1] <- id_col
  }

  # ---- 4. Combine and return ----
  out <- do.call(rbind, results)
  rownames(out) <- NULL
  return(out)
}


#' Interpolate small gaps in regularized GPS tracks
#'
#' Fills NA positions in a regularized track using linear interpolation,
#' but only across gaps smaller than max_gap_minutes. Larger gaps are left
#' as NA to avoid generating unreliable positions across long absences.
#'
#' This function expects output from \code{regularize_tracks()} as input.
#'
#' @param df A regularized track data.frame (output of \code{regularize_tracks()})
#' @param id_col Column name for bird ID
#' @param datetime_col Column name for the regular grid timestamps
#' @param lat_col Column name for latitude
#' @param lon_col Column name for longitude
#' @param max_gap_minutes Maximum gap length in minutes to interpolate across.
#'   Gaps longer than this are left as NA.
#'
#' @return The input data.frame with two additional columns:
#'   \itemize{
#'     \item Latitude and longitude filled by interpolation where gap <= \code{max_gap_minutes}
#'     \item \code{is_interpolated} – TRUE where a position was gap-filled
#'   }
#' @export
interpolate_tracks <- function(
    df,
    id_col          = "ID",
    datetime_col    = "datetime_regular",
    lat_col         = "lat",
    lon_col         = "lon",
    max_gap_minutes = 60
) {

  # ---- 1. Validate columns ----
  required <- c(id_col, datetime_col, lat_col, lon_col)
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(paste("Missing columns:", paste(missing, collapse = ", ")))
  }

  if (!inherits(df[[datetime_col]], "POSIXct")) {
    stop(paste(datetime_col, "must be POSIXct — pass output from regularize_tracks() directly"))
  }

  max_gap_secs <- max_gap_minutes * 60

  df$is_interpolated <- FALSE

  # ---- 2. Interpolate per individual ----
  ids <- unique(df[[id_col]])

  for (bird in ids) {

    idx  <- which(df[[id_col]] == bird)
    lats <- df[[lat_col]][idx]
    lons <- df[[lon_col]][idx]
    dts  <- as.numeric(df[[datetime_col]][idx])

    na_pos <- which(is.na(lats))
    if (length(na_pos) == 0) next

    # Identify contiguous NA runs
    runs      <- rle(is.na(lats))
    run_end   <- cumsum(runs$lengths)
    run_start <- c(1, run_end[-length(run_end)] + 1)

    for (k in seq_along(runs$lengths)) {

      if (!runs$values[k]) next   # not an NA run

      gap_idx   <- run_start[k]:run_end[k]
      before    <- run_start[k] - 1
      after_pos <- run_end[k]   + 1

      # Need valid anchors on both sides to interpolate
      if (before < 1 || after_pos > length(lats)) next
      if (is.na(lats[before]) || is.na(lats[after_pos])) next

      gap_duration <- abs(dts[after_pos] - dts[before])
      if (gap_duration > max_gap_secs) next

      # Linear interpolation
      t_before <- dts[before]
      t_after  <- dts[after_pos]
      t_gap    <- dts[gap_idx]
      w        <- (t_gap - t_before) / (t_after - t_before)

      df[[lat_col]][idx[gap_idx]] <- lats[before] + w * (lats[after_pos] - lats[before])
      df[[lon_col]][idx[gap_idx]] <- lons[before] + w * (lons[after_pos] - lons[before])
      df$is_interpolated[idx[gap_idx]] <- TRUE
    }
  }

  return(df)
}


#' Summarize data gaps and interpolation burden per individual
#'
#' Produces a per-bird QA summary table describing missingness, gap structure,
#' and how much of each track was gap-filled by interpolation. Intended for
#' QA reporting after running \code{regularize_tracks()} and
#' \code{interpolate_tracks()}.
#'
#' @param df A track data.frame after interpolation (output of \code{interpolate_tracks()})
#' @param id_col Column name for bird ID
#' @param datetime_col Column name for regular grid timestamps
#' @param lat_col Column name for latitude (used to detect missing positions)
#' @param is_observed_col Column name for the observed fix flag from regularize_tracks()
#' @param is_interpolated_col Column name for the interpolation flag from interpolate_tracks()
#'
#' @return A data.frame with one row per bird containing:
#'   \itemize{
#'     \item \code{n_fixes_total} – total grid points for this individual
#'     \item \code{n_observed} – fixes matched from raw data
#'     \item \code{n_interpolated} – fixes filled by interpolation
#'     \item \code{n_missing} – fixes still NA after interpolation
#'     \item \code{pct_observed} – percentage of grid observed
#'     \item \code{pct_interpolated} – percentage of grid interpolated
#'     \item \code{pct_missing} – percentage of grid still missing
#'     \item \code{n_gaps} – number of distinct NA runs in the raw data
#'     \item \code{max_gap_minutes} – longest raw gap in minutes
#'     \item \code{mean_gap_minutes} – mean raw gap length in minutes
#'     \item \code{track_start} – first grid timestamp
#'     \item \code{track_end} – last grid timestamp
#'     \item \code{track_duration_hours} – total track duration in hours
#'   }
#' @export
summarize_data_gaps <- function(
    df,
    id_col              = "ID",
    datetime_col        = "datetime_regular",
    lat_col             = "lat",
    is_observed_col     = "is_observed",
    is_interpolated_col = "is_interpolated"
) {

  # ---- 1. Validate columns ----
  required <- c(id_col, datetime_col, lat_col, is_observed_col, is_interpolated_col)
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(paste("Missing columns:", paste(missing, collapse = ", ")))
  }

  ids     <- unique(df[[id_col]])
  results <- vector("list", length(ids))

  for (i in seq_along(ids)) {

    bird <- ids[i]
    idx  <- df[[id_col]] == bird

    dts          <- df[[datetime_col]][idx]
    lats         <- df[[lat_col]][idx]
    is_obs       <- df[[is_observed_col]][idx]
    is_interp    <- df[[is_interpolated_col]][idx]

    n_total      <- sum(idx)
    n_observed   <- sum(is_obs,    na.rm = TRUE)
    n_interp     <- sum(is_interp, na.rm = TRUE)
    n_missing    <- sum(is.na(lats))

    # ---- Gap structure from the pre-interpolation NA pattern ----
    # Reconstruct original missingness: not observed and not interpolated
    was_missing <- !is_obs & !is_interp

    runs         <- rle(was_missing)
    gap_lengths  <- runs$lengths[runs$values]
    n_gaps       <- length(gap_lengths)

    # Convert gap lengths (in grid steps) to minutes using median spacing
    if (n_total > 1) {
      median_step_mins <- median(
        as.numeric(diff(dts), units = "mins"),
        na.rm = TRUE
      )
    } else {
      median_step_mins <- NA_real_
    }

    gap_lengths_mins <- gap_lengths * median_step_mins

    results[[i]] <- data.frame(
      id                  = bird,
      n_fixes_total       = n_total,
      n_observed          = n_observed,
      n_interpolated      = n_interp,
      n_missing           = n_missing,
      pct_observed        = round(100 * n_observed / n_total,  1),
      pct_interpolated    = round(100 * n_interp   / n_total,  1),
      pct_missing         = round(100 * n_missing  / n_total,  1),
      n_gaps              = n_gaps,
      max_gap_minutes     = if (n_gaps > 0) round(max(gap_lengths_mins),  1) else 0,
      mean_gap_minutes    = if (n_gaps > 0) round(mean(gap_lengths_mins), 1) else 0,
      track_start         = min(dts, na.rm = TRUE),
      track_end           = max(dts, na.rm = TRUE),
      track_duration_hours = round(
        as.numeric(difftime(max(dts, na.rm = TRUE), min(dts, na.rm = TRUE), units = "hours")),
        2
      ),
      stringsAsFactors = FALSE
    )
    names(results[[i]])[1] <- id_col
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  return(out)
}
