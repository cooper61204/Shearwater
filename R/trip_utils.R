summarize_trips <- function(track,
                            bird_id_col = "track_id",
                            trip_id_col = "trip_id",
                            datetime_col = "datetime_gmt",
                            distance_col = "dist_to_colony_m",
                            phase_col = "phase") {

  needed <- c(bird_id_col, trip_id_col, datetime_col)
  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }

  # Check optional columns once, outside the grouped context
  has_distance <- distance_col %in% names(track)
  has_diel     <- "diel_period" %in% names(track)
  has_phase    <- phase_col %in% names(track)

  track %>%
    filter(!is.na(.data[[trip_id_col]])) %>%
    arrange(.data[[bird_id_col]], .data[[trip_id_col]], .data[[datetime_col]]) %>%
    group_by(.data[[bird_id_col]], .data[[trip_id_col]]) %>%
    summarise(
      trip_start           = min(.data[[datetime_col]], na.rm = TRUE),
      trip_end             = max(.data[[datetime_col]], na.rm = TRUE),
      duration_h           = as.numeric(difftime(trip_end, trip_start, units = "hours")),
      n_fixes              = n(),
      max_dist_to_colony_m = if (has_distance) max(.data[[distance_col]], na.rm = TRUE) else NA_real_,
      prop_day             = if (has_diel) mean(diel_period == "day",    na.rm = TRUE) else NA_real_,
      prop_night           = if (has_diel) mean(diel_period == "night",  na.rm = TRUE) else NA_real_,
      prop_commuting       = if (has_phase) mean(.data[[phase_col]] == "commuting", na.rm = TRUE) else NA_real_,
      prop_foraging        = if (has_phase) mean(.data[[phase_col]] == "foraging",  na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
}
