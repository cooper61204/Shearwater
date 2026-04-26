library(dplyr)
library(lubridate)
library(suncalc)
library(geosphere)

############################################################
# 1. make_datetime()
# helper
############################################################
make_datetime <- function(track,
                          date_col = "date_gmt",
                          time_col = "time_gmt",
                          tz = "UTC",
                          output_col = "datetime_gmt") {

  if (!date_col %in% names(track)) stop(paste("Missing", date_col))
  if (!time_col %in% names(track)) stop(paste("Missing", time_col))

  track %>%
    mutate(
      !!output_col := ymd_hms(paste(.data[[date_col]], .data[[time_col]]), tz = tz)
    )
}

############################################################
# 2. identify_colony_visits()
# Mark rows as at colony if within threshold radius
############################################################
identify_colony_visits <- function(track,
                                   lon_col = "longitude",
                                   lat_col = "latitude",
                                   colony_lon_col = "lon_colony",
                                   colony_lat_col = "lat_colony",
                                   radius_m = 200,
                                   distance_col = "dist_to_colony_m",
                                   colony_flag_col = "at_colony") {

  needed <- c(lon_col, lat_col, colony_lon_col, colony_lat_col)
  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }

  point_coords <- cbind(track[[lon_col]], track[[lat_col]])
  colony_coords <- cbind(track[[colony_lon_col]], track[[colony_lat_col]])

  dists <- geosphere::distHaversine(point_coords, colony_coords)

  track %>%
    mutate(
      !!distance_col := dists,
      !!colony_flag_col := .data[[distance_col]] <= radius_m
    )
}

############################################################
# 3. segment_trips()
# Assign trip IDs to sequences away from colony
# Rows at colony get NA trip_id
############################################################
segment_trips <- function(track,
                          bird_id_col = "track_id",
                          datetime_col = "datetime_gmt",
                          colony_flag_col = "at_colony",
                          trip_id_col = "trip_id") {

  needed <- c(bird_id_col, datetime_col, colony_flag_col)
  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }

  track %>%
    arrange(.data[[bird_id_col]], .data[[datetime_col]]) %>%
    group_by(.data[[bird_id_col]]) %>%
    mutate(
      prev_at_colony = lag(.data[[colony_flag_col]], default = TRUE),
      departed = prev_at_colony & !.data[[colony_flag_col]],
      trip_counter = cumsum(departed),
      !!trip_id_col := if_else(.data[[colony_flag_col]], NA_integer_, trip_counter)
    ) %>%
    ungroup() %>%
    select(-prev_at_colony, -departed, -trip_counter)
}

############################################################
# 4. classify_trip_phase()
# Simple rule-based version
# commuting = faster movement
# foraging = slower movement
# mixed = in between
############################################################
classify_trip_phase <- function(track,
                                bird_id_col = "track_id",
                                trip_id_col = "trip_id",
                                datetime_col = "datetime_gmt",
                                lon_col = "longitude",
                                lat_col = "latitude",
                                phase_col = "phase",
                                commute_speed_threshold = 5,
                                forage_speed_threshold = 1) {

  needed <- c(bird_id_col, trip_id_col, datetime_col, lon_col, lat_col)
  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }

  out <- track %>%
    arrange(.data[[bird_id_col]], .data[[datetime_col]]) %>%
    group_by(.data[[bird_id_col]], .data[[trip_id_col]]) %>%
    mutate(
      next_lon = lead(.data[[lon_col]]),
      next_lat = lead(.data[[lat_col]]),
      next_time = lead(.data[[datetime_col]]),
      step_m = geosphere::distHaversine(
        cbind(.data[[lon_col]], .data[[lat_col]]),
        cbind(next_lon, next_lat)
      ),
      dt_s = as.numeric(difftime(next_time, .data[[datetime_col]], units = "secs")),
      speed_m_s = step_m / dt_s
    ) %>%
    ungroup()

  out %>%
    mutate(
      !!phase_col := case_when(
        is.na(.data[[trip_id_col]]) ~ "colony",
        is.na(speed_m_s) ~ "unknown",
        speed_m_s >= commute_speed_threshold ~ "commuting",
        speed_m_s <= forage_speed_threshold ~ "foraging",
        TRUE ~ "mixed"
      )
    ) %>%
    select(-next_lon, -next_lat, -next_time)
}

############################################################
# 5. label_day_night_period()
# Add day / night / dawn / dusk
############################################################
label_day_night_period <- function(track,
                                   datetime_col = "datetime_gmt",
                                   lat_col = "latitude",
                                   lon_col = "longitude",
                                   timezone = "UTC",
                                   output_col = "diel_period",
                                   twilight_buffer_mins = 45) {

  needed <- c(datetime_col, lat_col, lon_col)
  missing_cols <- setdiff(needed, names(track))

  if (length(missing_cols) > 0) {
    stop(paste("Missing:", paste(missing_cols, collapse = ", ")))
  }

  dates_only <- as.Date(with_tz(track[[datetime_col]], tzone = timezone))

  sun_tbl <- suncalc::getSunlightTimes(
    date = unique(dates_only),
    lat = mean(track[[lat_col]], na.rm = TRUE),
    lon = mean(track[[lon_col]], na.rm = TRUE),
    keep = c("sunrise", "sunset"),
    tz = timezone
  ) %>%
    mutate(
      dawn_start = sunrise - minutes(twilight_buffer_mins),
      dawn_end   = sunrise + minutes(twilight_buffer_mins),
      dusk_start = sunset  - minutes(twilight_buffer_mins),
      dusk_end   = sunset  + minutes(twilight_buffer_mins)
    ) %>%
    select(date, dawn_start, dawn_end, dusk_start, dusk_end)

  track %>%
    mutate(date_only = as.Date(with_tz(.data[[datetime_col]], tzone = timezone))) %>%
    left_join(sun_tbl, by = c("date_only" = "date")) %>%
    mutate(
      !!output_col := case_when(
        .data[[datetime_col]] >= dawn_start & .data[[datetime_col]] <= dawn_end ~ "dawn",
        .data[[datetime_col]] >= dusk_start & .data[[datetime_col]] <= dusk_end ~ "dusk",
        .data[[datetime_col]] > dawn_end & .data[[datetime_col]] < dusk_start ~ "day",
        TRUE ~ "night"
      )
    ) %>%
    select(-date_only, -dawn_start, -dawn_end, -dusk_start, -dusk_end)
}

############################################################
# 6. summarize_trips()
# One row per trip
############################################################
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
