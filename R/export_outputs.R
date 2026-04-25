# ------------------------------------------------------------------------------
# summarize_policy_exposure()
# Create a policy-facing summary of each bird's exposure to jurisdictions,
# MPAs, and candidate conservation priority areas.
#
# Inputs:
#   jurisdiction_summary  — output of calc_time_in_jurisdictions()
#   mpa_summary           — output of overlay_mpas() (point version),
#                           then aggregated; OR pass raw labeled track
#   priority_summary      — output of overlay_priority_areas() (point version),
#                           then aggregated; OR pass raw labeled track
#   transboundary_summary — output of calc_transboundary_movements()
#
# Output:
#   tibble with one row per bird summarizing all policy-relevant exposure metrics:
#     bird_id,
#     top_jurisdiction, pct_time_top_jurisdiction,
#     total_hours_in_mpa, pct_fixes_in_mpa,
#     total_hours_in_priority_area, pct_fixes_in_priority_area,
#     n_transboundary_trips, crossed_abnj_any_trip
# ------------------------------------------------------------------------------
summarize_policy_exposure <- function(jurisdiction_summary,
                                      mpa_track,
                                      priority_track,
                                      transboundary_summary) {

  # Jurisdiction: find top jurisdiction and % time for each bird
  jur <- jurisdiction_summary |>
    dplyr::group_by(.data$bird_id) |>
    dplyr::mutate(total_bird_hours = sum(.data$total_hours)) |>
    dplyr::slice_max(.data$total_hours, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      bird_id                      = .data$bird_id,
      top_jurisdiction             = .data$jurisdiction,
      pct_time_top_jurisdiction    = (.data$total_hours / .data$total_bird_hours) * 100
    )

  # MPA: summarize per bird from labeled track points
  mpa_tbl <- sf::st_drop_geometry(mpa_track)
  assert_required_cols(mpa_tbl, c("bird_id", "Date", "Time", "in_mpa"))

  mpa_tbl <- mpa_tbl |>
    dplyr::mutate(
      time_clean = dplyr::case_when(
        nchar(.data$Time) == 6 ~ paste0(
          substr(.data$Time, 1, 2), ":",
          substr(.data$Time, 3, 4), ":",
          substr(.data$Time, 5, 6)
        ),
        TRUE ~ .data$Time
      ),
      datetime = as.POSIXct(paste(.data$Date, .data$time_clean),
                            format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
    ) |>
    dplyr::arrange(.data$bird_id, .data$datetime) |>
    dplyr::group_by(.data$bird_id) |>
    dplyr::mutate(
      step_h = as.numeric(difftime(dplyr::lead(.data$datetime), .data$datetime,
                                   units = "hours"))
    ) |>
    dplyr::ungroup()

  mpa_per_bird <- mpa_tbl |>
    dplyr::group_by(.data$bird_id) |>
    dplyr::summarise(
      total_hours_in_mpa  = sum(.data$step_h[.data$in_mpa], na.rm = TRUE),
      pct_fixes_in_mpa    = mean(.data$in_mpa, na.rm = TRUE) * 100,
      .groups = "drop"
    )

  # Priority areas: same pattern as MPA
  pri_tbl <- sf::st_drop_geometry(priority_track)
  assert_required_cols(pri_tbl, c("bird_id", "Date", "Time", "in_priority_area"))

  pri_tbl <- pri_tbl |>
    dplyr::mutate(
      time_clean = dplyr::case_when(
        nchar(.data$Time) == 6 ~ paste0(
          substr(.data$Time, 1, 2), ":",
          substr(.data$Time, 3, 4), ":",
          substr(.data$Time, 5, 6)
        ),
        TRUE ~ .data$Time
      ),
      datetime = as.POSIXct(paste(.data$Date, .data$time_clean),
                            format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
    ) |>
    dplyr::arrange(.data$bird_id, .data$datetime) |>
    dplyr::group_by(.data$bird_id) |>
    dplyr::mutate(
      step_h = as.numeric(difftime(dplyr::lead(.data$datetime), .data$datetime,
                                   units = "hours"))
    ) |>
    dplyr::ungroup()

  pri_per_bird <- pri_tbl |>
    dplyr::group_by(.data$bird_id) |>
    dplyr::summarise(
      total_hours_in_priority_area = sum(.data$step_h[.data$in_priority_area],
                                         na.rm = TRUE),
      pct_fixes_in_priority_area   = mean(.data$in_priority_area,
                                          na.rm = TRUE) * 100,
      .groups = "drop"
    )

  # Transboundary: count trips and flag any ABNJ crossing
  trans_per_bird <- transboundary_summary |>
    dplyr::group_by(.data$bird_id) |>
    dplyr::summarise(
      n_transboundary_trips  = sum(.data$is_transboundary, na.rm = TRUE),
      crossed_abnj_any_trip  = any(.data$crossed_into_abnj, na.rm = TRUE),
      .groups = "drop"
    )

  # Join all summaries
  policy_summary <- jur |>
    dplyr::left_join(mpa_per_bird,   by = "bird_id") |>
    dplyr::left_join(pri_per_bird,   by = "bird_id") |>
    dplyr::left_join(trans_per_bird, by = "bird_id") |>
    dplyr::arrange(.data$bird_id)

  policy_summary
}
