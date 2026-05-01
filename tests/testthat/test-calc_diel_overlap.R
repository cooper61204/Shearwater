test_that("calc_diel_overlap calculates diel overlap when bird and fisheries diel periods match", {
  joined_data <- data.frame(
    track_id = c("bird_1", "bird_1", "bird_1", "bird_2", "bird_2"),
    diel_period.x = c("day", "day", "night", "night", "day"),
    diel_period.y = c("day", "day", "night", "day", "day"),
    effort_std = c(1, 3, 5, 10, 20)
  )

  result <- calc_diel_overlap(joined_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

  bird1_day <- result[
    result$track_id == "bird_1" & result$diel_period == "day",
  ]

  bird1_night <- result[
    result$track_id == "bird_1" & result$diel_period == "night",
  ]

  bird2_day <- result[
    result$track_id == "bird_2" & result$diel_period == "day",
  ]

  expect_equal(bird1_day$n_overlap_records, 2)
  expect_equal(bird1_day$diel_overlap, 4)
  expect_equal(bird1_day$mean_diel_overlap, 2)

  expect_equal(bird1_night$n_overlap_records, 1)
  expect_equal(bird1_night$diel_overlap, 5)
  expect_equal(bird1_night$mean_diel_overlap, 5)

  expect_equal(bird2_day$n_overlap_records, 1)
  expect_equal(bird2_day$diel_overlap, 20)
  expect_equal(bird2_day$mean_diel_overlap, 20)

  expect_false(any(
    result$track_id == "bird_2" & result$diel_period == "night"
  ))
})


test_that("calc_diel_overlap excludes rows where bird and fisheries diel periods do not match", {
  joined_data <- data.frame(
    track_id = c("bird_1", "bird_1", "bird_1"),
    diel_period.x = c("day", "day", "night"),
    diel_period.y = c("night", "day", "day"),
    effort_std = c(100, 5, 200)
  )

  result <- calc_diel_overlap(joined_data)

  expect_equal(nrow(result), 1)
  expect_equal(result$track_id, "bird_1")
  expect_equal(result$diel_period, "day")
  expect_equal(result$n_overlap_records, 1)
  expect_equal(result$diel_overlap, 5)
  expect_equal(result$mean_diel_overlap, 5)
})


test_that("calc_diel_overlap handles missing and non-numeric effort values", {
  joined_data <- data.frame(
    track_id = c("bird_1", "bird_1", "bird_1", "bird_1"),
    diel_period.x = c("day", "day", "day", "day"),
    diel_period.y = c("day", "day", "day", "day"),
    effort_std = c("1", "2", NA, "bad_value")
  )

  result <- calc_diel_overlap(joined_data)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_overlap_records, 2)
  expect_equal(result$diel_overlap, 3)
  expect_equal(result$mean_diel_overlap, 1.5)
})


test_that("calc_diel_overlap works with custom column names", {
  joined_data <- data.frame(
    bird_id = c("A", "A", "B"),
    bird_diel = c("day", "night", "day"),
    fishing_diel = c("day", "night", "day"),
    fishing_effort = c(4, 6, 8)
  )

  result <- calc_diel_overlap(
    joined_data,
    track_id_col = "bird_id",
    track_diel_col = "bird_diel",
    fisheries_diel_col = "fishing_diel",
    effort_col = "fishing_effort"
  )

  expect_equal(nrow(result), 3)

  bird_a_day <- result[
    result$bird_id == "A" & result$diel_period == "day",
  ]

  bird_a_night <- result[
    result$bird_id == "A" & result$diel_period == "night",
  ]

  bird_b_day <- result[
    result$bird_id == "B" & result$diel_period == "day",
  ]

  expect_equal(bird_a_day$diel_overlap, 4)
  expect_equal(bird_a_night$diel_overlap, 6)
  expect_equal(bird_b_day$diel_overlap, 8)
})


test_that("calc_diel_overlap works with sf objects by dropping geometry", {
  skip_if_not_installed("sf")

  joined_data <- sf::st_as_sf(
    data.frame(
      track_id = c("bird_1", "bird_1", "bird_2"),
      diel_period.x = c("day", "night", "day"),
      diel_period.y = c("day", "night", "night"),
      effort_std = c(1, 2, 100),
      lon = c(-122.1, -122.2, -122.3),
      lat = c(37.8, 37.9, 38.0)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  result <- calc_diel_overlap(joined_data)

  expect_s3_class(result, "data.frame")
  expect_false(inherits(result, "sf"))
  expect_equal(nrow(result), 2)

  bird1_day <- result[
    result$track_id == "bird_1" & result$diel_period == "day",
  ]

  bird1_night <- result[
    result$track_id == "bird_1" & result$diel_period == "night",
  ]

  expect_equal(bird1_day$diel_overlap, 1)
  expect_equal(bird1_night$diel_overlap, 2)
})


test_that("calc_diel_overlap errors when joined_data is not a data frame", {
  expect_error(
    calc_diel_overlap(list(track_id = "bird_1")),
    "joined_data must be a data frame or sf object."
  )
})


test_that("calc_diel_overlap errors when required columns are missing", {
  joined_data <- data.frame(
    track_id = "bird_1",
    diel_period.x = "day"
  )

  expect_error(
    calc_diel_overlap(joined_data),
    "Missing required columns: diel_period.y, effort_std"
  )
})


test_that("calc_diel_overlap errors when custom required columns are missing", {
  joined_data <- data.frame(
    bird_id = "bird_1",
    bird_diel = "day"
  )

  expect_error(
    calc_diel_overlap(
      joined_data,
      track_id_col = "bird_id",
      track_diel_col = "bird_diel",
      fisheries_diel_col = "fishing_diel",
      effort_col = "fishing_effort"
    ),
    "Missing required columns: fishing_diel, fishing_effort"
  )
})


test_that("calc_diel_overlap handles empty data frames with required columns", {
  joined_data <- data.frame(
    track_id = character(),
    diel_period.x = character(),
    diel_period.y = character(),
    effort_std = numeric()
  )

  result <- calc_diel_overlap(joined_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})


test_that("calc_diel_overlap handles data with no matching diel periods", {
  joined_data <- data.frame(
    track_id = c("bird_1", "bird_2"),
    diel_period.x = c("day", "night"),
    diel_period.y = c("night", "day"),
    effort_std = c(10, 20)
  )

  result <- calc_diel_overlap(joined_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})
