test_that("calc_fisheries_overlap calculates overlap metrics by track and gear", {
  joined_data <- data.frame(
    track_id = c("bird_1", "bird_1", "bird_1", "bird_2", "bird_2"),
    gear = c("longline", "longline", "trawl", "longline", "trawl"),
    effort_std = c(1, 2, 5, 10, NA)
  )

  result <- calc_fisheries_overlap(joined_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)

  bird1_longline <- result[
    result$track_id == "bird_1" & result$gear == "longline",
  ]

  expect_equal(bird1_longline$n_overlap_records, 2)
  expect_equal(bird1_longline$total_overlap, 3)
  expect_equal(bird1_longline$mean_overlap, 1.5)
  expect_equal(bird1_longline$max_overlap, 2)

  bird2_trawl <- result[
    result$track_id == "bird_2" & result$gear == "trawl",
  ]

  expect_equal(bird2_trawl$n_overlap_records, 0)
  expect_equal(bird2_trawl$total_overlap, 0)
  expect_true(is.na(bird2_trawl$mean_overlap))
  expect_true(is.na(bird2_trawl$max_overlap))
})


test_that("calc_fisheries_overlap groups by cell_id_col when provided", {
  joined_data <- data.frame(
    track_id = c("bird_1", "bird_1", "bird_1", "bird_1"),
    gear = c("longline", "longline", "longline", "longline"),
    cell_id = c("cell_a", "cell_a", "cell_b", "cell_b"),
    effort_std = c(1, 3, 10, 20)
  )

  result <- calc_fisheries_overlap(
    joined_data,
    cell_id_col = "cell_id"
  )

  expect_equal(nrow(result), 2)
  expect_true("cell_id" %in% names(result))

  cell_a <- result[result$cell_id == "cell_a", ]
  cell_b <- result[result$cell_id == "cell_b", ]

  expect_equal(cell_a$n_overlap_records, 2)
  expect_equal(cell_a$total_overlap, 4)
  expect_equal(cell_a$mean_overlap, 2)
  expect_equal(cell_a$max_overlap, 3)

  expect_equal(cell_b$n_overlap_records, 2)
  expect_equal(cell_b$total_overlap, 30)
  expect_equal(cell_b$mean_overlap, 15)
  expect_equal(cell_b$max_overlap, 20)
})


test_that("calc_fisheries_overlap works with custom column names", {
  joined_data <- data.frame(
    bird_id = c("A", "A", "B"),
    fishing_gear = c("trawl", "trawl", "longline"),
    fishing_effort = c(4, 6, 8)
  )

  result <- calc_fisheries_overlap(
    joined_data,
    track_id_col = "bird_id",
    effort_col = "fishing_effort",
    gear_col = "fishing_gear"
  )

  expect_equal(nrow(result), 2)

  bird_a <- result[result$bird_id == "A", ]

  expect_equal(bird_a$n_overlap_records, 2)
  expect_equal(bird_a$total_overlap, 10)
  expect_equal(bird_a$mean_overlap, 5)
  expect_equal(bird_a$max_overlap, 6)
})


test_that("calc_fisheries_overlap converts numeric-like effort values", {
  joined_data <- data.frame(
    track_id = c("bird_1", "bird_1", "bird_1"),
    gear = c("longline", "longline", "longline"),
    effort_std = c("1.5", "2.5", "bad_value")
  )

  result <- calc_fisheries_overlap(joined_data)

  expect_equal(result$n_overlap_records, 2)
  expect_equal(result$total_overlap, 4)
  expect_equal(result$mean_overlap, 2)
  expect_equal(result$max_overlap, 2.5)
})


test_that("calc_fisheries_overlap works with sf objects by dropping geometry", {
  skip_if_not_installed("sf")

  joined_data <- sf::st_as_sf(
    data.frame(
      track_id = c("bird_1", "bird_1", "bird_2"),
      gear = c("longline", "longline", "trawl"),
      effort_std = c(1, 2, 5),
      lon = c(-122.1, -122.2, -122.3),
      lat = c(37.8, 37.9, 38.0)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  result <- calc_fisheries_overlap(joined_data)

  expect_s3_class(result, "data.frame")
  expect_false(inherits(result, "sf"))
  expect_equal(nrow(result), 2)

  bird1 <- result[result$track_id == "bird_1", ]

  expect_equal(bird1$n_overlap_records, 2)
  expect_equal(bird1$total_overlap, 3)
  expect_equal(bird1$mean_overlap, 1.5)
  expect_equal(bird1$max_overlap, 2)
})


test_that("calc_fisheries_overlap errors when joined_data is not a data frame", {
  expect_error(
    calc_fisheries_overlap(list(track_id = "bird_1")),
    "joined_data must be a data frame or sf object."
  )
})


test_that("calc_fisheries_overlap errors when track_id_col is missing", {
  joined_data <- data.frame(
    gear = "longline",
    effort_std = 1
  )

  expect_error(
    calc_fisheries_overlap(joined_data),
    "track_id_col not found in joined_data."
  )
})


test_that("calc_fisheries_overlap errors when effort_col is missing", {
  joined_data <- data.frame(
    track_id = "bird_1",
    gear = "longline"
  )

  expect_error(
    calc_fisheries_overlap(joined_data),
    "effort_col not found in joined_data."
  )
})


test_that("calc_fisheries_overlap errors when gear_col is missing", {
  joined_data <- data.frame(
    track_id = "bird_1",
    effort_std = 1
  )

  expect_error(
    calc_fisheries_overlap(joined_data),
    "gear_col not found in joined_data."
  )
})


test_that("calc_fisheries_overlap errors when cell_id_col is missing", {
  joined_data <- data.frame(
    track_id = "bird_1",
    gear = "longline",
    effort_std = 1
  )

  expect_error(
    calc_fisheries_overlap(joined_data, cell_id_col = "cell_id"),
    "cell_id_col not found in joined_data."
  )
})


test_that("calc_fisheries_overlap handles empty data frames", {
  joined_data <- data.frame(
    track_id = character(),
    gear = character(),
    effort_std = numeric()
  )

  result <- calc_fisheries_overlap(joined_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})
