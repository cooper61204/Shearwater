test_that("calc_risk_index calculates risk index using named numeric gear weights", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_2", "bird_3"),
    gear = c("longline", "trawl", "purse_seine"),
    total_overlap = c(10, 20, 5)
  )

  gear_weights <- c(
    longline = 2,
    trawl = 3,
    purse_seine = 1
  )

  result <- calc_risk_index(
    overlap_data = overlap_data,
    gear_weights = gear_weights,
    scale_01 = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true("gear_weight" %in% names(result))
  expect_true("risk_index" %in% names(result))
  expect_false("risk_index_scaled" %in% names(result))

  longline_row <- result[result$gear == "longline", ]
  trawl_row <- result[result$gear == "trawl", ]
  purse_row <- result[result$gear == "purse_seine", ]

  expect_equal(longline_row$risk_index, 20)
  expect_equal(trawl_row$risk_index, 60)
  expect_equal(purse_row$risk_index, 5)
})


test_that("calc_risk_index rescales risk index to 0 and 1 when scale_01 is TRUE", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_2", "bird_3"),
    gear = c("longline", "trawl", "purse_seine"),
    total_overlap = c(10, 20, 5)
  )

  gear_weights <- c(
    longline = 2,
    trawl = 3,
    purse_seine = 1
  )

  result <- calc_risk_index(
    overlap_data = overlap_data,
    gear_weights = gear_weights,
    scale_01 = TRUE
  )

  expect_true("risk_index_scaled" %in% names(result))

  longline_row <- result[result$gear == "longline", ]
  trawl_row <- result[result$gear == "trawl", ]
  purse_row <- result[result$gear == "purse_seine", ]

  expect_equal(purse_row$risk_index_scaled, 0)
  expect_equal(trawl_row$risk_index_scaled, 1)

  expected_longline_scaled <- (20 - 5) / (60 - 5)
  expect_equal(longline_row$risk_index_scaled, expected_longline_scaled)
})


test_that("calc_risk_index accepts gear weights as a data frame", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_2"),
    gear = c("longline", "trawl"),
    total_overlap = c(10, 20)
  )

  gear_weights <- data.frame(
    gear = c("longline", "trawl"),
    gear_weight = c(2, 3)
  )

  result <- calc_risk_index(
    overlap_data = overlap_data,
    gear_weights = gear_weights,
    scale_01 = FALSE
  )

  longline_row <- result[result$gear == "longline", ]
  trawl_row <- result[result$gear == "trawl", ]

  expect_equal(longline_row$gear_weight, 2)
  expect_equal(trawl_row$gear_weight, 3)
  expect_equal(longline_row$risk_index, 20)
  expect_equal(trawl_row$risk_index, 60)
})


test_that("calc_risk_index uses default weight of 1 for unknown gear types", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_2"),
    gear = c("longline", "unknown_gear"),
    total_overlap = c(10, 20)
  )

  gear_weights <- c(
    longline = 2
  )

  result <- calc_risk_index(
    overlap_data = overlap_data,
    gear_weights = gear_weights,
    scale_01 = FALSE
  )

  known_row <- result[result$gear == "longline", ]
  unknown_row <- result[result$gear == "unknown_gear", ]

  expect_equal(known_row$gear_weight, 2)
  expect_equal(known_row$risk_index, 20)

  expect_equal(unknown_row$gear_weight, 1)
  expect_equal(unknown_row$risk_index, 20)
})


test_that("calc_risk_index works with custom overlap and gear column names", {
  overlap_data <- data.frame(
    bird_id = c("A", "B"),
    fishing_gear = c("longline", "trawl"),
    overlap_score = c(7, 9)
  )

  gear_weights <- c(
    longline = 2,
    trawl = 4
  )

  result <- calc_risk_index(
    overlap_data = overlap_data,
    overlap_col = "overlap_score",
    gear_col = "fishing_gear",
    gear_weights = gear_weights,
    scale_01 = FALSE
  )

  longline_row <- result[result$fishing_gear == "longline", ]
  trawl_row <- result[result$fishing_gear == "trawl", ]

  expect_equal(longline_row$risk_index, 14)
  expect_equal(trawl_row$risk_index, 36)
})


test_that("calc_risk_index converts numeric-like overlap values", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_2", "bird_3"),
    gear = c("longline", "longline", "longline"),
    total_overlap = c("10", "20", "bad_value")
  )

  gear_weights <- c(
    longline = 2
  )

  result <- calc_risk_index(
    overlap_data = overlap_data,
    gear_weights = gear_weights,
    scale_01 = FALSE
  )

  expect_equal(result$risk_index[1], 20)
  expect_equal(result$risk_index[2], 40)
  expect_true(is.na(result$risk_index[3]))
})


test_that("calc_risk_index gives scaled value of 0 when all risk values are equal", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_2"),
    gear = c("longline", "trawl"),
    total_overlap = c(10, 5)
  )

  gear_weights <- c(
    longline = 2,
    trawl = 4
  )

  result <- calc_risk_index(
    overlap_data = overlap_data,
    gear_weights = gear_weights,
    scale_01 = TRUE
  )

  expect_true("risk_index_scaled" %in% names(result))
  expect_equal(result$risk_index, c(20, 20))
  expect_equal(result$risk_index_scaled, c(0, 0))
})


test_that("calc_risk_index handles NA overlap values", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_2", "bird_3"),
    gear = c("longline", "trawl", "purse_seine"),
    total_overlap = c(10, NA, 30)
  )

  gear_weights <- c(
    longline = 2,
    trawl = 3,
    purse_seine = 1
  )

  result <- calc_risk_index(
    overlap_data = overlap_data,
    gear_weights = gear_weights,
    scale_01 = TRUE
  )

  trawl_row <- result[result$gear == "trawl", ]

  expect_true(is.na(trawl_row$risk_index))
  expect_true(is.na(trawl_row$risk_index_scaled))

  expect_true(all(result$risk_index_scaled[!is.na(result$risk_index_scaled)] >= 0))
  expect_true(all(result$risk_index_scaled[!is.na(result$risk_index_scaled)] <= 1))
})


test_that("calc_risk_index handles empty data frames", {
  overlap_data <- data.frame(
    track_id = character(),
    gear = character(),
    total_overlap = numeric()
  )

  gear_weights <- c(
    longline = 2,
    trawl = 3
  )

  result <- calc_risk_index(
    overlap_data = overlap_data,
    gear_weights = gear_weights
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("risk_index" %in% names(result))
  expect_true("risk_index_scaled" %in% names(result))
})


test_that("calc_risk_index errors when overlap_data is not a data frame", {
  expect_error(
    calc_risk_index(
      overlap_data = list(
        gear = "longline",
        total_overlap = 10
      ),
      gear_weights = c(longline = 2)
    ),
    "overlap_data must be a data frame."
  )
})


test_that("calc_risk_index errors when overlap_col is missing", {
  overlap_data <- data.frame(
    track_id = "bird_1",
    gear = "longline"
  )

  expect_error(
    calc_risk_index(
      overlap_data = overlap_data,
      gear_weights = c(longline = 2)
    ),
    "overlap_col not found in overlap_data."
  )
})


test_that("calc_risk_index errors when gear_col is missing", {
  overlap_data <- data.frame(
    track_id = "bird_1",
    total_overlap = 10
  )

  expect_error(
    calc_risk_index(
      overlap_data = overlap_data,
      gear_weights = c(longline = 2)
    ),
    "gear_col not found in overlap_data."
  )
})


test_that("calc_risk_index errors when gear_weights is missing", {
  overlap_data <- data.frame(
    track_id = "bird_1",
    gear = "longline",
    total_overlap = 10
  )

  expect_error(
    calc_risk_index(
      overlap_data = overlap_data
    ),
    "gear_weights must be provided."
  )
})


test_that("calc_risk_index errors when gear_weights is unnamed numeric vector", {
  overlap_data <- data.frame(
    track_id = "bird_1",
    gear = "longline",
    total_overlap = 10
  )

  expect_error(
    calc_risk_index(
      overlap_data = overlap_data,
      gear_weights = c(2, 3)
    ),
    "gear_weights must be a named numeric vector or data frame."
  )
})


test_that("calc_risk_index errors when gear_weights data frame is missing gear column", {
  overlap_data <- data.frame(
    track_id = "bird_1",
    gear = "longline",
    total_overlap = 10
  )

  gear_weights <- data.frame(
    fishing_gear = "longline",
    gear_weight = 2
  )

  expect_error(
    calc_risk_index(
      overlap_data = overlap_data,
      gear_weights = gear_weights
    ),
    "gear_weights data frame must contain the gear column."
  )
})


test_that("calc_risk_index errors when gear_weights data frame is missing gear_weight column", {
  overlap_data <- data.frame(
    track_id = "bird_1",
    gear = "longline",
    total_overlap = 10
  )

  gear_weights <- data.frame(
    gear = "longline",
    weight = 2
  )

  expect_error(
    calc_risk_index(
      overlap_data = overlap_data,
      gear_weights = gear_weights
    ),
    "gear_weights data frame must contain a gear_weight column."
  )
})
