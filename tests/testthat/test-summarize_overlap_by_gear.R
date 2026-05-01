test_that("summarize_overlap_by_gear summarizes overlap by gear type", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_2", "bird_3", "bird_4"),
    gear = c("longline", "longline", "trawl", "trawl"),
    total_overlap = c(10, 20, 5, 15)
  )

  result <- summarize_overlap_by_gear(overlap_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)

  longline_row <- result[result$gear == "longline", ]
  trawl_row <- result[result$gear == "trawl", ]

  expect_equal(longline_row$n_overlap_groups, 2)
  expect_equal(longline_row$total_overlap, 30)
  expect_equal(longline_row$mean_overlap, 15)
  expect_equal(longline_row$max_overlap, 20)

  expect_equal(trawl_row$n_overlap_groups, 2)
  expect_equal(trawl_row$total_overlap, 20)
  expect_equal(trawl_row$mean_overlap, 10)
  expect_equal(trawl_row$max_overlap, 15)
})


test_that("summarize_overlap_by_gear handles one record per gear type", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_2", "bird_3"),
    gear = c("longline", "trawl", "purse_seine"),
    total_overlap = c(10, 20, 30)
  )

  result <- summarize_overlap_by_gear(overlap_data)

  expect_equal(nrow(result), 3)

  longline_row <- result[result$gear == "longline", ]
  trawl_row <- result[result$gear == "trawl", ]
  purse_row <- result[result$gear == "purse_seine", ]

  expect_equal(longline_row$n_overlap_groups, 1)
  expect_equal(longline_row$total_overlap, 10)
  expect_equal(longline_row$mean_overlap, 10)
  expect_equal(longline_row$max_overlap, 10)

  expect_equal(trawl_row$n_overlap_groups, 1)
  expect_equal(trawl_row$total_overlap, 20)
  expect_equal(trawl_row$mean_overlap, 20)
  expect_equal(trawl_row$max_overlap, 20)

  expect_equal(purse_row$n_overlap_groups, 1)
  expect_equal(purse_row$total_overlap, 30)
  expect_equal(purse_row$mean_overlap, 30)
  expect_equal(purse_row$max_overlap, 30)
})


test_that("summarize_overlap_by_gear works with custom column names", {
  overlap_data <- data.frame(
    bird_id = c("A", "B", "C"),
    fishing_gear = c("longline", "longline", "trawl"),
    overlap_score = c(4, 6, 8)
  )

  result <- summarize_overlap_by_gear(
    overlap_data,
    gear_col = "fishing_gear",
    overlap_col = "overlap_score"
  )

  expect_equal(nrow(result), 2)
  expect_true("fishing_gear" %in% names(result))

  longline_row <- result[result$fishing_gear == "longline", ]
  trawl_row <- result[result$fishing_gear == "trawl", ]

  expect_equal(longline_row$n_overlap_groups, 2)
  expect_equal(longline_row$total_overlap, 10)
  expect_equal(longline_row$mean_overlap, 5)
  expect_equal(longline_row$max_overlap, 6)

  expect_equal(trawl_row$n_overlap_groups, 1)
  expect_equal(trawl_row$total_overlap, 8)
  expect_equal(trawl_row$mean_overlap, 8)
  expect_equal(trawl_row$max_overlap, 8)
})


test_that("summarize_overlap_by_gear ignores NA overlap values", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_2", "bird_3", "bird_4"),
    gear = c("longline", "longline", "trawl", "trawl"),
    total_overlap = c(10, NA, 5, 15)
  )

  result <- summarize_overlap_by_gear(overlap_data)

  longline_row <- result[result$gear == "longline", ]
  trawl_row <- result[result$gear == "trawl", ]

  expect_equal(longline_row$n_overlap_groups, 1)
  expect_equal(longline_row$total_overlap, 10)
  expect_equal(longline_row$mean_overlap, 10)
  expect_equal(longline_row$max_overlap, 10)

  expect_equal(trawl_row$n_overlap_groups, 2)
  expect_equal(trawl_row$total_overlap, 20)
  expect_equal(trawl_row$mean_overlap, 10)
  expect_equal(trawl_row$max_overlap, 15)
})


test_that("summarize_overlap_by_gear handles non-numeric overlap values", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_2", "bird_3"),
    gear = c("longline", "longline", "longline"),
    total_overlap = c("10", "20", "bad_value")
  )

  result <- summarize_overlap_by_gear(overlap_data)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_overlap_groups, 2)
  expect_equal(result$total_overlap, 30)
  expect_equal(result$mean_overlap, 15)
  expect_equal(result$max_overlap, 20)
})


test_that("summarize_overlap_by_gear returns NA mean and max when all overlap values are missing", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_2"),
    gear = c("longline", "longline"),
    total_overlap = c(NA, NA)
  )

  result <- summarize_overlap_by_gear(overlap_data)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_overlap_groups, 0)
  expect_equal(result$total_overlap, 0)
  expect_true(is.na(result$mean_overlap))
  expect_true(is.na(result$max_overlap))
})


test_that("summarize_overlap_by_gear handles empty data frames", {
  overlap_data <- data.frame(
    track_id = character(),
    gear = character(),
    total_overlap = numeric()
  )

  result <- summarize_overlap_by_gear(overlap_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)

  expect_true("gear" %in% names(result))
  expect_true("n_overlap_groups" %in% names(result))
  expect_true("total_overlap" %in% names(result))
  expect_true("mean_overlap" %in% names(result))
  expect_true("max_overlap" %in% names(result))
})


test_that("summarize_overlap_by_gear handles empty data frames with custom column names", {
  overlap_data <- data.frame(
    bird_id = character(),
    fishing_gear = character(),
    overlap_score = numeric()
  )

  result <- summarize_overlap_by_gear(
    overlap_data,
    gear_col = "fishing_gear",
    overlap_col = "overlap_score"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)

  expect_true("fishing_gear" %in% names(result))
  expect_true("n_overlap_groups" %in% names(result))
  expect_true("total_overlap" %in% names(result))
  expect_true("mean_overlap" %in% names(result))
  expect_true("max_overlap" %in% names(result))
})


test_that("summarize_overlap_by_gear errors when overlap_data is not a data frame", {
  expect_error(
    summarize_overlap_by_gear(
      overlap_data = list(
        gear = "longline",
        total_overlap = 10
      )
    ),
    "overlap_data must be a data frame."
  )
})


test_that("summarize_overlap_by_gear errors when gear_col is missing", {
  overlap_data <- data.frame(
    track_id = "bird_1",
    total_overlap = 10
  )

  expect_error(
    summarize_overlap_by_gear(overlap_data),
    "gear_col not found in overlap_data."
  )
})


test_that("summarize_overlap_by_gear errors when overlap_col is missing", {
  overlap_data <- data.frame(
    track_id = "bird_1",
    gear = "longline"
  )

  expect_error(
    summarize_overlap_by_gear(overlap_data),
    "overlap_col not found in overlap_data."
  )
})


test_that("summarize_overlap_by_gear works on output-like data from calc_fisheries_overlap", {
  overlap_data <- data.frame(
    track_id = c("bird_1", "bird_1", "bird_2", "bird_2"),
    gear = c("longline", "trawl", "longline", "trawl"),
    n_overlap_records = c(2, 1, 3, 2),
    total_overlap = c(10, 5, 20, 15),
    mean_overlap = c(5, 5, 6.67, 7.5),
    max_overlap = c(6, 5, 10, 9)
  )

  result <- summarize_overlap_by_gear(overlap_data)

  longline_row <- result[result$gear == "longline", ]
  trawl_row <- result[result$gear == "trawl", ]

  expect_equal(longline_row$n_overlap_groups, 2)
  expect_equal(longline_row$total_overlap, 30)
  expect_equal(longline_row$mean_overlap, 15)
  expect_equal(longline_row$max_overlap, 20)

  expect_equal(trawl_row$n_overlap_groups, 2)
  expect_equal(trawl_row$total_overlap, 20)
  expect_equal(trawl_row$mean_overlap, 10)
  expect_equal(trawl_row$max_overlap, 15)
})
