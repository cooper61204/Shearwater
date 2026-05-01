library(testthat)

test_that("standardize_fishing_effort works on valid input", {
  df <- data.frame(
    effort = c(5, 8, 0, 12),
    gear = c("longline", "trawl", "purse seine", "longline"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df)

  expect_s3_class(out, "data.frame")
  expect_true("effort" %in% names(out))
  expect_true("gear" %in% names(out))
  expect_equal(nrow(out), 4)
})

test_that("standardize_fishing_effort errors when input is not a data frame", {
  expect_error(standardize_fishing_effort(5))
  expect_error(standardize_fishing_effort("abc"))
  expect_error(standardize_fishing_effort(list(effort = 1, gear = "longline")))
})

test_that("standardize_fishing_effort errors when required columns are missing", {
  df_missing_effort <- data.frame(
    gear = c("longline", "trawl"),
    stringsAsFactors = FALSE
  )

  df_missing_gear <- data.frame(
    effort = c(1, 2),
    stringsAsFactors = FALSE
  )

  expect_error(standardize_fishing_effort(df_missing_effort))
  expect_error(standardize_fishing_effort(df_missing_gear))
})

test_that("standardize_fishing_effort converts effort column to numeric", {
  df_string_effort <- data.frame(
    effort = c("5", "8", "0", "12"),
    gear = c("longline", "trawl", "purse seine", "longline"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df_string_effort)

  expect_type(out$effort, "double")
  expect_equal(out$effort, c(5, 8, 0, 12))
})

test_that("standardize_fishing_effort converts non-numeric effort values to 0", {
  df_non_numeric_effort <- data.frame(
    effort = c("5", "bad", "7"),
    gear = c("longline", "trawl", "trawl"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df_non_numeric_effort)

  expect_equal(out$effort, c(5, 0, 7))
})

test_that("standardize_fishing_effort converts missing effort values to 0", {
  df_effort_missing_values <- data.frame(
    effort = c(5, NA, 7),
    gear = c("longline", "trawl", "trawl"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df_effort_missing_values)

  expect_equal(out$effort, c(5, 0, 7))
})

test_that("standardize_fishing_effort converts negative effort values to 0", {
  df_neg_effort <- data.frame(
    effort = c(5, -3, 7),
    gear = c("longline", "trawl", "trawl"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df_neg_effort)

  expect_equal(out$effort, c(5, 0, 7))
})

test_that("standardize_fishing_effort converts gear names to lowercase", {
  df_gear_lower <- data.frame(
    effort = c(1, 2, 3),
    gear = c("LongLine", "TRAWL", "Purse Seine"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df_gear_lower)

  expect_equal(out$gear, c("longline", "trawl", "purse seine"))
})

test_that("standardize_fishing_effort trims whitespace from gear names", {
  df_gear_trimmed <- data.frame(
    effort = c(1, 2, 3),
    gear = c(" longline ", "trawl  ", "  purse seine"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df_gear_trimmed)

  expect_equal(out$gear, c("longline", "trawl", "purse seine"))
})

test_that("standardize_fishing_effort applies gear_map correctly", {
  df_gear <- data.frame(
    effort = c(1, 2, 3),
    gear = c("drifting longline", "trawl", "purse seine"),
    stringsAsFactors = FALSE
  )

  gear_map <- c("drifting longline" = "longline")

  out <- standardize_fishing_effort(df_gear, gear_map = gear_map)

  expect_equal(out$gear, c("longline", "trawl", "purse seine"))
})

test_that("standardize_fishing_effort applies log transformation", {
  df_log <- data.frame(
    effort = c(1, 2, 3),
    gear = c("drifting longline", "trawl", "purse seine"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df_log, log_transform = TRUE)

  expect_equal(out$effort_log, log1p(c(1, 2, 3)))
})

test_that("standardize_fishing_effort standardizes effort to mean 0 and sd 1", {
  df_std <- data.frame(
    effort = c(1, 2, 3, 4),
    gear = c("a", "a", "b", "b"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df_std, standardize_effort = TRUE)

  expect_equal(mean(out$effort_std), 0, tolerance = 1e-8)
  expect_equal(stats::sd(out$effort_std), 1, tolerance = 1e-8)
})

test_that("standardize_fishing_effort handles identical effort values", {
  df_ind_effort <- data.frame(
    effort = c(5, 5, 5, 5),
    gear = c("a", "a", "b", "b"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df_ind_effort, standardize_effort = TRUE)

  expect_equal(out$effort_std, c(0, 0, 0, 0))
})

test_that("standardize_fishing_effort handles all NA effort values", {
  df_all_na <- data.frame(
    effort = c(NA, NA, NA),
    gear = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df_all_na)

  expect_equal(out$effort, c(0, 0, 0))
})

test_that("standardize_fishing_effort preserves extra columns", {
  df_extra <- data.frame(
    longitude = c(1, 2, 3),
    latitude = c(4, 5, 6),
    effort = c(2, 4, 6),
    gear = c("longline", "trawl", "trawl"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df_extra)

  expect_true("longitude" %in% names(out))
  expect_true("latitude" %in% names(out))
  expect_equal(out$longitude, c(1, 2, 3))
  expect_equal(out$latitude, c(4, 5, 6))
})

test_that("standardize_fishing_effort handles custom effort and gear column names", {
  df_custom <- data.frame(
    fishing_hours = c(3, 6, 9),
    gear_type = c("Longline", "TRAWL", "TRAWL"),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(
    df_custom,
    effort_col = "fishing_hours",
    gear_col = "gear_type"
  )

  expect_equal(out$fishing_hours, c(3, 6, 9))
  expect_equal(out$gear_type, c("longline", "trawl", "trawl"))
})

test_that("standardize_fishing_effort handles empty data frames", {
  df_empty <- data.frame(
    effort = numeric(0),
    gear = character(0),
    stringsAsFactors = FALSE
  )

  out <- standardize_fishing_effort(df_empty)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
  expect_true("effort" %in% names(out))
  expect_true("gear" %in% names(out))
})
