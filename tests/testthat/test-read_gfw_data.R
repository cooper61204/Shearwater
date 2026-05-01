test_that("read_gfw_data reads csv files", {
  temp_file <- tempfile(fileext = ".csv")

  original_data <- data.frame(
    cell_id = c("cell_1", "cell_2"),
    lon = c(-122.1, -122.2),
    lat = c(37.8, 37.9),
    effort = c(10, 20)
  )

  write.csv(original_data, temp_file, row.names = FALSE)

  result <- read_gfw_data(temp_file)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(names(result), names(original_data))

  expect_equal(result$cell_id, original_data$cell_id)
  expect_equal(as.numeric(result$lon), original_data$lon)
  expect_equal(as.numeric(result$lat), original_data$lat)
  expect_equal(as.numeric(result$effort), original_data$effort)
})


test_that("read_gfw_data reads tsv files", {
  temp_file <- tempfile(fileext = ".tsv")

  original_data <- data.frame(
    cell_id = c("cell_1", "cell_2"),
    gear = c("longline", "trawl"),
    effort = c(5, 15)
  )

  write.table(
    original_data,
    temp_file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  result <- read_gfw_data(temp_file)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(names(result), names(original_data))

  expect_equal(result$cell_id, original_data$cell_id)
  expect_equal(result$gear, original_data$gear)
  expect_equal(as.numeric(result$effort), original_data$effort)
})


test_that("read_gfw_data reads txt files as tab-delimited files", {
  temp_file <- tempfile(fileext = ".txt")

  original_data <- data.frame(
    vessel_id = c("vessel_1", "vessel_2"),
    gear = c("purse_seine", "longline"),
    effort = c(12, 18)
  )

  write.table(
    original_data,
    temp_file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  result <- read_gfw_data(temp_file)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(names(result), names(original_data))

  expect_equal(result$vessel_id, original_data$vessel_id)
  expect_equal(result$gear, original_data$gear)
  expect_equal(as.numeric(result$effort), original_data$effort)
})


test_that("read_gfw_data reads rds files", {
  temp_file <- tempfile(fileext = ".rds")

  original_data <- data.frame(
    cell_id = c("cell_1", "cell_2", "cell_3"),
    gear = c("longline", "trawl", "purse_seine"),
    effort = c(10, 20, 30)
  )

  saveRDS(original_data, temp_file)

  result <- read_gfw_data(temp_file)

  expect_s3_class(result, "data.frame")
  expect_equal(result, original_data)
})


test_that("read_gfw_data preserves empty data frames from csv files", {
  temp_file <- tempfile(fileext = ".csv")

  original_data <- data.frame(
    cell_id = character(),
    gear = character(),
    effort = numeric()
  )

  write.csv(original_data, temp_file, row.names = FALSE)

  result <- read_gfw_data(temp_file)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), names(original_data))
})


test_that("read_gfw_data handles single-row csv files", {
  temp_file <- tempfile(fileext = ".csv")

  original_data <- data.frame(
    cell_id = "cell_1",
    gear = "longline",
    effort = 10
  )

  write.csv(original_data, temp_file, row.names = FALSE)

  result <- read_gfw_data(temp_file)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$cell_id, "cell_1")
  expect_equal(result$gear, "longline")
  expect_equal(as.numeric(result$effort), 10)
})


test_that("read_gfw_data handles uppercase file extensions", {
  temp_file <- tempfile(fileext = ".CSV")

  original_data <- data.frame(
    cell_id = c("cell_1", "cell_2"),
    effort = c(10, 20)
  )

  write.csv(original_data, temp_file, row.names = FALSE)

  result <- read_gfw_data(temp_file)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$cell_id, c("cell_1", "cell_2"))
  expect_equal(as.numeric(result$effort), c(10, 20))
})


test_that("read_gfw_data errors when file_path is not a character string", {
  expect_error(
    read_gfw_data(123),
    "file_path must be a single, non-missing character string."
  )

  expect_error(
    read_gfw_data(TRUE),
    "file_path must be a single, non-missing character string."
  )

  expect_error(
    read_gfw_data(list("file.csv")),
    "file_path must be a single, non-missing character string."
  )
})


test_that("read_gfw_data errors when file_path has length greater than one", {
  expect_error(
    read_gfw_data(c("file1.csv", "file2.csv")),
    "file_path must be a single, non-missing character string."
  )
})


test_that("read_gfw_data errors when file_path is NA", {
  expect_error(
    read_gfw_data(NA_character_),
    "file_path must be a single, non-missing character string."
  )
})


test_that("read_gfw_data errors when file does not exist", {
  missing_file <- tempfile(fileext = ".csv")

  expect_false(file.exists(missing_file))

  expect_error(
    read_gfw_data(missing_file),
    "File does not exist:"
  )
})


test_that("read_gfw_data errors for unsupported file types", {
  temp_file <- tempfile(fileext = ".xlsx")

  writeLines("not,a,supported,file", temp_file)

  expect_error(
    read_gfw_data(temp_file),
    "Unsupported file type. Please provide a .csv, .tsv, .txt, or .rds file."
  )
})


test_that("read_gfw_data errors when rds object is not a data frame", {
  temp_file <- tempfile(fileext = ".rds")

  saveRDS(
    list(
      cell_id = c("cell_1", "cell_2"),
      effort = c(10, 20)
    ),
    temp_file
  )

  expect_error(
    read_gfw_data(temp_file),
    "Imported object is not a data frame."
  )
})


test_that("read_gfw_data reads files with extra columns", {
  temp_file <- tempfile(fileext = ".csv")

  original_data <- data.frame(
    cell_id = c("cell_1", "cell_2"),
    lon = c(-122.1, -122.2),
    lat = c(37.8, 37.9),
    gear = c("longline", "trawl"),
    effort = c(10, 20),
    year = c(2023, 2024)
  )

  write.csv(original_data, temp_file, row.names = FALSE)

  result <- read_gfw_data(temp_file)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 6)
  expect_equal(names(result), names(original_data))

  expect_equal(result$cell_id, original_data$cell_id)
  expect_equal(result$gear, original_data$gear)
  expect_equal(as.numeric(result$lon), original_data$lon)
  expect_equal(as.numeric(result$lat), original_data$lat)
  expect_equal(as.numeric(result$effort), original_data$effort)
  expect_equal(as.numeric(result$year), original_data$year)
})


test_that("read_gfw_data preserves id-like character columns with leading zeros", {
  temp_file <- tempfile(fileext = ".csv")

  original_data <- data.frame(
    cell_id = c("001", "002"),
    gear = c("longline", "trawl"),
    effort = c(10, 20)
  )

  write.csv(original_data, temp_file, row.names = FALSE)

  result <- read_gfw_data(temp_file)

  expect_type(result$cell_id, "character")
  expect_equal(result$cell_id, c("001", "002"))

  expect_type(result$gear, "character")
  expect_equal(result$gear, c("longline", "trawl"))

  expect_equal(as.numeric(result$effort), c(10, 20))
})


test_that("read_gfw_data preserves character columns in tsv files", {
  temp_file <- tempfile(fileext = ".tsv")

  original_data <- data.frame(
    vessel_id = c("0001", "0002"),
    gear = c("longline", "trawl"),
    effort = c(7, 9)
  )

  write.table(
    original_data,
    temp_file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  result <- read_gfw_data(temp_file)

  expect_type(result$vessel_id, "character")
  expect_equal(result$vessel_id, c("0001", "0002"))

  expect_type(result$gear, "character")
  expect_equal(result$gear, c("longline", "trawl"))

  expect_equal(as.numeric(result$effort), c(7, 9))
})


test_that("read_gfw_data preserves character columns in txt files", {
  temp_file <- tempfile(fileext = ".txt")

  original_data <- data.frame(
    cell_id = c("010", "020"),
    gear = c("purse_seine", "longline"),
    effort = c(3, 4)
  )

  write.table(
    original_data,
    temp_file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  result <- read_gfw_data(temp_file)

  expect_type(result$cell_id, "character")
  expect_equal(result$cell_id, c("010", "020"))

  expect_type(result$gear, "character")
  expect_equal(result$gear, c("purse_seine", "longline"))

  expect_equal(as.numeric(result$effort), c(3, 4))
})
