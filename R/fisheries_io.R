#' Read Global Fishing Watch fisheries data
#'
#' Imports Global Fishing Watch or similar fisheries-effort data into
#' an analysis-ready data frame.
#'
#' @param file_path Character string. Path to the input file.
#'
#' @return A data frame containing the raw fisheries table/grid.
#' @export
read_gfw_data <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1 || is.na(file_path)) {
    stop("file_path must be a single, non-missing character string.")
  }

  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }


  ext <- tolower(tools::file_ext(file_path))

  if (ext == "csv") {
    data <- read.csv(file_path, stringsAsFactors = FALSE)
  } else if (ext %in% c("tsv", "txt")) {
    data <- read.delim(file_path, stringsAsFactors = FALSE)
  } else if (ext == "rds") {
    data <- readRDS(file_path)
  } else {
    stop("Unsupported file type. Please provide a .csv, .tsv, .txt, or .rds file.")
  }

  if (!is.data.frame(data)) {
    stop("Imported object is not a data frame.")
  }

  return(data)
}


#' Standardize fisheries effort data
#'
#' Cleans and standardizes fisheries effort fields and gear categories.
#'
#' @param fisheries_data A data frame of raw fisheries data.
#' @param effort_col Character string. Name of the effort column.
#' @param gear_col Character string. Name of the gear column.
#' @param gear_map Optional named character vector used to recode gear values.
#'   Names should be old values and values should be new values.
#' @param standardize_effort Logical. If TRUE, create a z-scored effort column.
#' @param log_transform Logical. If TRUE, create a log1p-transformed effort column.
#'
#' @return A cleaned and standardized fisheries data frame.
#' @export
standardize_fishing_effort <- function(fisheries_data,
                                       effort_col = "effort",
                                       gear_col = "gear",
                                       gear_map = NULL,
                                       standardize_effort = TRUE,
                                       log_transform = FALSE) {
  if (!is.data.frame(fisheries_data)) {
    stop("fisheries_data must be a data frame.")
  }

  if (!effort_col %in% names(fisheries_data)) {
    stop("effort_col not found in fisheries_data.")
  }

  if (!gear_col %in% names(fisheries_data)) {
    stop("gear_col not found in fisheries_data.")
  }

  fisheries_data[[effort_col]] <- suppressWarnings(as.numeric(fisheries_data[[effort_col]]))
  fisheries_data[[gear_col]] <- tolower(trimws(as.character(fisheries_data[[gear_col]])))

  fisheries_data[[effort_col]][is.na(fisheries_data[[effort_col]])] <- 0
  fisheries_data[[effort_col]][fisheries_data[[effort_col]] < 0] <- 0

  if (!is.null(gear_map)) {
    fisheries_data[[gear_col]] <- ifelse(
      fisheries_data[[gear_col]] %in% names(gear_map),
      unname(gear_map[fisheries_data[[gear_col]]]),
      fisheries_data[[gear_col]]
    )
  }

  if (log_transform) {
    fisheries_data$effort_log <- log1p(fisheries_data[[effort_col]])
  }

  if (standardize_effort) {
    s <- stats::sd(fisheries_data[[effort_col]], na.rm = TRUE)
    m <- mean(fisheries_data[[effort_col]], na.rm = TRUE)

    if (is.na(s) || s == 0) {
      fisheries_data$effort_std <- 0
    } else {
      fisheries_data$effort_std <- (fisheries_data[[effort_col]] - m) / s
    }
  }

  fisheries_data
}
