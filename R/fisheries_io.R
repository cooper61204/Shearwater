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
}                                       ```````````````````````````````````````   `
