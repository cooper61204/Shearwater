#' Read raw GPS tracking data from file
#'
#' Reads raw GPS tracking files into R as a tibble. Currently supports CSV
#' format; extensible to other formats in future sprints. Keeps file-format
#' parsing completely separate from column standardization — do not rename
#' columns here.
#' raw <- read_gps_data("data/bird_tracks_2014.csv")
#' head(raw)
#' }
read_gps_data <- function(file_path, format = "csv", ...) {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  format <- tolower(trimws(format))

  if (format == "csv") {
    raw <- readr::read_csv(file_path, show_col_types = FALSE, ...)
  } else {
    stop(
      "Unsupported format: '", format, "'. ",
      "Currently supported formats: 'csv'."
    )
  }

  # Attach source metadata as attributes for downstream traceability
  attr(raw, "source_file") <- file_path
  attr(raw, "import_time") <- Sys.time()

  raw
}
