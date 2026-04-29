# =============================================================================
# jurisdiction_overlay.R
# Read and manage spatial jurisdiction and conservation layers
# =============================================================================


#' Read Management Layers (EEZ, ABNJ, MPA, Conservation Priority)
#'
#' Imports spatial management layers from disk, validates their CRS,
#' and returns them as a named list of sf objects. Supports multiple
#' input formats (GeoPackage, GeoJSON, Shapefile) and automatically
#' reprojects to a common CRS if needed.
#'
#' @param eez_path             Character. Path to EEZ (Exclusive Economic Zone) layer.
#'                             Accepts .gpkg, .geojson, .shp, or directory.
#' @param abnj_path            Character. Optional path to ABNJ (Area Beyond National
#'                             Jurisdiction) layer. If NULL, skipped.
#' @param mpa_path             Character. Optional path to MPA (Marine Protected Area)
#'                             layer. If NULL, skipped.
#' @param conservation_path    Character. Optional path to conservation-priority layer.
#'                             If NULL, skipped.
#' @param target_crs           Numeric or character. Target CRS to reproject all layers to.
#'                             Default 4326 (WGS84). Pass NULL to skip reprojection.
#' @param layer_name           Character. For multi-layer files (e.g., .gpkg),
#'                             specify which layer to read. Default NULL (read first/default).
#' @param simplify             Logical. If TRUE, simplify geometries to reduce file size.
#'                             Default FALSE.
#' @param simplify_tolerance   Numeric. Tolerance for simplification in degrees
#'                             (if simplify = TRUE). Default 0.01.
#' @param validate             Logical. If TRUE, run basic sf validity checks.
#'                             Default TRUE.
#'
#' @return A named list of sf objects:
#'   - \code{eez}:             EEZ layer (required).
#'   - \code{abnj}:            ABNJ layer (if provided).
#'   - \code{mpa}:             MPA layer (if provided).
#'   - \code{conservation}:    Conservation-priority layer (if provided).
#'
#'   Each object has attributes:
#'   - \code{source_path}:     Original file path.
#'   - \code{crs}:             Coordinate reference system.
#'   - \code{n_features}:      Number of features.
#'   - \code{read_date}:       Timestamp when layer was read.
#'
#' @details
#'
#' Column naming: If a layer has a column typically used for jurisdiction names
#' (e.g., \code{GEONAME}, \code{TERRITORY1}, \code{name}), it will be preserved
#' as-is. No renaming is applied unless explicitly requested.
#'
#' Reprojection: All layers are reprojected to \code{target_crs} (default WGS84).
#' This ensures spatial operations across layers work correctly.
#'
#' Simplification: For large polygon layers, setting \code{simplify = TRUE} can
#' significantly reduce file size in memory and on disk. Use with caution as
#' it may introduce small artifacts along boundaries.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read EEZ only
#' layers <- read_management_layers(
#'   eez_path = "data/eez_boundaries.gpkg"
#' )
#'
#' # Read EEZ and MPA, reproject to Web Mercator
#' layers <- read_management_layers(
#'   eez_path = "data/eez_boundaries.gpkg",
#'   mpa_path = "data/mpas.shp",
#'   target_crs = 3857
#' )
#'
#' # Read all layers with simplification
#' layers <- read_management_layers(
#'   eez_path = "data/eez_boundaries.gpkg",
#'   abnj_path = "data/abnj.geojson",
#'   mpa_path = "data/mpas.gpkg",
#'   conservation_path = "data/conservation_priority.gpkg",
#'   simplify = TRUE,
#'   simplify_tolerance = 0.05
#' )
#' }
read_management_layers <- function(
    eez_path,
    abnj_path = NULL,
    mpa_path = NULL,
    conservation_path = NULL,
    target_crs = 4326,
    layer_name = NULL,
    simplify = FALSE,
    simplify_tolerance = 0.01,
    validate = TRUE) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for read_management_layers().", call. = FALSE)
  }

  # --- Helper function to read a single layer ---
  read_single_layer <- function(path, layer_nm = NULL, label = "Layer") {
    if (is.null(path)) {
      return(NULL)
    }

    if (!file.exists(path)) {
      stop(paste0(label, " file not found: ", path), call. = FALSE)
    }

    # Determine file type and read accordingly
    ext <- tolower(tools::file_ext(path))

    layer_data <- tryCatch(
      {
        if (ext %in% c("gpkg", "geopackage")) {
          sf::st_read(path, layer = layer_nm, quiet = TRUE)
        } else if (ext == "geojson") {
          sf::st_read(path, quiet = TRUE)
        } else if (ext == "shp") {
          # For shapefiles, sf::st_read reads the .shp and handles the supporting files
          sf::st_read(path, quiet = TRUE)
        } else if (ext %in% c("gml", "kml")) {
          sf::st_read(path, quiet = TRUE)
        } else {
          stop(paste0(
            label, ": unsupported file format '.", ext, "'. ",
            "Supported: .gpkg, .geojson, .shp, .gml, .kml"
          ), call. = FALSE)
        }
      },
      error = function(e) {
        stop(paste0(label, " failed to read: ", conditionMessage(e)), call. = FALSE)
      }
    )

    layer_data
  }

  # --- Helper function to validate and process a layer ---
  process_layer <- function(data, label = "Layer", path = NULL) {
    if (is.null(data)) {
      return(NULL)
    }

    # Validate CRS
    tryCatch(
      assert_crs(data),
      error = function(e) {
        stop(paste0(label, ": ", conditionMessage(e)), call. = FALSE)
      }
    )

    # Reproject if needed
    if (!is.null(target_crs)) {
      current_crs <- sf::st_crs(data)
      target_crs_obj <- sf::st_crs(target_crs)

      if (current_crs != target_crs_obj) {
        data <- sf::st_transform(data, target_crs_obj)
      }
    }

    # Simplify if requested
    if (simplify) {
      data <- sf::st_simplify(data, dTolerance = simplify_tolerance)
    }

    # Validate spatial validity if requested
    if (validate) {
      invalid_geoms <- !sf::st_is_valid(data)
      if (any(invalid_geoms)) {
        n_invalid <- sum(invalid_geoms)
        warning(paste0(
          label, ": ", n_invalid, " invalid geometry/geometries detected. ",
          "Consider running sf::st_make_valid()."
        ))
      }
    }

    # Attach metadata
    attr(data, "source_path") <- path
    attr(data, "crs") <- sf::st_crs(data)
    attr(data, "n_features") <- nrow(data)
    attr(data, "read_date") <- Sys.time()

    data
  }

  # --- Main: Read all provided layers ---
  message("Reading management layers...")

  eez <- read_single_layer(eez_path, layer_name, "EEZ")
  eez <- process_layer(eez, "EEZ", eez_path)

  abnj <- read_single_layer(abnj_path, layer_name, "ABNJ")
  abnj <- process_layer(abnj, "ABNJ", abnj_path)

  mpa <- read_single_layer(mpa_path, layer_name, "MPA")
  mpa <- process_layer(mpa, "MPA", mpa_path)

  conservation <- read_single_layer(conservation_path, layer_name, "Conservation")
  conservation <- process_layer(conservation, "Conservation", conservation_path)

  # --- Assemble and return result ---
  result <- list(
    eez = eez,
    abnj = abnj,
    mpa = mpa,
    conservation = conservation
  )

  # Remove NULL entries
  result <- result[!sapply(result, is.null)]

  # Add class and metadata
  class(result) <- c("management_layers", "list")
  attr(result, "target_crs") <- target_crs
  attr(result, "read_date") <- Sys.time()
  attr(result, "n_layers") <- length(result)

  message("Successfully read ", length(result), " management layer(s).")

  result
}


#' Print Management Layers Object
#'
#' @param x A management_layers object.
#' @param ... Additional arguments (unused).
#'
#' @export
print.management_layers <- function(x, ...) {
  cat("=== Management Layers ===\n")
  cat("Target CRS:", attr(x, "target_crs"), "\n")
  cat("Layers loaded:", attr(x, "n_layers"), "\n\n")

  for (layer_name in names(x)) {
    layer <- x[[layer_name]]
    cat(toupper(layer_name), ":\n")
    cat("  Features:", nrow(layer), "\n")
    cat("  Geometry type:", sf::st_geometry_type(layer)[1], "\n")
    cat("  CRS:", sf::st_crs(layer)$epsg, "\n")
    cat("  Columns:", paste(names(layer)[-ncol(layer)], collapse = ", "), "\n\n")
  }

  invisible(x)
}
