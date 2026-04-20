# =============================================================================
# export_outputs.R
# Export spatial objects to GIS-ready formats
# =============================================================================


#' Export Spatial Objects to GIS-Ready Formats
#'
#' Exports tracks, polygons, overlap grids, or other spatial objects to
#' standard GIS formats (Shapefile, GeoPackage, GeoJSON). Validates CRS,
#' handles geometry types, and optionally simplifies geometries before export.
#'
#' @param x                 A spatial object or list of spatial objects.
#'                          Accepts sf, sfc, or Spatial* objects.
#' @param file_path         Character. Output file path. Extension determines
#'                          format: .shp (Shapefile), .gpkg (GeoPackage),
#'                          .geojson (GeoJSON).
#' @param layer_name        Character. For .gpkg output, the layer name within
#'                          the GeoPackage. If NULL, extracted from file_path.
#'                          Ignored for .shp and .geojson.
#' @param crs               Numeric or character. CRS to use for output.
#'                          Default NULL (use input CRS). Common values:
#'                          4326 (WGS84), 3857 (Web Mercator).
#' @param simplify          Logical. If TRUE, simplify geometries before export.
#'                          Default FALSE.
#' @param simplify_tolerance Numeric. Simplification tolerance in degrees
#'                          (if simplify = TRUE). Default 0.001.
#' @param overwrite         Logical. If TRUE, overwrite existing file.
#'                          Default FALSE.
#' @param quiet             Logical. If TRUE, suppress sf::st_write() messages.
#'                          Default TRUE.
#' @param driver_options    List. Additional driver-specific options to pass to
#'                          sf::st_write(). E.g. list(COMPRESSION = "DEFLATE")
#'                          for GeoPackage.
#'
#' @return Invisibly returns the path to the written file, along with
#'         metadata (n_features, geometry_type, crs, file_size_bytes).
#'
#' @details
#'
#' File format selection:
#' \itemize{
#'   \item \code{.shp}: Shapefile. Limited to 255 columns; best for simple features.
#'   \item \code{.gpkg}: GeoPackage. No column limit; supports multiple layers;
#'                       recommended for production use.
#'   \item \code{.geojson}: GeoJSON. Human-readable; widely supported; best for
#'                          web applications. Note: All features reprojected to
#'                          WGS84 (4326) automatically.
#' }
#'
#' CRS handling:
#' If \code{crs} is specified and differs from the input object's CRS, the object
#' is reprojected before export. This is useful for standardizing outputs across
#' multiple runs or delivering in a client-requested CRS.
#'
#' GeoJSON special behavior:
#' GeoJSON files are always exported in WGS84 (EPSG:4326) per the RFC 7946 standard.
#' If your input is in a different CRS, it will be automatically reprojected.
#'
#' Shapefile limitations:
#' Shapefiles support a maximum of 255 columns and are unable to store NULL/NA
#' values in numeric fields. Consider using GeoPackage for complex datasets.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Export a track layer to Shapefile
#' tracks_sf <- sf::st_as_sf(
#'   data.frame(track_id = 1, x = 0, y = 0),
#'   coords = c("x", "y"), crs = 4326
#' )
#' export_gis_layers(tracks_sf, "output/tracks.shp")
#'
#' # Export polygon layer to GeoPackage with compression
#' polygons_sf <- sf::st_as_sf(...)
#' export_gis_layers(
#'   polygons_sf,
#'   "output/analysis.gpkg",
#'   layer_name = "polygons",
#'   driver_options = list(COMPRESSION = "DEFLATE")
#' )
#'
#' # Export overlap grid to GeoJSON in WGS84
#' grid_sf <- sf::st_as_sf(...)
#' export_gis_layers(
#'   grid_sf,
#'   "output/overlap_grid.geojson",
#'   crs = 4326
#' )
#'
#' # Export multiple layers to GeoPackage
#' export_gis_layers(
#'   list(tracks = tracks_sf, zones = zones_sf),
#'   "output/combined.gpkg"
#' )
#' }
export_gis_layers <- function(
    x,
    file_path,
    layer_name = NULL,
    crs = NULL,
    simplify = FALSE,
    simplify_tolerance = 0.001,
    overwrite = FALSE,
    quiet = TRUE,
    driver_options = NULL) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for export_gis_layers().", call. = FALSE)
  }

  # --- Validate and normalize inputs ---
  if (is.null(file_path) || !is.character(file_path)) {
    stop("`file_path` must be a non-empty character string.", call. = FALSE)
  }

  file_path <- normalizePath(file_path, winslash = "/", mustWork = FALSE)
  output_dir <- dirname(file_path)

  if (!dir.exists(output_dir)) {
    tryCatch(
      dir.create(output_dir, recursive = TRUE),
      error = function(e) {
        stop(paste0("Cannot create output directory: ", output_dir), call. = FALSE)
      }
    )
  }

  ext <- tolower(tools::file_ext(file_path))
  if (!ext %in% c("shp", "gpkg", "geojson")) {
    stop(
      "Unsupported file format '.", ext, "'. ",
      "Supported: .shp, .gpkg, .geojson",
      call. = FALSE
    )
  }

  # --- Handle list of spatial objects (multi-layer export) ---
  if (is.list(x) && !inherits(x, c("sf", "sfc", "Spatial"))) {
    if (ext != "gpkg") {
      stop(
        "Multi-layer export requires .gpkg format. ",
        "Received: .", ext,
        call. = FALSE
      )
    }

    for (layer in names(x)) {
      lyr_name <- if (!is.null(layer_name)) layer_name else layer
      export_gis_layers(
        x[[layer]],
        file_path,
        layer_name = lyr_name,
        crs = crs,
        simplify = simplify,
        simplify_tolerance = simplify_tolerance,
        overwrite = overwrite,
        quiet = quiet,
        driver_options = driver_options
      )
    }

    return(invisible(
      structure(
        list(
          file_path = file_path,
          n_layers = length(x),
          overwrite = overwrite
        ),
        class = "exported_gis"
      )
    ))
  }

  # --- Convert Spatial* to sf if needed ---
  if (inherits(x, "Spatial")) {
    x <- sf::st_as_sf(x)
  }

  if (!inherits(x, c("sf", "sfc"))) {
    stop(
      "`x` must be an sf, sfc, or Spatial* object, or a list thereof.",
      call. = FALSE
    )
  }

  # --- Validate CRS ---
  tryCatch(
    assert_crs(x),
    error = function(e) {
      stop(paste0("CRS validation failed: ", conditionMessage(e)), call. = FALSE)
    }
  )

  # --- Reproject if needed ---
  if (!is.null(crs)) {
    target_crs <- sf::st_crs(crs)
    current_crs <- sf::st_crs(x)

    if (current_crs != target_crs) {
      message("Reprojecting from ", current_crs$epsg %||% current_crs$input,
              " to ", target_crs$epsg %||% target_crs$input, "...")
      x <- sf::st_transform(x, target_crs)
    }
  }

  # GeoJSON always uses WGS84 per RFC 7946
  if (ext == "geojson") {
    target_crs <- sf::st_crs(4326)
    current_crs <- sf::st_crs(x)
    if (current_crs != target_crs) {
      message("GeoJSON requires WGS84 (EPSG:4326); reprojecting...")
      x <- sf::st_transform(x, 4326)
    }
  }

  # --- Simplify if requested ---
  if (simplify) {
    message("Simplifying geometries (tolerance: ", simplify_tolerance, " degrees)...")
    x <- sf::st_simplify(x, dTolerance = simplify_tolerance)
  }

  # --- Check for column count (Shapefile limitation) ---
  if (ext == "shp" && ncol(x) > 255) {
    warning(
      "Shapefile format limited to 255 columns; ",
      ncol(x), " columns will be truncated. Consider using .gpkg instead."
    )
    # Keep only first 254 columns + geometry
    geom_col <- attr(x, "sf_column")
    x <- x[, c(setdiff(names(x)[1:254], geom_col), geom_col)]
  }

  # --- Prepare layer name for GeoPackage ---
  if (ext == "gpkg" && is.null(layer_name)) {
    layer_name <- tools::file_path_sans_ext(basename(file_path))
  }

  # --- Construct st_write arguments ---
  write_args <- list(
    obj = x,
    dsn = file_path,
    quiet = quiet,
    delete_dsn = overwrite && !file.exists(file_path),
    delete_layer = overwrite && file.exists(file_path)
  )

  if (ext == "gpkg" && !is.null(layer_name)) {
    write_args$layer <- layer_name
    write_args$append <- file.exists(file_path)  # Append if multi-layer
  }

  if (!is.null(driver_options) && is.list(driver_options)) {
    write_args$layer_options <- driver_options
  }

  # --- Write file ---
  message("Writing to ", file_path, "...")
  tryCatch(
    {
      do.call(sf::st_write, write_args)
    },
    error = function(e) {
      stop(
        "Failed to write file: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # --- Collect metadata ---
  file_size <- file.size(file_path)
  n_features <- nrow(x)
  geom_type <- unique(sf::st_geometry_type(x))
  output_crs <- sf::st_crs(x)

  message("Success! Exported ", n_features, " feature(s) to ", file_path)

  # --- Return metadata invisibly ---
  invisible(
    structure(
      list(
        file_path = file_path,
        n_features = n_features,
        geometry_type = paste(geom_type, collapse = ", "),
        crs = output_crs$epsg %||% output_crs$input,
        file_size_bytes = file_size,
        export_date = Sys.time()
      ),
      class = "exported_gis"
    )
  )
}


#' Print Exported GIS Object Metadata
#'
#' @param x An exported_gis object.
#' @param ... Additional arguments (unused).
#'
#' @export
print.exported_gis <- function(x, ...) {
  cat("=== GIS Export Result ===\n")
  cat("File:", x$file_path, "\n")
  if (!is.null(x$n_features)) {
    cat("Features:", x$n_features, "\n")
    cat("Geometry type:", x$geometry_type, "\n")
    cat("CRS:", x$crs, "\n")
    cat("File size:", format(object.size(x$file_size_bytes), units = "auto"), "\n")
  }
  if (!is.null(x$n_layers)) {
    cat("Layers written:", x$n_layers, "\n")
  }
  cat("Exported:", format(x$export_date, "%Y-%m-%d %H:%M:%S"), "\n")

  invisible(x)
}
