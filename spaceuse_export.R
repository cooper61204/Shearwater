library(adehabitatHR)
library(sf)
library(sp)

calculate_trip_stats <- function(tracks, colony_coords) {
  
  # 1. Calculate time in hours for each trip
  # Group by trip_id and find the difference between start and end
  tracks <- tracks %>%
    dplyr::group_by(trip_id) %>%
    dplyr::mutate(
      duration_hrs = as.numeric(difftime(max(time), min(time), units = "hours"))
    )
  
  # 2. Calculate distance from colony in meters
  # Create a spatial point for the colony
  colony_sf <- sf::st_sfc(sf::st_point(colony_coords), crs = sf::st_crs(tracks))
  
  # Calculate distance for every point in the track
  tracks$dist_to_colony_m <- as.numeric(sf::st_distance(tracks, colony_sf))
  
  # 3. Derive Max Distance from Colony (Objective 2 requirement)
  tracks <- tracks %>%
    dplyr::mutate(max_dist_km = max(dist_to_colony_m) / 1000) %>%
    dplyr::ungroup()
  
  return(tracks)
}

#This needs to be revisited
calculate_area_metrics <- function(sf_polys) {
  # Calculate area in square kilometers
  sf_polys$area_km2 <- as.numeric(sf::st_area(sf_polys)) / 1e6
  return(sf_polys)
}

get_spatial_centroids <- function(sf_polys) {
  # Compute the geometric center of the core foraging areas
  centroids <- sf::st_centroid(sf_polys)
  return(centroids)
}

export_spaceuse_layers <- function(sf_obj, filename) {
  # Ensure units are explicit in metadata before export
  sf::st_write(sf_obj, dsn = paste0(filename, ".gpkg"), append = FALSE)
  message(paste("Layer exported to", filename))
}