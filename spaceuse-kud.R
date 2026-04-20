#kud
#install.packages("sp")

library(adehabitatHR)
library(sf)
library(sp)


calculate_kud <- function(tracks, ref = "href") {
  # Convert sf to SpatialPointsDataFrame for adehabitatHR
  coords <- sf::st_coordinates(tracks)
  sp_df <- sp::SpatialPointsDataFrame(coords, data = as.data.frame(tracks))
  # Calculate KUD
  kud <- adehabitatHR::kernelUD(sp_df[, "trip_id"], h = ref)
  return(kud)
}


get_isopleths <- function(kud, levels = c(50, 95)) {
  # Extract polygons at specified levels
  polys <- adehabitatHR::getverticeshr(kud, percent = levels)
  
  sf_polys <- sf::st_as_sf(polys)
  return(sf_polys)
}


