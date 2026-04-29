library(sf)

track_df <- data.frame(
  track_id = c("bird1", "bird2", "bird3"),
  x = c(0.5, 1.5, 5),
  y = c(0.5, 1.5, 5)
)

track_sf <- st_as_sf(track_df, coords = c("x", "y"), crs = 4326)

poly1 <- st_polygon(list(rbind(
  c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
)))

poly2 <- st_polygon(list(rbind(
  c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1)
)))

fisheries_sf <- st_sf(
  cell_id = c("cellA", "cellB"),
  gear = c("longline", "trawl"),
  geometry = st_sfc(poly1, poly2),
  crs = 4326
)
