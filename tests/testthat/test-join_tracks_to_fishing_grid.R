test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

library(testthat)
library(sf)

test_that("join_tracks_to_fishing_grid works for intersects", {
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

  out <- join_tracks_to_fishing_grid(track_sf, fisheries_sf, "intersects")

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 2)
  expect_true(all(c("cell_id", "gear") %in% names(out)))
  expect_true(all(out$track_id %in% c("bird1", "bird2")))
  expect_false("bird3" %in% out$track_id)
})

test_that("join_tracks_to_fishing_grid works for within", {
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

  out <- join_tracks_to_fishing_grid(track_sf, fisheries_sf, "within")

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 2)
  expect_true(all(out$track_id %in% c("bird1", "bird2")))
})

test_that("join_tracks_to_fishing_grid works for nearest", {
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

  out <- join_tracks_to_fishing_grid(track_sf, fisheries_sf, "nearest")

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 3)
  expect_true(all(c("cell_id", "gear") %in% names(out)))
  expect_true(all(c("bird1", "bird2", "bird3") %in% out$track_id))
})

test_that("join_tracks_to_fishing_grid errors if track_data is not sf", {
  track_df <- data.frame(track_id = "bird1", x = 0.5, y = 0.5)

  poly1 <- st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  )))
  fisheries_sf <- st_sf(
    cell_id = "cellA",
    geometry = st_sfc(poly1),
    crs = 4326
  )

  expect_error(
    join_tracks_to_fishing_grid(track_df, fisheries_sf),
    "track_data must be an sf object."
  )
})

test_that("join_tracks_to_fishing_grid errors if fisheries_data is not sf", {
  track_df <- data.frame(track_id = "bird1", x = 0.5, y = 0.5)
  track_sf <- st_as_sf(track_df, coords = c("x", "y"), crs = 4326)
  fisheries_df <- data.frame(cell_id = "cellA")

  expect_error(
    join_tracks_to_fishing_grid(track_sf, fisheries_df),
    "fisheries_data must be an sf object."
  )
})

test_that("join_tracks_to_fishing_grid errors on CRS mismatch", {
  track_df <- data.frame(track_id = "bird1", x = 0.5, y = 0.5)
  track_sf <- st_as_sf(track_df, coords = c("x", "y"), crs = 4326)

  poly1 <- st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  )))
  fisheries_sf <- st_sf(
    cell_id = "cellA",
    geometry = st_sfc(poly1),
    crs = 4326
  )

  fisheries_sf_3857 <- st_transform(fisheries_sf, 3857)

  expect_error(join_tracks_to_fishing_grid(track_sf, fisheries_sf_3857))
})

test_that("default join_type is intersects", {
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
    geometry = st_sfc(poly1, poly2),
    crs = 4326
  )

  out_default <- join_tracks_to_fishing_grid(track_sf, fisheries_sf)
  out_intersects <- join_tracks_to_fishing_grid(track_sf, fisheries_sf, "intersects")

  expect_equal(out_default$track_id, out_intersects$track_id)
  expect_equal(out_default$cell_id, out_intersects$cell_id)
})

test_that("invalid join_type errors", {
  track_df <- data.frame(track_id = "bird1", x = 0.5, y = 0.5)
  track_sf <- st_as_sf(track_df, coords = c("x", "y"), crs = 4326)

  poly1 <- st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  )))
  fisheries_sf <- st_sf(
    cell_id = "cellA",
    geometry = st_sfc(poly1),
    crs = 4326
  )

  expect_error(
    join_tracks_to_fishing_grid(track_sf, fisheries_sf, "bad_join")
  )
})
