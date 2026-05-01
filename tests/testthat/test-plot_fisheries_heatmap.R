test_that("plot_fisheries_heatmap returns a ggplot object for valid sf input", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  poly1 <- sf::st_polygon(list(matrix(
    c(0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0),
    ncol = 2,
    byrow = TRUE
  )))

  poly2 <- sf::st_polygon(list(matrix(
    c(1, 0,
      2, 0,
      2, 1,
      1, 1,
      1, 0),
    ncol = 2,
    byrow = TRUE
  )))

  grid_data <- sf::st_sf(
    cell_id = c("a", "b"),
    total_overlap = c(10, 20),
    geometry = sf::st_sfc(poly1, poly2),
    crs = 4326
  )

  p <- plot_fisheries_heatmap(grid_data)

  expect_s3_class(p, "ggplot")
})


test_that("plot_fisheries_heatmap works with a custom fill column", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  poly1 <- sf::st_polygon(list(matrix(
    c(0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0),
    ncol = 2,
    byrow = TRUE
  )))

  poly2 <- sf::st_polygon(list(matrix(
    c(1, 0,
      2, 0,
      2, 1,
      1, 1,
      1, 0),
    ncol = 2,
    byrow = TRUE
  )))

  grid_data <- sf::st_sf(
    cell_id = c("a", "b"),
    risk_index = c(0.25, 0.75),
    geometry = sf::st_sfc(poly1, poly2),
    crs = 4326
  )

  p <- plot_fisheries_heatmap(
    grid_data,
    fill_col = "risk_index"
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_fisheries_heatmap stores custom title and legend title", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  poly1 <- sf::st_polygon(list(matrix(
    c(0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0),
    ncol = 2,
    byrow = TRUE
  )))

  grid_data <- sf::st_sf(
    cell_id = "a",
    total_overlap = 10,
    geometry = sf::st_sfc(poly1),
    crs = 4326
  )

  p <- plot_fisheries_heatmap(
    grid_data,
    title = "Overlap Heatmap",
    legend_title = "Overlap Intensity"
  )

  expect_equal(p$labels$title, "Overlap Heatmap")

  fill_scale <- p$scales$get_scales("fill")
  expect_equal(fill_scale$name, "Overlap Intensity")
})


test_that("plot_fisheries_heatmap handles NA fill values", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  poly1 <- sf::st_polygon(list(matrix(
    c(0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0),
    ncol = 2,
    byrow = TRUE
  )))

  poly2 <- sf::st_polygon(list(matrix(
    c(1, 0,
      2, 0,
      2, 1,
      1, 1,
      1, 0),
    ncol = 2,
    byrow = TRUE
  )))

  grid_data <- sf::st_sf(
    cell_id = c("a", "b"),
    total_overlap = c(10, NA),
    geometry = sf::st_sfc(poly1, poly2),
    crs = 4326
  )

  p <- plot_fisheries_heatmap(grid_data)

  expect_s3_class(p, "ggplot")
})


test_that("plot_fisheries_heatmap accepts numeric-like character values", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  poly1 <- sf::st_polygon(list(matrix(
    c(0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0),
    ncol = 2,
    byrow = TRUE
  )))

  poly2 <- sf::st_polygon(list(matrix(
    c(1, 0,
      2, 0,
      2, 1,
      1, 1,
      1, 0),
    ncol = 2,
    byrow = TRUE
  )))

  grid_data <- sf::st_sf(
    cell_id = c("a", "b"),
    total_overlap = c("10", "20"),
    geometry = sf::st_sfc(poly1, poly2),
    crs = 4326
  )

  p <- plot_fisheries_heatmap(grid_data)

  expect_s3_class(p, "ggplot")
})


test_that("plot_fisheries_heatmap handles empty sf objects", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  poly1 <- sf::st_polygon(list(matrix(
    c(0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0),
    ncol = 2,
    byrow = TRUE
  )))

  grid_data <- sf::st_sf(
    cell_id = "a",
    total_overlap = 10,
    geometry = sf::st_sfc(poly1),
    crs = 4326
  )

  empty_grid <- grid_data[0, ]

  p <- plot_fisheries_heatmap(empty_grid)

  expect_s3_class(p, "ggplot")
})


test_that("plot_fisheries_heatmap errors when grid_data is not an sf object", {
  grid_data <- data.frame(
    cell_id = c("a", "b"),
    total_overlap = c(10, 20)
  )

  expect_error(
    plot_fisheries_heatmap(grid_data),
    "grid_data must be an sf object."
  )
})


test_that("plot_fisheries_heatmap errors when fill_col is missing", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  poly1 <- sf::st_polygon(list(matrix(
    c(0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0),
    ncol = 2,
    byrow = TRUE
  )))

  grid_data <- sf::st_sf(
    cell_id = "a",
    total_overlap = 10,
    geometry = sf::st_sfc(poly1),
    crs = 4326
  )

  expect_error(
    plot_fisheries_heatmap(grid_data, fill_col = "risk_index"),
    "fill_col not found in grid_data."
  )
})


test_that("plot_fisheries_heatmap errors when fill column is not numeric-like", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  poly1 <- sf::st_polygon(list(matrix(
    c(0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0),
    ncol = 2,
    byrow = TRUE
  )))

  poly2 <- sf::st_polygon(list(matrix(
    c(1, 0,
      2, 0,
      2, 1,
      1, 1,
      1, 0),
    ncol = 2,
    byrow = TRUE
  )))

  grid_data <- sf::st_sf(
    cell_id = c("a", "b"),
    total_overlap = c("high", "low"),
    geometry = sf::st_sfc(poly1, poly2),
    crs = 4326
  )

  expect_error(
    plot_fisheries_heatmap(grid_data),
    "fill_col must contain numeric or numeric-like values."
  )
})
