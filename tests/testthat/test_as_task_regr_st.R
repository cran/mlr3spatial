
test_that("as_task_regr_st works on data.frame objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 100)
  vector = st_as_sf(sample_stack(stack, n = 100))
  data = as.data.frame(vector)
  data$geometry = NULL
  data = cbind(data, st_coordinates(vector))

  task = as_task_regr_st(data, target = "y", coordinate_names = c("X", "Y"), crs = "EPSG:4326")
  expect_class(task, "TaskRegrST")
  expect_data_table(task$coordinates(), types = "numeric", ncols = 2, nrows = 100)
  expect_names(colnames(task$coordinates()), identical.to = c("X", "Y"))
  expect_equal(task$coordinate_names, c("X", "Y"))
  expect_equal(task$crs, "EPSG:4326")
  expect_equal(task$col_roles$feature, "x_1")
  expect_equal(task$col_roles$coordinate, c("X", "Y"))
})

test_that("as_task_regr_st works on DataBackendDataTable objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 100)
  vector = st_as_sf(sample_stack(stack, n = 100))
  data = as.data.frame(vector)
  data$geometry = NULL
  data = cbind(data, st_coordinates(vector))
  backend = as_data_backend(data)

  task = as_task_regr_st(backend, target = "y", coordinate_names = c("X", "Y"), crs = "EPSG:4326")
  expect_class(task, "TaskRegrST")
  expect_data_table(task$coordinates(), types = "numeric", ncols = 2, nrows = 100)
  expect_names(colnames(task$coordinates()), identical.to = c("X", "Y"))
  expect_equal(task$coordinate_names, c("X", "Y"))
  expect_equal(task$crs, "EPSG:4326")
  expect_equal(task$col_roles$feature, "x_1")
  expect_equal(task$col_roles$coordinate, c("X", "Y"))
})

test_that("as_task_regr_st works on sf objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 100)
  vector = st_as_sf(sample_stack(stack, n = 100))

  task = as_task_regr_st(vector, target = "y")
  expect_class(task, "TaskRegrST")
  expect_data_table(task$coordinates(), types = "numeric", ncols = 2, nrows = 100)
  expect_names(colnames(task$coordinates()), identical.to = c("X", "Y"))
  expect_equal(task$coordinate_names, c("X", "Y"))
  expect_equal(task$crs, st_crs(vector)$wkt)
  expect_equal(task$col_roles$feature, "x_1")
  expect_equal(task$col_roles$coordinate, c("X", "Y"))
})

test_that("as_task_regr_st works on TaskRegrST objects", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y")),
  dimension = 100)
  vector = st_as_sf(sample_stack(stack, n = 100))

  task = as_task_regr_st(vector, target = "y")
  task = as_task_regr_st(task)
  expect_class(task, "TaskRegrST")
  expect_data_table(task$coordinates(), types = "numeric", ncols = 2, nrows = 100)
  expect_names(colnames(task$coordinates()), identical.to = c("X", "Y"))
  expect_equal(task$coordinate_names, c("X", "Y"))
  expect_equal(task$crs, st_crs(vector)$wkt)
  expect_equal(task$col_roles$feature, "x_1")
  expect_equal(task$col_roles$coordinate, c("X", "Y"))
})

test_that("convert from TaskClassifST to TaskRegrST", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("x_2"),
    factor_layer("y", levels = c("a", "b"))),
  dimension = 100)
  vector = st_as_sf(sample_stack(stack, n = 100))

  task = as_task_classif_st(vector, target = "y")
  task = as_task_regr_st(task, target = "x_2", drop_original_target = TRUE)
  expect_class(task, "TaskRegrST")
  expect_data_table(task$coordinates(), types = "numeric", ncols = 2, nrows = 100)
  expect_names(colnames(task$coordinates()), identical.to = c("X", "Y"))
  expect_equal(task$coordinate_names, c("X", "Y"))
  expect_equal(task$crs, st_crs(vector)$wkt)
  expect_equal(task$col_roles$feature, c("x_1", "y"))
  expect_equal(task$col_roles$coordinate, c("X", "Y"))
})

test_that("as_task_regr_st throws an error when coordinates are already in the data", {
  stack = generate_stack(list(
    numeric_layer("x_1"),
    numeric_layer("y"),
    numeric_layer("X"),
    numeric_layer("Y")),
  dimension = 100)
  vector = st_as_sf(sample_stack(stack, n = 100))

  expect_error(as_task_regr_st(vector, target = "y"), "contains columns named 'X' and 'Y' which are reserved for coordinates")
})
