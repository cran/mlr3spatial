#' @title Convert to a Spatiotemporal Regression Task
#'
#' @description
#' Convert object to a [TaskRegrST].
#' This is a S3 generic, specialized for at least the following objects:
#'
#' 1. [TaskRegrST]: Ensure the identity.
#' 2. [data.frame()] and [mlr3::DataBackend]: Provides an alternative to the constructor of [TaskRegrST].
#' 3. [sf::sf]: Extracts spatial meta data before construction.
#' 4. [mlr3::TaskClassif]: Calls [mlr3::convert_task()].
#'
#' @inheritParams mlr3::as_task_regr
#' @template param_coords_as_features
#' @template param_crs
#' @template param_coordinate_names
#'
#' @return [TaskRegrST]
#' @export
as_task_regr_st = function(x, ...) {
  UseMethod("as_task_regr_st")
}

#' @rdname as_task_regr_st
#' @export as_task_regr_st.TaskRegrST
#' @exportS3Method
as_task_regr_st.TaskRegrST = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @rdname as_task_regr_st
#' @export as_task_regr_st.data.frame
#' @exportS3Method
as_task_regr_st.data.frame = function(x, target, id = deparse(substitute(x)), coordinate_names, crs = NA_character_, coords_as_features = FALSE, label = NA_character_, ...) {
  ii = which(map_lgl(keep(x, is.double), anyInfinite))
  if (length(ii)) {
    warningf("Detected columns with unsupported Inf values in data: %s", str_collapse(names(ii)))
  }
  TaskRegrST$new(id = id, backend = x, target = target, coords_as_features = coords_as_features, crs = crs, coordinate_names = coordinate_names, label = label)
}

#' @rdname as_task_regr_st
#' @export as_task_regr_st.DataBackend
#' @exportS3Method
as_task_regr_st.DataBackend = function(x, target, id = deparse(substitute(x)), coordinate_names, crs, coords_as_features = FALSE, label = NA_character_, ...) {
  TaskRegrST$new(id = id, backend = x, target = target, coords_as_features = coords_as_features, crs = crs, coordinate_names = coordinate_names, label = label)
}

#' @rdname as_task_regr_st
#' @export as_task_regr_st.sf
#' @exportS3Method
as_task_regr_st.sf = function(x, target = NULL, id = deparse(substitute(x)), coords_as_features = FALSE, label = NA_character_, ...) {
  id = as.character(id)
  geometries = as.character(unique(sf::st_geometry_type(x)))
  if (!test_names(geometries, identical.to = "POINT")) {
    stop("Simple feature may not contain geometries of type '%s'", str_collapse(setdiff(geometries, "POINT")))
  }

  if (any(c("X", "Y") %in% colnames(x))) {
    stopf("Data contains columns named 'X' and 'Y' which are reserved for coordinates. The sf object might contain coordinates in the geometry column and the `X` and `Y` columns. Please remove or rename them before converting to a task.")
  }

  # extract spatial meta data
  crs = sf::st_crs(x)$wkt
  coordinates = as.data.frame(sf::st_coordinates(x))
  coordinate_names = colnames(coordinates)

  # convert sf to data.frame
  x[[attr(x, "sf_column")]] = NULL
  attr(x, "sf_column") = NULL
  x = as.data.frame(x)

  # add coordinates
  x = cbind(x, coordinates)

  as_task_regr_st(x, target = target, id = id, coords_as_features = coords_as_features, crs = crs, coordinate_names = coordinate_names, label = label)
}

#' @rdname as_task_regr_st
#' @export as_task_regr_st.TaskClassifST
#' @exportS3Method
as_task_regr_st.TaskClassifST = function(x, target = NULL, drop_original_target = FALSE, drop_levels = TRUE, ...) {
  convert_task(intask = x, target = target, new_type = "regr_st", drop_original_target = FALSE, drop_levels = TRUE)
}
