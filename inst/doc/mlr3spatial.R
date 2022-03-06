## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = TRUE
)

## -----------------------------------------------------------------------------
library("mlr3")
library("mlr3spatial")

## ---- error=TRUE--------------------------------------------------------------
tif = system.file("tif/L7_ETMs.tif", package = "stars")
stack = stars::read_stars(tif)

backend = as_data_backend(stack)
task = as_task_regr(backend, target = "band1")

print(task)

## -----------------------------------------------------------------------------
learner = lrn("regr.rpart")
set.seed(42)
row_ids = sample(1:task$nrow, 500)
learner$train(task, row_ids = row_ids)

print(learner)

## -----------------------------------------------------------------------------
ras = predict_spatial(task, learner, format = "stars")
names(ras) = "cadmium"

print(ras)

## -----------------------------------------------------------------------------
rpart_learner = rpart::rpart(band1 ~ ., data = task$data(rows = row_ids))
stars_stack = as.data.table(split(stack, "band"))
stars_stack[, c("x", "y", "X1")] = NULL
colnames(stars_stack) = task$feature_names

stars_pred = predict(rpart_learner, stars_stack)

# subset stars object to one band only
stars_pred_ras = stack[, , , 1]
# rename the layer name
names(stars_pred_ras) = "pred"
# assign predictions
stars_pred_ras$pred = stars_pred

print(stars_pred_ras)

## -----------------------------------------------------------------------------
all.equal(as.numeric(stars_pred_ras$pred), as.numeric(ras$cadmium))

## -----------------------------------------------------------------------------
plot(ras, col = c("#440154FF", "#443A83FF", "#31688EFF", "#21908CFF", "#35B779FF", "#8FD744FF", "#FDE725FF"))

