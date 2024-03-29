test_that("LearnerRegrSpatial ignores observations with missing values", {
  skip_if_not_installed("mlr3learners")
  require_namespaces("mlr3learners")

  # train task
  stack = generate_stack(list(
    factor_layer("c_1", levels = c("a", "b")),
    numeric_layer("y")),
  dimension = 100)
  vector = sample_stack(stack, n = 100)
  task_train = as_task_regr_st(vector, id = "test_vector", target = "y")
  learner = lrn("regr.ranger")
  learner$train(task_train)

  # predict task
  stack$y = NULL
  stack = mask_stack(stack)
  task_predict = as_task_unsupervised(stack, id = "test")
  learner_spatial = LearnerRegrSpatial$new(learner)
  pred = learner_spatial$predict(task_predict)

  expect_true(all(is.na(pred$response[seq(100)])))
  expect_numeric(pred$response, any.missing = TRUE, all.missing = FALSE)
})
