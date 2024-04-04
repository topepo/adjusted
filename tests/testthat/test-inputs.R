test_that("check workflow object", {
  skip_if_not_installed("modeldata")
  library(workflows)
  library(parsnip)

  data(two_class_dat, package = "modeldata")
  wflow <- workflow(Class ~ ., logistic_reg())
  fit <- wflow %>% fit(data = two_class_dat)

  expect_snapshot(nn_adjust(fit, two_class_dat), error = TRUE)
  expect_snapshot(nn_adjust(wflow, two_class_dat), error = TRUE)
})

