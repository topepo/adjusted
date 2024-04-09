#' Make adjusted predictions
#' @param object An object of class [nn_adjust()].
#' @param new_data A data frame with the original predictors in their original
#' format.
#' @param neighbors An integer for the number of neighbors. Zero indicates no
#' adjustment.
#' @param eps A small constant added to distance to avoid divide by zero.
#' @param cores An integer for how many cores [gower::gower_topn()] should use.
#' @param ... Not currently used.
#' @return A tibble with a numeric column `.pred` that are the adjusted
#' predictions.
#' @seealso [nn_adjust()], [augment.nn_adjust()]
#' @examples
#' # example code
#'
#' if (rlang::is_installed(c("ggplot2", "parsnip", "rpart", "MASS"))) {
#'
#'   library(workflows)
#'   library(dplyr)
#'   library(parsnip)
#'   library(ggplot2)
#'
#'   # ------------------------------------------------------------------------------
#'   # Use the 1D motorcycle helmet data as an example
#'
#'   data(mcycle, package = "MASS")
#'
#'   # Use every fifth data point as a test point
#'   in_test <- ( 1:nrow(mcycle) ) %% 5 == 0
#'   cycl_train <- mcycle[-in_test, ]
#'   cycl_test  <- mcycle[ in_test, ]
#'
#'   # ------------------------------------------------------------------------------
#'   # Fit a decision tree
#'
#'   cart_spec <- decision_tree() %>% set_mode("regression")
#'
#'   cart_fit <-
#'     workflow(accel ~ times, cart_spec) %>%
#'     fit(data = cycl_train)
#'
#'   adj_obj <- nn_adjust(cart_fit, cycl_train)
#'
#'   # Raw predictions plus data:
#'   augment(cart_fit, head(cycl_test))
#'
#'   # Adjusted predictions:
#'   predict(adj_obj, head(cycl_test), neighbors = 10)
#'
#'   # Add the data too
#'   augment(adj_obj, head(cycl_test), neighbors = 10)
#'
#' }
#' @export
predict.nn_adjust <- function(object, new_data, neighbors = 3, eps = 1 / 2, cores = 1, ...) {
  rlang::check_installed(object$pkgs)
  mold <- hardhat::extract_mold(object$fit)
  new_data <- hardhat::forge(new_data, blueprint = mold$blueprint)$predictors

  neighbors <- check_neighbors(neighbors, object)
  # check data via ptype (via forge?)

  new_predictions <- predict(object$fit, new_data)$.pred
  if (neighbors == 0) {
    return(tibble::tibble(.pred = new_predictions))
  }

  nn_object <-  gower::gower_topn(new_data, object$predictors, n = neighbors, nthread = cores)
  ref_predictions <- apply(nn_object$index, 1, function(x) object$predictions[x])
  ref_outcomes    <- apply(nn_object$index, 1, function(x) object$outcome[x])
  nn_wts <- 1 / ( t(nn_object$distance) + eps )
  wt_sum <- apply(nn_wts, 1, sum)
  adjusted <- apply( ( (new_predictions - ref_predictions) + ref_outcomes ) * nn_wts, 1, sum)
  tibble::tibble(.pred = adjusted / wt_sum)
}


check_neighbors <- function(neighbors, object) {
  if (neighbors < 0) {
    neighbors <- 0
  } else {
    neighbors <- min(neighbors, length(object$outcome))
  }
  neighbors
}


# ------------------------------------------------------------------------------

#' Augment data with predicted values
#' @inheritParams predict.nn_adjust
#' @param x An object of class [nn_adjust()].
#' @return The data being predicted with an additional column `.pred` that are
#' the adjusted predictions. If `new_data` contains the original outcome column,
#' there is also a `.resid` column.
#' @seealso [nn_adjust()], [predict.nn_adjust()]
#' @examples
#' # example code
#'
#' if (rlang::is_installed(c("ggplot2", "parsnip", "rpart", "MASS"))) {
#'
#'   library(workflows)
#'   library(dplyr)
#'   library(parsnip)
#'   library(ggplot2)
#'
#'   # ------------------------------------------------------------------------------
#'   # Use the 1D motorcycle helmet data as an example
#'
#'   data(mcycle, package = "MASS")
#'
#'   # Use every fifth data point as a test point
#'   in_test <- ( 1:nrow(mcycle) ) %% 5 == 0
#'   cycl_train <- mcycle[-in_test, ]
#'   cycl_test  <- mcycle[ in_test, ]
#'
#'   # ------------------------------------------------------------------------------
#'   # Fit a decision tree
#'
#'   cart_spec <- decision_tree() %>% set_mode("regression")
#'
#'   cart_fit <-
#'     workflow(accel ~ times, cart_spec) %>%
#'     fit(data = cycl_train)
#'
#'   adj_obj <- nn_adjust(cart_fit, cycl_train)\
#'
#'   # Raw predictions plus data:
#'   augment(cart_fit, head(cycl_test))
#'
#'   # Adjusted predictions:
#'   predict(adj_obj, head(cycl_test), neighbors = 10)
#'
#'   # Add the data too
#'   augment(adj_obj, head(cycl_test), neighbors = 10)
#'
#' }
#' @export
augment.nn_adjust <- function(x, new_data, ...) {
  predictions <- predict(x, new_data, ...)
  mold <- hardhat::extract_mold(x$fit)
  y_names <- names(mold$outcomes)
  has_outcome <- any(names(new_data) %in% y_names)
  forged <- hardhat::forge(new_data, blueprint = mold$blueprint, outcomes = has_outcome)

  predictors <- forged$predictors
  outcomes <-  forged$outcomes
  res <- dplyr::bind_cols(predictions, outcomes, predictors)
  if (has_outcome) {
    res$.resid <- res[[y_names]] - res$.pred
    res <- dplyr::relocate(res, .resid, .after = dplyr::all_of(y_names))
  }
  res
}
