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
#' @return A tibble with a numeric column `.pred` that are the adjusted and the
#' data being predicted.
#' @export
augment.nn_adjust <- function(x, new_data, ...) {
  predictions <- predict(x, new_data, ...)
  mold <- hardhat::extract_mold(x$fit)
  new_data <- hardhat::forge(new_data, blueprint = mold$blueprint)$predictors
  dplyr::bind_cols(predictions, new_data)
}
