# find a way make this self-referential so that we can put it in a workflow
# and not save the workflow

# notes: move multi_predict to generics

#' Nearest Neighbor Adjustment of Predictions
#'
#' @param wflow A fitted [workflows::workflow()] object.
#' @param training A data frame containing the predictors and outcome data used
#' to create `wflow`.
#' @param butcher A logical: should [butcher::butcher()] be used to trim the
#' workflow's size?
#' @param ... Not currently used.
#' @return An object of class `nn_adjust`.
#' @export
nn_adjust <- function(wflow, training, ...) {
  UseMethod("nn_adjust")
}

#' @rdname nn_adjust
#' @export
nn_adjust.default <- function(wflow, training, ...) {
  cli::cli_abort("There are no methods for this type of object.")
  invisible(NULL)
}

#' @rdname nn_adjust
#' @export
nn_adjust.workflow <- function(wflow, training, butcher = FALSE, ...) {
  if (!workflows::is_trained_workflow(wflow)) {
    cli::cli_abort("{.arg wflow} should be trainined.")
  }

  pkgs <- required_pkgs(wflow)
  rlang::check_installed(pkgs)

  mold <- hardhat::extract_mold(wflow)
  forged <- hardhat::forge(training, blueprint = mold$blueprint, outcomes = TRUE)

  train_pred <- predict(wflow, forged$predictors)$.pred

  if (butcher) {
    wflow <- butcher::butcher(wflow)
  }
  res <- list(fit = wflow, predictions = train_pred, pkgs = pkgs,
              predictors = forged$predictors, outcome = forged$outcomes[[1]])
  class(res) <- "nn_adjust"
  res
}
