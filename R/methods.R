#' @export
print.nn_adjust <- function(x, ...) {
  cat("Post-hoc Nearest Neighbor Adjustments\n")
  cls <- x$fit %>% hardhat::extract_spec_parsnip() %>% class() %>% purrr::pluck(1)
  cat("Model:", cls, "\n")
  cat("Training set: n = ", length(x$predictions), ", ", ncol(x$predictors), " predictors\n", sep = "")
  invisible(x)
}

#' @export
required_pkgs.nn_adjust <- function(x, ...) {
  c("adjusted", x$pkgs)
}

# Add multi_predict
