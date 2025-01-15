#' @export
print.nn_adjust <- function(x, ...) {
	cls <- x$fit %>% hardhat::extract_spec_parsnip() %>% class()
	cli::cli_inform("Post-hoc Nearest Neighbor Adjustments\n")
	cli::cli_inform("Model classes: {cls}")
	cli::cli_inform("Reference data:")
	cli::cli_inform(c(i = "{length(x$outcome)} sample{?s}"))
	cli::cli_inform(c(i = "{ncol(x$predictors)} predictor{?s}"))
	invisible(x)
}

print.nn_cal_adjust <- function(x, ...) {
	cli::cli_inform("Post-hoc Nearest Neighbor Adjustments\n")
	cli::cli_inform("Reference data:")
	cli::cli_inform(c(i = "{length(x$outcome)} sample{?s}"))
	cli::cli_inform(c(i = "{ncol(x$predictors)} predictor{?s}"))
	invisible(x)
}

#' @export
required_pkgs.nn_adjust <- function(x, ...) {
	c("adjusted", x$pkgs)
}

# Add multi_predict
