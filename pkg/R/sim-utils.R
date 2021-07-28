#' Generate data from sparse linear model
#' @export
gen_dat <- function(n = 1e2, p = 20, nz = 5, b = rep(1:0, c(nz, p - nz))) {
	X <- matrix(rnorm(n * p), nrow = n, ncol = p)
	Y <- factor(rbinom(n = n, size = 1, plogis(X %*% b)))
	return(data.frame(Y = Y, X = X))
}

#' Compute accuracy
#' @export
acc <- function(y_true, y_pred) {
	mean(y_true == y_pred)
}

#' Compute negative log-likelihood / n
#' @export
nll <- function(y_pred, y_true) {
	if (is.factor(y_true) & length(levels(y_true)) == 2)
		y_true <- as.numeric(y_true) - 1
	- mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
}

#' Evaluate model at new observations with `loss`
#' @export
eval_mod <- function(m, newx, y_true, loss, ...) {
	preds <- predict(m, newx = newx, ... = ...)
	loss(preds, y_true)
}
