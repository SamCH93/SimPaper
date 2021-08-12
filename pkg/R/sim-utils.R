
### Data generating process

#' Generate data from sparse linear model
#' @export
generateData <- function(n = 1e2, p = 20, nz = 5, b = rep(1:0, c(nz, p - nz))) {
	X <- matrix(rnorm(n * p), nrow = n, ncol = p)
	Y <- factor(rbinom(n = n, size = 1, plogis(X %*% b)))
	return(data.frame(Y = Y, X = X))
}

### Evaluation

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
evaluateModel <- function(m, newx, y_true, loss, ...) {
  # TODO: vectorize over loss functions
	preds <- predict(m, newx = newx, ... = ...)
	loss(preds, y_true)
}

### Sim Design Functions

#' SimDesign function for generating the data
#' @export
generate <- function(condition, fixed_objects = list(ntest = 1e4)) {
  ## Condition
  n <- condition$n
  epv <- condition$epv
  p <- condition$p
  q <- condition$q
  rho <- condition$rho
  prev <- condition$prev

  ## Simulate beta
  if (q > 0) {
    nzbetas <- rnorm(q)
  } else {
    nzbetas <- NULL
  }
  betas <- c(nzbetas, rep(0, p - q))

  ## Generate training data
  train <- generateData(n = n, p = p, nz = q, b = betas)
  test <- generateData(n = fixed_objects$ntest, p = p, nz = q, b = betas)
  list(train, test)
}

#' SimDesign function for analyzing simulated data
#' @export
analyze <- function(condition, dat, fixed_objects = NULL) {
  ## Data
  train <- dat$train
  test <- dat$test

  ## AINET
  # TODO: cross validate importance?
	pen.f <- ainet:::.vimp(Y ~ ., train, which = "impurity", gamma = 1, renorm = "trunc")
	cvAINET <- cv.fglmnet(fml, train, pen.f = pen.f, family = "binomial", relax = TRUE)
  AINET <- ainet(Y ~ ., data = train, pen.f = pen.f, lambda = cvAINET$relaxed$lambda.1se,
                 alpha = cvAINET$relaxed$gamma.1se)

  ## Logistic regression
  cvGLM <- cv.fglmnet(Y ~ ., data = train, alpha = 0, family = "binomial")
  GLM <- fglmnet(Y ~ ., data = train, alpha = 0, lambda = cvGLM$lambda.1se,
                 family = "binomial")

  ## Elastic net
  cvEN <- cv.fglmnet(Y ~ ., data = train, alpha = 0, relax = TRUE, family = "binomial")
  GLM <- fglmnet(Y ~ ., data = train, alpha = cvEN$relaxed$gamma.1se,
                 lambda = cvGLM$relaxed$lambda.1se, family = "binomial")

  ## Adaptive elastic net
  # TODO: or adaptive lasso only? -> clarify in protocol
  # TODO: Penalty factor
  cvAEN <- cv.fglmnet(Y ~ ., data = train, alpha = 0, penalty.factor = NULL)
  AEN <- fglmnet(Y ~ ., data = train, alpha = 0, penalty.factor = NULL)

  ## Random forest
  RF <- ranger(Y ~ ., data = train)

  ## Return
  # TODO: Evaluate
  ret <- c(stat1 = NaN, stat2 = NaN)
  ret
}

#' SimDesign function for summarizing simulation results
#' @export
summarize <- function(condition, results, fixed_objects = NULL) {
  ret <- c(bias = NaN, RMSE = NaN)
  ret
}
