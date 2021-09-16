
### Data generating process

#' Generate data from sparse linear model
#' @export
generateData <- function(n = 1e2, p = 20, nz = 5, b = rep(1:0, c(nz, p - nz)),
                         prev = 0.5, rho = 0) {
  b0 <- qlogis(prev)
  Sigma <- matrix(rho, nrow = p, ncol = p)
  diag(Sigma) <- 1
	X <- rmvnorm(n, sigma = Sigma)
	Y <- factor(rbinom(n = n, size = 1, plogis(b0 + X %*% b)))
	return(data.frame(Y = Y, X = X))
}

### Evaluation

#' Compute accuracy
#' @examples
#' dat <- generateData()
#' ndat <- generateData()
#' m <- glm(Y ~ ., data = dat, family = binomial)
#' preds <- predict(m, type = "response", newdata = ndat)
#' acc(ndat$Y, preds)
#' @export
acc <- function(y_true, y_pred) {
  y_true <- .assert_numeric_binary(y_true)
  y_pred <- y_pred >= 0.5
	mean(y_true == y_pred)
}

#' Compute negative log-likelihood / n (log-score)
#' @examples
#' dat <- generateData()
#' ndat <- generateData()
#' m <- glm(Y ~ ., data = dat, family = binomial)
#' preds <- predict(m, type = "response", newdata = ndat)
#' nll(ndat$Y, preds)
#' @export
nll <- function(y_true, y_pred) {
  y_true <- .assert_numeric_binary(y_true)
	- mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
}

#' Compute AUC
#' @examples
#' dat <- generateData()
#' ndat <- generateData()
#' m <- glm(Y ~ ., data = dat, family = binomial)
#' preds <- predict(m, type = "response", newdata = ndat)
#' auroc(ndat$Y, preds)
#' @export
auroc <- function(y_true, y_pred) {
  auc(y_true, as.vector(y_pred))
}

#' Compute Brier score
#' @examples
#' dat <- generateData()
#' ndat <- generateData()
#' m <- glm(Y ~ ., data = dat, family = binomial)
#' preds <- predict(m, type = "response", newdata = ndat)
#' brier(ndat$Y, preds)
#' @export
brier <- function(y_true, y_pred) {
  y_true <- .assert_numeric_binary(y_true)
  mean((y_true - y_pred)^2)
}

#' Compute scaled Brier score
#' @examples
#' dat <- generateData()
#' ndat <- generateData()
#' m <- glm(Y ~ ., data = dat, family = binomial)
#' preds <- predict(m, type = "response", newdata = ndat)
#' scaledBrier(ndat$Y, preds)
#' @export
scaledBrier <- function(y_true, y_pred) {
  y_true <- .assert_numeric_binary(y_true)
  prev <- mean(y_true)
  1 - brier(y_true, y_pred) / brier(y_true, prev)
}

#' Compute calibration slope
#' @examples
#' dat <- generateData()
#' ndat <- generateData()
#' m <- glm(Y ~ ., data = dat, family = binomial)
#' preds <- predict(m, type = "response", newdata = ndat)
#' calibrationSlope(ndat$Y, preds)
#' @export
calibrationSlope <- function(y_true, y_pred) {
  logits <- qlogis(y_pred)
  m <- glm(y_true ~ logits, family = binomial)
  unname(coef(m)[2])
}

#' Compute calibration in the large
#' @examples
#' dat <- generateData()
#' ndat <- generateData()
#' m <- glm(Y ~ ., data = dat, family = binomial)
#' preds <- predict(m, type = "response", newdata = ndat)
#' calibrationInTheLarge(ndat$Y, preds)
#' @export
calibrationInTheLarge <- function(y_true, y_pred) {
  logits <- qlogis(y_pred)
  m <- glm(y_true ~ 1 + offset(logits), family = binomial)
  unname(coef(m)[1])
}

#' Evaluate model at new observations with `loss`
#' @export
evaluateModel <- function(m, newx, y_true, loss, ...) {
  UseMethod("evaluateModel")
}

#' Evaluate ranger
#' @examples
#' dat <- generateData()
#' ndat <- generateData()
#' m <- ranger(Y ~ ., data = dat, probability = TRUE)
#' evaluateModel(m, newx = ndat, y_true = ndat$Y, loss = nll)
#' @method evaluateModel ranger
#' @export
evaluateModel.ranger <- function(m, newx, y_true, loss, ...) {
	y_pred <- predict(m, data = newx, ... = ...)$predictions[, 2]
	loss(y_true, y_pred)
}

#' Evaluate glmnet
#' @examples
#' set.seed(10)
#' dat <- generateData()
#' ndat <- generateData()
#' m <- fglmnet(Y ~ ., data = dat, alpha = 0.5, lambda = 0.01, family = "binomial")
#' evaluateModel(m, newx = as.matrix(ndat[,-1]), y_true = ndat$Y, loss = nll)
#' @method evaluateModel glmnet
#' @export
evaluateModel.glmnet <- function(m, newx, y_true, loss, ...) {
	y_pred <- predict(m, newx = newx, type = "response", ... = ...)
	loss(y_true, y_pred)
}

### Sim Design Functions

#' SimDesign function for generating the data
#' @export
generate <- function(condition, fixed_objects = list(ntest = 1e4)) {
  ## Condition
  n <- condition$n
  epv <- condition$epv
  sigma2 <- condition$sigma2
  p <- condition$p
  q <- condition$q
  rho <- condition$rho
  prev <- condition$prev

  ## Random number generation
  # TODO: Seed in condition? Or where to generate?
  seed <- condition$seed
  set.seed(seed)

  ## Simulate beta
  if (q > 0) {
    nzbetas <- rnorm(q)
  } else {
    nzbetas <- NULL
  }
  betas <- c(nzbetas, rep(0, p - q))

  ## Simulate training and test data
  train <- generateData(n = n, p = p, nz = q, b = betas)
  test <- generateData(n = fixed_objects$ntest, p = p, nz = q, b = betas)
  list(train = train, test = test)
}

#' SimDesign function for analyzing simulated data
#' @examples
#' condition <- data.frame(n = 100, epv = 10, sigma2 = 1, p = 10, q = 3, rho = 0,
#' prev = 0.5, seed = 1)
#' dat <- generate(condition)
#' analyze(condition, dat)
#' @export
analyze <- function(condition, dat, fixed_objects = NULL) {
  ## Data
  train <- dat$train
  test <- dat$test
  fml <- Y ~ .
  newx <- ainet:::.rm_int(model.matrix(fml, test))
  y_true <- test$Y

  # TODO: Convergence checks, try-error exceptions

  ## AINET
	pen.f <- ainet:::.vimp(fml, train, which = "impurity", gamma = 1, renorm = "trunc")
	cvAINET <- cv.fglmnet(fml, train, pen.f = pen.f, family = "binomial", relax = TRUE)
  AINET <- ainet(fml, data = train, pen.f = pen.f, lambda = cvAINET$relaxed$lambda.1se,
                 alpha = cvAINET$relaxed$gamma.1se, family = "binomial")

  ## Logistic regression
  if (condition$p < condition$n) {
    GLM <- fglmnet(fml, data = train, alpha = 0, lambda = 0, family = "binomial")
  } else {
    cvGLM <- cv.fglmnet(fml, data = train, alpha = 0, family = "binomial")
    GLM <- fglmnet(fml, data = train, alpha = 0, lambda = cvGLM$lambda.1se,
                   family = "binomial")
  }

  ## Elastic net
  cvEN <- cv.fglmnet(fml, data = train, relax = TRUE, family = "binomial")
  EN <- fglmnet(fml, data = train, alpha = cvEN$relaxed$gamma.1se,
                 lambda = cvEN$relaxed$lambda.1se, family = "binomial")

  ## Adaptive elastic net
  ENpenf <- 1 / abs(coef(GLM)[-1])
  cvAEN <- cv.fglmnet(fml, data = train, relax = TRUE, pen.f = ENpenf, family = "binomial")
  AEN <- fglmnet(fml, data = train, pen.f = ENpenf, alpha = cvAEN$relaxed$gamma.1se,
                 lambda = cvAEN$relaxed$lambda.1se, family = "binomial")

  ## Random forest
  RF <- ranger(fml, data = train, probability = TRUE)

  ## Return
  metrics <- list(brier = brier, scaledBrier = scaledBrier, nll = nll, acc = acc,
                  auc = auroc, cslope = calibrationSlope, clarge = calibrationInTheLarge)
  models <- list(AINET = AINET, GLM = GLM, EN = EN, AEN = AEN, RF = RF)
  # TODO: coerce ret to data.frame
  res <- sapply(models, function(mod) {
    sapply(metrics, function(met) {
      evaluateModel(mod, newx = newx, y_true = y_true, loss = met)
    })
  })
  ret <- data.frame(t(res))
  ret
}

#' SimDesign function for summarizing simulation results
#' @export
summarize <- function(condition, results, fixed_objects = NULL) {
  # TODO: Add all summary metrics (anova, multcomp, visualization separately)
  ret <- c(bias = NaN, RMSE = NaN)
  ret
}

### Helpers

.assert_numeric_binary <- function(vec) {
	if (is.factor(vec) & length(levels(vec)) == 2)
		vec <- as.numeric(vec) - 1
  if(any(!vec %in% c(0, 1)) | is.factor(vec))
    stop("Wrong formatting for binary outcome. Need numeric vector of 0's and 1's.")
	vec
}
