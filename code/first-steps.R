
# Dependencies ------------------------------------------------------------

library(randomForest)
library(glmnet)

# iris example ------------------------------------------------------------

data("iris")


iris$bin <- factor(iris$Species == "setosa")

m <- randomForest(bin ~ . - Species, data = iris)

gamma <- 0.5
pen.f <- 1 - (m$importance / sum(m$importance))^gamma
 
varImpPlot(m)

X <- scale(model.matrix(bin ~ . - Species, data = iris))[, -1]
Y <- as.numeric(iris$bin) - 1

plot(glmnet(x = X, y = Y, penalty.factor = pen.f, alpha = 0.5))
plot(glmnet(x = X, y = Y, alpha = 0.5))

# FUNs --------------------------------------------------------------------

.rm_int <- function(X) {
	if (all(X[, 1] == 1))
		return(X[, -1, drop = FALSE])
	return(X)
}

.importance_penalty <- function(rf, gamma = 1, which = c("MeanDecreaseGini", 
																												 "MeanDecreaseAccuracy",
																												 "adaptive lasso")) {
	which <- match.arg(which)
	if (which %in% c("MeanDecreaseGini", "MeanDecreaseAccuracy")) {
		imp <- rf$importance[, which]
		imp <- pmax(imp, 0)
		ret <- 1 / (imp / sum(imp))^gamma
	} else if (which == "adaptive lasso") {
		ret <- 1 / abs(coef(rf))^gamma	
	}
	return(ret)
}

.preproc <- function(formula, data) {
	X <- .rm_int(model.matrix(formula, data))
	Y <- model.response(model.frame(formula, data))
	if (is.factor(Y) & length(levels(Y)) == 2L) {
		Y <- as.numeric(Y) - 1
	}
	return(list(Y = Y, X = X))
}

fglmnet <- function(formula, data, ...) {
	dat <- .preproc(formula, data)
	glmnet(x = dat$X, y = dat$Y, ... = ...)
}

.vimp <- function(formula, data, which = c("MeanDecreaseGini",
																					 "MeanDecreaseAccuracy",
																					 "adaptive lasso"), ...) {
	which <- match.arg(which)
	if (which %in% c("MeanDecreaseGini", "MeanDecreaseAccuracy")) {
		rf <- randomForest(formula, data, importance = TRUE)
	} else if (which == "adaptive lasso") {
		rf <- glm(formula, data, family = binomial)
	}
	.importance_penalty(rf, which = which, ... = ...)
}

cv.fglmnet <- function(formula, data, imp_data = NULL, pen.f = NULL, ...) {
	dat <- .preproc(formula, data)
	if (is.null(pen.f))
		pen.f <- .vimp(formula, ifelse(is.null(imp_data), list(data), 
																	 list(imp_data))[[1]])
	cv.glmnet(x = dat$X, y = dat$Y, penalty.factor = pen.f, ... = ...)
}

ai_net <- function(formula, data, imp_data = NULL, pen.f = NULL, plot = FALSE, 
									 ...) {
	if (plot)
		varImpPlot(rf)
	if (is.null(pen.f))
		pen.f <- .vimp(formula, ifelse(is.null(imp_data), list(data),
																	 list(imp_data))[[1]])
	fglmnet(formula, data, penalty.factor = pen.f, ... = ...)
}

# Sim example -------------------------------------------------------------

gen_dat <- function(n = 1e2, p = 20, nz = 5, b = rep(1:0, c(nz, p - nz))) {
	X <- matrix(rnorm(n * p), nrow = n, ncol = p)
	Y <- factor(rbinom(n = n, size = 1, plogis(X %*% b)))
	return(data.frame(Y = Y, X = X))
}

acc <- function(y_true, y_pred) {
	mean(y_true == y_pred)
}

nll <- function(y_pred, y_true) {
	if (is.factor(y_true) & length(levels(y_true)) == 2)
		y_true <- as.numeric(y_true) - 1
	- mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
}

eval_mod <- function(m, newx, y_true, loss, ...) {
	preds <- predict(m, newx = newx, ... = ...)
	loss(preds, y_true)
}

talp <- 0.5 # elastic net penalty
measure <- "nll"
if (measure == "nll") {
	pred_type <- "response"
	loss <- nll
} else {
	pred_type <- "class"
	loss <- acc
}

res <- replicate(100, {
	train <- gen_dat()
	tune <- gen_dat()
	test <- gen_dat()
	
	fml <- Y ~ .
	pen.f <- .vimp(fml, tune, which = "MeanDecreaseGini", gamma = 1)
	print(pen.f)
	cvm <- cv.fglmnet(fml, train, pen.f = pen.f, alpha = talp, family = "binomial")
	
	m <- ai_net(fml, data = train, pen.f = pen.f, plot = FALSE,
							lambda = cvm$lambda.1se, alpha = talp, family = "binomial")
	AINET <- eval_mod(m, newx = .rm_int(model.matrix(fml, test)), 
										y_true = test$Y, loss = loss, type = pred_type)
	
	# bl <- glm(Y ~ ., data = train, family = binomial)
	# pbl <- as.numeric(predict(bl, newdata = test, type = "response") > 0.5)
	
	dd <- .preproc(fml, train)
	cbl <- cv.glmnet(x = dd$X, y = dd$Y, alpha = talp, family = "binomial")
	bl <- glmnet(x = dd$X, y = dd$Y, alpha = talp, lambda = cbl$lambda.1se, 
							 family = "binomial")
	BL <- eval_mod(bl, newx = .rm_int(model.matrix(fml, test)), 
								 y_true = test$Y, loss = loss, type = pred_type)
	
	c(
		BL = BL,
		AINET = AINET
	)
})

boxplot(t(res), las = 1, ylab = "NLL")
points(rowMeans(res), pch = 4)
