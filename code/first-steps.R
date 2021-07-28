
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

.importance_penalty <- function(rf, gamma = 1) {
	imp <- rf$importance[, "MeanDecreaseGini"]
	imp <- pmax(imp, 0)
	1 - (imp / sum(imp))^gamma
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

.vimp <- function(formula, data, ...) {
	rf <- randomForest(formula, data, importance = TRUE)
	.importance_penalty(rf)
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

gen_dat <- function(n = 1e2, p = 1e1, b = rep(1:0, c(2, p - 2))) {
	X <- matrix(rnorm(n * p), nrow = n, ncol = p)
	Y <- factor(rbinom(n = n, size = 1, plogis(X %*% b)))
	return(data.frame(Y = Y, X = X))
}

res <- replicate(100, {
	train <- gen_dat()
	tune <- gen_dat()
	test <- gen_dat()
	
	fml <- Y ~ .
	pen.f <- .vimp(fml, tune)
	cvm <- cv.fglmnet(fml, train, pen.f = pen.f)
	
	m <- ai_net(fml, data = train, pen.f = pen.f, plot = FALSE,
							lambda = cvm$lambda.1se, alpha = 0.5)
	
	preds <- as.numeric(predict(m, newx = .rm_int(model.matrix(fml, test))) > 0.5)
	
	# bl <- glm(Y ~ ., data = train, family = binomial)
	# pbl <- as.numeric(predict(bl, newdata = test, type = "response") > 0.5)
	
	dd <- .preproc(fml, train)
	cbl <- cv.glmnet(x = dd$X, y = dd$Y, alpha = 0.5)
	bl <- glmnet(x = dd$X, y = dd$Y, alpha = 0.5, lambda = cbl$lambda.1se)
	pbl <- as.numeric(predict(bl, newx = .rm_int(model.matrix(fml, test))) > 0.5)
	
	c(
		BL = mean(pbl == test$Y),
		AINET = mean(preds == test$Y)
	)
})

boxplot(t(res))
