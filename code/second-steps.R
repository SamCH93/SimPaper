

# Dependencies ------------------------------------------------------------

library(randomForest)
library(glmnet)

# iris example ------------------------------------------------------------

data("iris")


iris$bin <- factor(iris$Species == "setosa")

m <- randomForest(bin ~ . - Species, data = iris, importance = TRUE)

gamma <- 1 - (m$importance[, "MeanDecreaseGini"] / sum(m$importance[, "MeanDecreaseGini"]))

betas <- coef(glm(bin ~ . - Species, data = iris, family = binomial))[-1]

pen.f <- (1 / abs(betas))^(1 / gamma)
 
varImpPlot(m)

X <- scale(model.matrix(bin ~ . - Species, data = iris))[, -1]
Y <- as.numeric(iris$bin) - 1

plot(glmnet(x = X, y = Y, penalty.factor = pen.f, alpha = 0.5))
plot(glmnet(x = X, y = Y, alpha = 0.5))
