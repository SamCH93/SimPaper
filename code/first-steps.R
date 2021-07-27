library(randomForest)
library(glmnet)


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
