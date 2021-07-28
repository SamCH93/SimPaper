# Simulation: ainet in sparse lm
# LK, SP
# July 2021

# Dependencies ------------------------------------------------------------

library(ainet)

# Parms -------------------------------------------------------------------

talp <- 0.5 # elastic net penalty
measure <- "nll"

if (measure == "nll") {
	pred_type <- "response"
	loss <- nll
} else {
	pred_type <- "class"
	loss <- acc
}

# Sim ---------------------------------------------------------------------

res <- replicate(100, {
	train <- gen_dat()
	tune <- gen_dat()
	test <- gen_dat()

	fml <- Y ~ .
	pen.f <- ainet:::.vimp(fml, tune, which = "MeanDecreaseGini", gamma = 1)
	cvm <- cv.fglmnet(fml, train, pen.f = pen.f, alpha = talp, family = "binomial")

	m <- ai_net(fml, data = train, pen.f = pen.f, plot = FALSE,
							lambda = cvm$lambda.1se, alpha = talp, family = "binomial")
	AINET <- eval_mod(m, newx = ainet:::.rm_int(model.matrix(fml, test)),
										y_true = test$Y, loss = loss, type = pred_type)

	dd <- ainet:::.preproc(fml, train)
	cbl <- cv.glmnet(x = dd$X, y = dd$Y, alpha = talp, family = "binomial")
	bl <- glmnet(x = dd$X, y = dd$Y, alpha = talp, lambda = cbl$lambda.1se,
							 family = "binomial")
	BL <- eval_mod(bl, newx = ainet:::.rm_int(model.matrix(fml, test)),
								 y_true = test$Y, loss = loss, type = pred_type)

	c(
		BL = BL,
		AINET = AINET
	)
})

# Vis ---------------------------------------------------------------------

boxplot(t(res), las = 1, ylab = "NLL")
points(rowMeans(res), pch = 4)
