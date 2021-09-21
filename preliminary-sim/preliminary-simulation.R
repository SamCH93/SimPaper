# Preliminary simulations
# SP, LK, KR
set.seed(42)

# Dependencies ------------------------------------------------------------
library(ainet)

# Conditions --------------------------------------------------------------
n <- c(100, 500, 1000, 5000)
EPV <- c(20, 10, 1, 0.1, 0.05)
prev <- c(0.01, 0.05, 0.1)
sigma2 <- c(1)
rho <- c(0, 0.3, 0.6, 0.95)
simGrid <- createDesign(n = n, EPV = EPV, prev = prev, sigma2 = sigma2,
                        rho = rho, stringsAsFactors = FALSE)
simGrid$p <- with(simGrid, ceiling(n * prev / EPV))
simGrid$p[simGrid$p == 1] <- 2
nScenarios <- nrow(simGrid)

# Simulation --------------------------------------------------------------
res <- runSimulation(
    design = simGrid,
    replications = 100, # for preliminary simulations
    generate = generate,
    analyse = analyze,
    summarise = summarize,
    save = TRUE,
    save_seeds = TRUE,
    save_results = TRUE,
    save_details = list(safe = TRUE,
                        save_results_dirname = "simResults",
                        save_seeds_dirname = "simSeeds"),
    parallel = TRUE,
    ncores = 20, # number of (available) cores on the server from LK
    fixed_objects = list(ntest = 1e4),
    packages = c("ainet")
)

## ## extract summaries
## simSum <- SimExtract(res, what = "summarise")

## ## extract raw simulation results
## estimands <- do.call("rbind", lapply(
##     X = SimResults(results = res),
##     FUN = function(x) {
##         do.call("rbind", lapply(
##             X = x$results,
##             FUN = function(y)
##                 y$estimands
##         ))
##     }
## ))

## coefs <- do.call("rbind", lapply(
##     X = SimResults(results = res),
##     FUN = function(x) {
##         do.call("rbind", lapply(
##             X = x$results,
##             FUN = function(y)
##                 y$coefs
##         ))
##     }
## ))

## ## Visualize raw simulation results
## library(tidyverse)
## theme_set(theme_bw())

## ggplot(estimands, aes(x = model, y = brier - brier_oracle)) +
##     geom_point(alpha = 0.5) +
##     facet_wrap(~ paste0("n==", n, "~EPV==", EPV, "~prev==", prev, "~rho==", rho, "~p==", p),
##                labeller = label_parsed) +
##     labs(y = "Brier score (difference from oracle)")

## gather(coefs, key = "model", value = "estimate", AINET:AEN) %>%
##     ggplot(aes(x = model, y = estimate - oracle, color = coef)) +
##     geom_point(alpha = 0.7, position = position_dodge(0.5)) +
##     facet_wrap(~ paste0("n==", n, "~EPV==", EPV, "~prev==", prev, "~rho==", rho, "~p==", p),
##                labeller = label_parsed) +
##     labs(y = expression(hat(beta[j])-beta[j]^0))
