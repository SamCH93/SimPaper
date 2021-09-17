# Preliminary simulations
# SP, LK
# Aug 2021

set.seed(2410)

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
nScenarios <- nrow(simGrid)

# Simulation --------------------------------------------------------------

res <- runSimulation(
    design = simGrid[13, ],
    replications = 20, # TODO perform sample size calculation and adjust
    generate = generate,
    analyse = analyze,
    summarise = NA, # TODO write summarize function to compute summary stats
    save = TRUE,
    save_seeds = TRUE,
    save_results = TRUE,
    save_details = list(safe = TRUE,
                        save_results_dirname = "simResults",
                        save_seeds_dirname = "simSeeds"),
    parallel = TRUE,
    ## ncores = 1000, # TODO set to cores of our simulation machine
    fixed_objects = list(ntest = 1e4),
    packages = c("ainet", "pROC", "glmnet")
)

## SimResults(results = res) ## can alternatively also read with readRDS
## boxplot(brier ~ model,
##         data = do.call("rbind", SimResults(results = res)$results))
