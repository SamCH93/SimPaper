# Simulations
# SP, LK, KR
# Oct 2021

set.seed(42)

# Dependencies ------------------------------------------------------------

library(ainet)

name <- "nonlin_fix"

# Conditions --------------------------------------------------------------

n <- c(100, 500, 1000) # remove 5000
EPV <- c(20, 10, 1, 0.5)
prev <- c(0.01, 0.05, 0.1)
sigma2 <- c(1)
rho <- c(0, 0.95)
sparsity <- c(0, 0.9) # add sparsity
simGrid <- createDesign(n = n, EPV = EPV, prev = prev, sigma2 = sigma2,
                        rho = rho, sparsity = sparsity, stringsAsFactors = FALSE)
simGrid$p <- with(simGrid, ceiling(n * prev / EPV))
simGrid <- simGrid[simGrid$p != 1,]
simGrid <- simGrid[simGrid$p <= 100,]
simGrid <- simGrid[order(simGrid$p),]
simGrid$q <- with(simGrid, ceiling(p * sparsity))
simGrid$nonlin <- TRUE # nonlinear effect of X1
nScenarios <- nrow(simGrid) # 288
simGrid$q

# Write conditions --------------------------------------------------------

write.csv(simGrid, paste0(name, "-conditions.csv"), row.names = FALSE, quote = FALSE)

# Simulation --------------------------------------------------------------

res <- runSimulation(
    design = simGrid,
    replications = 500,
    generate = generate,
    analyse = analyze,
    summarise = summarize,
    save = TRUE,
    save_seeds = FALSE,
    save_results = TRUE,
    save_details = list(safe = TRUE,
                        save_results_dirname = paste0("simResults-", name),
                        save_seeds_dirname = "simSeeds"),
    parallel = TRUE,
    ncores = 60, 
    fixed_objects = list(ntest = 1e4),
    packages = c("ainet")
)
