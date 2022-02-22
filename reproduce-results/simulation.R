#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

# Simulations
# SP, LK, KR
# Oct 2021

set.seed(42)

# Dependencies ------------------------------------------------------------

library(ainet)

# Setting -----------------------------------------------------------------

setting <- args[1]
tn <- c(100, 500, 1000, 5000)

if (setting == "full") {
    tsparse <- 0
    tnonlin <- FALSE
    tfixed <- FALSE
} else if (setting == "nonlin") {
    tsparse <- c(0, 0.3, 0.6, 0.9)
    tnonlin <- TRUE
    tfixed <- FALSE
    tn <- tn[-length(tn)]
} else if (setting == "nonlin-fixed") {
    tsparse <- c(0, 0.3, 0.6, 0.9)
    tnonlin <- TRUE
    tfixed <- TRUE
    tn <- tn[-length(tn)]
} else if (setting == "sparse") {
    tsparse <- c(0, 0.3, 0.6, 0.9)
    tnonlin <- FALSE
    tfixed <- FALSE
    tn <- tn[-length(tn)]
} else if (setting == "trunc") {
    # TODO: Pass trunc through fixed_objects in runSimulation
    warning("trunc setting not implemented yet.")
    tsparse <- 0
    tnonlin <- FALSE
    tfixed <- FALSE
    tn <- tn[-length(tn)]
}

message("Running setting: ", setting)

# Conditions --------------------------------------------------------------

n <- tn
EPV <- c(20, 10, 1, 0.5)
prev <- c(0.01, 0.05, 0.1)
sigma2 <- c(1)
rho <- c(0, 0.3, 0.6, 0.95)
simGrid <- createDesign(n = n, EPV = EPV, prev = prev, sigma2 = sigma2,
                        rho = rho, sparsity = tsparse, stringsAsFactors = FALSE)
simGrid$p <- with(simGrid, ceiling(n * prev / EPV))
simGrid <- simGrid[simGrid$p != 1,]
simGrid <- simGrid[simGrid$p <= 100,]
simGrid <- simGrid[order(simGrid$p),]
simGrid$nonlin <- tnonlin
simGrid$fixed <- tfixed
nScenarios <- nrow(simGrid)

simGrid <- simGrid[1:5, ]

# Write conditions --------------------------------------------------------

write.csv(simGrid, "simulation-conditions.csv", row.names = FALSE,
          quote = FALSE)

# Simulation --------------------------------------------------------------

res <- runSimulation(
    design = simGrid,
    replications = 2,
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
    ncores = 2,
    fixed_objects = list(ntest = 1e4),
    packages = c("ainet")
)
