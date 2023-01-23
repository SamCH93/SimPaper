#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
setting <- args[1]
if (is.na(setting)) {
    setting <- "full"
}
TEST <- as.logical(as.numeric(args[2]))
if (is.na(TEST)) {
    TEST <- FALSE
}

## This script initializes and runs the simulation studies. Functions for
## organizing and executing the study are included in the package "ainet" which
## itself is based on the "SimDesign" package. The results are continuously
## saved in the folders "simResults-full" or "simResults-nonlin", depending on
## whether the setting "full" or "nonlin" is used.

set.seed(42) # the ultimate seed

## Dependencies ------------------------------------------------------------

library("ainet")

## Setting -----------------------------------------------------------------

## change number of cores here
ncores <- 8 # 60

## original sample sizes
tn <- c(100, 500, 1000, 5000)

## setting specific conditions
if (setting == "full") {
    tsparse <- 0 # no sparsity
    tnonlin <- FALSE # no nonlinear effect
    nsim <- 2000 # 2000 repetitions
} else if (setting == "nonlin") {
    tsparse <- c(0, 0.3, 0.6, 0.9) # sparsity levels
    tnonlin <- TRUE # nonlinear effect
    tn <- tn[-length(tn)] # the tweaked simulations didn't use the largest
                          # sample size
    nsim <- 1000 # 1000 repetitions
}

message("Running setting: ", setting)

## Conditions --------------------------------------------------------------

n <- tn # sample size
EPV <- c(20, 10, 1, 0.5) # events per variable
prev <- c(0.01, 0.05, 0.1) # baseline prevalence
sigma2 <- c(1) # variance
rho <- c(0, 0.3, 0.6, 0.95) # correlation among covariates
simGrid <- SimDesign::createDesign(n = n, EPV = EPV, prev = prev,
                                   sigma2 = sigma2, rho = rho,
                                   sparsity = tsparse, stringsAsFactors = FALSE)
simGrid$p <- with(simGrid, ceiling(n * prev / EPV)) # compute number of covariates
simGrid <- simGrid[simGrid$p != 1,] # remove conditions with only one covariate
simGrid <- simGrid[simGrid$p <= 100,] # remove conditions with more than 100 covariates
simGrid <- simGrid[order(simGrid$p),] # order according number of covariates
simGrid$nonlin <- tnonlin # everywhere the same presence/absence of nonlinear effect
nScenarios <- nrow(simGrid)

## testing conditions that correspond to a subset of figure 1
if (TEST) {
    if (setting == "full") {
        simGrid <- subset(simGrid, EPV == 1 & rho == 0.95 & prev == 0.05 & n < 5000)
    } else if (setting == "nonlin") {
        simGrid <- subset(simGrid, sparsity == 0.9 & EPV == 1 & rho == 0.95 &
                                   prev == 0.05 & n < 5000)
    }
    nsim <- 10
}

## Write conditions --------------------------------------------------------
## uncomment this to save all conditions as csv

## write.csv(simGrid, paste0(setting, "simulation-conditions.csv"),
##           row.names = FALSE, quote = FALSE)

## Simulation --------------------------------------------------------------

res <- SimDesign::runSimulation(
    design = simGrid,
    replications = nsim,
    generate = ainet::generate,
    analyse = ainet::analyze,
    summarise = ainet::summarize,
    save = TRUE,
    save_seeds = TRUE,
    save_results = TRUE,
    save_details = list(safe = TRUE,
                        save_results_dirname = paste0("simResults-", setting),
                        save_seeds_dirname = paste0("simSeeds-", setting)),
    parallel = TRUE,
    ncores = ncores,
    fixed_objects = list(ntest = 1e4),
    packages = c("ainet")
)
