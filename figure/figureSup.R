#!/usr/bin/env Rscript
repro_type <- commandArgs(trailingOnly = TRUE)[1] # full / partial / figure
if (is.na(repro_type))
    repro_type <- "figure"

## This script contains code to produce the "tie-fighter" figures from the
## supplementary material

library("ainet") # visuzalization functions

## folder with intermediate simulation results
if (repro_type == "figure") {
    anovafolder <- file.path("..", "results-simulations", "pre-registered",
                             "results-anova")
    calibfolder <- file.path("..", "results-simulations", "pre-registered",
                             "results-simulation")
} else if (repro_type %in% c("full", "partial")) {
    anovafolder <- file.path("..", "reproduce-simulations", "results_anova-full")
    calibfolder <- file.path("..", "reproduce-simulations", "simResults-full")
} else {
    stop("Supplied type of reproducibility not implemented.")
}

## create a "tie-fighter" plot for each metric
outdir <- "."
metrics <- c("brier", "scaledBrier", "nll", "auc")
sapply(metrics, function(met) {
    dat <- read.csv(file.path(anovafolder, paste0("anova_", met, ".csv")))
    try(ainet::vis_results(dat, xlab = met, save = TRUE, save_data = FALSE,
                    outdir = outdir))
})

## create calibration slope and calibration in the large "tie-fighter" plots
calibdat <- ainet::read_results(path = calibfolder)
ainet::vis_calibration(pdat = calibdat, metric = "cslope", save = TRUE,
                       lim = c(-10, 10), outdir = outdir)
ainet::vis_calibration(pdat = calibdat, metric = "clarge", save = TRUE,
                       lim = c(-5, 5), outdir = outdir)
