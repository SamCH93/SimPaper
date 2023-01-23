#!/usr/bin/env Rscript
setting <- commandArgs(trailingOnly = TRUE)[1]
tpath <- paste0("simResults-", setting)

## This script runs multiplicity corrected ANOVAs on the raw simulation results
## to estimate the difference in 5 different metrics (Brier, scaled Brier,
## negative log likelihood, accuracy, AUC) between AINET and the competitor
## methods.

## Requires: raw simulation results (simResults-full or simResults-nonlin)
## Output: data.frames with estimated differences and confidence intervals for
## each metric (in folder results_anova)

## Deps --------------------------------------------------------------------

library("SimDesign")
library("tidyverse")
library("multcomp")
library("ggpubr")
library("ainet")

## Load --------------------------------------------------------------------

adat <- ainet::read_results(tpath)

## Run ---------------------------------------------------------------------

outdir <- paste0("results_anova-", setting)
if (!dir.exists(outdir)) {
    dir.create(outdir)
}

metrics <- c("brier", "scaledBrier", "nll", "acc", "auc")

sapply(metrics, function(met) {
    mdat <- adat %>% filter(is.finite(!!sym(met)))
    cat("\nRemoved", nrow(adat) - nrow(mdat),
        "rows due to infinite values / missingness in", met, "\n")
    fml <- as.formula(paste(met, "~ 0 + fct"))
    out <- ainet::run_anova(formula = fml, data = mdat)
    try(ainet::vis_results(out, xlab = met, outdir = outdir, save = FALSE))
})
