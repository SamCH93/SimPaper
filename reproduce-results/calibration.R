#!/usr/bin/env Rscript
setting <- commandArgs(trailingOnly = TRUE)[1]
tpath <- paste0("simResults-", setting)

# Calibration plots
# SP, LK, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(multcomp)
library(ggpubr)
library(ainet)

inp <- tpath
outdir <- paste0("figures-", setting)
if (!dir.exists(outdir)) {
    dir.create(outdir)
}

# Load --------------------------------------------------------------------

adat <- read_results(inp) 

# Run ---------------------------------------------------------------------

vis_calibration(adat, metric = "cslope", save = TRUE, lim = c(-10, 10))
vis_calibration(adat, metric = "clarge", save = TRUE, lim = c(-5, 5))
