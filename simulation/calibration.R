#!/usr/bin/env Rscript
repro_type <- commandArgs(trailingOnly = TRUE)[1] # full/partial/figure
outdir <- commandArgs(trailingOnly = TRUE)[2] # "../figure"

if (is.na(outdir))
    outdir <- "figures"

if (is.na(repro_type))
    repro_type <- "figure"

# Calibration plots
# SP, LK, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(multcomp)
library(ggpubr)
library(ainet)

inp <- if (repro_type %in% c("partial", "figure")) 
	"simResults" else if (repro_type == "full") "" else 
		stop("`repro_type' not implemented.")

if (!dir.exists(outdir)) {
    dir.create(outdir)
}

# Load --------------------------------------------------------------------

adat <- read_results(inp) 

# Run ---------------------------------------------------------------------

vis_calibration(adat, metric = "cslope", save = TRUE, lim = c(-10, 10))
vis_calibration(adat, metric = "clarge", save = TRUE, lim = c(-5, 5))
