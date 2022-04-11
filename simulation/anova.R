# ANOVA for brier score
# SP, LK, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(multcomp)
library(ggpubr)
library(ainet)

inp <- "simResults"

outdir <- "results_anova"
if (!dir.exists(outdir)) {
    dir.create(outdir)
}

# Load --------------------------------------------------------------------

# adat <- read_results(inp)

# Run ---------------------------------------------------------------------

metrics <- c("brier", "scaledBrier", "nll", "acc", "auc")

sapply(metrics, function(met) {
    # mdat <- adat %>% filter(is.finite(!!sym(met)))
    # cat("\nRemoved", nrow(adat) - nrow(mdat),
        # "rows due to infinite values / missingness in", met, "\n")
    # fml <- as.formula(paste(met, "~ 0 + fct"))
    # out <- run_anova(formula = fml, data = mdat)
    out <- read.csv(paste0("results_anova/anova_", met, ".csv"))
    vis_results(out, xlab = met)
})
