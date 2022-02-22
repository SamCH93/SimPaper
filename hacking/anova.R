# ANOVA for brier score
# SP, LK, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(multcomp)
library(ggpubr)
library(ainet)

inp <- "simResults-nonlin"

outdir <- paste0(inp, "-results")
if (!dir.exists(outdir)) {
    dir.create(outdir)
}

# Load --------------------------------------------------------------------

res <- read_results(inp)

# Run ---------------------------------------------------------------------

metrics <- c("brier", "scaledBrier", "nll", "acc", "auc")

sapply(metrics, function(met) {
    mdat <- adat %>% filter(is.finite(!!sym(met)))
    nadat <- adat %>% 
        group_by(n, EPV, prev, rho, sparsity, model) %>% 
        summarise(frac_na = round(100 * mean(is.na(!!sym(met))), 1))
    cat("\nRemoved", nrow(adat) - nrow(mdat),
        "rows due to infinite values / missingness in", met, "\n")
    fml <- as.formula(paste(met, "~ 0 + fct"))
    out <- run_anova(formula = fml, data = mdat)
    # out <- read.csv(file.path(outdir, paste0("anova_", met, ".csv")))
    try(vis_results(out, xlab = met))
    try(vis_na(pdat = nadat, xlab = met))
})
