# ANOVA with worst-case imputation
# SP, LK, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(multcomp)
library(ggpubr)
library(ainet)

inp <- "simResults-nonlin"

outdir <- paste0(inp, "-results-imputed")
if (!dir.exists(outdir)) {
    message("Creating ", outdir)
    dir.create(outdir)
}

# Load --------------------------------------------------------------------

adat <- read_results(inp)

# Impute ------------------------------------------------------------------

ps <- 0.8 # 80th percentile imputation
adat_imp <- adat %>% 
    group_by(fct, model) %>%
    mutate(worst_case_brier = quantile(brier, p = ps, na.rm = TRUE),
           imp_brier = case_when(
               is.na(brier) ~ worst_case_brier,
               !is.na(brier) ~ brier
           ))

# Run ---------------------------------------------------------------------

metrics <- c("imp_brier") # , "scaledBrier", "nll", "acc", "auc")

sapply(metrics, function(met) {
    mdat <- adat_imp %>% filter(is.finite(!!sym(met)))
    nadat <- adat_imp %>% 
        group_by(n, EPV, prev, rho, sparsity, model) %>% 
        summarise(frac_na = round(100 * mean(is.na(!!sym(met))), 1))
    cat("\nRemoved", nrow(adat_imp) - nrow(mdat),
        "rows due to infinite values in", met, "\n")
    fml <- as.formula(paste(met, "~ 0 + fct"))
    out <- run_anova(formula = fml, data = mdat)
    # out <- read.csv(file.path(outdir, paste0("anova_", met, ".csv")))
    try(vis_results(out, xlab = met))
    try(vis_na(pdat = nadat, xlab = met))
})
