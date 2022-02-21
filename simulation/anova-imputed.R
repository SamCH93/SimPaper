# ANOVA with worst-case imputation
# SP, LK, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(multcomp)
library(ggpubr)

inp <- "simResults"

outdir <- paste0(inp, "-results-imputed")
if (!dir.exists(outdir)) {
    message("Creating ", outdir)
    dir.create(outdir)
}

# Load --------------------------------------------------------------------

adat <- read_results(inp)

# Impute ------------------------------------------------------------------

ps <- 0.99 # percentile imputation
worst_cases <- adat %>% 
    group_by(fct, model) %>%
    summarize(
        brier = quantile(brier[!is.infinite(brier)], p = ps, na.rm = TRUE),
        scaledBrier = quantile(scaledBrier[!is.infinite(scaledBrier)], p = 1 - ps, na.rm = TRUE),
        nll = quantile(nll[!is.infinite(nll)], p = ps, na.rm = TRUE),
        auc = quantile(auc[!is.infinite(auc)], p = 1 - ps, na.rm = TRUE),
        acc = quantile(acc[!is.infinite(acc)], p = 1 - ps, na.rm = TRUE),
    )

jdat <- full_join(adat, worst_cases, by = c("fct", "model"), suffix = c("", "_wc")) %>% 
    mutate(
        imp_brier = case_when(is.na(brier) ~ brier_wc, TRUE ~ brier),
        imp_scaledBrier = case_when(is.na(scaledBrier) ~ scaledBrier_wc, TRUE ~ scaledBrier),
        imp_nll = case_when(is.na(nll) | is.infinite(nll) ~ nll_wc, TRUE ~ nll),
        imp_auc = case_when(is.na(auc) ~ auc_wc, TRUE ~ auc),
        imp_acc = case_when(is.na(acc) ~ acc_wc, TRUE ~ acc),
    )

# Run ---------------------------------------------------------------------

metrics <- c("imp_brier", "imp_scaledBrier", "imp_nll", "imp_acc", "imp_auc")

sapply(metrics, function(met) {
    mdat <- jdat %>% filter(is.finite(!!sym(met)))
    nadat <- jdat %>% 
        group_by(n, EPV, prev, rho, model) %>% 
        summarize(frac_na = round(100 * mean(is.na(!!sym(met))), 1))
    cat("\nRemoved", nrow(jdat) - nrow(mdat),
        "rows due to infinite values in", met, "\n")
    fml <- as.formula(paste(met, "~ 0 + fct"))
    out <- run_anova(formula = fml, data = mdat)
    # out <- read.csv(file.path(outdir, paste0("anova_", met, ".csv")))
    try(vis_results(out, xlab = met))
})
