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

files <- list.files(path = inp, pattern = ".rds", full.names = TRUE)
simres <- lapply(X = files, FUN = function(filename) {
    resList <- readRDS(file = filename)
    lapply(X = resList$results, FUN = function(resListSimi) {
        resListSimi$estimands
    }) %>%
        bind_rows()
}) %>%
    bind_rows()

adat <- simres %>% 
    mutate(inputp = ceiling(n * prev / EPV)) %>% 
    filter(inputp != 1) %>% 
    mutate_at(c("n", "EPV", "prev", "rho"), 
              ~ factor(.x, levels = sort(unique(.x)))) %>% 
    mutate(fct = factor(paste0(model, "n", n, "EPV", EPV, "prev", 
                               prev, "rho", rho)))

# Impute ------------------------------------------------------------------

ps <- 0.99 # 80th percentile imputation
worst_cases <- adat %>% 
    group_by(fct, model) %>%
    summarize_at(c("brier", "scaledBrier", "nll", "auc", "acc"),
              ~ quantile(.x[!is.infinite(.x)], p = ps, na.rm = TRUE))

jdat <- full_join(adat, worst_cases, by = c("fct", "model"), suffix = c("", "_wc")) %>% 
    mutate(
        imp_brier = case_when(is.na(brier) ~ brier_wc, TRUE ~ brier),
        imp_scaledBrier = case_when(is.na(scaledBrier) ~ scaledBrier_wc, TRUE ~ scaledBrier),
        imp_nll = case_when(is.na(nll) | is.infinite(nll) ~ nll_wc, TRUE ~ nll),
        imp_auc = case_when(is.na(auc) ~ auc_wc, TRUE ~ auc),
        imp_acc = case_when(is.na(acc) ~ acc_wc, TRUE ~ acc),
    )

# ANOVA -------------------------------------------------------------------

run_anova <- function(formula = brier ~ 0 + fct, data = adat,
                      conds = with(data, unique(paste0("n", n, "EPV", EPV, "prev", prev, "rho", rho))),
                      models = c("GLM", "EN", "AEN", "RF"), compare_against = "AINET") {
    m <- lm(formula, data = data)
    out <- list()
    
    pb <- txtProgressBar(min = 1, max = length(conds), style = 3)
    for (cond in seq_along(conds)) {
        setTxtProgressBar(pb, cond)
        g1 <- paste0("fct", compare_against, conds[cond])
        g2 <- paste0("fct", models, conds[cond])
        lfct <- paste(g1, "-", g2, "== 0")
        res <- try(glht(m, linfct = lfct))
        if (inherits(res, "try-error"))
            next 
        pval <- summary(res)$test$pvalues
        cf <- confint(res)$confint
        nms <- str_split(conds[cond], pattern = "[0-9]|\\.")[[1]]
        nms <- nms[nms != ""]
        nums <- str_split(conds[cond], pattern = "[a-z]|[A-Z]")[[1]]
        nums <- nums[nums != ""]
        names(nums) <- nms
        out[[cond]] <- data.frame(cf, pval = pval, contrast = models, t(nums))
    }
    return(out) 
}

vis_results <- function(pdat, xlab = "brier", save = TRUE, 
                        lim = range(pdat$Estimate, na.rm = TRUE)) {
    
    out2 <- pdat %>% 
        bind_rows() %>% 
        mutate_at(c("n", "EPV", "prev", "rho"), 
                  ~ factor(.x, levels = sort(unique(as.numeric(as.character(.x))))))
    
    write.csv(out2, file.path(outdir, paste0("anova_", xlab, ".csv")),
              row.names = FALSE, quote = FALSE)
}

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
