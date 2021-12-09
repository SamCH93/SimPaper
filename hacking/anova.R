# ANOVA for brier score
# SP, LK, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(multcomp)
library(ggpubr)

inp <- "simResults-trunc"

outdir <- paste0(inp, "-results")
if (!dir.exists(outdir)) {
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
    mutate_at(c("n", "EPV", "prev", "rho", "sparsity"), 
              ~ factor(.x, levels = sort(unique(.x)))) %>% 
    mutate(fct = factor(paste0(model, "n", n, "EPV", EPV, "prev", 
                               prev, "rho", rho, "sparsity", sparsity)))

# ANOVA -------------------------------------------------------------------

run_anova <- function(formula = brier ~ 0 + fct, data = adat,
                      conds = with(data, unique(paste0("n", n, "EPV", EPV, "prev", prev, "rho", rho,
                                                       "sparsity", sparsity))),
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
    
    xxlab <- switch(xlab, "brier" = "Brier score",
                    "scaledBrier" = "scaled Brier score",
                    "nll" = "log score",
                    "acc" = "accuracy",
                    "auc" = "AUC")
    
    out2 <- pdat %>% 
        bind_rows() %>% 
        mutate_at(c("n", "EPV", "prev", "rho", "sparsity"), 
                  ~ factor(.x, levels = sort(unique(as.numeric(as.character(.x))))))
    
    rho_plot <- function(trho, tsparse) {
        ggplot(out2 %>% filter(rho == trho, sparsity == tsparse), 
               aes(x = contrast, y = Estimate, ymin = lwr, ymax = upr,
                   color = ordered(EPV))) +
            geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
            geom_pointrange(fatten = 0.75, position = position_dodge(width = 0.7)) +
            geom_errorbar(width = 0.35, position = position_dodge(width = 0.7)) +
            facet_grid(prev ~ n, labeller = label_both) +
            theme_bw() +
            theme(legend.position = "top", panel.grid.major.y = element_blank(),
                  axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 7)) +
            labs(y = paste("Difference in", xxlab, "(AINET - other method)"), 
                 x = "Contrast", subtitle = bquote(rho==~.(trho)~sparsity==.(tsparse)), 
                 color = "EPV") +
            geom_vline(xintercept = seq(1.5, 3.5, 1), alpha = 0.1, size = 0.8) +
            coord_flip()
    }
    
    lapply(unique(as.numeric(as.character(out2$rho))), function(rho) {
        ps <- lapply(unique(as.numeric(as.character(out2$sparsity))), function(sparse) {
            rho_plot(trho = rho, tsparse = sparse)
        })
        pf <- ggarrange(plotlist = ps, common.legend = TRUE, ncol = 2, nrow = 2)
        if (save) {
            pnm <- file.path(outdir, paste0("tie-fighter_", xlab, "_rho", rho, ".pdf"))
            ggsave(pnm, plot = pf, height = 1.5 * 8.3, width = 1.5 * 11.7)
            write.csv(out2, file.path(outdir, paste0("anova_", xlab, ".csv")),
                      row.names = FALSE, quote = FALSE)
        }
    })
    
    return(pf)
}

vis_na <- function(pdat, rhos = unique(as.numeric(as.character(pdat$rho))), 
                   sparsities = unique(as.numeric(as.character(pdat$sparsity))),
                   xlab = "brier") {
    lapply(rhos, function(trho) {
        lop <- lapply(sparsities, function(tsparse) {
            pdat %>% 
                filter(rho == trho, sparsity == tsparse) %>% 
                ggplot(aes(x = model, y = EPV, fill = frac_na)) +
                geom_tile(alpha = 0.3) +
                geom_text(aes(label = paste0(frac_na, "%"))) +
                facet_grid(prev ~ n, labeller = label_both) +
                theme_bw() +
                theme(panel.grid = element_blank()) +
                labs(y = "EPV", 
                     x = element_blank(), 
                     subtitle = bquote(rho==~.(trho)~sparsity==.(tsparse)), 
                     fill = paste0("Percent missing in ", xlab)) +
                coord_flip() +
                scale_fill_viridis_c()
        })
        pf <- ggarrange(plotlist = lop, common.legend = TRUE, ncol = 2, nrow = 2)
        pnm <- file.path(outdir, paste0("missing_", xlab, "_rho", trho, ".pdf"))
        ggsave(pnm, plot = pf, height = 1.5 * 8.3, width = 1.5 * 11.7)
    })
}

# Run ---------------------------------------------------------------------

metrics <- c("brier", "scaledBrier", "nll", "acc", "auc")

sapply(metrics, function(met) {
    mdat <- adat %>% filter(is.finite(!!sym(met)))
    nadat <- adat %>% 
        group_by(n, EPV, prev, rho, sparsity, model) %>% 
        summarize(frac_na = round(100 * mean(is.na(!!sym(met))), 1))
    cat("\nRemoved", nrow(adat) - nrow(mdat),
        "rows due to infinite values / missingness in", met, "\n")
    fml <- as.formula(paste(met, "~ 0 + fct"))
    out <- run_anova(formula = fml, data = mdat)
    # out <- read.csv(paste0("results_anova/anova_", met, ".csv"))
    try(vis_results(out, xlab = met))
    try(vis_na(pdat = nadat, xlab = met))
})
