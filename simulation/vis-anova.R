# Vis anova
# LK, SP, KR
# Nov 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(multcomp)
library(ggpubr)

# Params ------------------------------------------------------------------

out <- "results_anova"

# Funs --------------------------------------------------------------------

vis_results <- function(pdat, xlab = "brier", save = TRUE) {
    
    xxlab <- switch(xlab, "brier" = "Brier score",
                   "scaledBrier" = "scaled Brier score",
                   "nll" = "log score",
                   "acc" = "accuracy",
                   "auc" = "AUC")
    
    out2 <- pdat %>% 
        bind_rows() %>% 
        mutate_at(c("n", "EPV", "prev", "rho"), 
                  ~ factor(.x, levels = sort(unique(as.numeric(as.character(.x))))))
    
    rho_plot <- function(trho) {
        ggplot(out2 %>% filter(rho == trho), aes(x = contrast, y = Estimate, ymin = lwr, ymax = upr,
                                                 color = ordered(EPV))) +
            geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
            geom_pointrange(fatten = 0.75, position = position_dodge(width = 0.7),
                            key_glyph = "point") +
            geom_errorbar(width = 0.35, position = position_dodge(width = 0.7)) +
            facet_grid(prev ~ n, labeller = label_both) +
            theme_bw() +
            theme(legend.position = "top", panel.grid.major.y = element_blank(),
                  axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 7)) +
            labs(y = paste("Difference in", xxlab, "(AINET - other method)"), 
                 x = "Contrast", subtitle = bquote(rho==~.(trho)), color = "EPV       ") +
            geom_vline(xintercept = seq(1.5, 3.5, 1), alpha = 0.1, size = 0.8) +
            coord_flip()
    }
    
    ps <- lapply(unique(as.numeric(as.character(out2$rho))), rho_plot)
    pf <- ggarrange(plotlist = ps, common.legend = TRUE, ncol = 2, nrow = 2)
    
    if (save) {
        pnm <- file.path(outdir, paste0("tie-fighter_", xlab, ".pdf"))
        ggsave(pnm, plot = pf, height = 1.5 * 8.3, width = 1.5 * 11.7)
        write.csv(out2, file.path(outdir, paste0("anova_", xlab, ".csv")),
                  row.names = FALSE, quote = FALSE)
    }
    
    return(pf)
}

# Run ---------------------------------------------------------------------

metrics <- c("brier", "scaledBrier", "nll", "acc", "auc")

sapply(metrics, function(met) {
    out <- read.csv(paste0("results_anova/anova_", met, ".csv"))
    vis_results(out, xlab = met, save = FALSE)
})
