# analysis of simulations
# LK, SP, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(ggpubr)

inp <- "simResults-nonlin"
outdir <- paste0(inp, "-results")

if (!dir.exists(outdir))
    dir.create(outdir)

# Read results ------------------------------------------------------------

## load simulation results
files <- list.files(path = inp)
simres <- lapply(X = files, FUN = function(filename) {
    resList <- readRDS(file = file.path(inp, filename))
    lapply(X = resList$results, FUN = function(resListSimi) {
        resListSimi$coefs
    }) %>%
        bind_rows()
}) %>%
    bind_rows()

# FUNs --------------------------------------------------------------------

filter_coef <- function(dat, cf = "X.0") {
    simres %>% 
        filter(coef == cf) %>% 
        gather("method", "estimate", AINET:AEN) %>% 
        group_by(ID, n, EPV, prev, sigma2, rho, sparsity, p, coef, method) %>% 
        mutate(bias = estimate - oracle) 
}

summarize_coef <- function(dat, svars = c("estimate", "bias"),
                           funs = list(mean = mean, sd = sd,
                                       n = \(.x, ...) length(na.omit(.x)))) {
    dat %>% 
        summarize_at(svars, funs, na.rm = TRUE) %>% 
        mutate(bias_lwr = bias_mean - 1.96 * bias_sd / sqrt(bias_n),
               bias_upr = bias_mean + 1.96 * bias_sd / sqrt(bias_n))
}

vis_results <- function(pdat, cf = "X.0", save = TRUE, 
                        lim = range(pdat$estimate, na.rm = TRUE)) {
    
    out <- pdat %>% 
        # summarize_coef() %>% 
        mutate_at(c("n", "EPV", "prev", "rho", "sparsity"), 
                  ~ factor(.x, levels = sort(unique(as.numeric(as.character(.x))))))
    
    rho_plot <- function(trho, tsparse) {
        ggplot(out %>% filter(rho == trho, sparsity == tsparse), 
               aes(x = method, y = bias, # _mean, ymin = bias_lwr, ymax = bias_upr,
                   color = ordered(EPV))) +
            geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
            geom_boxplot(position = position_dodge(width = 1), outlier.shape = NA) +
            stat_mean(shape = 4, position = position_dodge(width = 1)) +
            # geom_pointrange(fatten = 0.75, position = position_dodge(width = 0.7)) +
            # geom_errorbar(width = 0.35, position = position_dodge(width = 0.7)) +
            facet_grid(prev ~ n, labeller = label_both) +
            theme_bw() +
            theme(legend.position = "top", panel.grid.major.y = element_blank(),
                  axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 7)) +
            labs(y = "Bias", x = "Method", subtitle = bquote(rho==~.(trho)), 
                 color = "EPV", caption = cf) +
            geom_vline(xintercept = seq(1.5, 3.5, 1), alpha = 0.1, size = 0.8) +
            coord_flip(ylim = lim) # +
            # ylim(-10, 10)
    }
    
    lapply(unique(as.numeric(as.character(out$rho))), function(rho) {
        ps <- lapply(unique(as.numeric(as.character(out$sparsity))), function(sparse) {
            rho_plot(trho = rho, tsparse = sparse)
        })
        pf <- ggarrange(plotlist = ps, common.legend = TRUE, ncol = 2, nrow = 2)
        if (save) {
            pnm <- file.path(outdir, paste0("coef", cf, "_rho", rho, ".pdf"))
            ggsave(pnm, plot = pf, height = 1.5 * 8.3, width = 1.5 * 11.7)
        }
    })
    
    return(pf)
}

vis_na <- function(pdat, rhos = unique(as.numeric(as.character(pdat$rho))), 
                   sparsities = unique(as.numeric(as.character(pdat$sparsity))),
                   cf = "X.0") {
    lapply(rhos, function(trho) {
        lop <- lapply(sparsities, function(tsparse) {
            pdat %>% 
                filter(rho == trho, sparsity == tsparse) %>% 
                ggplot(aes(x = method, y = ordered(EPV), fill = frac_na)) +
                geom_tile(alpha = 0.3) +
                geom_text(aes(label = paste0(frac_na, "%"))) +
                facet_grid(prev ~ n, labeller = label_both) +
                theme_bw() +
                theme(panel.grid = element_blank()) +
                labs(y = "EPV", 
                     x = element_blank(), 
                     subtitle = bquote(rho==~.(trho)~sparsity==.(tsparse)), 
                     fill = paste0("Percent missing in ", cf)) +
                coord_flip() +
                scale_fill_viridis_c()
        })
        pf <- ggarrange(plotlist = lop, common.legend = TRUE, ncol = 2, nrow = 2)
        pnm <- file.path(outdir, paste0("coef-missing_", cf, "_rho", trho, ".pdf"))
        ggsave(pnm, plot = pf, height = 1.5 * 8.3, width = 1.5 * 11.7)
    })
}

# Vis ---------------------------------------------------------------------

cfs <- c("X.0", "X.1", "X.2")

sapply(cfs, function(tcf) {
    adat <- filter_coef(simres, tcf)
    nadat <- adat %>% 
        group_by(n, EPV, prev, rho, sparsity, method, coef) %>% 
        summarize(frac_na = round(100 * mean(is.na(estimate)), 1))
    try(vis_results(adat, cf = tcf, lim = c(-10, 10)))
    try(vis_na(pdat = nadat, cf = tcf))
})
