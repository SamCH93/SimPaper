#!/usr/bin/env Rscript
repro_type <- commandArgs(trailingOnly = TRUE)[1] # full / partial / figure

if (is.na(repro_type))
    repro_type <- "figure"

## Code to produce Figure 1 in "Pitfalls and Potentials in Simulation Studies"
## Lucas Kook, Samuel Pawel, Kelly Reeve
## August 2022

## Dependencies ----------------------------------------------------------------

library(tidyverse) # data manipulation and plotting
library(ggpubr) # combining ggplots
library(ggpp) # for position_dodgenudge

## Data ------------------------------------------------------------------------

# names and labels of simulation studies
tnms <- c("final", "nonlin")
tlabs <- c("final", "nonlinear") 

# evaluation metrics
which <- c("brier", "scaledBrier", "nll", "auc", "acc")

# load data from simulation studies
if (repro_type %in% c("figure", "partial")) {
    folders <- c("../simulation/results_anova/",
                 "../hacking/simResults-nonlin-results/")
} else if (repro_type == "full") {
    folders <- c("../reproduce-results/results_anova-full/",
                 "../reproduce-results/results_anova-nonlin/")
} else {
    stop("Supplied type of reproducibility not implemented.")
}

paths <- expand_grid(folder = folders, metric = which)
paths$path <- paste0(paths$folder, paste0("anova_", paths$metric, ".csv"))
pdat <- paths %>% 
    mutate(d = map(path, ~ read_csv(.x, show_col_types = FALSE))) %>% 
    unnest(c(d)) %>% 
    mutate(set = factor(folder, levels = folders, labels = tlabs)) %>% 
    select(-path, -folder)

## Create Figure 1 -------------------------------------------------------------

# plot parameters
point_alpha <- 0.9 # Alpha for points and error bars
arrow_alpha <- 0.7 # Alpha value for arrows
arrow_head_size <- 4 # Size of arrow heads
ny <- 0 # Nudge of arrow heads in y direction
arrow_col <- "gray60" # color for arrows
plotDeco <- function(ggp) {
    # function to add the same plot style to all plots
    out <- ggp +
        theme_bw() +
        labs(x = expression(AINET~better%<-%Difference~"in"~Brier~score%->%AINET~worse),
             y = element_blank(), color = "EPV") +
        guides(color = guide_legend(reverse = TRUE,
                                    override.aes = list(size = 3),
                                    label.hjust = 0,
                                    label.position = "left")) +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              legend.text = element_text(size = 11),
              axis.text.y = element_text(size = 12),
              axis.text.x = element_text(size = 10),
              axis.title = element_text(size = 13),
              plot.subtitle = element_text(size = 13),
              strip.text = element_text(size = 12),
              legend.spacing = unit(0.1, "cm"),
              legend.margin = margin(t = 0.1, unit = 'cm')## ,
              ## legend.text.align = 2
              ) +
        geom_hline(yintercept = seq(1.5, 3.5, 1), alpha = 0.1, size = 0.8) +
        scale_alpha_manual(values = c(0.1, 1)) +
        scale_shape_manual(values = c("better" = 60, "worse" = 62))
    return(out)
}

# subset of simulation conditions used for Figure
corr <- 0.95 # correlation of covariates
baseprev <- 0.05 # baseline prevalence
spars <- 0.9 # proportion of regression coefficients which are non-zero (only
             # for modified simulations)
nmax <- 5000 # upper bound for sample size
eval <- "brier" # evaluation metric
pdatSubset <- pdat %>%
    filter(
        rho == corr,
        metric == eval,
        n < nmax,
        prev == baseprev
    )

## plot of pre-registered simulation results ("final")
pdatOG <- pdatSubset %>%
    filter(sparsity %in% c(0, NA), # no sparsity in pre-registered simulation
           set %in% c("final"))
p0raw <- ggplot(data = pdatOG, aes(y = contrast)) +
    facet_grid(. ~ n,
               labeller = label_bquote(cols = italic(n) == .(n))) +
    geom_vline(xintercept = 0, lty = 2, alpha = 0.3) +
    geom_point(aes(x = Estimate, color = ordered(EPV)),
               position = position_dodge(width = 0.5), size = 0.8) +
    geom_errorbarh(aes(xmin = lwr, xmax = upr, color = ordered(EPV)),
                   position = position_dodge(width = 0.5),
                   height = 0.25, show.legend = FALSE)
p0 <- plotDeco(ggp = p0raw)

## plot simulation results after QRP E2 modifying the DGP:
## adding nonlinear effect and sparsity to DGP ("nonlinear")
pdatE2 <- pdatSubset %>%
    filter(sparsity %in% c(0, 0.9),
           set %in% c("final", "nonlinear"))
pdatE2wide <- pdatE2 %>%
    # creating variable to indicate whether modified DGP improved performance
    mutate(sparsity = 0.9) %>% # overwrite sparsity column
    select(contrast, n, EPV, prev, rho, sparsity, Estimate, lwr, upr, set) %>%
    pivot_wider(names_from = set, values_from = c(Estimate, lwr, upr)) %>%
    mutate(better = ifelse(Estimate_nonlinear < Estimate_final, "better", "worse"),
           from = ifelse(better  == "better", lwr_final, lwr_nonlinear),
           to = ifelse(better  == "better", upr_nonlinear, upr_final),
           head = ifelse(better == "better", upr_nonlinear, lwr_nonlinear),
           head = ifelse((pmin(lwr_nonlinear, lwr_final) <= pmax(upr_nonlinear, upr_final) &
                          pmax(lwr_nonlinear, lwr_final) <= pmin(upr_nonlinear, upr_final)),
                         NA, head))
p1raw <- ggplot(data = pdatE2, aes(y = contrast)) +
    facet_grid(. ~ n,
               labeller = label_bquote(cols = italic(n) == .(n))) +
    geom_vline(xintercept = 0, lty = 2, alpha = 0.3) +
    geom_linerange(data = pdatE2wide, alpha = arrow_alpha, lwd = 0.3,
                   aes(xmin = from, xmax = to, y = contrast,
                       color = NULL, group = ordered(EPV)), color = arrow_col,
                   position = position_dodgenudge(0.5, y = ny),
                   show.legend = FALSE) +
    geom_point(data = pdatE2wide,
               aes(x = head, group = ordered(EPV), shape = better),
               size = arrow_head_size, position = position_dodgenudge(width = 0.5, y = ny),
               alpha = arrow_alpha, show.legend = FALSE, color = arrow_col) +
    geom_errorbarh(aes(xmin = lwr, xmax = upr, color = ordered(EPV)),
                   position = position_dodge(width = 0.5), alpha = point_alpha,
                   height = 0.25, show.legend = FALSE) +
    geom_point(data = pdatE2wide,
               aes(x = Estimate_final, color = ordered(EPV)), size = 0.8,
               position = position_dodge(width = 0.5), alpha = point_alpha) +
    geom_point(data = pdatE2wide,
               aes(x = Estimate_nonlinear, color = ordered(EPV)), size = 0.8,
               position = position_dodge(width = 0.5), alpha = point_alpha)
p1 <- plotDeco(ggp = p1raw)

## plot simulation results after QRPs E2 + E3 (removing competitor EN)
pdatE3 <- pdatSubset %>%
    filter(sparsity %in% c(0, NA, 0.9),
           set %in% c("nonlinear", "final")) %>%
    mutate(alp1 = contrast != "EN", 
           alp = paste0(EPV, contrast != "EN"),
           alp = ordered(alp, levels = str_sort(unique(alp), numeric = TRUE)))
pdatE3wide <- pdatE3 %>%
    # creating variable to indicate whether modified DGP improved performance
    mutate(sparsity = 0.9) %>% # overwrite sparsity column
    select(contrast, n, EPV, prev, rho, sparsity, Estimate, lwr, upr, set, alp, alp1) %>%
    pivot_wider(names_from = set, values_from = c(Estimate, lwr, upr)) %>%
    mutate(better = ifelse(Estimate_nonlinear < Estimate_final, "better", "worse"),
           from = ifelse(better  == "better", lwr_final, lwr_nonlinear),
           to = ifelse(better  == "better", upr_nonlinear, upr_final),
           head = ifelse(better == "better", upr_nonlinear, lwr_nonlinear),
           head = ifelse((pmin(lwr_nonlinear, lwr_final) <= pmax(upr_nonlinear, upr_final) &
                    pmax(lwr_nonlinear, lwr_final) <= pmin(upr_nonlinear, upr_final)),
               NA, head))
p2raw <- ggplot(data = pdatE3, aes(y = contrast)) +
    facet_grid(. ~ n,
               labeller = label_bquote(cols = italic(n) == .(n))) +
    geom_vline(xintercept = 0, lty = 2, alpha = 0.3) +
    geom_linerange(data = pdatE3wide, lwd = 0.3,
                   aes(xmin = from, xmax = to, y = contrast, alpha = alp,
                       color = NULL), color = arrow_col,
                   position = position_dodgenudge(0.5, y = ny),
                   show.legend = FALSE) +
    geom_point(data = pdatE3wide,
               aes(x = head, color = NULL, alpha = alp, shape = better), 
               size = arrow_head_size, position = position_dodgenudge(width = 0.5, y = ny),
               color = arrow_col, show.legend = FALSE) +
    geom_errorbarh(aes(xmin = lwr, xmax = upr, color = ordered(EPV), alpha = alp1),
                   position = position_dodge(width = 0.5),
                   height = 0.25, show.legend = FALSE) +
    geom_point(data = pdatE3wide,
               aes(x = Estimate_final, color = ordered(EPV), alpha = alp1), size = 0.8,
               position = position_dodge(width = 0.5)) +
    geom_point(data = pdatE3wide,
               aes(x = Estimate_nonlinear, color = ordered(EPV), alpha = alp1), size = 0.8,
               position = position_dodge(width = 0.5))
p2 <- plotDeco(ggp = p2raw) +
    scale_alpha_manual(values = c(rep(c(0.1, arrow_alpha), 4), 0.1, point_alpha)) +
    theme(axis.text.y.left = element_text(color = c("gray30", "gray80", "gray30", "gray30")))

## plot simulation results after QRPs E2 + E3 + R2 (selective reporting of low EPV settings)
pdatR2 <- pdatSubset %>%
    filter(sparsity %in% c(0, NA, 0.9),
           set %in% c("nonlinear")) %>%
    mutate(alp = contrast != "EN" & EPV < 10)
p3raw <- ggplot(data = pdatR2, aes(y = contrast, alpha = alp)) +
    facet_grid(. ~ n,
               labeller = label_bquote(cols = italic(n) == .(n))) +
    geom_vline(xintercept = 0, lty = 2, alpha = 0.3) +
    geom_point(aes(x = Estimate, color = ordered(EPV), alpha = alp),
               position = position_dodge(width = 0.5), size = 0.8) +
    geom_errorbarh(aes(xmin = lwr, xmax = upr, color = ordered(EPV)),
                   position = position_dodge(width = 0.5),
                   height = 0.25, show.legend = FALSE)
p3 <- plotDeco(ggp = p3raw) +
    theme(axis.text.y.left = element_text(color = c("gray30", "gray80", "gray30", "gray30"))) +
    xlim(min(p1$data$Estimate) * 1.05, max(p1$data$Estimate) * 1.13)

## combine plots in one Figure and save
ggarrange(
    p0 + labs(subtitle = "Per-protocol"),
    p1 + labs(subtitle = "QRP: E2"),
    p2 + labs(subtitle = "QRP: E2 + E3"),
    p3 + labs(subtitle = "QRP: E2 + E3 + R2 (reported)"),
    common.legend = TRUE, ncol = 1, legend = "right"
)
ggsave("ainet-results.pdf", height = 13 * 0.9, width = 10 * 0.95)
