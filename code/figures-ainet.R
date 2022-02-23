# Figures for manuscript
# LK, SP, KR
# Feb 2022

# Deps --------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(ggbeeswarm)
library(factoextra)
theme_set(theme_bw())

# Params ------------------------------------------------------------------

tsparse <- 0.3
tnms <- c("prelim", "final", "final (imputed)", "nonlin_fix", "nonlin") 
tlabs <- c("preliminary", "final", "final (imputed)", "nonlinear (fixed)", "nonlinear") 

which <- c("brier", "scaledBrier", "nll", "auc", "acc")

folders <- c("preliminary-sim/results_anova/", 
             "simulation/results_anova/",
             "simulation/simResults-results-imputed/",
             "hacking/simResults-nonlin_fix-results/",
             "hacking/simResults-nonlin-results/")

paths <- expand_grid(folder = folders, metric = which)
paths$path <- paste0(paths$folder, paste0("anova_", paths$metric, ".csv"))

pdat <- paths %>% 
	mutate(d = map(path, ~ read_csv(.x, show_col_types = FALSE))) %>% 
	unnest(c(d)) %>% 
	mutate(set = factor(folder, levels = folders, labels = tlabs)) %>% 
	select(-path, -folder)


# Figure for E1
colnames(pdat)
pdatE1 <- pdat %>%
    filter(
        rho == 0.95,
        sparsity %in% c(NA, 0.9),
        set %in% c("final", "nonlinear"),
        metric == "brier",
        n < 5000,
        prev == 0.05
    )
pdatE1wide <- pdatE1 %>%
    mutate(sparsity = 0.9) %>%
    select(contrast, n, EPV, prev, rho, sparsity, Estimate, set) %>%
    spread(key = set, value = Estimate) %>%
    mutate(better = ifelse(nonlinear < final, "better", "worse"))

p1 <- ggplot(data = pdatE1, aes(y = contrast)) +
    facet_grid(. ~ n, scales = "free",
               labeller = label_bquote(cols = italic(n) == .(n))) +
    geom_vline(xintercept = 0, lty = 2, alpha = 0.3) +
    geom_linerange(data = pdatE1wide,
                   aes(xmin = final, xmax = nonlinear, y = contrast,
                       col = ordered(EPV)),
                   position = position_dodge(width = 0.5),
                   show.legend = FALSE) +
    geom_errorbarh(aes(xmin = lwr, xmax = upr, col = ordered(EPV)),
                   position = position_dodge(width = 0.5), alpha = 0.3,
                   height = 0.25, show.legend = FALSE) +
    ## geom_point(aes(x = Estimate, col = ordered(EPV)), size = 0.8,
    ##            position = position_dodge(width = 0.5), alpha = 0.5) +
    geom_point(data = pdatE1wide,
               aes(x = final, col = ordered(EPV)), size = 0.8,
               position = position_dodge(width = 0.5), alpha = 0.5) +
    geom_point(data = pdatE1wide,
               aes(x = nonlinear, col = ordered(EPV)), size = 0.8,
               position = position_dodge(width = 0.5), alpha = 0.5) +
    geom_point(data = pdatE1wide,
               aes(x = nonlinear, col = ordered(EPV), shape = better), size = 4,
               position = position_dodge(width = 0.5), alpha = 1,
               show.legend = FALSE) +
    scale_shape_manual(values = c("better" = 60, "worse" = 62)) +
    labs(x = "Difference in Brier score (negative: AINET better)",
         y = element_blank(), color = "EPV") +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank()) +
    geom_hline(yintercept = seq(1.5, 3.5, 1), alpha = 0.1, size = 0.8)
ggsave(p1, filename = "p1.pdf", height = 4.5)


# Vis ---------------------------------------------------------------------

ggplot(pdat, aes(x = set, y = Estimate, 
                 group = paste(n, EPV, prev, rho), color = ordered(EPV))) +
	geom_boxplot(aes(x = set, y = Estimate), inherit.aes = FALSE, outlier.shape = NA) +
	geom_line(alpha = 0.1) +
	geom_quasirandom(width = 0.3, alpha = 0.3) +
	## facet_grid(metric ~ contrast, scales = "free_y") +
	facet_grid(metric ~ contrast, scales = "free_y",
						 switch = "y", labeller = label_bquote(.(metric) ~ "difference")) +
	geom_hline(yintercept = 0, lty = 2) +
	## labs(x = "Simulation setting", y = "Difference in estimand",
	##      color = "EPV")
	labs(x = "Simulation setting", y = NULL, color = "EPV") +
	theme(strip.background.y = element_blank(), strip.placement = "outside")

# Brier only
ggplot(pdat %>% filter(metric == "brier"), aes(x = set, y = Estimate, 
								 group = paste(n, EPV, prev, rho), color = ordered(EPV))) +
	geom_boxplot(aes(x = set, y = Estimate), inherit.aes = FALSE, outlier.shape = NA) +
	geom_line(alpha = 0.1) +
	geom_quasirandom(width = 0.3, alpha = 0.3) +
	facet_wrap(~ contrast, scales = "free_y") +
	geom_hline(yintercept = 0, lty = 2) +
	labs(x = "Simulation setting", y = "Difference in Brier score",
			 color = "EPV")

# E1: Altering the DGP
pdat %>% filter(metric == "brier", contrast == "EN", 
								sparsity %in% c(NA, 0.9),
								set %in% c("final", "nonlinear")) %>% 
	ggplot(aes(x = set, y = Estimate, group = paste(n, EPV, prev, rho), color = ordered(EPV))) +
	geom_boxplot(aes(x = set, y = Estimate), inherit.aes = FALSE, outlier.shape = NA) +
	geom_line(alpha = 0.1) +
	geom_quasirandom(width = 0.3, alpha = 0.3) +
	geom_hline(yintercept = 0, lty = 2) +
	labs(x = "Simulation setting", y = "Difference in Brier score",
			 color = "EPV", subtitle = "E1: Altering the DGP")

# E4: Switching the primary estimand
pdat %>% filter(metric %in% c("brier", "auc"), 
								sparsity %in% c(NA, 0.9),
								set %in% c("final")) %>% 
	ggplot(aes(x = contrast, y = Estimate, group = paste(n, EPV, prev, rho), color = ordered(EPV))) +
	geom_boxplot(aes(x = contrast, y = Estimate), inherit.aes = FALSE, outlier.shape = NA) +
	geom_line(alpha = 0.1) +
	facet_wrap(~ metric, scales = "free_y") +
	geom_quasirandom(width = 0.3, alpha = 0.3) +
	geom_hline(yintercept = 0, lty = 2) +
	labs(x = "Simulation setting", y = "Difference in estimand",
			 color = "EPV", subtitle = "E4: Switching the primary estimand")

# E3: Removing comparators
pdat %>% filter(contrast %in% c("EN"),
								metric == "brier",
								set %in% c("final")) %>% 
	ggplot(aes(x = set, y = Estimate, group = paste(n, EPV, prev, rho), 
						 color = ordered(EPV))) +
	geom_boxplot(aes(x = set, y = Estimate), inherit.aes = FALSE, outlier.shape = NA) +
	geom_quasirandom(width = 0.3, alpha = 0.3) +
	geom_hline(yintercept = 0, lty = 2) +
	labs(x = "Simulation setting", y = "Difference in estimand (EN vs AINET)",
			 color = "EPV", subtitle = "E4: Removing comparators")

# E?: Removing unfavourable conditions
pdat %>% filter(sparsity %in% c(NA, 0.9),
								EPV <= 1,
								set %in% c("final")) %>% 
	ggplot(aes(x = contrast, y = Estimate, group = paste(n, EPV, prev, rho), color = ordered(EPV))) +
	geom_boxplot(aes(x = contrast, y = Estimate), inherit.aes = FALSE, outlier.shape = NA) +
	geom_line(alpha = 0.1) +
	facet_wrap(~ metric, scales = "free") +
	geom_quasirandom(width = 0.3, alpha = 0.3) +
	geom_hline(yintercept = 0, lty = 2) +
	labs(x = "Simulation setting", y = "Difference in estimand",
			 color = "EPV", subtitle = "E?: Removing unfavorable conditions")

# E6: Handling exceptions
pdat %>% filter(metric == "brier", contrast == "EN", 
								sparsity %in% c(NA, 0.9),
								set %in% c("final", "final (imputed)")) %>% 
	ggplot(aes(x = set, y = Estimate, group = paste(n, EPV, prev, rho), color = ordered(EPV))) +
	geom_boxplot(aes(x = set, y = Estimate), inherit.aes = FALSE, outlier.shape = NA) +
	geom_line(alpha = 0.1) +
	geom_quasirandom(width = 0.3, alpha = 0.3) +
	geom_hline(yintercept = 0, lty = 2) +
	labs(x = "Simulation setting", y = "Difference in Brier score",
			 color = "EPV", subtitle = "E6: Handling exceptions")

# Clustering --------------------------------------------------------------

cdat <- pdat %>% 
	filter(contrast == "GLM", set == "final", metric == "brier", sparsity %in% c(NA, 0.9)) %>% 
	mutate_at(c("contrast", "n", "EPV", "prev", "rho"), factor)

fm <- ~ Estimate + n + EPV + prev + rho + - 1
cdat <- model.matrix(fm, cdat)

pca <- prcomp(cdat, scale. = TRUE)
fviz_eig(pca)
fviz_pca_ind(pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_pca_var(pca,
						 col.var = "contrib", # Color by contributions to the PC
						 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
						 repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(pca,
								col.var = "#2E9FDF", # Variables color
								col.ind = "#696969"  # Individuals color
)
