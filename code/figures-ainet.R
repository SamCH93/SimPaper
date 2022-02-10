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
						 "hacking/simResults-nonlin-results/",
						 "hacking/simResults-nonlin_fix-results/")

paths <- expand_grid(folder = folders, metric = which)
paths$path <- paste0(paths$folder, paste0("anova_", paths$metric, ".csv"))

pdat <- paths %>% 
	mutate(d = map(path, ~ read_csv(.x, show_col_types = FALSE))) %>% 
	unnest(c(d)) %>% 
	mutate(set = factor(folder, levels = folders, labels = tlabs)) %>% 
	select(-path, -folder)

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
