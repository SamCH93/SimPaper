# Figures for manuscript
# LK, SP, KR
# Feb 2022

# Deps --------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(ggbeeswarm)
library(factoextra)
theme_set(theme_bw())

tnms <- c("prelim", "final", "nonlin_fix", "nonlin", "nonlin_imp")
tlabs <- c("preliminary", "final", "nonlinear (fixed)", "nonlinear", "nonlinear (imputed)")

# Read --------------------------------------------------------------------

res_prelim <- read_csv("preliminary-sim/results_anova/anova_brier.csv") %>% 
	mutate(set = "prelim")
res_final <- read_csv("simulation/results_anova/anova_brier.csv") %>% 
	mutate(set = "final")
res_nonlin <- read_csv("hacking/simResults-nonlin-results/anova_brier.csv") %>% 
	mutate(set = "nonlin") %>% filter(sparsity == 0.9) %>% select(-sparsity)
res_nonlin_fix <- read_csv("hacking/simResults-nonlin_fix-results/anova_brier.csv") %>% 
	mutate(set = "nonlin_fix") %>% filter(sparsity == 0.9) %>% select(-sparsity)
res_imp <- read_csv("hacking/simResults-nonlin-results-imputed/anova_imp_brier.csv") %>% 
	mutate(set = "nonlin_imp") %>% filter(sparsity == 0.9) %>% select(-sparsity)

pdat <- full_join(res_prelim, res_final) %>% full_join(res_nonlin) %>% 
	full_join(res_nonlin_fix) %>% full_join(res_imp) %>% 
	mutate(set = factor(set, levels = tnms, labels = tlabs))

# Vis ---------------------------------------------------------------------

ggplot(pdat, aes(x = set, y = Estimate, 
								 group = paste(n, EPV, prev, rho), color = factor(EPV))) +
	geom_boxplot(aes(x = set, y = Estimate), inherit.aes = FALSE, outlier.shape = NA) +
	geom_line(alpha = 0.1) +
	geom_quasirandom(width = 0.3, alpha = 0.3) +
	facet_wrap(~ contrast) +
	geom_hline(yintercept = 0, lty = 2) +
	labs(x = "Simulation setting", y = "Difference in Brier score (smaller: AINET better)",
			 color = "EPV")

# Clustering --------------------------------------------------------------

cdat <- pdat %>% 
	filter(contrast == "GLM") %>% 
	mutate_at(c("contrast", "n", "EPV", "prev", "rho"), factor)

fm <- ~ Estimate + n + EPV + prev + rho + set - 1
cdat <- model.matrix(fm, pdat)

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
