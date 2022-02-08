# Figures for manuscript
# LK, SP, KR
# Feb 2022

# Deps --------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(ggbeeswarm)
theme_set(theme_bw())

tnms <- c("prelim", "final", "nonlin", "nonlin_fix", "nonlin_imp")

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
	mutate(set = factor(set, levels = tnms))

# Vis ---------------------------------------------------------------------

ggplot(pdat, aes(x = set, y = Estimate, 
								 group = paste(n, EPV, prev, rho))) +
	geom_boxplot(aes(x = set, y = Estimate), inherit.aes = FALSE, outlier.shape = NA) +
	geom_line(alpha = 0.1) +
	geom_quasirandom(width = 0.3, alpha = 0.1) +
	facet_wrap(~ contrast) +
	geom_hline(yintercept = 0, lty = 2)
