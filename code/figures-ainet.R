# Figures for manuscript
# LK, SP, KR
# Feb 2022

# Deps --------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(ggbeeswarm)
library(factoextra)
library(patchwork)
library(ggpp)
theme_set(theme_bw())

# Params ------------------------------------------------------------------

tsparse <- 0.3
tnms <- c(
	"prelim",
	"final",
	"final (imputed)",
	"nonlin_fix",
	"nonlin_fix (imputed)",
	"nonlin"
)
tlabs <- c(
	"preliminary",
	"final",
	"final (imputed)",
	"nonlinear (fixed)",
	"nonlinear (fixed+imputed)",
	"nonlinear"
) 

which <- c("brier", "scaledBrier", "nll", "auc", "acc")

folders <- c("preliminary-sim/results_anova/", 
						 "simulation/results_anova/",
						 "simulation/simResults-results-imputed/",
						 "hacking/simResults-nonlin_fix-results/",
						 "hacking/simResults-nonlin_fix-results-imputed/",
						 "hacking/simResults-nonlin-results/")

paths <- expand_grid(folder = folders, metric = which)
paths$path <- paste0(paths$folder, paste0("anova_", paths$metric, ".csv"))

pdat <- paths %>% 
	mutate(d = map(path, ~ read_csv(.x, show_col_types = FALSE))) %>% 
	unnest(c(d)) %>% 
	mutate(set = factor(folder, levels = folders, labels = tlabs)) %>% 
	select(-path, -folder)

## OG results
pdatE46 <- pdat %>%
	filter(
		rho == 0.95,
		sparsity %in% c(0, NA),
		set %in% c("final"), # , "final"),
		# contrast != "EN",
		metric == "brier",
		n < 5000,
		prev == 0.05,
		# EPV < 10
	)

palpha <- 0.9
aalpha <- 0.7
ny <- 0

p0 <- ggplot(data = pdatE46, aes(y = contrast)) +
	facet_grid(. ~ n, # scales = "free",
						 labeller = label_bquote(cols = italic(n) == .(n))) +
	geom_vline(xintercept = 0, lty = 2, alpha = 0.3) +
	geom_point(aes(x = Estimate, col = ordered(EPV)),
						 position = position_dodge(width = 0.5), size = 0.8) +
	geom_errorbarh(aes(xmin = lwr, xmax = upr, col = ordered(EPV)),
								 position = position_dodge(width = 0.5),
								 height = 0.25, show.legend = FALSE) +
	labs(x = "Difference in Brier score (negative: AINET better)",
			 y = element_blank(), color = "EPV") +
	theme(panel.grid.major.y = element_blank(),
				panel.grid.minor.x = element_blank()) +
	geom_hline(yintercept = seq(1.5, 3.5, 1), alpha = 0.1, size = 0.8) +
	scale_alpha_manual(values = c(0.1, 1))

## E1
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
	facet_grid(. ~ n, # scales = "free",
						 labeller = label_bquote(cols = italic(n) == .(n))) +
	geom_vline(xintercept = 0, lty = 2, alpha = 0.3) +
	geom_linerange(data = pdatE1wide, alpha = aalpha,
								 aes(xmin = final, xmax = nonlinear, y = contrast,
								 		col = ordered(EPV)),
								 position = position_dodgenudge(0.5, y = ny),
								 show.legend = FALSE) +
	geom_errorbarh(aes(xmin = lwr, xmax = upr, col = ordered(EPV)),
								 position = position_dodge(width = 0.5), alpha = palpha,
								 height = 0.25, show.legend = FALSE) +
	## geom_point(aes(x = Estimate, col = ordered(EPV)), size = 0.8,
	##            position = position_dodge(width = 0.5), alpha = 0.5) +
	geom_point(data = pdatE1wide,
						 aes(x = final, col = ordered(EPV)), size = 0.8,
						 position = position_dodge(width = 0.5), alpha = palpha) +
	geom_point(data = pdatE1wide,
						 aes(x = nonlinear, col = ordered(EPV)), size = 0.8,
						 position = position_dodge(width = 0.5), alpha = palpha) +
	geom_point(data = pdatE1wide,
						 aes(x = nonlinear, col = ordered(EPV), shape = better), size = 4,
						 position = position_dodgenudge(width = 0.5, y = ny), alpha = aalpha,
						 show.legend = FALSE) +
	scale_shape_manual(values = c("better" = 60, "worse" = 62)) +
	labs(x = "Difference in Brier score (negative: AINET better)",
			 y = element_blank(), color = "EPV") +
	theme(panel.grid.major.y = element_blank(),
				panel.grid.minor.x = element_blank()) +
	geom_hline(yintercept = seq(1.5, 3.5, 1), alpha = 0.1, size = 0.8)
#2" ggsave(p1, filename = "code/E1.pdf", height = 4.5)

## E1 + E3
pdatE46 <- pdat %>%
	filter(
		rho == 0.95,
		sparsity %in% c(0, NA, 0.9),
		set %in% c("nonlinear", "final"),
		metric == "brier",
		n < 5000,
		prev == 0.05
	) %>% 
	mutate(alp1 = contrast != "EN", 
				 alp = paste0(EPV, contrast != "EN"),
				 alp = ordered(alp, levels = str_sort(unique(alp), numeric = TRUE)))
pdatE46wide <- pdatE46 %>%
	mutate(sparsity = 0.9) %>%
	select(contrast, n, EPV, prev, rho, sparsity, Estimate, set, alp, alp1) %>%
	spread(key = set, value = Estimate) %>%
	mutate(better = ifelse(nonlinear < final, "better", "worse"))

p2 <- ggplot(data = pdatE46, aes(y = contrast)) +
	facet_grid(. ~ n, # scales = "free",
						 labeller = label_bquote(cols = italic(n) == .(n))) +
	geom_vline(xintercept = 0, lty = 2, alpha = 0.3) +
	geom_linerange(data = pdatE46wide, # alpha = aalpha,
								 aes(xmin = final, xmax = `nonlinear`, y = contrast, alpha = alp,
								 		color = ordered(EPV)),
								 position = position_dodgenudge(0.5, y = ny),
								 show.legend = FALSE) +
	geom_errorbarh(aes(xmin = lwr, xmax = upr, col = ordered(EPV), alpha = alp1),
								 position = position_dodge(width = 0.5), # alpha = palpha,
								 height = 0.25, show.legend = FALSE) +
	## geom_point(aes(x = Estimate, col = ordered(EPV)), size = 0.8,
	##            position = position_dodge(width = 0.5), alpha = 0.5) +
	geom_point(data = pdatE46wide,
						 aes(x = final, col = ordered(EPV), alpha = alp1), size = 0.8,
						 position = position_dodge(width = 0.5)) + # , alpha = palpha) +
	geom_point(data = pdatE46wide,
						 aes(x = `nonlinear`, col = ordered(EPV), alpha = alp1), size = 0.8,
						 position = position_dodge(width = 0.5)) + # , alpha = palpha) +
	geom_point(data = pdatE46wide,
						 aes(x = `nonlinear`, col = ordered(EPV), alpha = alp, shape = better), size = 4,
						 position = position_dodgenudge(width = 0.5, y = ny), # alpha = aalpha,
						 show.legend = FALSE) +
	scale_shape_manual(values = c("better" = 60, "worse" = 62)) +
	labs(x = "Difference in Brier score (negative: AINET better)",
			 y = element_blank(), color = "EPV") +
	theme(panel.grid.major.y = element_blank(),
				panel.grid.minor.x = element_blank()) +
	scale_alpha_manual(values = c(rep(c(0.1, aalpha), 4), 0.1, palpha)) +
	geom_hline(yintercept = seq(1.5, 3.5, 1), alpha = 0.1, size = 0.8) +
	theme(axis.text.y.left = element_text(color = c("gray30", "gray80", "gray30", "gray30")))

## E1 + E3 + E6
pdatE46 <- pdat %>%
	filter(
		rho == 0.95,
		sparsity %in% c(0, NA, 0.9),
		set %in% c("nonlinear"), # , "final"),
		# contrast != "EN",
		metric == "brier",
		n < 5000,
		prev == 0.05,
		# EPV < 10
	) %>% 
	mutate(alp = contrast != "EN" & EPV < 10)

p3 <- ggplot(data = pdatE46, aes(y = contrast, alpha = alp)) +
	facet_grid(. ~ n, # scales = "free",
						 labeller = label_bquote(cols = italic(n) == .(n))) +
	geom_vline(xintercept = 0, lty = 2, alpha = 0.3) +
	geom_point(aes(x = Estimate, col = ordered(EPV), alpha = alp),
						 position = position_dodge(width = 0.5), size = 0.8) +
	geom_errorbarh(aes(xmin = lwr, xmax = upr, col = ordered(EPV)),
								 position = position_dodge(width = 0.5),
								 height = 0.25, show.legend = FALSE) +
	labs(x = "Difference in Brier score (negative: AINET better)",
			 y = element_blank(), color = "EPV") +
	theme(panel.grid.major.y = element_blank(),
				panel.grid.minor.x = element_blank()) +
	geom_hline(yintercept = seq(1.5, 3.5, 1), alpha = 0.1, size = 0.8) +
	scale_alpha_manual(values = c(0.1, 1)) +
	theme(axis.text.y.left = element_text(color = c("gray30", "gray80", "gray30", "gray30"))) +
	xlim(min(p1$data$Estimate) * 1.05, max(p1$data$Estimate) * 1.13)

ggarrange(
	p0 + labs(subtitle = "Per-protocol"),
	p1 + labs(subtitle = "QRP: E1"),
	p2 + labs(subtitle = "QRP: E1 + E2"),
	p3 + labs(subtitle = "QRP: E1 + E2 + R1 (reported)"),
	common.legend = TRUE, ncol = 1, legend = "right"
)

ggsave("ainet-results.pdf", height = 13 * 0.9, width = 10 * 0.9)

