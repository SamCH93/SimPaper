# analysis of simulations
# LK, SP, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(ggpubr)

# Read results ------------------------------------------------------------

## load simulation results
files <- list.files(path = "simResults/")
simres <- lapply(X = files, FUN = function(filename) {
    resList <- readRDS(file = paste0("simResults/", filename))
    lapply(X = resList$results, FUN = function(resListSimi) {
        resListSimi$coefs
    }) %>%
        bind_rows()
}) %>%
    bind_rows()

pdat <- simres %>% 
	filter(coef == "X.0") %>% 
	gather("method", "estimate", AINET:AEN) %>% 
	group_by(ID, n, EPV, prev, sigma2, rho, p, coef, method) %>% 
	mutate(bias_mean = estimate - oracle) # %>% 
	# summarize_at(c("estimate", "bias"), list(mean = mean, sd = sd,
	# 																				 n = \(.x, ...) length(na.omit(.x))),
	# 						 na.rm = TRUE) %>% 
	# mutate(bias_lwr = bias_mean - 1.96 * bias_sd / sqrt(bias_n),
	# 			 bias_upr = bias_mean + 1.96 * bias_sd / sqrt(bias_n))

rho_plot <- function(trho) {
	ggplot(pdat %>% filter(rho == trho), 
				 aes(x = method, y = bias_mean, # ymin = bias_lwr, ymax = bias_upr,
				 		color = ordered(EPV))) +
		geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
		geom_boxplot(position = position_dodge(width = 1), outlier.shape = NA) +
		stat_mean(shape = 4, position = position_dodge(width = 1)) +
		#geom_pointrange(fatten = 0.75, position = position_dodge(width = 0.7)) +
		#geom_errorbar(width = 0.35, position = position_dodge(width = 0.7)) +
		facet_grid(prev ~ n, labeller = label_both) +
		theme_bw() +
		theme(legend.position = "top", panel.grid.major.y = element_blank(),
					axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 7)) +
		labs(y = "Bias", x = "Method", subtitle = bquote(rho==~.(trho)), color = "EPV") +
		geom_vline(xintercept = seq(1.5, 3.5, 1), alpha = 0.1, size = 0.8) +
		coord_flip() +
		ylim(-10, 10)
}

ps <- lapply(unique(as.numeric(as.character(simres$rho))), rho_plot)
pf <- ggarrange(plotlist = ps, common.legend = TRUE, ncol = 2, nrow = 2)

ggsave("bias.pdf", plot = pf, height = 1.5 * 8.3, width = 1.5 * 11.7)
