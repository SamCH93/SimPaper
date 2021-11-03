# Calibration plots
# SP, LK, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(multcomp)
library(ggpubr)

outdir <- "figures"
if (!dir.exists(outdir)) {
    dir.create(outdir)
}

# Load --------------------------------------------------------------------

files <- list.files(path = "simResults/")
simres <- lapply(X = files, FUN = function(filename) {
    resList <- readRDS(file = paste0("simResults/", filename))
    lapply(X = resList$results, FUN = function(resListSimi) {
        resListSimi$estimands
    }) %>%
        bind_rows()
}) %>%
    bind_rows()

adat <- simres %>% 
    mutate(inputp = ceiling(n * prev / EPV)) %>% 
    filter(inputp != 1) %>% 
	mutate_at(c("n", "EPV", "prev", "rho"), ~ factor(.x, levels = sort(unique(.x)))) %>% 
	mutate(fct = factor(paste0(model, "n", n, "EPV", EPV, "prev", prev, "rho", rho)))

# Funs --------------------------------------------------------------------

vis_results <- function(pdat, metric = c("cslope", "clarge"), save = TRUE,
												lim = c(-100, 100)) {
	
	metric <- match.arg(metric)
	yint <- switch(metric, "cslope" = 1, "clarge" = 0)	
	xxlab <- switch(metric, "cslope" = "calibration slope", 
								 "clarge" = "calibration in the large")
    
	out2 <- pdat %>% 
		bind_rows() %>% 
		mutate_at(c("n", "EPV", "prev", "rho"), 
							~ factor(.x, levels = sort(unique(as.numeric(as.character(.x))))))
	
	rho_plot <- function(trho) {
		ggplot(out2 %>% filter(rho == trho), 
					 aes(x = model, y = !!sym(metric), color = ordered(EPV))) +
			geom_hline(yintercept = yint, linetype = 2, alpha = 0.5) +
			geom_boxplot(position = position_dodge(width = 0.7), outlier.size = 0.1) +
			stat_mean(shape = 4, position = position_dodge(width = 0.7)) +
			facet_grid(prev ~ n, labeller = label_both) +
			theme_bw() +
			theme(legend.position = "top", panel.grid.major.y = element_blank(),
						axis.text.x = element_text(size = 7)) +
			labs(y = xxlab, x = element_blank(), subtitle = bquote(rho==~.(trho)), color = "EPV") +
			coord_flip(ylim = lim)
	}
	
	ps <- lapply(unique(as.numeric(as.character(out2$rho))), rho_plot)
	pf <- ggarrange(plotlist = ps, common.legend = TRUE, ncol = 2, nrow = 2)
	
	if (save) {
		pnm <- file.path(outdir, paste0("calibration-", metric, ".pdf"))
		ggsave(pnm, plot = pf, height = 1.5 * 8.3, width = 1.5 * 11.7)
	}
	
	return(pf)
}

# Run ---------------------------------------------------------------------

vis_results(adat, metric = "cslope", save = TRUE, lim = c(-10, 10))
vis_results(adat, metric = "clarge", save = TRUE, lim = c(-10, 10))
