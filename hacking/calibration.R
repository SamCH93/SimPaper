# Calibration plots
# SP, LK, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(multcomp)
library(ggpubr)

inp <- "simResults-sparse"

outdir <- paste0(inp, "-results")
if (!dir.exists(outdir)) {
	dir.create(outdir)
}

# Load --------------------------------------------------------------------

files <- list.files(path = inp, pattern = ".rds", full.names = TRUE)
simres <- lapply(X = files, FUN = function(filename) {
	resList <- readRDS(file = filename)
	lapply(X = resList$results, FUN = function(resListSimi) {
		resListSimi$estimands
	}) %>%
		bind_rows()
}) %>%
	bind_rows()

adat <- simres %>% 
	mutate(inputp = ceiling(n * prev / EPV)) %>% 
	filter(inputp != 1) %>% 
	mutate_at(c("n", "EPV", "prev", "rho", "sparsity"), 
						~ factor(.x, levels = sort(unique(.x)))) %>% 
	mutate(fct = factor(paste0(model, "n", n, "EPV", EPV, "prev", prev, "rho", rho,
														 "sparsity", sparsity)))

# Funs --------------------------------------------------------------------

vis_results <- function(pdat, metric = c("cslope", "clarge"), save = TRUE,
												lim = c(-100, 100), only_one = FALSE) {
	
	metric <- match.arg(metric)
	yint <- switch(metric, "cslope" = 1, "clarge" = 0)	
	xxlab <- switch(metric, "cslope" = "calibration slope", 
									"clarge" = "calibration in the large")
	
	out2 <- pdat %>% 
		bind_rows() %>% 
		mutate_at(c("n", "EPV", "prev", "rho", "sparsity"), 
							~ factor(.x, levels = sort(unique(as.numeric(as.character(.x))))))
	
	nadat <- out2 %>% 
		group_by(n, EPV, prev, rho, sparsity, model) %>% 
		summarize(frac_na = round(100 * mean(is.na(!!sym(metric))), 1),
							frac_na = paste0(frac_na, "%"))
	
	rho_plot <- function(trho, tsparse) {
		ggplot(out2 %>% filter(rho == trho, sparsity == tsparse), 
					 aes(x = model, y = !!sym(metric), color = ordered(EPV))) +
			geom_hline(yintercept = yint, linetype = 2, alpha = 0.5) +
			geom_boxplot(position = position_dodge(width = 0.7), outlier.size = 0.1) +
			stat_mean(shape = 4, position = position_dodge(width = 0.7)) +
			facet_grid(prev ~ n, labeller = label_both) +
			geom_text(aes(y = lim[1] * 0.9, label = frac_na), data = nadat %>% filter(rho == trho, sparsity == tsparse),
								position = position_dodge(width = 0.7)) +
			theme_bw() +
			theme(legend.position = "top", panel.grid.major.y = element_blank(),
						axis.text.x = element_text(size = 7)) +
			labs(y = xxlab, x = element_blank(), 
					 subtitle = bquote(rho==~.(trho)~sparsity==~.(tsparse)), color = "EPV") +
			coord_flip(ylim = lim)
	}
	
	if (only_one)
		return(rho_plot(0.95, 0.9))
	
	lapply(unique(as.numeric(as.character(out2$rho))), function(rho) {
		ps <- lapply(unique(as.numeric(as.character(out2$sparsity))), function(sparse) {
			rho_plot(rho, sparse)	
		})
		pf <- ggarrange(plotlist = ps, common.legend = TRUE, ncol = 2, nrow = 2)
		
		if (save) {
			pnm <- file.path(outdir, paste0("calibration-", metric, "_rho", rho, ".pdf"))
			ggsave(pnm, plot = pf, height = 1.5 * 8.3, width = 1.5 * 11.7)
		}
	})
	
	return(pf)
}

# Run ---------------------------------------------------------------------

vis_results(adat, metric = "cslope", save = TRUE, lim = c(-10, 10))
vis_results(adat, metric = "clarge", save = TRUE, lim = c(-5, 5))
