# analysis of simulations
# LK, SP, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(ggpubr)
library(ainet)

inp <- "simResults/"
outdir <- "results_anova"
if (!dir.exists(outdir)) {
    dir.create(outdir)
}

# Read results ------------------------------------------------------------

simres <- read_results(inp, "coefs")

# Coefs -------------------------------------------------------------------

cfs <- c("X.0", "X.1", "X.2")

sapply(cfs, function(tcf) {
	adat <- filter_coef(simres, tcf)
	nadat <- adat %>% 
		group_by(n, EPV, prev, rho, sparsity, method, coef) %>% 
		summarise(frac_na = round(100 * mean(is.na(estimate)), 1))
	try(vis_coefs(adat, cf = tcf, lim = c(-10, 10)))
	try(vis_coefs_na(pdat = nadat, cf = tcf))
})
