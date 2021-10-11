# ANOVA for brier score
# SP, LK, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)
library(multcomp)

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
	mutate_at(c("n", "p", "prev", "rho"), as.factor) %>% 
	mutate(fct = factor(paste0(model, "n", n, "p", p, "prev", prev, "rho", rho)))

# ANOVA -------------------------------------------------------------------

m <- lm(brier ~ 0 + fct, data = adat)

conds <- with(adat, unique(paste0("n", n, "p", p, "prev", prev, "rho", rho)))
models <- unique(adat$model)[-1]
out <- list()

pb <- txtProgressBar(min = 1, max = length(conds), style = 3)
for (cond in seq_along(conds)) {
    setTxtProgressBar(pb, cond)
    g1 <- paste0("fct", "AINET", conds[cond])
    g2 <- paste0("fct", models, conds[cond])
    lfct <- paste(g1, "-", g2, "== 0")
    res <- try(glht(m, linfct = lfct))
    if (inherits(res, "try-error"))
       next 
    pval <- summary(res)$test$pvalues
    cf <- confint(res)$confint
    nms <- str_split(conds[cond], pattern = "[a-z]")[[1]]
    nms <- nms[nms != ""]
    out[[cond]] <- data.frame(cf, pval = pval, n = nms[1], p = nms[2],
                              prev = nms[3], rho = nms[4], contrast = models)
}

out2 <- out %>% 
    bind_rows() %>% 
    mutate(p = factor(as.numeric(as.character(p))))

ggplot(out2, aes(y = contrast, x = Estimate, xmin = lwr, xmax = upr,
                 color = case_when(upr < 0 ~ "AINET better", 
                                   lwr > 0 ~ "AINET worse",
                                   TRUE ~ "Neutral"))) +
    geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
    geom_pointrange(fatten = 1) +
    geom_errorbarh() +
    facet_grid(p + prev ~ n + rho, labeller = label_both) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_color_manual(values = c("AINET better" = "orange",
                                  "AINET worse" = "cornflowerblue",
                                  "Neutral" = "black"))
