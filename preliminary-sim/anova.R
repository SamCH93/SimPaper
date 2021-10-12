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
    mutate(inputp = ceiling(n * prev / EPV)) %>% 
    filter(inputp != 1) %>% 
	mutate_at(c("n", "EPV", "prev", "rho"), ~ factor(.x, levels = sort(unique(.x)))) %>% 
	mutate(fct = factor(paste0(model, "n", n, "EPV", EPV, "prev", prev, "rho", rho)))

# ANOVA -------------------------------------------------------------------

m <- lm(brier ~ 0 + fct, data = adat)

conds <- with(adat, unique(paste0("n", n, "EPV", EPV, "prev", prev, "rho", rho)))
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
    nms <- str_split(conds[cond], pattern = "[a-z]|[A-Z]")[[1]]
    nms <- nms[nms != ""]
    out[[cond]] <- data.frame(cf, pval = pval, n = nms[1], EPV = nms[2],
                              prev = nms[3], rho = nms[4], contrast = models)
}

out2 <- out %>% 
    bind_rows() %>% 
	mutate_at(c("n", "EPV", "prev", "rho"), ~ factor(.x, levels = sort(unique(as.numeric(as.character(.x))))))

ggplot(out2, aes(y = contrast, x = Estimate, xmin = lwr, xmax = upr,
                 color = case_when(upr < 0 ~ "AINET better", 
                                   lwr > 0 ~ "AINET worse",
                                   TRUE ~ "Neutral"))) +
    geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
    geom_pointrange(fatten = 0.75) +
    geom_errorbarh(height = 0.5) +
    facet_grid(EPV + rho ~ n + prev, labeller = label_both) +
    theme_bw() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    scale_color_manual(values = c("AINET better" = "orange",
                                  "AINET worse" = "cornflowerblue",
                                  "Neutral" = "black")) +
    labs(x = "Difference in Brier score (AINET - other method)", y = element_blank())
    
ggsave("figures/tie-fighter_brier.pdf", height = 1.5 * 8.3, width = 1.5 * 11.7)
