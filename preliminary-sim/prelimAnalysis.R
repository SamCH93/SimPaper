## analysis of preliminary simulations
library(SimDesign)
library(tidyverse)


## load simulation results
files <- list.files(path = "simResults/")
simres <- lapply(X = files, FUN = function(filename) {
    resList <- readRDS(file = paste0("simResults/", filename))
    lapply(X = resList$results, FUN = function(resListSimi) {
        resListSimi$estimands
    }) %>%
        bind_rows()
}) %>%
    bind_rows()


## some plots
ggplot(data = simres,
       aes(x = model, y = brier - brier_oracle, color = model)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.5)
    geom_boxplot() +
    facet_grid(EPV + rho ~ n + prev, labeller = label_both, scales = "free") +
    lims(y = c(-2, 2)) +
    coord_flip() +
    theme_bw()

ggplot(data = simres,
       aes(x = model, y = cslope - cslope_oracle, color = model)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
    geom_boxplot() +
    facet_grid(EPV + rho ~ n + prev, labeller = label_both, scales = "free") +
    lims(y = c(-2, 2)) +
    coord_flip() +
    theme_bw()

ggplot(data = simres,
       aes(x = model, y = auc - auc_oracle, color = model)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
    geom_boxplot() +
    facet_grid(EPV + rho ~ n + prev, labeller = label_both, scales = "free") +
    coord_flip() +
    theme_bw()

ggplot(data = simres,
       aes(x = model, y = scaledBrier - scaledBrier_oracle, color = model)) +
    geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
    geom_boxplot() +
    facet_grid(prev + n ~ EPV + rho, labeller = label_both, scales = "free") +
    coord_flip() +
    theme_bw()

## compute worst case variance of brier score
simres %>%
    group_by(n, EPV, prev, rho, model) %>%
    summarise(varBrierMax = max(brierVar)) %>%
    arrange(-varBrierMax)

## taking 0.2 as upper bound on variance to compute sample size
0.2/(10000*0.0001^2)
