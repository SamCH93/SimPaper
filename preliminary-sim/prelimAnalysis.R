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
    geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
    geom_boxplot() +
    facet_grid(EPV + rho ~ n + prev, labeller = label_both, scales = "free") +
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

## analysis of warnings/error
## -----------------------------------------------------------------------------

## convergence/cv errors
simres %>%
    group_by(n, EPV, prev, rho, model) %>%
    summarise(failed = mean(is.na(brier))) %>%
    arrange(-failed) %>%
    filter(failed >= 0.1) %>%
    print(n = 100)

## errors
iamerror <- lapply(X = files, FUN = function(filename) {
    resList <- readRDS(file = paste0("simResults/", filename))
    err <- resList$errors
    data.frame(resList$condition,
               error = ifelse(is.null(err), NA, err),
               type = ifelse(is.null(names(err)), NA, names(err)))
}) %>%
    bind_rows()

## warnings
iamwarning <-
    lapply(X = files, FUN = function(filename) {
    resList <- readRDS(file = paste0("simResults/", filename))
    waluigi <- resList$warnings
    tibble(resList$condition, number = list(waluigi),
           warning = list(names(waluigi)))
}) %>%
    bind_rows() %>%
    unnest(c(number, warning)) %>%
    filter(!stringr::str_detect(string = warning, pattern = "epv")) %>%
    arrange(-number)

unique(iamwarning$warning)

iamwarning %>%
    ## uncomment below to see different warnings
    ## -------------------------------------------------------------------------
    ## fewer than 8 events in cv fold
    ## filter(stringr::str_detect(string = warning, pattern = "lognet")) %>%

    ## no non-zero coefficients, empty model returned
    ## filter(stringr::str_detect(string = warning, pattern = "getcoef")) %>%

    ## glmnet cv convergence problem for some lambda values
    ## filter(stringr::str_detect(string = warning, pattern = "Fortran code")) %>%

    ## RF calibration slope problem because of p = 0 or p = 1
    ## filter(stringr::str_detect(string = warning, pattern = "glm.fit")) %>%

    print(n = 50)
