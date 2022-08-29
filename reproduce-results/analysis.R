#!/usr/bin/env Rscript
setting <- commandArgs(trailingOnly = TRUE)[1]
tpath <- paste0("simResults-", setting)

# analysis of simulations
# LK, SP, KR
# Oct 2021

# Deps --------------------------------------------------------------------

library(SimDesign)
library(tidyverse)

# Read results ------------------------------------------------------------

## load simulation results
files <- list.files(path = tpath)
simres <- lapply(X = files, FUN = function(filename) {
    resList <- readRDS(file = paste0(tpath, filename))
    lapply(X = resList$results, FUN = function(resListSimi) {
        resListSimi$estimands
    }) %>%
        bind_rows()
}) %>%
    bind_rows()

# Warnings & Errors -------------------------------------------------------

## convergence/cv errors
iamna <- simres %>%
    group_by(n, EPV, prev, rho, model) %>%
    summarise(failed = mean(is.na(brier))) %>%
    arrange(-failed)

ggplot(iamna, aes(x = model, y = failed)) +
    geom_col() +
    facet_grid(EPV + rho ~ n + prev, labeller = label_both) +
    theme_bw() +
    coord_flip() +
    theme(text = element_text(size = 7)) +
    labs(y = "Proportion of simulation with convergence issues (among 100 simulations)", x = "Model")
ggsave(file.path(tpath, "failed.pdf"), height = 8.3, width = 11.7)

## errors
iamerror <- lapply(X = files, FUN = function(filename) {
    resList <- readRDS(file = paste0(tpath, filename))
    err <- resList$errors
    data.frame(resList$condition,
               error = ifelse(is.null(err), NA, err),
               type = ifelse(is.null(names(err)), NA, names(err)))
}) %>%
    bind_rows()

unique(iamerror$type)

perr <- iamerror %>% 
    mutate(type = case_when(str_detect(type, "No events.") ~ "No events"))

ggplot(perr %>% filter(!is.na(type)), aes(x = type, y = error / 100)) +
    geom_col() +
    facet_grid(EPV + rho ~ n + prev, labeller = label_both) +
    theme_bw() +
    coord_flip() +
    theme(text = element_text(size = 7), axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    labs(y = "Proportion of simulation with errors (among 100 simulations)", x = "Type of error")
ggsave(file.path(tpath, "errors.pdf"), height = 8.3, width = 4.5)

## warnings
iamwarning <- lapply(X = files, FUN = function(filename) {
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

pwarn <- iamwarning %>% 
    mutate(warning = case_when(
        str_detect(warning, "lognet") ~ "fewer than 8 events",
        str_detect(warning, "getcoef") ~ "empty model",
        str_detect(warning, "Fortran code") ~ "cv.glmnet convergence",
        str_detect(warning, "glm.fit") ~ "RF predicts 0 or 1"
    ))

ggplot(pwarn, aes(x = warning, y = as.numeric(number))) +
    geom_col() +
    facet_grid(EPV + rho ~ n + prev, labeller = label_both) +
    theme_bw() +
    scale_y_log10() +
    coord_flip() +
    labs(x = "Type of warning", y = "Count") +
    theme(text = element_text(size = 7), axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
ggsave(file.path(tpath, "warnings.pdf"), height = 8.3, width = 11.7)
