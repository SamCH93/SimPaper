# Reproduce all simulation studies from the paper

1. Install the `ainet` R package and its dependencies as indicated in
   [README](../README.md) in the parent folder

2. The simulation study can be started by running
```
make all SETTING=<setting>
```
with `<setting>` being either `full` or `nonlin`:

* `full` refers to the pre-registered simulation study with 2000 replications,
  see the simulation protocol in the appendix of the paper for details.

* `nonlin` includes different degrees of sparsity in the coefficients and a
   non-linear effect of random magnitude in `X.1`. This is the "tweaked"
   simulation study reported in the paper. Only 1000 replications were used here
   to reduce computation time, as there are more conditions overall.

This will create a folder with the raw simulation results
(`./reproduce-simulations/simResults-<setting>/`) and intermediate simulation
results after applying the ANOVA
(`./reproduce-simulations/results_anova-<setting>/`).

# Session Info

The below session info was taken from the time of the first (preliminary)
simulation study.

```
## R version 4.1.1 (2021-08-10)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 20.04.3 LTS
##
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.3
## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.3
##
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C
##  [3] LC_TIME=de_CH.UTF-8        LC_COLLATE=en_US.UTF-8
##  [5] LC_MONETARY=de_CH.UTF-8    LC_MESSAGES=en_US.UTF-8
##  [7] LC_PAPER=de_CH.UTF-8       LC_NAME=C
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C
## [11] LC_MEASUREMENT=de_CH.UTF-8 LC_IDENTIFICATION=C
##
## attached base packages:
## [1] stats graphics grDevices utils datasets methods
## [7] base
##
## other attached packages:
##  [1] ainet_0.0-1   magrittr_2.0.1 tidyr_1.1.3
##  [4] dplyr_1.0.6   pROC_1.17.0.1  mvtnorm_1.1-2
##  [7] SimDesign_2.7 glmnet_4.1-1   Matrix_1.3-4
## [10] ranger_0.13.1 knitr_1.33
##
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.7        plyr_1.8.6       compiler_4.1.1
##  [4] pillar_1.6.2      highr_0.9        iterators_1.0.13
##  [7] tools_4.1.1       jsonlite_1.7.2   evaluate_0.14
## [10] lifecycle_1.0.0   tibble_3.1.4     lattice_0.20-44
## [13] pkgconfig_2.0.3   rlang_0.4.11     foreach_1.5.1
## [16] cli_3.0.1         DBI_1.1.1        curl_4.3.1
## [19] parallel_4.1.1    xfun_0.23        withr_2.4.2
## [22] stringr_1.4.0     generics_0.1.0   vctrs_0.3.8
## [25] RPushbullet_0.3.4 grid_4.1.1       tidyselect_1.1.1
## [28] glue_1.4.2        R6_2.5.1         pbapply_1.4-3
## [31] fansi_0.5.0       survival_3.2-13  sessioninfo_1.1.1
## [34] purrr_0.3.4       codetools_0.2-18 ellipsis_0.3.2
## [37] splines_4.1.1     assertthat_0.2.1 shape_1.4.6
## [40] utf8_1.2.2        stringi_1.6.2    crayon_1.4.1
```
