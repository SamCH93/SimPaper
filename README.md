# Pitfalls and Potentials in Simulation Studies

This GitHub repository accompanies the arXiv preprint
[arXiv:2203.13076](https://arxiv.org/abs/2203.13076). All current and previous
versions of the files are archived at <https://doi.org/10.5281/zenodo.6364574>.



## Folder structure

* `./figure/` contains R code and a Makefile to reproduce figure 2 and the six
   figures from the supplementary.

* `./results-simulations/` contains raw and intermediate results of
  pre-registered and tweaked simulation studies.
  
* `./reproduce-simulations/` contains the R code and Makefile skeleton for
  reproducing all simulation results from scratch or from the intermediate
  results.

* `./Makefile` master Makefile for reproducing the results as outlined below.

* `./dependencies.R` R script for installing all necessary dependencies.

* `./ainet_0.0-1.tar.gz` R package containing functions for model fitting,
  performing the simulation studies and the analyses. See also
  <https://github.com/LucasKook/ainet> for the most recent version.


## Installing dependencies

The `./dependencies.R` file installs all dependencies necessary for reproducing
the results. It can be executed via `make dependencies`. This will also install
a custom package `ainet` which includes functions for fitting the different
methods, simulating data, running the simulation study and producing figures.


## Reproducing the results

Due to the computational overhead of running the simulations from scratch
(several weeks of computation time on a 60 core server), we make our results
reproducible in three different levels that require different amounts of
computation.

1. The simulation results can be reproduced from scratch by running `make
   full-repro`. This includes the pre-registered simulation and the tweaked
   simulations. The script will save the raw and intermediate simulation results
   in `./reproduce-simulations/`, and then reproduce all figures in `./figure/`.
   Adjust the parameter `ncores` in `./reproduce-simulations/simulation.R` to
   fit your system (currently set to 8).

2. The raw simulation results are saved in `./results-simulations/`, such that
   only the _analyses_ (ANOVAs) can be reproduced. To do so, run `make
   partial-repro`. Due to the many conditions, running the ANOVAs takes about 2
   hours on our server (64GB of RAM). R crashes on our personal computers with
   16GB of RAM while running the ANOVA. The script will save the intermediate
   results from the ANOVAs in `./reproduce-simulations/`, and then reproduce all
   figures in `./figure/`.

3. The intermediate results are saved in `./results-simulations/`, such that
   figure 2 and the six supplementary figures can be reproduced with `make
   figure-repro`. The reproduced figure can be found in `./figures/`. This is
   the only option that runs on our personal computers and does not require a
   more powerful server.
   
## Testing the pipeline

By running `make test-repro` the full pipeline (simulation -> ANOVA -> figures)
can be tested based on a subset of the simulation conditions and fewer
repetitions than in the simulation studies reported in the paper. This should
produce the same folders and outputs files as in reproducibility level 1. When
reproduced in this way, figure 2 will differ from the manuscript figure 2 as
much fewer repetitions are performed. Similarly, the supplementary figures will
differ and some parts of them will be empty as these conditions are not
simulated.

## Windows compatibility

All simulations and analyses were performed in Ubuntu Linux 20.04 LTS. As only
basic Unix shell utilities are required, they should also work on Mac. To
reproduce the results under Windows, install the [Chocolatey package
manager](https://chocolatey.org/install) and then install make by running `choco
install make` from PowerShell. Typically, the `Rscript.exe` executable is not in
the Windows PATH. Therefore modify the makefiles `./Makefile`,
`./figure/Makefile` and `./reproduce-simulations/Makefile` as indicated in the
comments starting with `## Windows` in each file. Based on these changes we ran
successfully `make test-repro` and `make figure-repro` on Windows 10 with a
fresh installation of R 4.2.2.

## Session Info

The below session info was taken from the time of the first (preliminary)
simulation.

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
