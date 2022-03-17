
# Reproduce all simulation studies from the paper

1. Install the `ainet` package by running
```r
remotes::install_github("SamCH93/SimPaper/pkg")
```
or via
```
make pkgall
```
in the home directory of this project.

2. All settings can be reproduced by running
```
make all SETTING="<setting>"
```
with `SETTING` being one of
```
["full" "nonlin" "nonlin-fixed" "sparse" "trunc"]
```

`full` refers to the originally planned simulation study with 2000 replications.

`nonlin` allows for a non-linear effect of random magnitude in `X.1`.

`nonline-fixed` fixes the magnitude of the non-linear effect to 1.1.

`sparse` is the same logistic-linear DGP as in `full` but with varying degrees
of sparsity in the coefficients.

`trunc` alters the way `ainet` computes adaptive weights from random forest
variable importance.


last tested on Ubuntu 20.04.3 LTS with R version 4.1.1
