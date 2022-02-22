
# Reproduce all simulation studies from the paper

All settings can be reproduced by running
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

