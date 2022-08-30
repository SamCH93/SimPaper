
if (!require(remotes))
  install.packages("remotes")

if (!require(tidyverse))
  install.packages("tidyverse")

if (!require(ggpp))
  install.packages("ggpp")

remotes::install_github("LucasKook/ainet", ref = "0.0-1")

