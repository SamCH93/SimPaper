
if (!require(remotes))
  install.packages("remotes", repo = "http://cran.ch.r-project.org")

if (!require(tidyverse))
  install.packages("tidyverse", repo = "http://cran.ch.r-project.org")

if (!require(ggpp))
  install.packages("ggpp", repo = "http://cran.ch.r-project.org")

remotes::install_github("LucasKook/ainet", ref = "0.0-1")

