
if (!require(remotes))
  install.packages("remotes", repo = "http://cran.ch.r-project.org")

if (!require(tidyverse))
  install.packages("tidyverse", repo = "http://cran.ch.r-project.org")

if (!require(ggpp))
  install.packages("ggpp", repo = "http://cran.ch.r-project.org")

remotes::install_local("ainet_0.0-1.tar.gz")

