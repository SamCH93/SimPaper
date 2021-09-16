# Preliminary simulations
# SP, LK
# Aug 2021

set.seed(2410)

# Dependencies ------------------------------------------------------------

library(ainet)

# Conditions --------------------------------------------------------------

n <- c(100, 500, 1000, 5000)
EPV <- c(20, 10, 1, 0.1, 0.05)
prev <- c(0.01, 0.05, 0.1)
sigma2 <- c(1)
rho <- c(0, 0.3, 0.6, 0.95)
simGrid <- expand.grid(n = n, EPV = EPV, prev = prev, sigma2 = sigma2,
                       rho = rho, stringsAsFactors = FALSE)
simGrid$p <- with(simGrid, ceiling(n * prev / EPV))
simGrid$q <- simGrid$p
nScenarios <- nrow(simGrid)

# Simulation --------------------------------------------------------------

res <- runSimulation(
  simGrid[240, ],
  replications = 10,
  generate = generate,
  analyse = analyze,
  summarise = summarize,
  save_seeds = TRUE
)
