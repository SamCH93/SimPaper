# Preliminary simulations
# SP, LK
# Aug 2021

set.seed(2410)

# Dependencies ------------------------------------------------------------

library(ainet)

# Conditions --------------------------------------------------------------

n <- c(100, 500, 1000, 5000)
EPV <- c(20, 10, 1, 0.1, 0.05)
# TODO: Kick out sparsity (+ protocol)
sparsity <- c("dense", "sparse")
prev <- c(0.01, 0.05, 0.1)
sigma2 <- c(1)
rho <- c(0, 0.3, 0.6, 0.95)
simGrid <- expand.grid(n = n, EPV = EPV, sparsity = sparsity, prev = prev,
                       sigma2 = sigma2, rho = rho, stringsAsFactors = FALSE)
simGrid$p <- with(simGrid, ceiling(n * prev / EPV))
simGrid$q <- with(simGrid, ifelse(sparsity == "sparse", floor(sqrt(n / log(p))), p))
simGrid$q <- with(simGrid, ifelse(!is.finite(q), 0, q))
nScenarios <- nrow(simGrid)

# TODO: Add seed column

# Simulation --------------------------------------------------------------

res <- runSimulation(
  simGrid[240, ],
  replications = 10,
  generate = generate,
  analyse = analyze,
  summarise = summarize,
  save_seeds = TRUE
)
