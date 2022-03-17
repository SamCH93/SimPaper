# Pitfalls and Potentials in Simulation Studies

This GitHub repository accompanies the arXiv preprint [arXiv:<number>](link).

## Reproducing the results

Due to the computational overhead of running the simulations from scratch
(several weeks of computation time on a 60 core server), we make our results
reproducible in three steps.

1. The simulation results can be reproduced from scratch following the
   instructions [here](./reproduce-results/). This includes the final
   simulation, the tweaked simulations considered in the manuscript and more.

2. The simulation results are saved, such that only the _analysis_ (ANOVAs) can
   be reproduced. To reproduce the results of the final simulation, follow the
   instructions [here](./simulation/). To reproduce the results of the tweaked
   simulation, follow the instructions [here](./hacking/). Due to the many
   conditions, running the ANOVAs may take some time on a standard machine.

3. All figures are reproducible from the results obtained by running the ANOVAs
   in step 2. Figure 1 can be reproduced by sourcing `./code/figures-ainet.R`.
   Figures for the simulations can be reproduced by following the instruction in
   the respective folders, as described in 1. and 2.



