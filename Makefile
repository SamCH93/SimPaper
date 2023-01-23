## Windows: change Rscript to location of Rscript.exe, for example:
## SR = C:\Program Files\R\R-4.2.2\bin\Rscript.exe --vanilla --no-restore-data
SR = Rscript --vanilla --no-restore-data

## install R dependencies
dependencies:
	$(SR) dependencies.R

## the pre-registered simulation study
final:
	cd reproduce-simulations && make all SETTING=full

## the tweaked simulation study
nonlin:
	cd reproduce-simulations && make all SETTING=nonlin

## run ANOVA for pre-registered simulation study
anovafinal:
	cd reproduce-simulations && make anova SETTING=full

## run ANOVA and create plots for tweaked simulation study
anovanonlin:
	cd reproduce-simulations && make anova SETTING=nonlin

## level 1: run simulation from scratch, run ANOVA, create figures
full-repro: final nonlin
	cd figure && make all SETTING=full

## level 2: run ANOVA, create figures from intermediate simulation results
partial-repro:
	cp -r results-simulations/pre-registered/results-simulation reproduce-simulations/simResults-full/
	cp -r results-simulations/tweaked/results-simulation reproduce-simulations/simResults-nonlin/
	make anovafinal anovanonlin
	cd figure && make all SETTING=partial

## level 3: create figures from pre-run ANOVA results
figure-repro:
	cd figure && make all SETTING=figure

## test pipeline from start to finish with number of simulation conditions and
## small number of repetitions
test-repro:
	cd reproduce-simulations && make all SETTING=full TEST=1
	cd reproduce-simulations && make all SETTING=nonlin TEST=1
	cd figure && make all SETTING=full
