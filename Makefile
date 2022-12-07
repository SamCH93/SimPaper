
SR = Rscript --vanilla --no-restore-data

dependencies:
	$(SR) dependencies.R

final:
	cd reproduce-results && make all SETTING=full

nonlin:
	cd reproduce-results && make all SETTING=nonlin

nonlin-fixed:
	cd reproduce-results && make all SETTING=nonlin-fixed

sparse:
	cd reproduce-results && make all SETTING=sparse

trunc:
	cd reproduce-results && make all SETTING=trunc

pfinal:
	cd simulation && make all

pnonlin:
	cd hacking && make all SETTING=nonlin

pnonlin-fixed:
	cd hacking && make all SETTING=nonlin_fixed

psparse:
	cd hacking && make all SETTING=sparse

ptrunc:
	cd hacking && make all SETTING=trunc

full-repro: final nonlin # nonlin-fixed sparse trunc
	cd figure && $(SR) figure1.R full
	cp -r manuscript reproduce-results
	cp figure/ainet-results.pdf reproduce-results/manuscript
	cp reproduce-results/results_anova-full/*sparsity*.pdf reproduce-results/manuscript
	cd reproduce-results/manuscript && make all

partial-repro: pfinal pnonlin # pnonlin-fixed psparse ptrunc
	cd figure && $(SR) figure1.R partial
	cp -r manuscript reproduce-manuscript
	cp figure/ainet-results.pdf reproduce-manuscript
	cp simulation/figures/*sparsity*.pdf reproduce-manuscript/figures-appendix/
	cd reproduce-manuscript && make all

figure-repro:
	cd figure && $(SR) figure1.R figure
	cd simulation && $(SR) anova.R figure "../figure"
	cd simulation && $(SR) calibration.R figure "../figure"

clean:
	rm -rf reproduce-manuscript figure/*.pdf figure/*.csv
