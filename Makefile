full:
	cd reproduce-results && make all SETTING=full

nonlin:
	cd reproduce-results && make all SETTING=nonlin

nonlin-fixed:
	cd reproduce-results && make all SETTING=nonlin-fixed

sparse:
	cd reproduce-results && make all SETTING=sparse

trunc:
	cd reproduce-results && make all SETTING=trunc

figure:
	cd figure && R -- vanilla < figure1.R

full-repro: full nonlin nonlin-fixed sparse trunc figure

partial-repro:
	cd simulation && make all

figure-repro: figure
