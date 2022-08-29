
SR = Rscript --vanilla --no-restore-data

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

partial-repro: pfinal pnonlin # pnonlin-fixed psparse ptrunc
	cd figure && $(SR) figure1.R partial

figure-repro:
	cd figure && $(SR) figure1.R figure
