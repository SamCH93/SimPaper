
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

pfull:
	cd simulation && make all

pnonlin:
	cd hacking && make all SETTING=nonlin

pnonlin-fixed:
	cd hacking && make all SETTING=nonlin_fixed

psparse:
	cd hacking && make all SETTING=sparse

ptrunc:
	cd hacking && make all SETTING=trunc

full-repro: full nonlin nonlin-fixed sparse trunc

partial-repro: pfull pnonlin pnonlin-fixed psparse ptrunc

figure-repro:
	cd figure && R --vanilla < figure1.R
