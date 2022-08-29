TR = R --vanilla --no-restore-data

simulation:
	$(TR) < simulation.R

anova:
	$(TR) < anova.R

impute:
	$(TR) < anova-imputed.R

coefs:
	$(TR) < analyze-coefs.R

calib:
	$(TR) < calibration.R

all: simulation anova impute coefs calib

clean:
	rm -rf simResults* simSeeds* results_anova TEMP* figures \
		Rplots.pdf simulation-conditions.csv

