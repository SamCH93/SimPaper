
TR = R --vanilla --no-restore-data
SR = Rscript --vanilla --no-restore-data
SETTING = "full"

simulation:
	$(SR) simulation.R $(SETTING)

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


