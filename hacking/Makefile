SR = Rscript --vanilla --no-restore-data
SETTING = "full"

simulation:
	$(SR) simulation.R $(SETTING)

anova:
	$(SR) anova.R $(SETTING)

impute:
	$(SR) anova-imputed.R $(SETTING)

coefs:
	$(SR) analyze-coefs.R $(SETTING)

calib:
	$(SR) calibration.R $(SETTING)

all: anova coefs calib

clean:
	rm -rf simResults* simSeeds* results_anova TEMP* figures \
		Rplots.pdf simulation-conditions.csv

