## Windows: change Rscript to location of Rscript.exe, for example:
## SR = C:\Program Files\R\R-4.2.2\bin\Rscript.exe --vanilla --no-restore-data
SR = Rscript --vanilla --no-restore-data
SETTING = "full"
TEST = 0

simulation:
	$(SR) simulation.R $(SETTING) $(TEST)

anova:
	$(SR) anova.R $(SETTING) $(TEST)

all: simulation anova

clean:
	rm -rf simResults* simSeeds* results_anova* TEMP* figures* \
		Rplots.pdf *simulation-conditions.csv
