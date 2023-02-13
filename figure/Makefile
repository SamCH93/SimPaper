## Windows: change Rscript to location of Rscript.exe, for example:
## SR = C:\Program Files\R\R-4.2.2\bin\Rscript.exe --vanilla --no-restore-data
SR = Rscript --vanilla --no-restore-data
SETTING = "figure"

figure2:
	$(SR) figure2.R $(SETTING)
## convert to png
## pdftoppm -r 600 figure2.pdf figure2 -png

figureSup:
	$(SR) figureSup.R $(SETTING)

clean:
	rm Rplots.pdf

cleanall:
	rm *.pdf **/*.pdf

## Windows: change "mv" to "move"
renameSup:
	-mv tie-fighter_brier_sparsity0.pdf figureS1.pdf
	-mv tie-fighter_scaledBrier_sparsity0.pdf figureS2.pdf
	-mv tie-fighter_nll_sparsity0.pdf figureS3.pdf
	-mv tie-fighter_auc_sparsity0.pdf figureS4.pdf
	-mv calibration-cslope_sparsity0.pdf figureS5.pdf
	-mv calibration-clarge_sparsity0.pdf figureS6.pdf

all: figure2 figureSup renameSup clean
