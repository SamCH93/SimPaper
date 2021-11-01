FILE = reportSeriousSimulation

pdf:
	Rscript -e "library(knitr); knit2pdf('$(FILE).Rnw')"

clean:  
	rm $(FILE).aux  $(FILE).blg  $(FILE).log  $(FILE).tex  $(FILE).bbl  $(FILE).out $(FILE).brf $(FILE).tex $(FILE)Pandoc.tex $(FILE).log $(FILE).bbl $(FILE).synctex.gz; \
	rm -r ./figure/ $(FILE)-concordance.tex

all: pdf clean
