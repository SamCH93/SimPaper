all: pdf clean

FILE=QRP
BIB=bibliography
BIBSTYLE=apalikedoiurl

pdf:
## generate .pdf file from .tex file
	pdflatex $(FILE)
	bibtex $(FILE)
	pdflatex $(FILE)
	pdflatex $(FILE)

texdiff: $(FILE).tex old/$(FILE)v1.tex old/ld.cfg
## generate .pdf which shows changes to previous version
	latexdiff -c old/ld.cfg old/$(FILE)v1.tex  $(FILE).tex > $(FILE)changes.tex
## TODO fix issue that also tables work, for now don't apply texdiff to them
	pdflatex $(FILE)changes
	bibtex $(FILE)changes
	pdflatex $(FILE)changes
	pdflatex $(FILE)changes

clean:  
	rm $(FILE).aux  $(FILE).blg $(FILE).bbl \
		$(FILE).out $(FILE).brf *.log \
		$(FILE)changes.aux  $(FILE)changes.blg  $(FILE)changes.bbl \
		$(FILE)changes.out $(FILE)changes.brf $(FILE)changes.tex
