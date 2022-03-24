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

clean:  
	rm $(FILE).aux  $(FILE).blg  $(FILE).log  $(FILE).bbl \
		$(FILE).out $(FILE).brf *.log
