all: pdf

FILE=QRP
BIB=bibliography
BIBSTYLE=apalikedoiurl

tex: $(FILE).Rnw $(BIB).bib $(BIBSTYLE).bst
	## generate .tex file from .Rnw file with knitr
	Rscript -e "library(knitr); knit('$(FILE).Rnw')"

pdf: tex
	## generate .pdf file from .tex file
	pdflatex $(FILE)
	bibtex $(FILE)
	pdflatex $(FILE)
	pdflatex $(FILE)

# generate docx file with pandoc 
## on debian/ubuntu install with sudo apt install pandoc
## FIXME: references don't work
docx: tex
	# create copy of tex file as input for pandoc
	cp ./$(FILE).tex ./$(FILE)Pandoc.tex
		
	# use sed to add .pdf extension to file-names in tex file (ugly hack)
	figures=`ls figure/*.pdf | sed -e 's/\.pdf$///'`; \
	for f in $$figures; do sed -i "s|$$f|$$f\.pdf|g" ./$(FILE)Pandoc.tex; done
		
	# use pandoc to generate .docx from .tex file
	pandoc $(FILE)Pandoc.tex --bibliography=$(BIB).bib --natbib -o $(FILE).docx
	
clean:  
	rm $(FILE).aux  $(FILE).blg  $(FILE).log  $(FILE).tex  $(FILE).bbl  $(FILE).out $(FILE).brf $(FILE).tex $(FILE)Pandoc.tex $(FILE).log $(FILE).bbl $(FILE).synctex.gz; \
	rm -r ./figure/

build:
	R CMD build pkg

install:
	R CMD INSTALL ainet*.tar.gz

pkgclean:
	rm ainet*.tar.gz

pkgall: build install pkgclean
