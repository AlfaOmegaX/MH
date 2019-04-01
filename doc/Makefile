PANDOC:=pandoc
ASTS:=assets
FILTERS:= filters/pandoc-crossref pandoc-citeproc
PFLAGS:= $(foreach filter,$(FILTERS),-F $(filter)) --template $(ASTS)/plantilla.tex
MDS:=P1.md P2.md P3.md
PDFS:=$(MDS:%.md=%.pdf)

.PHONY: all clean

all: filters/pandoc-crossref $(PDFS)

filters/pandoc-crossref:
	wget https://github.com/lierdakil/pandoc-crossref/releases/download/v0.3.4.0d/linux-pandoc_2_7.tar.gz
	tar -xzf linux-pandoc_2_7.tar
	mkdir filters
	cp pandoc-crossref filters/pandoc-crossref
	rm pandoc-crossref linux-pandoc_2_7.tar pandoc-crossref.1

%.pdf: %.md $(ASTS)/citas.bib $(ASTS)/plantilla.tex $(ASTS)/descripcion.md
	$(PANDOC) $(PFLAGS) -H $(ASTS)/header.sty $(ASTS)/descripcion.md $< -o $@

clean:
	rm $(PDFS)
