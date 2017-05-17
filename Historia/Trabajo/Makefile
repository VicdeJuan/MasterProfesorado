all: evolution_of_cryptography.pdf

MAKE=pdflatex --interaction=batchmode


evolution_of_cryptography.pdf: evolution_of_cryptography.tex memoria.bib
	@$(MAKE) evolution_of_cryptography.tex; bibtex evolution_of_cryptography; makeglossaries evolution_of_cryptography; makeindex evolution_of_cryptography; $(MAKE) evolution_of_cryptography.tex; $(MAKE) evolution_of_cryptography.tex

bib: memoria.bib
	@bibtex evolution_of_cryptography; $(MAKE) evolution_of_cryptography.tex;
	
clean:
	@rm -f evolution_of_cryptography.dvi evolution_of_cryptography.ps *.snm *.out *.nav *.log *.aux *.toc *.vrb *.pdf *~ *.lof *.blg *.bbl *.acn *.acr *.alg *.glg *.glo *.gls *.glsdefs *.idx *.ilg *.ind *.ist *.lot *.tmp *.xdy *.fls *.fdb_latexmk
