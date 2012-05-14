# Tools

PDFLATEX=pdflatex -interaction=nonstopmode
TD=./report/utils/texdepend
D2T=dot2tex -f pgf --crop --docpreamble "\usepackage[T2A]{fontenc} \usepackage[utf8]{inputenc} \usepackage[english, russian]{babel}"
PDFTRIMWHITE=report/utils/pdftrimwhite


# Output file
PDF=rpz.pdf

GRAPHeR=report/utils/grapher

# Input paths
EXP=report/graphics/exp
DIA=report/graphics/dia
DOT=report/graphics/dot
SVG=report/graphics/svg
TEX=report/tex
DEPS=report/.deps
SRC=report/src
INC=$(TEX)/inc

# Input files
# no .tex allowed in MAINTEX!
MAINTEX=rpz
BIBFILE=$(TEX)/rpz.bib
PREAMBLE=preamble-std.tex
STYLES=$(TEX)/GostBase.clo $(TEX)/G7-32.sty $(TEX)/G7-32.cls $(TEX)/G2-105.sty
PARTS_TEX = $(wildcard $(TEX)/[0-9][0-9]-*.tex)
HS = $(wildcard src/Model/*.hs)

all: report

report: $(PDF)

.PHONY: all tarball clean

PARTS_DEPS=$(PARTS_TEX:report/tex/%=$(DEPS)/%-deps.mk)
-include $(PARTS_DEPS)

MAIN_DEP=$(DEPS)/$(MAINTEX).tex-deps.mk
-include $(MAIN_DEP)


$(DEPS)/%-deps.mk: $(TEX)/% Makefile
	mkdir -p $(DEPS)
	(echo -n "$(PDF): " ; $(TD) -print=fi -format=1 $< | grep -v '^#' | xargs echo) > $@

$(PDF): $(TEX)/$(MAINTEX).tex $(STYLES) $(BIBFILE)
	cd $(TEX) && $(PDFLATEX) $(MAINTEX) &&  $(PDFLATEX) $(MAINTEX) && $(PDFLATEX) $(MAINTEX) && cp $(PDF) ../..

$(INC)/dia/%.eps: $(DIA)/%.dia
	mkdir -p $(INC)/dia
	dia -e $(@:%.pdf=%.eps) -t eps $<


report/utils/grapher: $(HS) src/MOK.cabal
	cd src && cabal configure && cabal build && cp dist/build/grapher/grapher ../report/utils

# .exp -> .svg -> .pdf
$(INC)/exp/%.svg: $(EXP)/%.exp report/utils/grapher
	mkdir -p $(INC)/exp
	report/utils/grapher $< $@
	
$(INC)/exp/%.pdf: $(INC)/exp/%.svg
	inkscape -A $(INC)/exp/$*-tmp.pdf $< && cd $(INC)/exp && ../../../../$(PDFTRIMWHITE) $*-tmp.pdf $*.pdf && rm $*-tmp.pdf
		
     

# .dot -> .eps (via dot2tex)
$(INC)/dot/%.eps: $(DOT)/%.dot
	mkdir -p $(INC)/dot
	dot -Teps $< > $@


$(INC)/svg/%.pdf : $(SVG)/%.svg
	mkdir -p $(INC)/svg/
# 	inkscape -A $@ $<
# Обрезаем поля в svg автоматом:
	inkscape -A $(INC)/svg/$*-tmp.pdf $< && cd $(INC)/svg && ../../../$(PDFTRIMWHITE) $*-tmp.pdf $*.pdf && rm $*-tmp.pdf


# .eps --> .pdf
$(INC)/%.pdf: $(INC)/%.eps
	epstopdf --outfile $@ $<


# .dot -> .tex (via dot2tex)
$(INC)/dot/%.tex: $(DOT)/%.dot
	mkdir -p $(INC)/dot
	$(D2T) --preproc $< | $(D2T)  > $@
# 	$(D2T) $< > $@

# .dot -> .tex --> .pdf
$(INC)/dot/%.pdf: $(INC)/dot/%.tex
	$(PDFLATEX) -output-directory=$(INC)/dot $<

$(INC)/src/%: $(SRC)/%
	mkdir -p $(INC)/src
	iconv -f=UTF-8 -t=KOI8-R $< > $@

clean:
	find $(TEX)/ -regextype posix-egrep -type f ! -regex ".*\.(sty|tex|clo|cls|bib|bst|gitignore)" -exec $(RM) {} \; ;
# 	$(RM) $(DIA)/*.pdf $(DIA)/*.eps
	$(RM) -r $(DEPS)
	$(RM) -r $(INC)

distclean: clean

PACK = $(addprefix latex-g7-32/, Makefile tex/* src/* utils/* graphics/*)

tarball: $(PDF) clean
	cd ..; rm latex-G7-32.tar.gz; tar -czf latex-G7-32.tar.gz $(PACK)
