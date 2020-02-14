# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SOURCEDIR     = .
BUILDDIR      = _build

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
# |:here:|

SNIPPETS = snippets.pl
SNC_ = $(SNIPPETS)
SNC = $(SNC_) --cat
SNR_ = $(SNIPPETS) --replace --process
SNR = $(SNR_) --cat

# |:here:| generic rules

_static/%.png: %.msc
	mscgen -T png -F FreeSans -o $@ $<

%.png: %.msc
	mscgen -T png -F FreeSans -o $@ $<

_static/%.png: %.eps
	convert $(CONVERT_IN_OPTS) $< $(CONVERT_OUT_OPTS) $@

%.png: %.eps
	convert $(CONVERT_IN_OPTS) $< $(CONVERT_OUT_OPTS) $@

_static/%.png: %.dot
	dot -T png -o $@ $<

%.png: %.dot
	dot -T png -o $@ $<

_static/%.png: %.svg
	convert $< $@

%.png: %.svg
	convert $< $@

_static/%.png: %.puml
	plantuml $<
	mv $(*F).png _static/

%.png: %.puml
	plantuml $<

_static/%.svg: %.puml
	plantuml -tsvg $<
	mv $(*F).svg _static/

%.svg: %.puml
	plantuml -tsvg $<

_static/%.eps: %.iwk
	$(tooldir)/iwps.pl $(IWPS_OPTS) $<
	$(PS2EPSI) $<.ps
	rm -f $<.ps
	mv $<.epsi $@

%.eps: %.iwk
	$(tooldir)/iwps.pl $(IWPS_OPTS) $<
	$(PS2EPSI) $<.ps
	rm -f $<.ps
	mv $<.epsi $@

_static/%.svg: %.html
	webvector $< $@ svg

%.svg: %.html
	webvector $< $@ svg

_static/%.pdf: _static/%.svg
	convert $< $@

_static/%.pdf: %.svg
	convert $< $@

%.pdf: %.svg
	convert $< $@

_static/%.txt: %.xlsx
	@(						\
	echo '.. table:: $<';				\
	echo '   :class: longtable';			\
	echo;						\
	xlsx-dump.sh --rest $< | sed 's,^,   ,';	\
	) >$@ || rm -f $@
	test -s $@ || ( rm -f $@; exit 1 );

# |:here:| chapters

PRESENTATION = 0
MAIN_DOC = README.txt
CLEANFILES += index.rst.auto overview.rst.auto

# CHAPTERS += ../README-topic.txt

# |:chapter:|

#CHAPTERS_INC += ../README-stations.txt
#CHAPTERS_SRC += ../README-inst-tagideasy-station.src
CHAPTERS_GEN := $(patsubst %.src, %.txt, $(CHAPTERS_SRC))
CHAPTERS_RC := $(patsubst %.src, %.rc, $(CHAPTERS_SRC))

CHAPTERS_RST := $(patsubst ../README-%.txt, %.rst.auto, $(CHAPTERS_GEN) $(CHAPTERS))
CLEANFILES += $(CHAPTERS_RST)

../%.txt: ../%.src $(CHAPTERS_INC)
	sphinx_doc_snip.py --force --format '$@' --config $(basename $<).rc $(patsubst %, --include %, $(CHAPTERS_INC)) '$<'

# ../README-inst-tagideasy-station.txt: ../README-inst-tagideasy-station.rc

ifeq ($(PRESENTATION),1)

index: index.rst.auto
	@printf ""

index.rst.auto: ../$(MAIN_DOC) doc_defs.snip chapter-header.snip chapter-footer.snip
	@(							\
	rm -f $@;						\
	export SNIPS_PATH="..:.:$SNIPS_PATH";			\
	echo "$(SNR_) --key doc_slides --cat $< >$@";		\
	$(SNR_) --key doc_slides --cat  $< >$@ || rm -f $@;	\
	( test -s $@ && chmod 444 $@ ) || ( rm -f $@; exit 1 );	\
	)
else

index: index.rst.auto overview.rst.auto
	@printf ""

index.rst.auto: ../$(MAIN_DOC) doc_defs.snip index-header.snip index-footer.snip
	@(							\
	rm -f $@;						\
	export SNIPS_PATH="..:.:$SNIPS_PATH";			\
	echo "$(SNR_) --key doc_index --cat $< >$@";		\
	$(SNR_) --key doc_index --cat  $< >$@ || rm -f $@;	\
	( test -s $@ && chmod 444 $@ ) || ( rm -f $@; exit 1 );	\
	)

overview.rst.auto: ../$(MAIN_DOC) doc_defs.snip overview-header.snip overview-footer.snip
	@(							\
	rm -f $@;						\
	export SNIPS_PATH="..:.:$SNIPS_PATH";			\
	echo "$(SNR_) --key doc_overview --cat $< >$@";		\
	$(SNR_) --key doc_overview --cat  $< >$@ || rm -f $@;	\
	( test -s $@ && chmod 444 $@ ) || ( rm -f $@; exit 1 );	\
	)

endif

%.rst.auto: ../README-%.txt doc_defs.snip chapter-header.snip chapter-footer.snip
	@(							\
	rm -f $@;						\
	export SNIPS_PATH="..:.:$SNIPS_PATH";			\
	echo "$(SNR_) --key doc_chapter --cat $< >$@";		\
	$(SNR_) --key doc_chapter --cat $< >$@ || rm -f $@;	\
	( test -s $@ && chmod 444 $@ ) || ( rm -f $@; exit 1 );	\
	)

# |:here:| images

# |:sec:| source image in doc/ directory
#IMAGES_HTML += @example@.html
#IMAGES_HTML_SVG += @example@.html
#IMAGES_HTML_PDF += @example@.html

#IMAGES_PUML_PNG += @example@.puml
#IMAGES_PUML_SVG += @example@.puml
#IMAGES_PUML_PDF += @example@.puml

# |:sec:| generated source images in doc/directory
#IMAGES_PUML_GEN_SRC += @example-src@.puml
#IMAGES_PUML_PNG_GEN += @example@.puml
#IMAGES_PUML_SVG_GEN += @example@.puml
#IMAGES_PUML_PDF_GEN += @example@.puml
#@example@.puml: @example-src@.puml
#	grep -v "/'s.'/" $< >$@ || rm -f $@

# |:sec:| source image in _static/ directory (only for special cases, e.g. where the PDF won't build)
#IMAGES_STATIC_PUML_PNG += _static/@example@.puml
#IMAGES_STATIC_PUML_SVG += _static/@example@.puml
#IMAGES_STATIC_PUML_PDF += _static/@example@.puml

# |:sec:| generated source images in _static/ directory (only for special cases, e.g. where the PDF won't build)
#IMAGES_STATIC_PUML_GEN_SRC += _static/@example-src@.puml
#IMAGES_STATIC_PUML_PNG_GEN += _static/@example@.puml
#IMAGES_STATIC_PUML_SVG_GEN += _static/@example@.puml
#IMAGES_STATIC_PUML_PDF_GEN += _static/@example@.puml
#_static/@example@.puml: _static/@example-src@.puml
#	grep -v "/'s.'/" $< >$@ || rm -f $@

IMAGES_HTML_SVG += $(IMAGES_HTML)
IMAGES_HTML_PDF += $(IMAGES_HTML)

IMAGES += $(IMAGES_HTML_SVG)
IMAGES += $(IMAGES_HTML_PDF)
IMAGES += $(IMAGES_PUML_PNG)
IMAGES += $(IMAGES_PUML_SVG)
IMAGES += $(IMAGES_PUML_PDF)
IMAGES += $(IMAGES_PUML_GEN_SRC)
IMAGES += $(IMAGES_STATIC_PUML_SVG)
IMAGES += $(IMAGES_STATIC_PUML_PNG)
IMAGES += $(IMAGES_STATIC_PUML_PDF)
IMAGES += $(IMAGES_STATIC_PUML_GEN_SRC)

IMAGES_GEN += $(IMAGES_PUML_PNG_GEN)
IMAGES_GEN += $(IMAGES_PUML_SVG_GEN)
IMAGES_GEN += $(IMAGES_PUML_PDF_GEN)
IMAGES_GEN += $(IMAGES_STATIC_PUML_PNG_GEN)
IMAGES_GEN += $(IMAGES_STATIC_PUML_SVG_GEN)
IMAGES_GEN += $(IMAGES_STATIC_PUML_PDF_GEN)

IMAGES_STATIC_PNG := $(patsubst %.puml, _static/%.png, $(IMAGES_PUML_PNG)) $(patsubst %.puml, _static/%.png, $(IMAGES_PUML_PNG_GEN)) $(patsubst %.puml, %.png, $(IMAGES_STATIC_PUML_PNG)) $(patsubst %.puml, %.png, $(IMAGES_STATIC_PUML_PNG_GEN))
IMAGES_STATIC_SVG := $(patsubst %.html, _static/%.svg, $(IMAGES_HTML_SVG)) $(patsubst %.puml, _static/%.svg, $(IMAGES_PUML_SVG_GEN)) $(patsubst %.puml, %.svg, $(IMAGES_STATIC_PUML_SVG)) $(patsubst %.puml, %.svg, $(IMAGES_STATIC_PUML_SVG_GEN))
IMAGES_STATIC_PDF := $(patsubst %.html, _static/%.pdf, $(IMAGES_HTML_PDF)) $(patsubst %.puml, _static/%.pdf, $(IMAGES_PUML_PDF_GEN)) $(patsubst %.puml, %.pdf, $(IMAGES_STATIC_PUML_PDF)) $(patsubst %.puml, %.pdf, $(IMAGES_STATIC_PUML_PDF_GEN))

IMAGES_STATIC += $(IMAGES_STATIC_PNG)
IMAGES_STATIC += $(IMAGES_STATIC_SVG)
IMAGES_STATIC += $(IMAGES_STATIC_PDF)

# |:here:| tables

# TABLES += matrix.xlsx
TABLES_STATIC := $(patsubst %.xlsx, _static/%.txt, $(TABLES))

# |:here:| diagrams

UML_EXTRACT = line_diversion.py

# # |:info:| the UML SOURCE block is needed once per source script:
# fillme_SOURCE   = ../bin/fillme.sh
# fillme_NAME     = $(lastword $(subst /, ,$(fillme_SOURCE)))
# fillme_EXT      = $(lastword $(subst ., ,$(fillme_NAME)))
# fillme_BASE     = $(patsubst %.$(fillme_EXT),%,$(fillme_NAME))
# DIAGRAM_SOURCES += $(fillme_SOURCE)

# # |:info:| the UML OUTPUT block is needed once per diagram:
# fillme_a0_MARKER   = a0
# fillme_a0_SFX      = -$(fillme_a0_MARKER)
# fillme_a0_MATCH    = '^$(fillme_a0_MARKER)'
# fillme_a0_PUML     = _static/$(fillme_BASE)$(fillme_a0_SFX).puml
# DIAGRAMS           += $(fillme_a0_PUML)
# CLEANFILES         += $(fillme_a0_PUML)
# $(fillme_a0_PUML): $(fillme_SOURCE)
# 	$(UML_EXTRACT) --match $(fillme_a0_MATCH) $< >$@ || ( rm -f $@; exit 1 )
# 	test -s $@ || ( rm -f $@; exit 1 )

# |:here:| glossary/abbrevs

SPHINX_DOC_GLOSSARY = sphinx_doc_glossary.py --enable gl,gt,-ga,ab,ad

GLOSSARY_SOURCES = glossary.src

GLOSSARY_FILES += abbrevs.inc
GLOSSARY_FILES += abbrev_defs.inc
GLOSSARY_FILES += glossary.inc
CLEANFILES += $(GLOSSARY_FILES)

abbrevs.inc: $(GLOSSARY_SOURCES)
	$(SPHINX_DOC_GLOSSARY) $^

abbrev_defs.inc: $(GLOSSARY_SOURCES)
	$(SPHINX_DOC_GLOSSARY) $^

glossary.inc: $(GLOSSARY_SOURCES)
	$(SPHINX_DOC_GLOSSARY) $^

# |:here:| local targets

pdf: latexpdf
	@printf ""

clean: clean-local

clean-local:
	test -z '$(CLEANFILES)' || rm -f $(CLEANFILES)

chapters: $(CHAPTERS_GEN) $(CHAPTERS_RST)
	@printf ""

images: $(IMAGES_GEN) $(IMAGES_STATIC)
	@printf ""

tables: $(TABLES_STATIC)
	@printf ""

diagrams: $(DIAGRAMS)
	@printf ""

glossaries: $(GLOSSARY_FILES)
	@printf ""

LINK_DIR =
LINK_FILES =
LINK_CHAPTERS =

# LINK_DIR = doc
# LINK_FILES += _static
# LINK_FILES += bibliography.inc
# LINK_FILES += doc_defs.inc
# LINK_FILES += doc_defs.snip
# LINK_FILES += doc_defs_global.snip
# LINK_FILES += doc_defs_combined.inc
# LINK_FILES += doc_defs_standalone.inc
# LINK_FILES += docutils.conf
# LINK_FILES += glossary.src
# LINK_FILES += module.rst_template
# LINK_CHAPTERS += appendix
# |:links:|

links:
	( test -z '$(LINK_DIR)' || ( cd ../$(LINK_DIR) && make all-local ) )
	@(list='$(LINK_CHAPTERS)'; test -z "$$list" ||		\
	for link in $$list; do					\
	  test -r $$link.rst.auto && continue;			\
	  echo "ln -s ../$(LINK_DIR)/$$link.rst.auto .";	\
	  ln -s ../$(LINK_DIR)/$$link.rst.auto .;		\
	done							\
	)
	@(list='$(LINK_FILES)'; test -z "$$list" ||	\
	for link in $$list; do				\
	  test -L $$link && continue;			\
	  echo "ln -s ../$(LINK_DIR)/$$link .";		\
	  rm -rf $$link;				\
	  ln -s ../$(LINK_DIR)/$$link .;		\
	done						\
	)

all-local: links index chapters images tables diagrams glossaries
	@printf ""

html dirhtml singlehtml pickle json htmlhelp qthelp devhelp epub latex latexpdf text man changes linkcheck doctest gettext: all-local

# existing files must have a more specific rule than the default
../$(MAIN_DOC) \
doc_defs.snip \
standalone-header.snip standalone-footer.snip \
index-header.snip index-footer.snip \
overview-header.snip overview-footer.snip \
chapter-header.snip chapter-footer.snip \
$(CHAPTERS_SRC) $(CHAPTERS_RC) $(CHAPTERS) \
$(IMAGES) $(TABLES) $(DIAGRAM_SOURCES) \
$(GLOSSARY_SOURCES):
	@printf ""

# :ide: COMPILE: Standard
# . (let ((args "")) (compile (concat "make -k " args)))

# :ide: COMPILE: all-local
# . (let ((args "all-local")) (compile (concat "make -k " args)))

# :ide: COMPILE: epub
# . (let ((args "epub")) (compile (concat "make -k " args)))

# :ide: COMPILE: latexpdf
# . (let ((args "latexpdf")) (compile (concat "make -k " args)))

# :ide: COMPILE: html
# . (let ((args "html")) (compile (concat "make -k " args)))
