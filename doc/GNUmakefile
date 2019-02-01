TRACES_TOP = ..


DOC_BASE_NAME := Ceylan-Traces-Layer-technical-manual-english

DOC_PDF_NAME := $(DOC_BASE_NAME).pdf

OVERALL_DOCUMENT_TARGET = $(DOC_BASE_NAME).rst

DOC_GENERATED_FILES = $(DOC_PDF_NAME) $(DOC_BASE_NAME).html

DOC_FILES = $(DOC_GENERATED_FILES)


# The current, operational version of Traces:
CURRENT_VERSION := 1.0

CURRENT_TRACES_DOC := $(DOC_PDF_NAME)-$(CURRENT_VERSION).pdf

PROJECT_CSS := pygments-default.css,traces.css


WEB_CONTENT = *.html *.css *.png *.pdf

# Read from the user's environment:
GITHUB_PAGES_BASE := $(shell basename "$(TRACES_WEB_MIRROR_BRANCH)")


OVERALL_DOCUMENT_SOURCE = $(DOC_BASE_NAME).rst


.PHONY: all doc full-doc clone-mirror                    \
		export-doc export-to-official export-to-mirror   \
		info-web clean clean-doc test uml-diagram-test



# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' to generate the manual of the 'Traces' layer."


doc:


full-doc: $(DOC_BASE_NAME).pdf $(DOC_BASE_NAME).html


# Creates a separate repository for the GitHub pages branch:
# (please then remove all initial content of that branch)
#
clone-mirror:
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/Ceylan-Traces.git $(GITHUB_PAGES_BASE) && cd $(GITHUB_PAGES_BASE) && git checkout -b gh-pages


export-doc: clean-doc full-doc export-to-official export-to-mirror


export-to-official:
	@echo "   Exporting Traces documentation to official website"
	@/bin/scp $(SP) $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/Traces/


export-to-mirror:
	@echo "   Exporting Traces documentation to mirror website in $(TRACES_WEB_MIRROR_BRANCH)"
	@/bin/cp -f $(WEB_CONTENT) $(TRACES_WEB_MIRROR_BRANCH) && cd $(TRACES_WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "Traces doc updated." && git push


clean: clean-doc

clean-doc:
	-@/bin/rm -f *.aux *.log *.maf *.mtc* *.stc* *.tex *.toc \
	$(CURRENT_TRACES_DOC) $(DOC_GENERATED_FILES)


info-web:
	@echo "WEB_CONTENT = $(WEB_CONTENT)"
	@echo "TRACES_WEB_MIRROR_BRANCH = $(TRACES_WEB_MIRROR_BRANCH)"
	@echo "GITHUB_PAGES_BASE = $(GITHUB_PAGES_BASE)"


DOCUTILS_TOP = .

include $(TRACES_TOP)/GNUmakesettings.inc
