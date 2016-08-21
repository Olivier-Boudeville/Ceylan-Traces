TRACES_TOP = .


.PHONY: all register-version-in-header register-traces info-traces


MODULES_DIRS = src doc #conf

# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true


# No trace supervisor or graphical output wanted when running all tests from a
# root directory (batch mode vs interactive one):
CMD_LINE_OPT = "--batch"


# Default target:
all:


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ] ; then \
	echo "Error, no version file defined." 1>&2 ; exit 52 ; else \
	$(MAKE) register-traces ; fi


register-traces:
	@echo "-define( traces_version, \"$(TRACES_VERSION)\" )." >> $(VERSION_FILE)


info-traces:
	@echo "ENABLE_TRACE_OPT = $(ENABLE_TRACE_OPT)"


include $(TRACES_TOP)/GNUmakesettings.inc
