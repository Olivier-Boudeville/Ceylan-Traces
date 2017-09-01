TRACES_TOP = .


.PHONY: help help-intro help-traces                                   \
		all register-version-in-header register-traces list-beam-dirs \
		add-prerequisite-plts link-plt                                \
		info-traces


MODULES_DIRS = src doc #conf

# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true


# No trace supervisor or graphical output wanted when running all tests from a
# root directory (batch mode vs interactive one):
CMD_LINE_OPT = "--batch"



# Default target:
help: help-intro help-traces


help-intro:
	@echo " Following main make targets are available for package $(PACKAGE_NAME):"


help-traces:
	@cd $(WOOPER_TOP) && $(MAKE) -s help-wooper



register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ] ; then \
	echo "Error, no version file defined." 1>&2 ; exit 52 ; else \
	$(MAKE) register-traces ; fi


register-traces:
	@echo "-define( traces_version, \"$(TRACES_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(TRACES_BEAM_DIRS) ; do echo $$(readlink -f $$d) ; done


add-prerequisite-plts: link-plt


# As upper layers may rely on the 'traces' naming:
link-plt:
	@/bin/ln -s $(PLT_FILE) $(TRACES_PLT_FILE)


info-traces:
	@echo "ENABLE_TRACE_OPT = $(ENABLE_TRACE_OPT)"


include $(TRACES_TOP)/GNUmakesettings.inc
