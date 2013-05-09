TRACES_TOP = .


.PHONY: release release-zip release-bz2   \
	prepare-release clean-release clean-archive


MODULES_DIRS = src doc #conf

all: $(PREREQUISITES_DIRS)
	@echo "   Building all, in parallel over $(CORE_COUNT) core(s), from "$(PWD) #`basename $(PWD)`
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m &&  \
	$(MAKE) -s all-recurse -j $(CORE_COUNT) && cd .. ; else echo "     (directory $$m skipped)" ; \
	fi ) ; then exit 1; fi ; done


# No trace supervisor or graphical output wanted when running all tests from
# a root directory (batch mode vs interactive one):
CMD_LINE_OPT = "--batch"


include $(TRACES_TOP)/GNUmakesettings.inc
