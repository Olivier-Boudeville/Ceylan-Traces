# Here are gathered explicit (immediate, static) rules for Traces.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: TRACES_TOP must be set.


.PHONY: default-traces-rule clean clean-traces

# The first non-generic, therefore default, rule:
default-traces-rule: all



# Not wanting to define a default rule:
clean: clean-traces

clean-traces:
	-@/bin/rm -f $(TRACES_FILES)
