# Here are gathered explicit (immediate, static) rules for Traces.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: TRACES_TOP must be set.


.PHONY: default-traces-rule clean clean-traces
		rebar3-local-update rebar3-local-update-for-traces

# The first non-generic, therefore default, rule:
default-traces-rule: all


rebar3-local-update: rebar3-local-update-for-traces

rebar3-local-update-for-traces: rebar3-local-update-for-wooper
	@echo "  Updating WOOPER from local sibling build tree $(WOOPER_SIBLING_BUILD)"
	@cd $(WOOPER_SIBLING_BUILD) && $(MAKE) -s rebar3-compile
	@mkdir -p $(WOOPER_LOCAL_EBIN)
	@/bin/cp -f $(WOOPER_SIBLING_BUILD)/_build/default/lib/wooper/ebin/* $(WOOPER_LOCAL_EBIN)



# Not wanting to define a default rule:
clean: clean-traces

clean-traces:
	-@/bin/rm -f $(TRACES_FILES)
