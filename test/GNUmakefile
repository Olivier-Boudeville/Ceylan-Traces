TRACES_TOP = ..

.PHONY: all test-local prepare-otp-test test-rotation clean clean-rotated-traces

# To allow local tests (e.g. trace_bridging_test.erl) to depend on Myriad ones:
BEAM_DIRS += $(MYRIAD_TOP)/test/utils

# So that traces_otp_application_test reads my-traces-test.config:
INTERNAL_OPTIONS += --config my-traces-test

ROTATION_TEST_TRACES :=trace_rotation_test.traces

# So that it remains the default target:
all:

test: test-local

test-local: prepare-otp-test


prepare-otp-test:
	@cd $(TRACES_TOP) && $(MAKE) -s create-app-file


# Best triggered interactively:
test-rotation: clean-rotated-traces
	-@/bin/rm -f $(ROTATION_TEST_TRACES)
	@$(MAKE) -s trace_rotation_run $$BATCH
	@$(TRACES_TOP)/priv/bin/unrotate-trace-files.sh
	@echo "Expecting the next test messages to be listed in-order in $(ROTATION_TEST_TRACES):"
	@grep 'Another test message:' $(ROTATION_TEST_TRACES)


clean: clean-rotated-traces

# XZ archives or in decrompressed form:
clean-rotated-traces:
	-@/bin/rm -f trace_rotation_test.traces.*


include $(TRACES_TOP)/GNUmakesettings.inc
