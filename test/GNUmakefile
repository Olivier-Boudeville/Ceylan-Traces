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

test-local: prepare-otp-test test-rotation


prepare-otp-test:
	@cd $(TRACES_TOP) && $(MAKE) -s create-app-file


test-rotation: $(ROTATION_TEST_TRACES)
	@echo "Expecting the next test messages to be listed in-order:"
	@grep 'Another test message:' $(ROTATION_TEST_TRACES)


clean: clean-rotated-traces

clean-rotated-traces:
	-@/bin/rm -f trace_rotation_test.traces.*.*.xz


include $(TRACES_TOP)/GNUmakesettings.inc
