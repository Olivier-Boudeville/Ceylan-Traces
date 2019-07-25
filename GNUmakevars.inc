# Prerequisite: TRACES_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Ceylan-Traces
endif


# REBAR3_PROJECT_NAME should be defined on a per-project basis.
ifndef REBAR3_PROJECT_NAME
	REBAR3_PROJECT_NAME := traces
endif


TRACES_VERSION := 0.3.0

# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = $(TRACES_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = Ceylan-Traces
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(TRACES_TOP)
endif


TRACES_RELEASE_BASE := Traces-$(PROJECT_VERSION)

TRACES_RELEASE_ARCHIVE_ZIP := $(TRACES_RELEASE_BASE).zip
TRACES_RELEASE_ARCHIVE_BZ2 := $(TRACES_RELEASE_BASE).tar.bz2
TRACES_RELEASE_ARCHIVE_XZ  := $(TRACES_RELEASE_BASE).tar.xz


# Source section.
TRACES_SRC := $(TRACES_TOP)/src

# BEAM path section.
TRACES_BEAM := $(TRACES_TOP)

# Include path section.
TRACES_INC = -I$(TRACES_SRC)


# In an OTP/rebar3-style application layout, at compilation time, libraries
# making use of WOOPER will expect its includes to be located in:
#
# (Myriad already taken care of, at the WOOPER level)
#
WOOPER_OTP_INC := -I$(TRACES_TOP)/../wooper/include/

INC += $(TRACES_INC) $(WOOPER_OTP_INC)


DOC_ROOT = $(TRACES_TOP)/../../../doc



ifndef VM_TEST_NAME

VM_NAME := traces_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := traces_test

endif



# Overall settings section.


ifndef EXECUTION_TARGET

  # Other possible value: production
  EXECUTION_TARGET=development

endif


ifeq ($(EXECUTION_TARGET),development)

  #$(info Execution target is development)

  ENABLE_TRACES := true


else ifeq ($(EXECUTION_TARGET),production)

  #$(info Execution target is production)

  ENABLE_TRACES := false

else

  $(error Invalid execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif


# Enforce default values:


ifndef ENABLE_TRACES

	ENABLE_TRACES := true

endif


# To selectively force any (de)activation:
#ENABLE_TRACES := false


# Tells whether traces will be activated for the next modules to be built.
# By default, they are enabled.
ifeq ($(ENABLE_TRACES),true)

	ENABLE_TRACE_OPT := -Dtracing_activated

else

	ENABLE_TRACE_OPT :=

endif



BEAM_DIRS += $(TRACES_BEAM_DIRS)


# So that they can be fetched from outside
# (see the 'list-beam-dirs' target)
#
TRACES_BEAM_DIRS = $(TRACES_BEAM)/src


ERLANG_COMPILER_OPT_BASE += $(ENABLE_TRACE_OPT)


# Strangely not taken into account by the compiler, hence commented-out here and
# replaced by a in-file -compile/1 directive (in class_TraceEmitter.hrl):
#
# (same for {test,app}_trace_disabled/{1,2} and al)
#
#ERLANG_COMPILER_OPT_BASE += "+{nowarn_unused_function,[{trace_disabled,1},{trace_disabled,2},{trace_disabled,3},{trace_disabled,4},{trace_disabled,5},{test_trace_disabled,1},{test_trace_disabled,2}]}"


TRACES_FILES = $(wildcard *.traces)


AUX_GENERATED_FILES  := $(AUX_GENERATED_FILES)  \
	$(patsubst %.traces,%.aux,$(TRACES_FILES))

TEX_GENERATED_FILES  := $(TEX_GENERATED_FILES)  \
	$(patsubst %.traces,%.tex,$(TRACES_FILES))

OUT_GENERATED_FILES  := $(OUT_GENERATED_FILES)  \
	$(patsubst %.traces,%.out,$(TRACES_FILES))

LOG_GENERATED_FILES  := $(LOG_GENERATED_FILES)  \
	$(patsubst %.traces,%.log,$(TRACES_FILES))

HTML_GENERATED_FILES := $(HTML_GENERATED_FILES) \
	$(patsubst %.traces,%.html,$(TRACES_FILES))

PDF_GENERATED_FILES  := $(PDF_GENERATED_FILES)  \
	$(patsubst %.traces,%.pdf,$(TRACES_FILES))


TRACES_GENERATED_FILES := $(AUX_GENERATED_FILES) $(TEX_GENERATED_FILES)    \
	$(OUT_GENERATED_FILES) $(LOG_GENERATED_FILES) $(HTML_GENERATED_FILES)  \
	$(PDF_GENERATED_FILES) $(GENERATOR_LOG_FILE) Listener-*.traces


# Used by the clean-override target in Myriad:
FILES_TO_CLEAN += $(TRACES_FILES) $(TRACES_GENERATED_FILES)


# For later reuse in the next layer of the software stack:
TRACES_PLT_FILE := $(TRACES_TOP)/traces.plt


# This is the merged PLT of the level just below in the software stack.
# For Traces, it is the PLT of 'WOOPER':
ifndef PREDECESSOR_PLT
   PREDECESSOR_PLT = $(WOOPER_PLT_FILE)
endif


# We define the path to the WOOPER layer:
#
# (it may be a symbolic link pointing to the actual WOOPER package to be used,
# which itself may be either a 'Ceylan-WOOPER' directory or a 'wooper' one)
#
ifneq ($(wildcard $(TRACES_TOP)/../wooper),)

	WOOPER_TOP = $(TRACES_TOP)/../wooper

else

	# Default:
	WOOPER_TOP = $(TRACES_TOP)/../Ceylan-WOOPER

endif
