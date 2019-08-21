# Prerequisite: TRACES_TOP must be set.


# Useful to target for example the root of the current layer (ex: to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	   LAYER_TOP := $(TRACES_TOP)
endif


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Ceylan-Traces
endif


# REBAR3_PROJECT_NAME should be defined on a per-project basis.
ifndef REBAR3_PROJECT_NAME
	REBAR3_PROJECT_NAME := traces
endif


# The uniquely-defined version of this layer, for all uses including rebar:
TRACES_VERSION := 1.0.3


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



# For any quick, local, non-packaged update thereof:
WOOPER_SIBLING_BUILD = $(LAYER_TOP)/../Ceylan-WOOPER


# The OTP tree of the WOOPER application within a local OTP _build tree:
WOOPER_LOCAL_APP := $(LAYER_TOP)/_build/default/lib/wooper

# Where WOOPER BEAMs are to lie when used as an OTP application:
WOOPER_LOCAL_EBIN := $(WOOPER_LOCAL_APP)/ebin/



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


# When building Traces in an OTP context from its usual, GIT root (ex: with 'make
# rebar3-application'), the BEAMs of WOOPER are to be found in
# the OTP build tree - rather than in a supposedly fully-built usual root for
# WOOPER:
#
WOOPER_OTP_BEAM_DIR_FROM_USUAL = $(LAYER_TOP)/$(WOOPER_REBAR_BUILD_BASE)/ebin


# In an OTP/rebar3-style application layout, at compilation time, modules
# compiled (directly or not) by the WOOPER parse transform will expect its
# module to be located in:
#
WOOPER_OTP_BEAM_DIR_FROM_OTP = $(LAYER_TOP)/../wooper/ebin/



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


# We rely on Myriad and WOOPER as well:
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


# Not wanting to trigger WOOPER-specific settings (ex: the modules bootstrapped
# for it, like wooper_info), hence we define the corresponding Make variables
# with an empty value (otherwise if using 'MY_VAR= ', MY_VAR would be deemed not
# defined); however of course most rules/variables still apply.


#ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES
#
#	ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES = $(empty)
#
#endif


ifndef PARSE_TRANSFORM_DIRS

	PARSE_TRANSFORM_DIRS = $(empty)

endif


#ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT
#
#	ERLANG_COMPILER_PARSE_TRANSFORM_OPT = $(empty)
#
#endif


ifndef BOOTSTRAP_MODULES

	BOOTSTRAP_MODULES = $(empty)

endif



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
