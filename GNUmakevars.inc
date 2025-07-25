# Prerequisite: TRACES_TOP must be set.


# To understand why taking into account such conditionals requires to specify
# them first, and in each layer, rather than at the end of the last one, refer
# to the section about conditionals at the top of the GNUmakevars.inc of Myriad.


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: production
	EXECUTION_TARGET = development

endif


ifeq ($(EXECUTION_TARGET),development)

	#$(info Execution target is development)

	ENABLE_TRACES = true
	ENABLE_DEBUG = true

else ifeq ($(EXECUTION_TARGET),production)

	#$(info Execution target is production)

	ENABLE_TRACES = false
	ENABLE_DEBUG = false

else

	$(error Invalid Traces execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif


# Enforce default values:


# To selectively force any (de)activation:
#ENABLE_TRACES := false

# Should already be set through EXECUTION_TARGET above:
ifndef ENABLE_TRACES
	ENABLE_TRACES := true
endif


# Tells whether traces will be activated for the next modules to be built.
# By default, they are enabled.
#
ifeq ($(ENABLE_TRACES),true)
	ENABLE_TRACES_OPT := -Dtracing_activated
else
	ENABLE_TRACES_OPT :=
endif


# Preformatting implies the use to the 'advanced_traces' type, but should lessen
# considerably the overall traces overhead, so this is worth it:
#
TRACES_OPT_FLAGS = -Dtraces_are_preformatted
#TRACES_OPT_FLAGS =


# Defining (optional) layer-specific flags, typically used by in-code
# conditional compilation primitives (see cond_utils for their management):

# Topics covered by flags:
#
# - registration: regarding the naming registration/look-up, notably of the
#   trace aggregator
#
# - types: regarding the overhead induced by the traces system (useful to ensure
# a good efficiency/compactness is met)
#
# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	#TRACES_DEBUG_FLAGS += -Dtraces_debug_registration
endif

# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	# To be left commented in general terms (as checking is expensive):
	#TRACES_CHECK_FLAGS += -Dtraces_check_types
endif

# As soon as Traces is used, no need to ellipse traces anymore, they can be
# fully preserved:
#
ERLANG_COMPILER_OPT_BASE += -Dtraces_version=\"$(TRACES_VERSION)\" \
							$(TRACES_OPT_FLAGS)                    \
							$(TRACES_DEBUG_FLAGS)                  \
							$(TRACES_CHECK_FLAGS)                  \
							-Dwooper_preserve_traces


# Useful to target for example the root of the current layer (e.g. to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	LAYER_TOP := $(TRACES_TOP)
endif


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME := Ceylan-Traces
endif


# PROJECT_SHORT_NAME should be defined on a per-project basis.
ifndef PROJECT_SHORT_NAME
	PROJECT_SHORT_NAME := traces
endif


# PROJECT_NAME_FOR_OTP should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_OTP
	PROJECT_NAME_FOR_OTP := $(PROJECT_SHORT_NAME)
endif


# The uniquely-defined version of this layer, for all uses (including rebar, hex
# and the code itself, see traces_utils:get_traces_version{_string,}/0):
#
TRACES_VERSION := 1.0.26



# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(TRACES_VERSION)
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP := $(TRACES_TOP)
endif



# For any quick, local, non release-packaged update thereof:
WOOPER_CHECKOUT_BUILD = $(LAYER_TOP)/_checkouts/wooper
WOOPER_SIBLING_BUILD = $(LAYER_TOP)/../wooper


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
TRACES_BEAM_DIR := $(TRACES_TOP)/src


# Defined so that full traces are kept in messages (as the user of Traces can
# properly consult longer ones):
#
# (note: only taken into account in an integrated build, i.e. when the WOOPER
# build happens whereas this GNUmakevars.inc has been included)
#
WOOPER_UNELLIPSED_TRACES_OPT= -Dwooper_unellipsed_traces


# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
TRACES_BEAM_DIRS := $(TRACES_BEAM_DIR) $(TRACES_TOP)/..


# No need for any INC += -I$(TRACES_SRC)/xxx


# For OTP releases and all:

TRACES_REBAR_BUILD_BASE = $(WOOPER_REBAR_BUILD_DIR)/lib/traces


# When building Traces in an OTP context from its usual (Git) root (e.g. with
# 'make rebar3-application'), the BEAMs of WOOPER (the layer just below it;
# taking care itself of Myriad) are to be found in the OTP build tree - rather
# than in their supposedly fully-built usual roots:
#
# (these information are useful only at compilation-time, in order to locate the
# needed parse transforms and their dependencies; at runtime, the OTP rules
# ensure that the relevant ebin directories are in the code path)
#
# Note that this implies that the BEAM files in all ebin directories should be
# up to date with the ones in the build trees, otherwise hard-to-debug
# discrepancies may happen (these OTP/rebar-related directories act as default
# catch-alls, should no prior directory correspond in the BEAM paths).
#
WOOPER_OTP_BEAM_DIR_FROM_USUAL = $(LAYER_TOP)/$(WOOPER_REBAR_BUILD_BASE)/ebin


# In an OTP/rebar3-style application layout (not speaking of sibling base
# directories here), at compilation time as well, modules compiled (directly or
# not) by the WOOPER parse transform will expect its modules to be located in:
#
# (a third place could be considered, in the _build tree of WOOPER itself, yet
# it would not provide any additional chance)
#
WOOPER_OTP_BEAM_DIR_FROM_OTP = $(LAYER_TOP)/../wooper/ebin/


# To locate WOOPER modules from all OTP contexts:
WOOPER_PARSE_TRANSFORMS_PZ_OPT = -pz $(WOOPER_OTP_BEAM_DIR_FROM_USUAL) \
								 -pz $(WOOPER_OTP_BEAM_DIR_FROM_OTP)


# We rely on Myriad and WOOPER as well:
BEAM_DIRS += $(TRACES_BEAM_DIRS)



ifndef VM_TEST_NAME
	VM_NAME := traces_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := traces_test
endif



ERLANG_COMPILER_OPT_BASE += $(ENABLE_TRACES_OPT)


# Strangely not taken into account by the compiler, hence commented-out here and
# replaced by a in-file -compile/1 directive (in class_TraceEmitter.hrl):
#
# (same for {test,app}_trace_disabled/{1,2} and al)
#
#ERLANG_COMPILER_OPT_BASE += "+{nowarn_unused_function,[{trace_disabled,1},{trace_disabled,2},{trace_disabled,3},{trace_disabled,4},{trace_disabled,5},{test_trace_disabled,1},{test_trace_disabled,2}]}"


# Not wanting to trigger WOOPER-specific settings (e.g. the modules bootstrapped
# for it, like wooper_info), hence we define the corresponding Make variables
# with an empty value (otherwise if using 'MY_VAR= ', MY_VAR would be deemed not
# defined); however of course most rules/variables still apply.


#ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES
#	ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES = $(empty)
#endif


ifndef PARSE_TRANSFORM_DIRS
	PARSE_TRANSFORM_DIRS = $(empty)
endif


#ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT
#  ERLANG_COMPILER_PARSE_TRANSFORM_OPT = $(empty)
#endif


ifndef BOOTSTRAP_MODULES
	BOOTSTRAP_MODULES = $(empty)
endif



## Parse-transform related section.

# At least currently, Traces does not need any parse-transform for itself: calls
# akin to '?info_fmt(Str,Values)' are translated to calls like
# 'class_TraceEmitter:send_*(Severity, State, text_utils:format( Message,
# FormatValues))', and thus Myriad's checks in text_utils:format/2 apply
# naturally.


# List here all the directories expected to contain parse transforms:
#
# (to be overridden by each upper layer)
#
#ifndef PARSE_TRANSFORM_DIRS
#
#   PARSE_TRANSFORM_DIRS = $(TRACES_TOP)/src
#
#endif


# Reusing for the (compilation of the) Traces parse transform the Myriad parse
# transform (as it does not rely on WOOPER), so no need to update
# ERLANG_COMPILER_OPT_FOR_PT.


# Variable defined to be re-usable by upper layers, so that compiling their own
# parse transforms can take advantage of the Traces one:
#
# (to find traces_parse_transform.beam, both the Traces 'src' and 'ebin'
# directories are specified: the former for our native build, the latter for a
# rebar3-based one)
#
#ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_TRACES = \
#   $(OVERALL_PA_OPTS)                           \
#   -pz $(TRACES_TOP)/ebin                       \
#   -pz $(TRACES_TOP)/src                        \
#   $(WOOPER_PARSE_TRANSFORMS_PZ_OPT)            \
#   '+{parse_transform,traces_parse_transform}'


# We conditionally defined this option, as upper layers may want to trigger
# their own parse transforms *instead* (probably that they will call directly,
# by themselves, at the Erlang level, this 'Traces' parse transform):
#
#ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT
#   ERLANG_COMPILER_PARSE_TRANSFORM_OPT = $(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_TRACES)
#endif


# Variable defined to be re-usable by upper layers, so that compiling their own
# parse transforms can take advantage of the Traces one:
#
#COMPILER_PARSE_TRANSFORM_FOR_TRACES_CLASSES_OPT = \
#   '+{parse_transform,traces_parse_transform}'



# To be available from a Makefile despite not being in the user's PATH:
KILL_TRACE_SUPERVISION_TOOL = $(TRACES_TOP)/priv/bin/kill-trace-supervision-tool.sh




# No $(LAYER_TOP) in includes, it would be misleading as edoc is run from the
# clone root, not from doc:
#
ifndef EDOC_OPTS
	EDOC_OPTS = '[{dir,"$(API_DOC_ROOT)"}, {def,{vsn,"$(PROJECT_VERSION)"}}, {preprocess,true}, {macros,[{$(PROJECT_EDOC_VERSION_KEY),"$(PROJECT_VERSION)"}]}, {includes,["..", "$(WOOPER_TOP)/..", "$(MYRIAD_TOP)/.."]}, {overview, "doc/overview.edoc"}]'
endif



TRACES_FILES = $(wildcard *.traces)

TRACES_PREVIOUS_FILES = $(wildcard *.traces.previous)



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
TRACES_PLT_FILE := $(TRACES_TOP)/Ceylan-Traces.plt


# This is the merged PLT of the level just below in the software stack.
# For Traces, it is the PLT of 'WOOPER':
#
ifndef PREDECESSOR_PLT_FILE
	PREDECESSOR_PLT_FILE = $(WOOPER_PLT_FILE)
endif


# We define the path to the WOOPER layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, Ceylan dependencies will be
# searched (in that order, the one enforced in otp_utils) in a local _checkouts
# child directory (if checkout directories are used and if the top-level package
# of interest is the current one), otherwise in sibling directories (i.e. whose
# build root is at the same level as this one).
#
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (e.g. myriad) as opposed to
# project/repository names (e.g. Ceylan-Myriad):
#
# First priority is an existing checkout directory:
ifneq ($(wildcard $(TRACES_TOP)/_checkouts/wooper/.),)
	WOOPER_TOP = $(TRACES_TOP)/_checkouts/wooper
else
	# Second, a sibling one is searched for:
	ifneq ($(wildcard $(TRACES_TOP)/../wooper/.),)
		WOOPER_TOP = $(TRACES_TOP)/../wooper
   else
		# Otherwise most probably in a 'rebar3 compile' context:
		ifneq ($(wildcard $(TRACES_TOP)/_build/default/lib/wooper/.),)
			WOOPER_TOP = $(TRACES_TOP)/_build/default/lib/wooper
		else
			FORCE_EVAL := $(error Unable to find the build root of Ceylan-WOOPER from '$(realpath .)'.)
		endif
	endif
endif

ifndef PREDECESSOR_LAYER_TOPS
	PREDECESSOR_LAYER_TOPS := $(WOOPER_TOP)
endif
