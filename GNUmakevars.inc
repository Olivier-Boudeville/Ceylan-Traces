# Prerequisite: TRACES_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Traces
endif


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = 0.3
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = traces
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(TRACES_TOP)
endif


TRACES_RELEASE_BASE := Traces-$(PROJECT_VERSION)

TRACES_RELEASE_ARCHIVE_ZIP := $(TRACES_RELEASE_BASE).zip
TRACES_RELEASE_ARCHIVE_BZ2 := $(TRACES_RELEASE_BASE).tar.bz2


# Source section.
TRACES_SRC := $(TRACES_TOP)/src

# BEAM path section.
TRACES_BEAM := $(TRACES_TOP)/src

# Include path section.
TRACES_INC = -I$(TRACES_SRC)


DOC_ROOT = $(TRACES_TOP)/../../../doc


INC += $(TRACES_INC)



ifndef VM_TEST_NAME

VM_NAME := traces_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := traces_test

endif


BEAM_DIRS += $(TRACES_BEAM)

ERLANG_COMPILER_OPT += "+{nowarn_unused_function,[{trace_disabled,1},{trace_disabled,2},{trace_disabled,3},{trace_disabled,4},{trace_disabled,5},{test_trace_disabled,1},{test_trace_disabled,2} ]}"


# WOOPER_TOP may be a symbolic link pointing to the actual WOOPER package to be
# used.
#
WOOPER_TOP = $(TRACES_TOP)/../wooper


