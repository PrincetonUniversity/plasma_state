# Makefile to build all Plasma State libraries and executables
#

SHELL := /bin/bash


# ======================================================================
# Build options
# ======================================================================

# Default to static libraries.
# Use:
#
#     gmake MAKE_SO=1
#
# to build shared libraries in addition to static libraries.

MAKE_SO ?= 0

# Number of parallel jobs used INSIDE an individual library build.
NTASKS ?= 1


# ======================================================================
# Check external dependencies
# ======================================================================

ifeq (, $(shell command -v nc-config 2>/dev/null))
$(error Cannot find nc-config; is NetCDF installed?)
endif

ifndef EZCDF_HOME
$(error EZCDF_HOME is not set; is EZCDF installed?)
endif

ifndef PSPLINE_HOME
$(error PSPLINE_HOME is not set; is PSPLINE installed?)
endif

ifndef UFILES_ROOT
$(error UFILES_ROOT is not set; is UFILES installed?)
endif

ifndef MDSPLUS_DIR
$(warning MDSPLUS_DIR is not set; building without MDS+ support)
endif


# ======================================================================
# Libraries
#
# IMPORTANT:
#
# The order is significant for shared-library builds.
# A library must be built before another library that links against it.
#
# Known dependencies include:
#
#     ps_bloat       -> ps_smlib
#
#     xplasma2       -> ps_comput
#                      nscrunch
#                      ps_bloat
#
#     ps_xplasma2    -> xplasma2
#
#     plasma_state   -> plasma_state_kernel
#
#     xplasma_debug  -> libraries built earlier in this list
#
#     cstate_lib     -> plasma_state
#                      plasma_state_kernel
#                      xplasma2
#                      ps_comput
#                      ps_xplasma2
#                      ps_portlib
#                      xplasma_debug
#
# Therefore xplasma_debug MUST be built before cstate_lib.
#
# The loop in the "libs" target below is intentionally sequential so
# that this ordering is preserved even if the top-level make is invoked
# with -j.
# ======================================================================

LIBS := ps_physconst \
	ps_portlib \
	ps_comput \
	nscrunch \
	ps_smlib \
	ps_bloat \
	xplasma2 \
	old_xplasma \
	plasma_state_kernel \
	ps_xplasma2 \
	plasma_state \
	xplasma_debug \
	ccps_lib \
	cstate_lib \
	geqdsk_mds \
	mdescr


# ======================================================================
# Executables
# ======================================================================

EXES := ccps_test \
	cstate \
	convert_state \
	geq2ps \
	geqxpl \
	geqxpl2 \
	geq_plot \
	geq_test \
	grab_xplasma \
	plasma_state_test \
	plot_xplasma \
	ps2geq \
	ps2jso \
	ps2xplasma \
	ps_adas_test \
	scrdescur_test \
	test_descur \
	test_xplasma \
	update_state \
	xplasma2_test


# ======================================================================
# Phony targets
# ======================================================================

.PHONY: all \
	check \
	libs \
	exes \
	clean \
	realclean \
	clobber \
	depend


# ======================================================================
# Main build
#
# Do NOT use:
#
#     all: check libs exes
#
# because top-level "gmake -j" may execute these targets simultaneously.
#
# Instead enforce:
#
#     check
#       |
#     libs
#       |
#     exes
# ======================================================================

all: exes


# ======================================================================
# Directory setup and subdirectory checks
# ======================================================================

check:
	@mkdir -p $(PREFIX)/bin
	@mkdir -p $(PREFIX)/include
	@mkdir -p $(PREFIX)/lib
	@mkdir -p $(PREFIX)/mod
	@mkdir -p $(PREFIX)/obj

	@set -e; \
	for l in $(LIBS); do \
		mkdir -p $(PREFIX)/obj/$${l}; \
	done

	@set -e; \
	for e in $(EXES); do \
		mkdir -p $(PREFIX)/obj/$${e}; \
	done

	@set -e; \
	for l in $(LIBS); do \
		$(MAKE) -C $${l} MAKE_SO=$(MAKE_SO) check; \
	done

	@set -e; \
	for e in $(EXES); do \
		$(MAKE) -C $${e} MAKE_SO=$(MAKE_SO) check; \
	done


# ======================================================================
# Libraries
#
# The outer library loop is intentionally sequential.
#
# Shared libraries may depend on libraries appearing earlier in LIBS.
# Parallelism is allowed only INSIDE an individual library build.
# ======================================================================

libs: check
	@set -e; \
	for l in $(LIBS); do \
		echo "============================================================"; \
		echo "Building library: $${l}   MAKE_SO=$(MAKE_SO)"; \
		echo "============================================================"; \
		$(MAKE) -C $${l} MAKE_SO=$(MAKE_SO) -j $(NTASKS); \
	done


# ======================================================================
# Executables
#
# ALL libraries must be completely built before executable linking starts.
# ======================================================================

exes: libs
	@set -e; \
	for e in $(EXES); do \
		echo "============================================================"; \
		echo "Building executable: $${e}"; \
		echo "============================================================"; \
		$(MAKE) -C $${e} MAKE_SO=$(MAKE_SO); \
	done


# ======================================================================
# Clean
# ======================================================================

clean:
	@set -e; \
	for l in $(LIBS); do \
		$(MAKE) -C $${l} MAKE_SO=$(MAKE_SO) clean; \
	done

	@set -e; \
	for e in $(EXES); do \
		$(MAKE) -C $${e} MAKE_SO=$(MAKE_SO) clean; \
	done


# ======================================================================
# Real clean
# ======================================================================

realclean:
	@set -e; \
	for l in $(LIBS); do \
		$(MAKE) -C $${l} MAKE_SO=$(MAKE_SO) realclean; \
	done

	@set -e; \
	for e in $(EXES); do \
		$(MAKE) -C $${e} MAKE_SO=$(MAKE_SO) realclean; \
	done


# ======================================================================
# Remove complete installation/build tree
# ======================================================================

clobber:
	@echo "Removing $(PREFIX)"
	@rm -rf $(PREFIX)


# ======================================================================
# Dependencies
# ======================================================================

depend: check
	@set -e; \
	for l in $(LIBS); do \
		$(MAKE) -C $${l} MAKE_SO=$(MAKE_SO) depend; \
	done
