# ======================================================================
# Top-level Makefile for Plasma State
#
# Supported compiler toolchains:
#   Intel oneAPI 2023 (ifort, icc, icpc)
#   GCC 13.x          (gfortran, gcc, g++)
#
# Build options:
#
#   make                         Build static libraries and executables
#   make MAKE_SO=1               Build static + shared libraries
#   make MAKE_SO=1 DEBUG=1       Debug build
#   make MAKE_SO=1 NTASKS=4      Parallel compilation within libraries
#
#   make libs                    Build libraries only
#   make exes                    Build libraries and executables
#   make clean                   Clean object files
#   make realclean               Perform full subdirectory cleanup
#   make clobber                 Remove the complete build directory
#
# Compiler-specific options are provided by the environment and
# individual library Makefiles.
#
# IMPORTANT:
#   Libraries are built sequentially to preserve link dependencies.
#   Parallel compilation is allowed within individual libraries.
# ======================================================================

SHELL := /bin/bash


# ======================================================================
# Build options
# ======================================================================

MAKE_SO ?= 0
DEBUG   ?= 0
NTASKS  ?= 1


# ======================================================================
# Validate installation prefix
# ======================================================================

ifndef PREFIX
$(error PREFIX is not set; source the compiler environment first)
endif

ifeq ($(strip $(PREFIX)),)
$(error PREFIX is empty)
endif

ifeq ($(strip $(PREFIX)),/)
$(error PREFIX cannot be /)
endif


# ======================================================================
# Check external dependencies
# ======================================================================

ifeq (, $(shell command -v nc-config 2>/dev/null))
$(error Cannot find nc-config; NetCDF C is not available)
endif

ifeq (, $(shell command -v nf-config 2>/dev/null))
$(error Cannot find nf-config; NetCDF Fortran is not available)
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
$(warning MDSPLUS_DIR is not set; MDSplus-dependent targets may fail)
endif


# ======================================================================
# Libraries
#
# IMPORTANT:
#
# Library order is significant, especially for shared-library builds.
# Each library must be built before libraries that link against it.
#
# Known dependencies:
#
#   ps_bloat
#       -> ps_smlib
#
#   xplasma2
#       -> ps_comput
#       -> nscrunch
#       -> ps_bloat
#
#   ps_xplasma2
#       -> xplasma2
#
#   plasma_state
#       -> plasma_state_kernel
#
#   xplasma_debug
#       -> libraries built earlier
#
#   cstate_lib
#       -> plasma_state
#       -> plasma_state_kernel
#       -> xplasma2
#       -> ps_comput
#       -> ps_xplasma2
#       -> ps_portlib
#       -> xplasma_debug
#
# Therefore, xplasma_debug must be built before cstate_lib.
#
# The outer library loop is intentionally sequential.
# ======================================================================

LIBS := \
	ps_physconst \
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

EXES := \
	ccps_test \
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

.PHONY: \
	all \
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
# Build order:
#
#   check
#     |
#   libs
#     |
#   exes
#
# Do not use:
#
#   all: check libs exes
#
# because parallel top-level Make may execute these targets
# simultaneously.
# ======================================================================

.DEFAULT_GOAL := all

all: exes


# ======================================================================
# Directory setup and subdirectory checks
# ======================================================================

check:
	@echo "============================================================"
	@echo "Checking Plasma State build environment"
	@echo "============================================================"
	@echo "PREFIX  = $(PREFIX)"
	@echo "FC      = $(FC)"
	@echo "CC      = $(CC)"
	@echo "CXX     = $(CXX)"
	@echo "MAKE_SO = $(MAKE_SO)"
	@echo "DEBUG   = $(DEBUG)"
	@echo "NTASKS  = $(NTASKS)"
	@echo "============================================================"

	@mkdir -p "$(PREFIX)/bin"
	@mkdir -p "$(PREFIX)/include"
	@mkdir -p "$(PREFIX)/lib"
	@mkdir -p "$(PREFIX)/mod"
	@mkdir -p "$(PREFIX)/obj"

	@set -e; \
	for l in $(LIBS); do \
		mkdir -p "$(PREFIX)/obj/$${l}"; \
	done

	@set -e; \
	for e in $(EXES); do \
		mkdir -p "$(PREFIX)/obj/$${e}"; \
	done

	@set -e; \
	for l in $(LIBS); do \
		echo "Checking library: $${l}"; \
		$(MAKE) -C "$${l}" \
			MAKE_SO=$(MAKE_SO) \
			DEBUG=$(DEBUG) \
			check; \
	done

	@set -e; \
	for e in $(EXES); do \
		echo "Checking executable: $${e}"; \
		$(MAKE) -C "$${e}" \
			MAKE_SO=$(MAKE_SO) \
			DEBUG=$(DEBUG) \
			check; \
	done


# ======================================================================
# Libraries
#
# Shared libraries may depend on libraries appearing earlier in LIBS.
#
# The outer loop is sequential.
# Parallel compilation is allowed inside each library.
# ======================================================================

libs: check
	@set -e; \
	for l in $(LIBS); do \
		echo "============================================================"; \
		echo "Building library: $${l}"; \
		echo "MAKE_SO=$(MAKE_SO) DEBUG=$(DEBUG) NTASKS=$(NTASKS)"; \
		echo "============================================================"; \
		$(MAKE) -C "$${l}" \
			MAKE_SO=$(MAKE_SO) \
			DEBUG=$(DEBUG) \
			-j $(NTASKS); \
	done


# ======================================================================
# Executables
#
# All libraries must finish building before executable linking starts.
# ======================================================================

exes: libs
	@set -e; \
	for e in $(EXES); do \
		echo "============================================================"; \
		echo "Building executable: $${e}"; \
		echo "MAKE_SO=$(MAKE_SO) DEBUG=$(DEBUG)"; \
		echo "============================================================"; \
		$(MAKE) -C "$${e}" \
			MAKE_SO=$(MAKE_SO) \
			DEBUG=$(DEBUG); \
	done


# ======================================================================
# Clean
#
# Removes intermediate build files according to each subdirectory's
# clean target.
# ======================================================================

clean:
	@set -e; \
	for l in $(LIBS); do \
		echo "Cleaning library: $${l}"; \
		$(MAKE) -C "$${l}" \
			MAKE_SO=$(MAKE_SO) \
			DEBUG=$(DEBUG) \
			clean; \
	done

	@set -e; \
	for e in $(EXES); do \
		echo "Cleaning executable: $${e}"; \
		$(MAKE) -C "$${e}" \
			MAKE_SO=$(MAKE_SO) \
			DEBUG=$(DEBUG) \
			clean; \
	done


# ======================================================================
# Real clean
#
# Performs the realclean target in every library and executable
# directory.
# ======================================================================

realclean:
	@set -e; \
	for l in $(LIBS); do \
		echo "Realclean library: $${l}"; \
		$(MAKE) -C "$${l}" \
			MAKE_SO=$(MAKE_SO) \
			DEBUG=$(DEBUG) \
			realclean; \
	done

	@set -e; \
	for e in $(EXES); do \
		echo "Realclean executable: $${e}"; \
		$(MAKE) -C "$${e}" \
			MAKE_SO=$(MAKE_SO) \
			DEBUG=$(DEBUG) \
			realclean; \
	done


# ======================================================================
# Clobber
#
# Removes the complete build/install tree.
#
# Safety checks prevent removal of an empty path or filesystem root.
# ======================================================================

clobber:
	@set -e; \
	prefix="$(PREFIX)"; \
	if [[ -z "$${prefix}" || "$${prefix}" == "/" ]]; then \
		echo "ERROR: Unsafe PREFIX='$${prefix}'"; \
		exit 1; \
	fi; \
	if [[ "$${prefix}" != /* ]]; then \
		echo "ERROR: PREFIX must be an absolute path"; \
		exit 1; \
	fi; \
	echo "============================================================"; \
	echo "Removing Plasma State build directory:"; \
	echo "  $${prefix}"; \
	echo "============================================================"; \
	rm -rf -- "$${prefix}"


# ======================================================================
# Dependencies
#
# Generate dependencies in library directories.
# ======================================================================

depend: check
	@set -e; \
	for l in $(LIBS); do \
		echo "============================================================"; \
		echo "Generating dependencies: $${l}"; \
		echo "============================================================"; \
		$(MAKE) -C "$${l}" \
			MAKE_SO=$(MAKE_SO) \
			DEBUG=$(DEBUG) \
			depend; \
	done
