# ---------------------------------------------------------------------------
# Makefile include file for PPPL Flux
# GCC 13.2.0
#
# Plasma State build configuration
# ---------------------------------------------------------------------------

ifndef MAKE_SO
	MAKE_SO = 0
endif

ifndef DEBUG
	DEBUG = 0
endif


# ---------------------------------------------------------------------------
# Shared-library support
# ---------------------------------------------------------------------------

FPIC :=

ifeq ($(MAKE_SO),1)
	FPIC := -fPIC
endif

ifeq ($(MAKE_SO),TRUE)
	FPIC := -fPIC
endif


# ---------------------------------------------------------------------------
# Architecture optimization
#
# Flux compute nodes use AMD Zen 4 processors.
# ---------------------------------------------------------------------------

ARCH_FLAGS := -march=znver4 -mtune=znver4


# ---------------------------------------------------------------------------
# Archiver
# ---------------------------------------------------------------------------

AR = ar rcs


# ---------------------------------------------------------------------------
# Fortran flags
#
# Plasma State Makefiles rely on -c being supplied through FFLAGS.
# ---------------------------------------------------------------------------

FFLAGS_COMMON := \
	-c \
	-m64 \
	-fallow-argument-mismatch \
	-fno-range-check \
	-fdollar-ok \
	$(FPIC)

FFLAGS_OPT := \
	$(FFLAGS_COMMON) \
	-O2 \
	-g \
	$(ARCH_FLAGS)

FFLAGS_DBG := \
	$(FFLAGS_COMMON) \
	-O0 \
	-g \
	-ffpe-trap=invalid,zero,overflow \
	-fcheck=all \
	-fbacktrace


ifeq ($(DEBUG),0)
	FFLAGS = $(FFLAGS_OPT)
else
	FFLAGS = $(FFLAGS_DBG)
endif


FPP = -cpp
MFLAG = -J


# ---------------------------------------------------------------------------
# Fortran runtime libraries
# ---------------------------------------------------------------------------

L_FLIBS = \
	-Wl,-rpath,$(FLIBROOT) \
	-L$(FLIBROOT) \
	-lgfortran \
	-lm


# ---------------------------------------------------------------------------
# C flags
#
# Plasma State Makefiles rely on -c being supplied through CFLAGS.
# ---------------------------------------------------------------------------

CFLAGS_COMMON := \
	-c \
	-m64 \
	$(FPIC)

CFLAGS_OPT := \
	$(CFLAGS_COMMON) \
	-O2 \
	-g \
	$(ARCH_FLAGS)

CFLAGS_DBG := \
	$(CFLAGS_COMMON) \
	-O0 \
	-g


ifeq ($(DEBUG),0)
	CFLAGS = $(CFLAGS_OPT)
else
	CFLAGS = $(CFLAGS_DBG)
endif


# ---------------------------------------------------------------------------
# C++ flags
#
# Plasma State Makefiles rely on -c being supplied through CXXFLAGS.
# ---------------------------------------------------------------------------

CXXFLAGS_COMMON := \
	-c \
	-m64 \
	$(FPIC)

CXXFLAGS_OPT := \
	$(CXXFLAGS_COMMON) \
	-O2 \
	-g \
	$(ARCH_FLAGS)

CXXFLAGS_DBG := \
	$(CXXFLAGS_COMMON) \
	-O0 \
	-g


ifeq ($(DEBUG),0)
	CXXFLAGS = $(CXXFLAGS_OPT)
else
	CXXFLAGS = $(CXXFLAGS_DBG)
endif


CPP = -cpp


# ---------------------------------------------------------------------------
# C/C++ runtime libraries
# ---------------------------------------------------------------------------

L_CLIBS = \
	-Wl,-rpath,$(FLIBROOT) \
	-L$(FLIBROOT) \
	-lstdc++ \
	-lgcc_s \
	-lgcc


# ---------------------------------------------------------------------------
# Readline etc.
# ---------------------------------------------------------------------------

L_EDIT = \
	-L/usr/lib64 \
	-lreadline \
	-lhistory \
	-ltermcap


# ---------------------------------------------------------------------------
# Default include directories
# ---------------------------------------------------------------------------

FINCL :=
CINCL :=


# ---------------------------------------------------------------------------
# Default definitions
# ---------------------------------------------------------------------------

FDEFS := -D__LINUX -D__UNIX -D__F90
CDEFS := -D__LINUX

#FDEFS := $(FDEFS) -D__NOMDSPLUS
#CDEFS := $(CDEFS) -D__NOMDSPLUS
