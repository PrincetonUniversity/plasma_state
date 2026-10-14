# ============================================================
# Compiler flags for Intel oneAPI 2023.2.0
#
# Compilers:
#   ifort
#   icc
#   icpc
#
# Supports:
#   Static libraries
#   Shared libraries (MAKE_SO=1)
#   Debug and optimized builds
# ============================================================

MAKE_SO ?= 0
DEBUG   ?= 0

# ============================================================
# Position-independent code
# ============================================================

FPIC :=

ifneq ($(filter 1 TRUE true YES yes,$(MAKE_SO)),)
    FPIC := -fPIC
endif

# ============================================================
# Archive tools
# ============================================================

AR      = ar rcs
RANLIB  = ranlib

# ============================================================
# Intel compiler options
# ============================================================

# Use generic architecture for portability.
# Replace with -xHost only when targeting the build machine.

ARCH_FLAGS :=

# Floating-point behavior
FP_FLAGS := -fp-model precise -no-ftz

# Memory model
MEM_FLAGS := -mcmodel=medium

# ============================================================
# Fortran flags
# ============================================================

FFLAGS_OPT = \
    -c \
    -O2 \
    -nowarn \
    -align dcommons \
    $(MEM_FLAGS) \
    $(ARCH_FLAGS) \
    $(FP_FLAGS) \
    $(FPIC)

FFLAGS_DBG = \
    -c \
    -g \
    -O0 \
    -check bounds \
    -check pointer \
    -check uninit \
    -traceback \
    -fpe0 \
    -align dcommons \
    -heap-arrays 20 \
    -init=zero \
    $(MEM_FLAGS) \
    $(FP_FLAGS) \
    $(FPIC)

ifeq ($(DEBUG),0)
    FFLAGS = $(FFLAGS_OPT)
else
    FFLAGS = $(FFLAGS_DBG)
endif

# ============================================================
# Fortran preprocessing and modules
# ============================================================

FPP   = -fpp
MFLAG = -module

# ============================================================
# Intel Fortran runtime libraries
# ============================================================

L_FLIBS = \
    -L$(FLIBROOT) \
    -lifport \
    -limf \
    -lifcore \
    -lm \
    -lirc

# ============================================================
# C flags
# ============================================================

CFLAGS_OPT = \
    -c \
    -O2 \
    -fp-model=precise \
    $(ARCH_FLAGS) \
    $(FPIC)

CFLAGS_DBG = \
    -c \
    -g \
    -O0 \
    -fp-model=precise \
    $(FPIC)

ifeq ($(DEBUG),0)
    CFLAGS = $(CFLAGS_OPT)
else
    CFLAGS = $(CFLAGS_DBG)
endif

# ============================================================
# C++ flags
# ============================================================

CXXFLAGS_OPT = \
    -c \
    -O2 \
    -fp-model=precise \
    $(ARCH_FLAGS) \
    $(FPIC)

CXXFLAGS_DBG = \
    -c \
    -g \
    -O0 \
    -fp-model=precise \
    $(FPIC)

ifeq ($(DEBUG),0)
    CXXFLAGS = $(CXXFLAGS_OPT)
else
    CXXFLAGS = $(CXXFLAGS_DBG)
endif

# ============================================================
# C preprocessing
# ============================================================

CPP = 

# ============================================================
# C/C++ libraries
# ============================================================

L_CLIBS = -lstdc++ -lm -ldl

# ============================================================
# Readline libraries
# ============================================================

L_EDIT = -lreadline -lhistory -ltinfo

# ============================================================
# Include directories
# ============================================================

FINCL :=
CINCL :=

# ============================================================
# Preprocessor definitions
# ============================================================

FDEFS := -D__LINUX -D__UNIX -D__F90
CDEFS := -D__LINUX

# ============================================================
# Optional MDSplus disabling
# ============================================================

# FDEFS += -D__NOMDSPLUS
# CDEFS += -D__NOMDSPLUS
