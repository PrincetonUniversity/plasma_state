# ---------------------------------------------------------------------------
# Makefile: xplasma_debug
#
# Builds:
#   libxplasma_debug.a
#   libxplasma_debug.so   when MAKE_SO=1
#
# Direct Plasma State dependencies:
#   libxplasma2.so
#   libps_portlib.so
#
# Direct UFILES dependencies:
#   libureadsub.so
#   libtrgraf.so
# ---------------------------------------------------------------------------

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := xplasma_debug

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Modules and sources
# ---------------------------------------------------------------------------

MODS := \
	xpl_dbg_module.mod \
	xplasma_debug_module.mod \
	xplasma_debug_link.mod

SRCM := $(MODS:%.mod=%.f90)
SRCF := $(SRCM) $(filter-out $(SRCM),$(wildcard *.f90))
OBJF := $(SRCF:%.f90=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# Compiler flags
# ---------------------------------------------------------------------------

FDEF := $(FDEFS)
FINC := $(FINCL)

ifeq ($(MAKE_SO),1)
FPIC := -fPIC
else
FPIC :=
endif

FFLG := $(FFLAGS) $(FPIC)


# ---------------------------------------------------------------------------
# Direct Plasma State dependencies
# ---------------------------------------------------------------------------

L_PS_DEP := \
	-L$(LIBDIR) \
	-lxplasma2 \
	-lps_portlib


# ---------------------------------------------------------------------------
# Direct UFILES dependencies
# ---------------------------------------------------------------------------

L_UFILES_DEP := \
	-L$(UFILES_ROOT)/lib \
	-lureadsub \
	-ltrgraf


# ---------------------------------------------------------------------------
# Runtime search paths
# ---------------------------------------------------------------------------

SHARED_RPATH := \
	-Wl,-rpath,'$$ORIGIN' \
	-Wl,-rpath,$(UFILES_ROOT)/lib


# ---------------------------------------------------------------------------
# Targets
# ---------------------------------------------------------------------------

.PHONY: all clean realclean clobber check depend

all: check $(NAME)


# ---------------------------------------------------------------------------
# Create output directories
# ---------------------------------------------------------------------------

check:
	@mkdir -p $(LIBDIR)
	@mkdir -p $(OBJDIR)
	@mkdir -p $(MODDIR)


# ---------------------------------------------------------------------------
# Libraries
# ---------------------------------------------------------------------------

$(NAME): $(OBJF)

	@echo "Building $(NAME) static library"
	@rm -f $(LIBAR)
	$(AR) $(LIBAR) $(OBJF)

ifeq ($(MAKE_SO),1)
	@echo "Building $(NAME) shared library"
	@rm -f $(LIBSO)
	$(FC) -shared $(LDFLAGS) \
		-o $(LIBSO) \
		$(OBJF) \
		$(L_PS_DEP) \
		$(L_UFILES_DEP) \
		$(SHARED_RPATH)
endif


# ---------------------------------------------------------------------------
# Dependencies
# ---------------------------------------------------------------------------

include make.depend


# ---------------------------------------------------------------------------
# Fortran compilation
# ---------------------------------------------------------------------------

$(OBJDIR)/%.o: %.f90 | check
	$(FC) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)


# ---------------------------------------------------------------------------
# Clean
# ---------------------------------------------------------------------------

clean:
	@rm -f $(OBJDIR)/*.o
	@rm -f $(addprefix $(MODDIR)/,$(MODS))


realclean: clean
	@rm -f $(LIBAR)
	@rm -f $(LIBSO)


clobber: realclean


# ---------------------------------------------------------------------------
# Generate Fortran dependencies
# ---------------------------------------------------------------------------

depend:
	@makedepf90 -b OBJDIR $(SRCF) > make.depend
	@sed -i 's/OBJDIR/\$$\(OBJDIR\)/g' make.depend
