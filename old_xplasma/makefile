# ---------------------------------------------------------------------------
# Makefile: old_xplasma
#
# Builds:
#   libold_xplasma.a
#   libold_xplasma.so   when MAKE_SO=1
# ---------------------------------------------------------------------------

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := old_xplasma

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Modules and sources
# ---------------------------------------------------------------------------

MODS := xplasma_calls.mod eq_module.mod

SRCM := xplasma_calls.for eq_module.f90

SF90 := $(SRCM) $(filter-out $(SRCM),$(wildcard *.f90))
SFOR := $(filter-out $(SRCM),$(wildcard *.for))

SRCF := $(SF90) $(SFOR)

OBJF := $(SRCF:%.f90=$(OBJDIR)/%.o)
OBJF := $(OBJF:%.for=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# Compiler flags
# ---------------------------------------------------------------------------

FDEF := $(FDEFS)

FINC := $(FINCL) \
	-I$(EZCDF_HOME)/mod

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
	-lps_portlib \
	-lnscrunch \
	-lps_smlib


# ---------------------------------------------------------------------------
# UFILES dependency
# ---------------------------------------------------------------------------

L_UFILES_DEP := \
	-L$(UFILES_ROOT)/lib \
	-lureadsub


# ---------------------------------------------------------------------------
# External dependencies
# ---------------------------------------------------------------------------

L_EXTERNAL := \
	-L$(EZCDF_HOME)/lib \
	-lezcdf


# ---------------------------------------------------------------------------
# Runtime search paths
# ---------------------------------------------------------------------------

SHARED_RPATH := \
	-Wl,-rpath,'$$ORIGIN' \
	-Wl,-rpath,$(UFILES_ROOT)/lib \
	-Wl,-rpath,$(EZCDF_HOME)/lib


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
		$(L_EXTERNAL) \
		$(SHARED_RPATH)
endif


# ---------------------------------------------------------------------------
# Dependencies
# ---------------------------------------------------------------------------

include make.depend


# ---------------------------------------------------------------------------
# Fortran compilation
# ---------------------------------------------------------------------------

$(OBJDIR)/%.o: %.for | check
	$(FC) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)


$(OBJDIR)/%.o: %.f90 | check
	$(FC) $(FPP) $(FFLG) -c $< -o $@ \
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
