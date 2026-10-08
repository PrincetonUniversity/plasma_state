# ---------------------------------------------------------------------------
# Makefile: nscrunch
#
# Builds:
#   libnscrunch.a
#   libnscrunch.so   when MAKE_SO=1
#
# Direct Plasma State dependency:
#   libps_comput.so
#
# External dependency:
#   libpspline.so
# ---------------------------------------------------------------------------

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := nscrunch

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Sources
# ---------------------------------------------------------------------------

MNMS := \
	eq_arc_scrunch_mod.mod \
	mintrp.mod \
	scrunch_inc1.mod \
	scrunch_inc2.mod

MODS := \
	eq_arc_scrunch_mod.o \
	scrscrmod.o

SRCM := $(MODS:%.o=%.f90)

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
	-lps_comput


# ---------------------------------------------------------------------------
# External dependencies
#
# Provides:
#   cspline_
#   spvec_
#   genxpkg_
# ---------------------------------------------------------------------------

L_EXTERNAL := \
	-L$(PSPLINE_HOME)/lib \
	-lpspline


# ---------------------------------------------------------------------------
# Runtime search paths
# ---------------------------------------------------------------------------

SHARED_RPATH := \
	-Wl,-rpath,'$$ORIGIN' \
	-Wl,-rpath,$(PSPLINE_HOME)/lib


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

$(OBJDIR)/%.o: %.f90 | check
	$(FC) $(FPP) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)


# ---------------------------------------------------------------------------
# Clean
# ---------------------------------------------------------------------------

clean:
	@rm -f $(OBJDIR)/*.o
	@rm -f $(addprefix $(MODDIR)/,$(MNMS))


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
