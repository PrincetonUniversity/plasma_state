# ---------------------------------------------------------------------------
# Makefile: plasma_state_kernel
#
# Builds:
#   libplasma_state_kernel.a
#   libplasma_state_kernel.so   when MAKE_SO=1
#
# Direct Plasma State dependencies:
#   libps_comput.so
#   libps_portlib.so
#   libxplasma2.so
# ---------------------------------------------------------------------------

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := plasma_state_kernel

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib
INCDIR := $(PREFIX)/include

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Include files
# ---------------------------------------------------------------------------

INCS := ps_container_size.h


# ---------------------------------------------------------------------------
# Modules and sources
# ---------------------------------------------------------------------------

MODS := \
	swim_global_data_mod.mod \
	plasma_state_constants_mod.mod \
	plasma_state_util_mod.mod \
	plasma_state_definition_mod.mod \
	ps_namwr_mod.mod

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
# Direct shared-library dependencies
# ---------------------------------------------------------------------------

L_PS_DEP := \
	-L$(LIBDIR) \
	-lps_comput \
	-lps_portlib \
	-lxplasma2


# ---------------------------------------------------------------------------
# Runtime search path
# ---------------------------------------------------------------------------

SHARED_RPATH := -Wl,-rpath,'$$ORIGIN'


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
	@mkdir -p $(INCDIR)


# ---------------------------------------------------------------------------
# Libraries
# ---------------------------------------------------------------------------

$(NAME): $(OBJF)

	@cp $(INCS) $(INCDIR)

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
	@rm -f $(addprefix $(INCDIR)/,$(INCS))
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
