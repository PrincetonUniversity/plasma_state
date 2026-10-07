# ---------------------------------------------------------------------------
# Makefile: xplasma2
# ---------------------------------------------------------------------------

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := xplasma2

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Modules and sources
# ---------------------------------------------------------------------------

MODS := eqi_geq_axis_data.mod eqi_geq_mod.mod eqi_intg_module.mod \
	eqi_rzbox_module.mod eqi_vtaub_mod.mod geqdsk_aux.mod \
	geqdsk_index_mod.mod geqdsk_nomds_aux.mod geqdsk_nomds_mod.mod \
	xdistrib_mod.mod xplasma2_calls.mod xplasma_avg1dmod.mod \
	xplasma_ctran.mod xplasma_definitions.mod xplasma_flxint.mod \
	xplasma_fromgeq.mod xplasma_mcgrid.mod xplasma_nclass.mod \
	xplasma_obj.mod xplasma_obj_instance_mod.mod xplasma_obj_instance.mod \
	xplasma_profs.mod xplasma_rzgeo.mod xplasma_sol.mod

SRCM := $(MODS:%.mod=%.f90)

SF90 := $(SRCM) $(filter-out $(SRCM),$(wildcard *.f90))
SFOR := $(wildcard *.for)

SRCF := $(SF90) $(SFOR)

OBJF := $(SF90:%.f90=$(OBJDIR)/%.o)
OBJF += $(SFOR:%.for=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# Compiler flags
# ---------------------------------------------------------------------------

FDEF := $(FDEFS)

FINC := $(FINCL) \
	-I$(NETCDF_FORTRAN_HOME)/include \
	-I$(EZCDF_HOME)/mod \
	-I$(PSPLINE_HOME)/include

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
	-lnscrunch \
	-lps_bloat \
	-lps_comput \
	-lps_smlib \
	-lps_portlib


# ---------------------------------------------------------------------------
# External dependencies
# ---------------------------------------------------------------------------

L_EXTERNAL := \
	-L$(PSPLINE_HOME)/lib \
	-lpspline \
	-L$(EZCDF_HOME)/lib \
	-lezcdf


# ---------------------------------------------------------------------------
# Runtime search paths
# ---------------------------------------------------------------------------

SHARED_RPATH := \
	-Wl,-rpath,'$$ORIGIN' \
	-Wl,-rpath,$(PSPLINE_HOME)/lib \
	-Wl,-rpath,$(EZCDF_HOME)/lib


# ---------------------------------------------------------------------------
# Targets
# ---------------------------------------------------------------------------

.PHONY: all clean realclean clobber check depend

all: check $(NAME)


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

$(OBJDIR)/%.o: %.for | check
	$(FC) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)


$(OBJDIR)/xplasma_avg1dmod.o: xplasma_avg1dmod.f90 | check
ifeq ($(_COMPILER),ICC)
	@echo "xplasma2/xplasma_avg1dmod.f90 compiled without optimization for Intel"
	$(FC) $(FPP) $(FFLAGS_DBG) $(FPIC) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)
else
	$(FC) $(FPP) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)
endif


$(OBJDIR)/xplasma_obj.o: xplasma_obj.f90 | check
ifeq ($(_COMPILER),ICC)
	@echo "xplasma2/xplasma_obj.f90 compiled without optimization for Intel"
	$(FC) $(FPP) $(FFLAGS_DBG) $(FPIC) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)
else
	$(FC) $(FPP) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)
endif


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
