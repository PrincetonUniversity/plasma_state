# ---------------------------------------------------------------------------
# Makefile: geqdsk_mds
#
# Builds:
#   libgeqdsk_mds.a
#   libgeqdsk_mds.so   when MAKE_SO=1
#
# Direct Plasma State dependencies:
#   libxplasma2.so
#   libold_xplasma.so
#   libps_portlib.so
#   libnscrunch.so
#   libps_comput.so
#
# External dependencies:
#   libpspline.so
#   libmdstransp.so
# ---------------------------------------------------------------------------

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := geqdsk_mds

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Modules and sources
# ---------------------------------------------------------------------------

MODS := \
	cgeq_paths.mod \
	geqdsk_mod.mod \
	eqi_geq_mds_mod.mod \
	eqm_mds_fromgeqdsk_mod.mod

SRCM := $(MODS:%.mod=%.f90)

SRCF := $(SRCM) $(filter-out $(SRCM),$(wildcard *.f90))
OBJF := $(SRCF:%.f90=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# Compiler flags
# ---------------------------------------------------------------------------

FDEF := $(FDEFS)

FINC := $(FINCL) \
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
	-lxplasma2 \
	-lold_xplasma \
	-lps_portlib \
	-lnscrunch \
	-lps_comput


# ---------------------------------------------------------------------------
# PSPLINE dependency
# ---------------------------------------------------------------------------

L_PSPLINE_DEP := \
	-L$(PSPLINE_HOME)/lib \
	-lpspline


# ---------------------------------------------------------------------------
# UFILES / MDS dependency
#
# libmdstransp.so provides the MDS/TRMDS routines used directly by
# geqdsk_mds, including:
#
#   idescr_long_
#   idescr_longarr_
#   idescr_floatarr_
#   idescr_cstring_
#   mdscacheunconnect_
#   mdsfconnect_
#   mds_open_
#   mds_close_
#   mds_value_
#   mdserr_
#   trmds_parse_
#   trmds_pshrink_
# ---------------------------------------------------------------------------

L_UFILES_DEP := \
	-L$(UFILES_ROOT)/lib \
	-lmdstransp


# ---------------------------------------------------------------------------
# Runtime search paths
# ---------------------------------------------------------------------------

SHARED_RPATH := \
	-Wl,-rpath,'$$ORIGIN' \
	-Wl,-rpath,$(PSPLINE_HOME)/lib \
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
		$(L_PSPLINE_DEP) \
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
