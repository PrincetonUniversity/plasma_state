# Makefile to build libxplasma2.a
#

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := xplasma2
OBJDIR := $(PREFIX)/obj/$(NAME)
LIBAR := $(PREFIX)/lib/lib$(NAME).a
LIBSO := $(PREFIX)/lib/lib$(NAME).so
MODS := eqi_geq_axis_data.mod eqi_geq_mod.mod eqi_intg_module.mod \
	eqi_rzbox_module.mod eqi_vtaub_mod.mod geqdsk_aux.mod \
	geqdsk_index_mod.mod geqdsk_nomds_aux.mod geqdsk_nomds_mod.mod \
	xdistrib_mod.mod xplasma2_calls.mod xplasma_avg1dmod.mod \
	xplasma_ctran.mod xplasma_definitions.mod xplasma_flxint.mod \
	xplasma_fromgeq.mod xplasma_mcgrid.mod xplasma_nclass.mod \
	xplasma_obj.mod xplasma_obj_instance_mod.mod xplasma_obj_instance.mod \
	xplasma_profs.mod xplasma_rzgeo.mod xplasma_sol.mod
SRCM := $(MODS:%.mod=%.f90)
SF90 := $(SRCM) $(filter-out $(SRCM), $(wildcard *.f90))
SFOR := $(wildcard *.for)
SRCF := $(SF90) $(SFOR)
OBJF := $(SRCF:%.f90=$(OBJDIR)/%.o)
OBJF := $(OBJF:%.for=$(OBJDIR)/%.o)

FDEF := $(FDEFS)
FINC := $(FINCL) -I$(NETCDF_FORTRAN_HOME)/include -I$(EZCDF_HOME)/mod -I$(PSPLINE_HOME)/include
ifeq ($(MAKE_SO),1)
    FPIC := -fPIC
else
    FPIC :=
endif

FFLG := $(FFLAGS) $(FPIC)


.PHONY: clean realclean clobber check

all:	check $(NAME)

check:
	@test -d $(PREFIX)/lib || mkdir -p $(PREFIX)/lib
	@test -d $(PREFIX)/obj/$(NAME) || mkdir -p $(PREFIX)/obj/$(NAME)

$(NAME): $(OBJF)
	@echo "Building $(NAME) static library"
	@$(AR) $(LIBAR) $(OBJF)
ifeq ($(MAKE_SO),1)
	@echo "Building $(NAME) shared library"
	@$(FC) -shared $(LDFLAGS) -o $(LIBSO) \
		$(OBJF) \
		-L$(PSPLINE_HOME)/lib \
		-lpspline \
		-L$(EZCDF_HOME)/lib \
		-lezcdf \
		-L$(PREFIX)/lib \
		-lnscrunch -lps_bloat -lps_comput
endif

include make.depend
$(OBJDIR)/%.o: %.for
	@$(FC) $(FFLG) $< -o $@ $(FDEF) $(FINC) $(MFLAG) $(PREFIX)/mod
$(OBJDIR)/xplasma_avg1dmod.o: xplasma_avg1dmod.f90
ifeq ($(_COMPILER),ICC)
	@echo "xplasma2/xplasma_avg1dmod.f90 compiled without optimization for Intel"
	@$(FC) $(FPP) $(FFLAGS_DBG) $< -o $@ $(FDEF) $(FINC) $(MFLAG) $(PREFIX)/mod
else
	@$(FC) $(FPP) $(FFLG) $< -o $@ $(FDEF) $(FINC) $(MFLAG) $(PREFIX)/mod
endif
$(OBJDIR)/xplasma_obj.o: xplasma_obj.f90
ifeq ($(_COMPILER),ICC)
	@echo "xplasma2/xplasma_obj.f90 compiled without optimization for Intel"
	@$(FC) $(FPP) $(FFLAGS_DBG) $< -o $@ $(FDEF) $(FINC) $(MFLAG) $(PREFIX)/mod
else
	@$(FC) $(FPP) $(FFLG) $< -o $@ $(FDEF) $(FINC) $(MFLAG) $(PREFIX)/mod
endif
$(OBJDIR)/%.o: %.f90
	@$(FC) $(FPP) $(FFLG) $< -o $@ $(FDEF) $(FINC) $(MFLAG) $(PREFIX)/mod

clean:
	@rm -f $(OBJDIR)/*.o
	@cd $(PREFIX)/mod ; rm -f $(MODS)

realclean: clean
	@rm -f $(LIBAR) $(LIBSO)

clobber: realclean

depend:
	@makedepf90 -b OBJDIR $(SRCF) > make.depend
	@sed -i 's/OBJDIR/\$$\(OBJDIR\)/g' make.depend
