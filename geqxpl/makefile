# Makefile to build geqxpl executable
#

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := geqxpl
LIBDIR := $(PREFIX)/lib
OBJDIR := $(PREFIX)/obj/$(NAME)
EXEDIR := $(PREFIX)/bin

SRCF := geqxpl_rawplot.f90
OBJF := $(OBJDIR)/geqxpl_rawplot.o

LIBS := old_xplasma xplasma_debug xplasma2 geqdsk_mds nscrunch \
	ps_bloat ps_smlib ps_comput ps_portlib

L_PLASMA_STATE := -L$(LIBDIR) $(addprefix -l, $(LIBS))

L_LIBS := $(L_PLASMA_STATE) $(L_UFILES) $(L_SGLIB) $(L_PSPLINE) \
	$(L_EZCDF) $(L_NETCDF) $(L_BLAS) $(L_LAPACK) $(L_MDSPLUS) $(L_EDIT)

FDEF := $(FDEFS)
FINC := $(FINCL) -I$(PREFIX)/mod
FFLG := $(FFLAGS)


.PHONY: clean realclean clobber check libs force build rebuild

all:	check $(NAME)

libs:	check build $(NAME)

force:	check rebuild $(NAME)

build:
	@set -e; for l in $(LIBS); do cd ../$${l}; make -j; done

rebuild:
	@set -e; for l in $(LIBS); do cd ../$${l}; make clobber; make -j; done

check:
	@test -d $(EXEDIR) || mkdir -p $(EXEDIR)
	@test -d $(OBJDIR) || mkdir -p $(OBJDIR)

$(NAME): $(OBJF) $(OBJDIR)/geqxpl.o
	@echo "Building $(NAME) executable"
	@$(FC) $(LDFLAGS) $(OBJF) $(OBJDIR)/geqxpl.o -o $(EXEDIR)/$@ $(L_LIBS)

$(OBJDIR)/%.o: %.for
	@$(FC) $(FPP) $(FFLG) $< -o $@ $(FDEF) $(FINC)
$(OBJDIR)/%.o: %.f90
	@$(FC) $(FFLG) $< -o $@ $(FDEF) $(FINC)

clean:
	@rm -f $(OBJDIR)/*.o
	@rm -f $(EXEDIR)/$(NAME)

realclean: clean

clobber: realclean
