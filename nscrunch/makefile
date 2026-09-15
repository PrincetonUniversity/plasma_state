# Makefile to build libnscrunch.a
#

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := nscrunch
OBJDIR := $(PREFIX)/obj/$(NAME)
LIBAR := $(PREFIX)/lib/lib$(NAME).a
LIBSO := $(PREFIX)/lib/lib$(NAME).so

MNMS := eq_arc_scrunch_mod.mod mintrp.mod scrunch_inc1.mod scrunch_inc2.mod
MODS := eq_arc_scrunch_mod.o scrscrmod.o
SRCM := $(MODS:%.o=%.f90)
SRCF := $(SRCM) $(filter-out $(SRCM), $(wildcard *.f90))
OBJF := $(SRCF:%.f90=$(OBJDIR)/%.o)

FDEF := $(FDEFS)
FINC := $(FINCL)
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
	@$(FC) -shared $(LDFLAGS) -o $(LIBSO) $(OBJF)
endif

include make.depend
$(OBJDIR)/%.o: %.f90
	@$(FC) $(FPP) $(FFLG) $< -o $@ $(FDEF) $(FINC) $(MFLAG) $(PREFIX)/mod

clean:
	@rm -f $(OBJDIR)/*.o
	@cd $(PREFIX)/mod ; rm -f $(MNMS)

realclean: clean
	@rm -f $(LIBAR) $(LIBSO)

clobber: realclean

depend:
	@makedepf90 -b OBJDIR $(SRCF) > make.depend
	@sed -i 's/OBJDIR/\$$\(OBJDIR\)/g' make.depend
