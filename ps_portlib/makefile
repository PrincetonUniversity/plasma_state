# Makefile to build libps_portlib.a
#

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := ps_portlib
OBJDIR := $(PREFIX)/obj/$(NAME)
LIBAR := $(PREFIX)/lib/lib$(NAME).a
LIBSO := $(PREFIX)/lib/lib$(NAME).so

SF90 := $(wildcard *.f90)
SFOR := $(wildcard *.for)
SRCF := $(SF90) $(SFOR)
OBJF := $(SRCF:%.f90=$(OBJDIR)/%.o)
OBJF := $(OBJF:%.for=$(OBJDIR)/%.o)

SRCC := $(wildcard *.c)
OBJC := $(SRCC:%.c=$(OBJDIR)/%.o)

FDEF := $(FDEFS)
FINC := $(FINCL)

CDEF := $(CDEFS)
CINC := $(CINCL) -I../include

ifeq ($(MAKE_SO),1)
    FPIC := -fPIC
else
    FPIC :=
endif

FFLG := $(FFLAGS) $(FPIC)
CFLG := $(CFLAGS) $(FPIC)


.PHONY: clean realclean clobber check

all:	check $(NAME)

check:
	@test -d $(PREFIX)/lib || mkdir -p $(PREFIX)/lib
	@test -d $(PREFIX)/obj/$(NAME) || mkdir -p $(PREFIX)/obj/$(NAME)

$(NAME): $(OBJF) $(OBJC)
	@echo "Building $(NAME) static library"
	@$(AR) $(LIBAR) $(OBJF) $(OBJC)
ifeq ($(MAKE_SO),1)
	@echo "Building $(NAME) shared library"
	@$(FC) -shared $(LDFLAGS) -o $(LIBSO) $(OBJF) $(OBJC)
endif

include make.depend
$(OBJDIR)/%.o: %.for
	@$(FC) $(FPP) $(FFLG) $< -o $@ $(FDEF) $(FINC) $(MFLAG) $(PREFIX)/mod
$(OBJDIR)/%.o: %.f90
	@$(FC) $(FPP) $(FFLG) $< -o $@ $(FDEF) $(FINC) $(MFLAG) $(PREFIX)/mod

$(OBJDIR)/%.o: %.c
	@$(CC) -cpp $(CFLG) $< -o $@ $(CDEF) $(CINC)

clean: 
	@rm -f $(OBJDIR)/*.o
	@cd $(PREFIX)/mod ; rm -f $(MODS)

realclean: clean
	@rm -f $(LIBAR) $(LIBSO)

clobber: realclean

depend:
	@makedepf90 -b OBJDIR $(SRCF) > make.depend
	@sed -i 's/OBJDIR/\$$\(OBJDIR\)/g' make.depend
