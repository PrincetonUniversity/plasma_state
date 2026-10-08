# ---------------------------------------------------------------------------
# Makefile: ps_comput
#
# Builds:
#   libps_comput.a
#   libps_comput.so   when MAKE_SO=1
#
# Shared-library dependencies:
#   libps_portlib
# ---------------------------------------------------------------------------

# Include system/compiler dependent flags
include $(COMPILER_FLAGS)

NAME := ps_comput

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Fortran modules and sources
# ---------------------------------------------------------------------------

MNMS := \
	plasma_hash_code.mod \
	ps_ode_mod.mod

MODS := \
	plasma_hash_code.o \
	ps_ode.o

SRCM := $(MODS:%.o=%.f90)

SF90 := $(SRCM) $(filter-out $(SRCM),$(wildcard *.f90))
SFOR := $(wildcard *.for)

SRCF := $(SF90) $(SFOR)

OBJF := $(SF90:%.f90=$(OBJDIR)/%.o)
OBJF += $(SFOR:%.for=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# C sources
# ---------------------------------------------------------------------------

SRCC := $(wildcard *.c)
OBJC := $(SRCC:%.c=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# Compiler flags
# ---------------------------------------------------------------------------

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


# ---------------------------------------------------------------------------
# Shared-library dependencies
#
# ps_comput uses routines from libps_portlib:
#
#   ps_bad_exit
#   find_io_unit
#   tmpfile
#   find_io_unit_list
# ---------------------------------------------------------------------------

L_PS_DEP := \
	-L$(LIBDIR) \
	-lps_portlib


# ---------------------------------------------------------------------------
# Runtime search path
#
# Plasma State shared libraries are installed together in $(LIBDIR).
# $ORIGIN tells the dynamic loader to search the directory containing
# libps_comput.so for sibling Plasma State shared libraries.
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


# ---------------------------------------------------------------------------
# Libraries
# ---------------------------------------------------------------------------

$(NAME): $(OBJF) $(OBJC)

	@echo "Building $(NAME) static library"
	@rm -f $(LIBAR)
	$(AR) $(LIBAR) $(OBJF) $(OBJC)

ifeq ($(MAKE_SO),1)
	@echo "Building $(NAME) shared library"
	@rm -f $(LIBSO)
	$(FC) -shared $(LDFLAGS) \
		-o $(LIBSO) \
		$(OBJF) \
		$(OBJC) \
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

$(OBJDIR)/%.o: %.for | check
	$(FC) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)


$(OBJDIR)/%.o: %.f90 | check
	$(FC) $(FPP) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)


# ---------------------------------------------------------------------------
# C compilation
# ---------------------------------------------------------------------------

$(OBJDIR)/%.o: %.c | check
	$(CC) $(CFLG) -c $< -o $@ \
		$(CDEF) $(CINC)


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
