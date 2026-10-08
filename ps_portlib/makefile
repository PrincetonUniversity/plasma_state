# ---------------------------------------------------------------------------
# Makefile: ps_portlib
#
# Builds:
#   libps_portlib.a
#   libps_portlib.so   when MAKE_SO=1
#
# Shared-library dependencies:
#   none
# ---------------------------------------------------------------------------

# Include system/compiler dependent flags
include $(COMPILER_FLAGS)

NAME := ps_portlib

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Fortran sources
# ---------------------------------------------------------------------------

SF90 := $(wildcard *.f90)
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
# Runtime search path
#
# Keep $ORIGIN in Plasma State shared libraries so that any present or
# future sibling shared-library dependencies can be resolved from LIBDIR.
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
	$(FC) $(FPP) $(FFLG) -c $< -o $@ \
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
	$(CC) -cpp $(CFLG) -c $< -o $@ \
		$(CDEF) $(CINC)


# ---------------------------------------------------------------------------
# Clean
# ---------------------------------------------------------------------------

clean:
	@rm -f $(OBJDIR)/*.o
ifneq ($(strip $(MODS)),)
	@rm -f $(addprefix $(MODDIR)/,$(MODS))
endif


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
