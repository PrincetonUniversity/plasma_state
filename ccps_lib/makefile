# ---------------------------------------------------------------------------
# Makefile: ccps_lib
#
# Builds:
#   libccps_lib.a
#   libccps_lib.so   when MAKE_SO=1
#
# Direct Plasma State dependencies:
#   libplasma_state_kernel.so
#   libplasma_state.so
#   libps_portlib.so
#   libps_xplasma2.so
# ---------------------------------------------------------------------------

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := ccps_lib

OBJDIR := $(PREFIX)/obj/$(NAME)
MODDIR := $(PREFIX)/mod
LIBDIR := $(PREFIX)/lib
INCDIR := $(PREFIX)/include

LIBAR := $(LIBDIR)/lib$(NAME).a
LIBSO := $(LIBDIR)/lib$(NAME).so


# ---------------------------------------------------------------------------
# Include files
# ---------------------------------------------------------------------------

INCS := ccps_base.h ccps.h cxxps.h multiarray.h


# ---------------------------------------------------------------------------
# Fortran sources
# ---------------------------------------------------------------------------

SRCF := $(wildcard *.f90) $(wildcard *.F90)

OBJF := $(SRCF:%.f90=$(OBJDIR)/%.o)
OBJF := $(OBJF:%.F90=$(OBJDIR)/%.o)


# ---------------------------------------------------------------------------
# C++ sources
# ---------------------------------------------------------------------------

SRCC := $(wildcard *.cpp) $(wildcard *.cxx)

OBJC := $(SRCC:%.cpp=$(OBJDIR)/%.o)
OBJC := $(OBJC:%.cxx=$(OBJDIR)/%.o)


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

FFLG   := $(FFLAGS)   $(FPIC)
CFLG   := $(CFLAGS)   $(FPIC)
CXXFLG := $(CXXFLAGS) $(FPIC)


# ---------------------------------------------------------------------------
# Direct Plasma State dependencies
# ---------------------------------------------------------------------------

L_PS_DEP := \
	-L$(LIBDIR) \
	-lplasma_state_kernel \
	-lplasma_state \
	-lps_portlib \
	-lps_xplasma2


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

$(NAME): $(OBJF) $(OBJC)

	@cp $(INCS) $(INCDIR)
	@cp ../include/f77name.h $(INCDIR)

	@echo "Building $(NAME) static library"
	@rm -f $(LIBAR)
	$(AR) $(LIBAR) $(OBJF) $(OBJC)

ifeq ($(MAKE_SO),1)
	@echo "Building $(NAME) shared library"
	@rm -f $(LIBSO)
	$(CXX) -shared $(LDFLAGS) \
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

$(OBJDIR)/%.o: %.f90 | check
	$(FC) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)

$(OBJDIR)/%.o: %.F90 | check
	$(FC) $(FPP) $(FFLG) -c $< -o $@ \
		$(FDEF) $(FINC) \
		$(MFLAG) $(MODDIR)


# ---------------------------------------------------------------------------
# C++ compilation
# ---------------------------------------------------------------------------

$(OBJDIR)/%.o: %.cpp | check
	$(CXX) $(CPP) $(CFLG) -c $< -o $@ \
		$(CDEF) $(CINC)

$(OBJDIR)/%.o: %.cxx | check
	$(CXX) $(CPP) $(CXXFLG) -c $< -o $@ \
		$(CDEF) $(CINC)


# ---------------------------------------------------------------------------
# Clean
# ---------------------------------------------------------------------------

clean:
	@rm -f $(OBJDIR)/*.o
	@rm -f $(addprefix $(INCDIR)/,$(INCS))
	@rm -f $(INCDIR)/f77name.h


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
