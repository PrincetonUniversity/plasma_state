# Makefile to build ccps_test executable
#

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := ccps_test
LIBDIR := $(PREFIX)/lib
OBJDIR := $(PREFIX)/obj/$(NAME)
EXEDIR := $(PREFIX)/bin

LIBS := ccps_lib plasma_state ps_xplasma2 plasma_state_kernel xplasma2 \
	nscrunch ps_bloat ps_smlib ps_comput ps_portlib

L_PLASMA_STATE := -L$(LIBDIR) $(addprefix -l, $(LIBS))

L_LIBS := $(L_PLASMA_STATE) $(L_PSPLINE) $(L_EZCDF) $(L_NETCDF) $(L_LAPACK) $(L_BLAS) \
	$(L_EDIT) $(L_FLIBS)

CDEF := $(DEFS)
CINC := $(CINCL) -I../include -I../ccps_lib
CXXFLG := $(CXXFLAGS)
LDCXX := $(CXX)
ifeq ($(_COMPILER),PGC)
	LDCXX := pgc++
endif


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

$(NAME): $(OBJDIR)/ccps_test.o
	@echo "Building $(NAME) executable"
	@$(LDCXX) $(LDFLAGS) $< -o $(EXEDIR)/$@ $(L_LIBS)

$(OBJDIR)/%.o: %.cxx
	@$(CXX) $(CXXFLG) $< -o $@ $(CDEF) $(CINC)

clean:
	@rm -f $(OBJDIR)/*.o
	@rm -f $(EXEDIR)/$(NAME)

realclean: clean

clobber: realclean
