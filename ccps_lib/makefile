# Makefile to build libccps_lib.a
#

# Include system dependent flags
include $(COMPILER_FLAGS)

NAME := ccps_lib
OBJDIR := $(PREFIX)/obj/$(NAME)
LIBAR := $(PREFIX)/lib/lib$(NAME).a
LIBSO := $(PREFIX)/lib/lib$(NAME).so

INCS := ccps_base.h ccps.h cxxps.h multiarray.h

SRCF := $(wildcard *.f90) $(wildcard *.F90)
OBJF := $(SRCF:%.f90=$(OBJDIR)/%.o)
OBJF := $(OBJF:%.F90=$(OBJDIR)/%.o)

SRCC := $(wildcard *.cpp) $(wildcard *.cxx)
OBJC := $(SRCC:%.cpp=$(OBJDIR)/%.o)
OBJC := $(OBJC:%.cxx=$(OBJDIR)/%.o)

FDEF := $(FDEFS)
FINC := $(FINCL)
FFLG := $(FFLAGS)

CDEF := $(CDEFS)
CINC := $(CINCL) -I../include
CFLG := $(CFLAGS)
CXXFLG := $(CXXFLAGS)


.PHONY: clean realclean clobber check

all:	check $(NAME)

check:
	@test -d $(PREFIX)/lib || mkdir -p $(PREFIX)/lib
	@test -d $(PREFIX)/obj/$(NAME) || mkdir -p $(PREFIX)/obj/$(NAME)

$(NAME): $(OBJF) $(OBJC)
	@cp $(INCS) $(PREFIX)/include
	@cp ../include/f77name.h $(PREFIX)/include
	@echo "Building $(NAME) static library"
	@$(AR) $(LIBAR) $(OBJF) $(OBJC)
ifeq ($(MAKE_SO),1)
	@echo "Building $(NAME) shared library"
	@$(CXX) -shared $(LDFLAGS) -o $(LIBSO) $(OBJF) $(OBJC)
endif

include make.depend
$(OBJDIR)/%.o: %.[fF]90
	@$(FC) $(FFLG) $< -o $@ $(FDEF) $(FINC) $(MFLAG) $(PREFIX)/mod

$(OBJDIR)/%.o: %.cpp
	@$(CXX) $(CPP) $(CFLG) $< -o $@ $(CDEF) $(CINC)
$(OBJDIR)/%.o: %.cxx
	@$(CXX) $(CPP) $(CXXFLG) $< -o $@ $(CDEF) $(CINC)

clean:
	@rm -f $(OBJDIR)/*.o
	@cd $(PREFIX)/include ; rm -f $(INCS)

realclean: clean
	@rm -f $(LIBAR) $(LIBSO)

clobber: realclean

depend:
	@makedepf90 -b OBJDIR $(SRCF) > make.depend
	@sed -i 's/OBJDIR/\$$\(OBJDIR\)/g' make.depend
