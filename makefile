# Makefile to build all libs and exes
#

SHELL := /bin/bash

# Check for dependencies
ifeq (, $(shell which nc-config))
$(error Cannot find nc-config, is NetCDF installed?)
endif
ifndef EZCDF_HOME
$(error EZCDF_HOME is not set, is ezcdf installed?)
endif
ifndef PSPLINE_HOME
$(error PSPLINE_HOME is not set, is pspline installed?)
endif
ifndef UFILES_ROOT
$(error UFILES_ROOT is not set, is ufiles installed?)
endif
ifndef MDSPLUS_DIR
$(warning MDSPLUS_DIR is not set, building without MDS+ support)
endif


# Libraries (order is important)
LIBS := ps_physconst \
	ps_portlib \
	ps_comput \
	xplasma2 \
	old_xplasma \
	plasma_state_kernel \
	plasma_state \
	ccps_lib \
	cstate_lib \
	geqdsk_mds \
	mdescr \
	nscrunch \
	ps_xplasma2 \
	ps_bloat \
	ps_smlib \
	xplasma_debug

# Executables
EXES := ccps_test \
	cstate \
	convert_state \
	geq2ps \
	geqxpl \
	geqxpl2 \
	geq_plot \
	geq_test \
	grab_xplasma \
	plasma_state_test \
	plot_xplasma \
	ps2geq \
	ps2jso \
	ps2xplasma \
	ps_adas_test \
	scrdescur_test \
	test_descur \
	test_xplasma \
	update_state \
	xplasma2_test

.PHONY: check clean realclean clobber libs exes depend

all:	check libs exes

check:
	@test -d $(PREFIX)/bin || mkdir -p $(PREFIX)/bin
	@test -d $(PREFIX)/include || mkdir -p $(PREFIX)/include
	@test -d $(PREFIX)/lib || mkdir -p $(PREFIX)/lib
	@test -d $(PREFIX)/mod || mkdir -p $(PREFIX)/mod
	@test -d $(PREFIX)/obj || mkdir -p $(PREFIX)/obj
	@set -e; for l in $(LIBS); do cd $${l}; make check; cd .. ; done
	@set -e; for l in $(EXES); do cd $${l}; make check; cd .. ; done

libs:
	@set -e; for l in $(LIBS); do cd $${l}; make -j $(NTASKS); cd .. ; done

exes:
	@set -e; for l in $(EXES); do cd $${l}; make; cd .. ; done

clean: 
	@set -e; for l in $(LIBS); do cd $${l}; make clean; cd .. ; done
	@set -e; for l in $(EXES); do cd $${l}; make clean; cd .. ; done

realclean:
	@set -e; for l in $(LIBS); do cd $${l}; make realclean; cd .. ; done
	@set -e; for l in $(EXES); do cd $${l}; make realclean; cd .. ; done

clobber:
	@rm -rf $(PREFIX)

depend:
	@set -e; for l in $(LIBS); do cd $${l}; make depend; cd .. ; done
