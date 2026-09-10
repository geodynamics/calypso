#
#  Makefile for Dynamo toolkit Calypso
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
CURRENTDIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

SRCDIR = /Users/matsui/src_kemo
MAKEDIR = $(SRCDIR)/work
BUILDDIR= $(SRCDIR)/bin
#
# Command settings
#
GMAKE= make
MPIRUN= mpirun   
NP = 3

TARGET_GEN_SPH_GRID= $(BUILDDIR)/utilities/gen_sph_grids
TARGET_NEWSPH=       $(BUILDDIR)/utilities/assemble_sph
TARGET_CMP_SPH_RST=  $(CURRENTDIR)/compare_sph_restart
#
#

all: test

target:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_CMP_SPH_RST='$(TARGET_CMP_SPH_RST)' \
		$(TARGET_CMP_SPH_RST)

test:  target
	rm -f *.dat *.txt restart/rst.*
	$(MPIRUN) -np $(NP) $(TARGET_GEN_SPH_GRID) > log.txt
	$(MPIRUN) -np $(NP) $(TARGET_NEWSPH)  >> log.txt
	$(MPIRUN) -np $(NP) $(TARGET_CMP_SPH_RST) >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed ASCII Data assemble check'; \
		exit 1;\
	fi
	@echo 'ASCII Data assemble check finished'

clean:
	rm -f *.dat *.txt restart/rst.* sph_lm7t12r14c_ic_3/in.* 

distclean: clean
