#
#  Makefile for Dynamo toolkit Calypso
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
#
# Command settings
#
MPIRUN= mpirun   
TARGET_GEN_SPH_GRID= $(SRCDIR)/bin/utilities/gen_sph_grids
TARGET_NEWSPH= $(SRCDIR)/bin/utilities/assemble_sph
TARGET_CMP_SPH_RST=  $(SRCDIR)/bin/tests/compare_sph_restart#
#

all: test

test: 
	rm -f *.dat *.txt restart/rst.*
	$(MPIRUN) -np 3 $(TARGET_GEN_SPH_GRID) > log.txt
	$(MPIRUN) -np 3 $(TARGET_NEWSPH)  >> log.txt
	$(MPIRUN) -np 3 $(TARGET_CMP_SPH_RST) >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed ASCII Data assemble check'; \
		exit 1;\
	fi
	@echo 'ASCII Data assemble check finished'

clean:
	rm -f *.dat *.txt restart/rst.* sph_lm7t12r14c_ic_3/in.* 

distclean: clean
