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
	$(MPIRUN) -np 3 $(TARGET_NEWSPH)  >> log.txt
	$(MPIRUN) -np 3 $(TARGET_CMP_SPH_RST) >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed BINARY Data assemble check'; \
		exit 1;\
	fi
	@echo 'BINARY Data assemble check finished'

clean:
	rm -f *.dat *.txt restart/rst.*

distclean: clean
