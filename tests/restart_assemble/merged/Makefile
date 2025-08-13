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
	rm -f *.dat *.txt rst.*
	$(MPIRUN) -np 4 $(TARGET_NEWSPH)  >> log.txt
	$(MPIRUN) -np 4 $(TARGET_CMP_SPH_RST) >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Merged ASCII Data assemble check'; \
		exit 1;\
	fi
	@echo 'Merged ASCII Data assemble check finished'

clean:
	rm -f *.dat *.txt rst.*

distclean: clean
