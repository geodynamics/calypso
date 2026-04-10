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
NP = 6
MPIRUN= mpirun
MPI_GEN =   $(MPIRUN) -np $(NP) $(SRCDIR)/bin/utilities/gen_sph_grids
MPI_CHECK = $(MPIRUN) -np $(NP) $(SRCDIR)/bin/tests/check_sph_grids
MPI_TEST =  $(MPIRUN) -np $(NP) $(SRCDIR)/bin/tests/check_sph_comms
#
#
#  -------------------------------------------------------------------------
#


all: test

test: 
	rm -f *.dat *.txt
	$(MPI_GEN) > log.txt
	$(MPI_CHECK) >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed spherical shell grid check'; \
		exit 1;\
	fi
	$(MPI_TEST) >>  log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed spherical shell communication check'; \
		exit 1;\
	fi
	@echo 'ASCII indexing file is correct'

clean:
	rm -f *.dat *.txt mesh/in.*

distclean: clean
