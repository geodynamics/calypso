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
MPI_TEST =  $(MPIRUN) -np $(NP) $(SRCDIR)/bin/tests/check_sph_comms
#
#
#  -------------------------------------------------------------------------
#


all: test

test: 
	rm -f *.dat *.txt
	$(MPI_TEST) >>  log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed spherical shell communication check'; \
		exit 1;\
	fi
	@echo 'Indexing is correct'

clean:
	rm -f *.dat *.txt

distclean: clean
