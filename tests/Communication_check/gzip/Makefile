#
#  Makefile for Dynamo toolkit Calypso
#    Written by H. Matsui
#
SHELL           = /bin/sh
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
	@echo 'Gzipped ASCII indexing file is correct'

clean:
	rm -f *.dat *.txt mesh/in.*

distclean: clean
