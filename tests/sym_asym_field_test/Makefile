#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
#
MHDDIR =  $(SRCDIR)/MHD

all: test

test: 
	rm -f *.dat
	$(MPIRUN) -np 4 $(MHDDIR)/bin/sph_mhd
#	@sync
#	@echo 'Compare with reference mean square data'
#	$(MHDDIR)/bin/tests/sph_ene_check sph_pwr_volume_s reference/sph_pwr_volume_s >> log.txt

clean:
	rm -f *.dat *.txt fort.*
	rm -f rst_4/rst.*
	rm -f section/*.vtk
	rm -f isosurface/*.vtk

distclean: clean
