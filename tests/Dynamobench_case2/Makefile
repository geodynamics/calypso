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
BUILDDIR= $(SRCDIR)/bin
TESTDIR=  $(SRCDIR)/tests
#
# MPI settings
#
MPICHDIR =    
MPICHLIBDIR = $(MPICHDIR)/lib
MPICHBINDIR = $(MPICHDIR)/bin
MPICHINCDIR = 
MPILIBS =      
#

all: test

test: 
	rm -f *.dat
	$(MPIRUN) -np 4 $(BUILDDIR)/sph_mhd
	echo 'Compare with reference mean square data'
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_ave_volume    sph_ave_volume    >>& log.txt
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_pwr_volume_s  sph_pwr_volume_s  >>& log.txt
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_pwr_volume_m0 sph_pwr_volume_m0 >>& log.txt
	$(BUILDDIR)/tests/sph_ene_check  reference/gauss_coefs_Re    gauss_coefs_Re    >>& log.txt

clean:
	rm -f *.dat *.txt
	rm -f picked/*.dat
	rm -f rst_4/rst.*
	rm -f section/*.vtk
	rm -f isosurface/*.vtk

distclean: clean
