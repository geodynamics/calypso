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
	$(MPIRUN) -np 4 $(BUILDDIR)/sph_snapshot
	echo 'Convert section data'
	$(BUILDDIR)/utilities/section_to_VTK 'section/psf_eq'  'sdt'  1  1  1
	$(BUILDDIR)/utilities/section_to_VTK 'section/psf_z0.3'  'sdt.gz'  1  1  1
	gunzip -f section/zm_y0.1.vtk.gz
	
	echo 'Convert isosurface data'
	$(BUILDDIR)/utilities/section_to_VTK 'isosurface/iso_w10n' 'sfm.gz'   1  1  1
	$(BUILDDIR)/utilities/section_to_VTK 'isosurface/iso_w10p' 'sfm'  1  1  1
	gunzip -f isosurface/iso_T0.6.1.vtk.gz
	
	echo 'Compare section and isosurface data'
	$(BUILDDIR)/tests/compare_psf

clean:
	rm -f *.dat
	rm -f section/psf_* section/zm_*
	rm -f isosurface/iso_*

distclean: clean
