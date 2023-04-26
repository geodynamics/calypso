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
	$(BUILDDIR)/section_to_VTK < controls/psf_to_vtk_z0.txt
	$(BUILDDIR)/section_to_VTK < controls/psf_to_vtk_z0.3.txt
	gunzip -f section/zm_y0.1.vtk.gz
	
	echo 'Convert isosurface data'
	$(BUILDDIR)/section_to_VTK < controls/iso_to_vtk_wz10n.txt
	$(BUILDDIR)/section_to_VTK < controls/iso_to_vtk_wz10p.txt
	gunzip -f isosurface/iso_T0.6.1.vtk.gz
	
	echo 'Compare section and isosurface data'
	$(BUILDDIR)/tests/compare_psf

clean:
	rm -f *.dat
	rm -f section/psf_* section/zm_*
	rm -f isosurface/iso_*

distclean: clean
