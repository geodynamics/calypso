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
	$(BUILDDIR)/psf_to_vtk < controls/psf_to_vtk_z0.txt
	$(BUILDDIR)/psf_to_vtk < controls/psf_to_vtk_z0.3.txt
	gunzip -f section/zm_y0.1.vtk.gz
	
	echo 'Convert isosurface data'
	$(BUILDDIR)/psf_to_vtk < controls/iso_to_vtk_wz10n.txt
	$(BUILDDIR)/psf_to_vtk < controls/iso_to_vtk_wz10p.txt
	gunzip -f isosurface/iso_T0.6.1.vtk.gz
	
	echo 'Compare section and isosurface data'
	diff section/psf_cmb.1.vtk ../Dynamobench_case2/section/psf_cmb.1.vtk
	diff section/psf_eq.1.vtk ../Dynamobench_case2/section/psf_eq.1.vtk
	diff section/psf_z0.3.1.vtk ../Dynamobench_case2/section/psf_z0.3.1.vtk
	diff isosurface/iso_T0.6.1.vtk ../Dynamobench_case2/isosurface/iso_T0.6.1.vtk
	diff isosurface/iso_w10n.1.vtk ../Dynamobench_case2/isosurface/iso_w10n.1.vtk
	diff isosurface/iso_w10p.1.vtk ../Dynamobench_case2/isosurface/iso_w10p.1.vtk

clean:
	rm -f *.dat
	rm -f section/psf_* section/zm_*
	rm -f isosurface/iso_*

distclean: clean
