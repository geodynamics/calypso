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
	rm -f *.dat monitor/*.dat
	$(MPIRUN) -np 4 $(BUILDDIR)/sph_mhd
	$(BUILDDIR)/utilities/t_ave_monitor_data
	echo 'Compare with reference mean square data'
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_ave_volume    monitor/sph_ave_volume
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_pwr_volume_s  monitor/sph_pwr_volume_s
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_pwr_volume_m0 monitor/sph_pwr_volume_m0
	$(BUILDDIR)/tests/sph_ene_check  reference/gauss_coefs       monitor/gauss_coefs
	$(BUILDDIR)/tests/sph_ene_check  reference/dipolarity        monitor/dipolarity
	$(BUILDDIR)/tests/sph_ene_check  reference/dynamobench       monitor/dynamobench
	$(BUILDDIR)/tests/sph_ene_check  reference/Nusselt           monitor/Nusselt
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_ave_convective    monitor/sph_ave_convective
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_pwr_convective_s    monitor/sph_pwr_convective_s

clean:
	rm -f *.dat
	rm -f picked/*.dat
	rm -f sections/*.vtk
	rm -f monitor/*.dat
	rm -f monitor/*.dat.gz

distclean: clean
