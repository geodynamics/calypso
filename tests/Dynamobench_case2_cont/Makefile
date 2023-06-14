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
	$(BUILDDIR)/utilities/t_ave_monitor_data >>& log.txt
	echo 'Compare with reference mean square data'
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_ave_volume    monitor/sph_ave_volume >>& log.txt
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_pwr_volume_s  monitor/sph_pwr_volume_s >>& log.txt
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_pwr_volume_m0 monitor/sph_pwr_volume_m0 >>& log.txt
	$(BUILDDIR)/tests/sph_ene_check  reference/gauss_coefs       monitor/gauss_coefs >>& log.txt
	$(BUILDDIR)/tests/sph_ene_check  reference/dipolarity        monitor/dipolarity >>& log.txt
	$(BUILDDIR)/tests/sph_ene_check  reference/dynamobench       monitor/dynamobench >>& log.txt
	$(BUILDDIR)/tests/sph_ene_check  reference/Nusselt           monitor/Nusselt >>& log.txt
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_ave_convective    monitor/sph_ave_convective >>& log.txt
	$(BUILDDIR)/tests/sph_ene_check  reference/sph_pwr_convective_s    monitor/sph_pwr_convective_s >>& log.txt

clean:
	rm -f *.dat *.txt
	rm -f picked/*.dat
	rm -f sections/*.vtk
	rm -f monitor/*.dat
	rm -f monitor/*.dat.gz

distclean: clean
