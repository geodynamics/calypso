#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
CURRENTDIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

SRCDIR = /Users/matsui/src_kemo
BUILDDIR = $(SRCDIR)/bin
MAKEDIR = $(SRCDIR)/work
#
TARGET_SPH_MHD =       $(BUILDDIR)/sph_mhd
#
TARGET_SPH_ENE_CHECK = $(CURRENTDIR)/sph_ene_check
#
GMAKE= make
MPIRUN= mpirun
NP = 4
#

all: test

targets: $(TARGET_SPH_INITIAL)
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_SPH_ENE_CHECK='$(TARGET_SPH_ENE_CHECK)' \
		$(TARGET_SPH_ENE_CHECK)

test: dataclean targets
	$(MPIRUN) -np $(NP) $(TARGET_SPH_MHD)
	
	@sync
	@echo 'Compare with reference mean square data'
	$(TARGET_SPH_ENE_CHECK)  reference/dynamobench       monitor/dynamobench       >  log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed dynamo benchmark data check'; \
		exit 1;\
	fi
	$(TARGET_SPH_ENE_CHECK)  reference/sph_pwr_volume_s  monitor/sph_pwr_volume_s  >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed dynamo benchmark data check'; \
		exit 1;\
	fi
	$(TARGET_SPH_ENE_CHECK)  reference/sph_pwr_volume_m0 monitor/sph_pwr_volume_m0 >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed dynamo benchmark data check'; \
		exit 1;\
	fi
	$(TARGET_SPH_ENE_CHECK)  reference/gauss_coefs_Re    monitor/gauss_coefs_Re    >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed dynamo benchmark data check'; \
		exit 1;\
	fi
	@echo 'Dynamo Benchmark Case 2 check finished'

dataclean:
	rm -f *.dat *.txt
	rm -f monitor/*.dat
	rm -f rst_4/rst.*
	rm -f sections/*.vtk
	rm -f isosurfaces/*.vtk

clean: dataclean
	rm -rf $(TARGET_SPH_ENE_CHECK)*

distclean: clean
