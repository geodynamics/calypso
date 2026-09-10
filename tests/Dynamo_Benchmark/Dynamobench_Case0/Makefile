#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
BUILDDIR = $(SRCDIR)/bin
MAKEDIR = $(SRCDIR)/work
#
CURRENTDIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

TARGET_SPH_MHD =       $(BUILDDIR)/sph_mhd
#
TARGET_SPH_ENE_CHECK = $(CURRENTDIR)/sph_ene_check
#
GMAKE= make
MPIRUN= mpirun
NP = 4

all: test

targets: $(TARGET_SPH_INITIAL)
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_SPH_ENE_CHECK='$(TARGET_SPH_ENE_CHECK)' \
		$(TARGET_SPH_ENE_CHECK)

test: targets
	rm -f *.dat *.txt rst_4/rst.* monitor/*.dat field/out.*
	$(MPIRUN) -np $(NP) $(TARGET_SPH_MHD)
	@sync
	@echo 'Compare with reference mean square data'
	$(TARGET_SPH_ENE_CHECK)  reference/dynamobench       monitor/dynamobench > log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed dynamo benchmark data check'; \
		exit 1;\
	fi
	$(TARGET_SPH_ENE_CHECK)  reference/sph_pwr_volume_s  monitor/sph_pwr_volume_s  >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed volume mean square data check'; \
		exit 1;\
	fi
	$(TARGET_SPH_ENE_CHECK)  reference/sph_pwr_volume_m0 monitor/sph_pwr_volume_m0 >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed axisymmetric mean square data check'; \
		exit 1;\
	fi
	@echo 'Dynamo Benchmark Case 0 check finished'

clean:
	rm -f *.dat *.txt rst_4/rst.* monitor/*.dat field/out.*
	rm -rf $(TARGET_SPH_ENE_CHECK)*

distclean: clean
