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
BUILDDIR= $(SRCDIR)/bin
MAKEDIR = $(SRCDIR)/work
#
TARGET_SPH_MHD =       $(BUILDDIR)/sph_mhd
TARGET_SPH_ENE_CHECK = $(CURRENTDIR)/sph_ene_check
TARGET_COMPARE_PSF =   $(CURRENTDIR)/compare_psf

TARGETS= \
$(TARGET_SPH_ENE_CHECK) \
$(TARGET_COMPARE_PSF)
#
GMAKE = make
MPIRUN = mpirun
NP = 6
#

all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_SPH_ENE_CHECK='$(TARGET_SPH_ENE_CHECK)' \
		TARGET_COMPARE_PSF='$(TARGET_COMPARE_PSF)' \
		$(TARGETS)

test: dataclean targets
	$(MPIRUN) -np $(NP) $(TARGET_SPH_MHD)
	
	@sync
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
	
	$(TARGET_COMPARE_PSF)
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed sectioning data check'; \
		exit 1;\
	fi
	@echo 'Equatorially symmetric field check is finished'

dataclean:
	rm -f *.dat *.txt
	rm -f monitor/[A-Q,S-Z,a-q,s-z]*
	rm -f sections/[A-Q,S-Z,a-q,s-z]*
	rm -f isosurfaces/[A-Q,S-Z,a-q,s-z]*

clean: dataclean
	rm -rf $(TARGET_SPH_ENE_CHECK)*
	rm -rf $(TARGET_COMPARE_PSF)*


distclean: clean
