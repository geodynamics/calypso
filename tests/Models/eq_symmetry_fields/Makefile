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
MPIRUN = mpirun
TARGET_SPH_MHD =       $(SRCDIR)/bin/sph_mhd
TARGET_SPH_ENE_CHECK = $(SRCDIR)/bin/tests/sph_ene_check
TARGET_COMPARE_PSF =   $(SRCDIR)/bin/tests/compare_psf
#

all: test

test: 
	make clean
	$(MPIRUN) -np 6 $(TARGET_SPH_MHD)
	
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


clean:
	rm -f *.dat *.txt
	rm -f monitor/[A-Q,S-Z,a-q,s-z]*
	rm -f sections/[A-Q,S-Z,a-q,s-z]*
	rm -f isosurfaces/[A-Q,S-Z,a-q,s-z]*

distclean: clean
