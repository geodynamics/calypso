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
	
	$(MPIRUN) -np 4 $(TARGET_SPH_MHD)
	
	@sync
	@echo 'Compare with reference mean square data'
	$(TARGET_SPH_ENE_CHECK)  reference/dynamobench         monitor/dynamobench > log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed dynamo benchmark data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_ENE_CHECK)  reference/sph_pwr_volume_s    monitor/sph_pwr_volume_s >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed dynamo benchmark data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_ENE_CHECK)  reference/sph_pwr_volume_m0   monitor/sph_pwr_volume_m0 >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed dynamo benchmark data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_ENE_CHECK)  reference/gauss_coefs         monitor/gauss_coefs >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed dynamo benchmark data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_ENE_CHECK)  reference/dipolarity          monitor/dipolarity >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed dynamo benchmark data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_ENE_CHECK)  reference/Nusselt             monitor/Nusselt >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed dynamo benchmark data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_ENE_CHECK)  reference/sph_pwr_convective_s    monitor/sph_pwr_convective_s >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed dynamo benchmark data check'; \
		exit 1;\
	fi
	
	$(TARGET_COMPARE_PSF)
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed sectioning data check'; \
		exit 1;\
	fi
	
	@echo 'Dynamo Benchmark Case 2 continue check finished'

clean:
	rm -f *.dat *.txt
	rm -f monitor/*.dat monitor/*.dat.gz
	rm -f sections/*.0.* sections/*.150?.*
	rm -f isosurfaces/*.0.* isosurfaces/*.150?.*

distclean: clean
