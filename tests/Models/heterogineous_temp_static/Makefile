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
#

all: test

test: 
	make clean
	$(MPIRUN) -np 2 $(TARGET_SPH_MHD)
	
	echo 'Compare with reference mean square data'
	$(TARGET_SPH_ENE_CHECK)  reference/sph_ave_volume     monitor/sph_ave_volume    >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed volume average data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_ENE_CHECK)  reference/sph_pwr_volume_s   monitor/sph_pwr_volume_s  >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed volume mean square data check'; \
		exit 1;\
	fi
	
	$(TARGET_SPH_ENE_CHECK)  reference/sph_pwr_volume_m0  monitor/sph_pwr_volume_m0 >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed axisymmetric mean square data check'; \
		exit 1;\
	fi

clean:
	rm -f *.dat *.txt rst_2/rst.*  sph_lm4r64c_2/in.*
	rm -f field/out.*
	rm -f monitor/*.dat
	rm -f rst_2/rst.*

distclean: clean
