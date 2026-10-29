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
MAKEDIR = $(SRCDIR)/work
BUILDDIR = $(SRCDIR)/bin
#
TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd
TARGET_SPH_ENE_CHECK =       $(CURRENTDIR)/sph_ene_check
TARGET_SPH_PICK_MODE_CHECK = $(CURRENTDIR)/sph_picked_mode_check

TARGETS= \
$(TARGET_SPH_PICK_MODE_CHECK) \
$(TARGET_SPH_ENE_CHECK)

#
GMAKE = make
MPIRUN = mpirun
NP= 2

all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_SPH_PICK_MODE_CHECK='$(TARGET_SPH_PICK_MODE_CHECK)' \
		TARGET_SPH_ENE_CHECK='$(TARGET_SPH_ENE_CHECK)' \
		$(TARGETS)

test: dataclean targets
	$(MPIRUN) -np $(NP) $(TARGET_SPH_MHD)
	
	@sync
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l0_m0c  monitor/picked_mode_l0_m0c >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{0}^{0} data check'; \
		exit 1;\
	fi
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l1_m1c  monitor/picked_mode_l1_m1c >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{1}^{1c} data check'; \
		exit 1;\
	fi
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l1_m1s  monitor/picked_mode_l1_m1s >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{1}^{1s} data check'; \
		exit 1;\
	fi
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l2_m0c  monitor/picked_mode_l2_m0c >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{2}^{0} data check'; \
		exit 1;\
	fi
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l2_m2c  monitor/picked_mode_l2_m2c >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{2}^{2c} data check'; \
		exit 1;\
	fi
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l2_m2s  monitor/picked_mode_l2_m2s >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{2}^{2s} data check'; \
		exit 1;\
	fi
	
	@echo 'Compare with reference mean square data'
	$(TARGET_SPH_ENE_CHECK)  reference/sph_ave_volume     monitor/sph_ave_volume    > log.txt
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

dataclean:
	rm -f *.dat *.txt rst_2/rst.*  sph_lm4r64c_2/in.*
	rm -f field/out.*
	rm -f monitor/*.dat
	rm -f rst_2/rst.*

clean: dataclean
	rm -rf $(TARGET_SPH_PICK_MODE_CHECK)* \
	$(TARGET_SPH_ENE_CHECK)*

distclean: clean
