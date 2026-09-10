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
TARGET_SPH_PICK_MODE_CHECK = $(CURRENTDIR)/sph_picked_mode_check
TARGET_SPH_REF_CHECK =       $(CURRENTDIR)/sph_ref_field_check

TARGETS= \
$(TARGET_SPH_PICK_MODE_CHECK) \
$(TARGET_SPH_REF_CHECK)

#
GMAKE = make
MPIRUN = mpirun
NP= 1

all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_SPH_PICK_MODE_CHECK='$(TARGET_SPH_PICK_MODE_CHECK)' \
		TARGET_SPH_REF_CHECK='$(TARGET_SPH_REF_CHECK)' \
		$(TARGETS)

test: dataclean targets
	$(MPIRUN) -np $(NP) $(TARGET_SPH_MHD)
	
	@sync
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l0_m0c  picked_mode_l0_m0c >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{0}^{0} data check'; \
		exit 1;\
	fi
	@sync
	$(TARGET_SPH_REF_CHECK)  reference/reference_fields.dat     reference_fields.dat >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed refernce field data (reference_fields.dat) check'; \
		exit 1;\
	fi
	@echo 'Check of static reference scalar fields by source data file is finished'


dataclean:
	rm -f *~ *.txt sph_ave_volume_s.dat picked_mode_l0_m0c.dat
	rm -f radial_point.dat reference_fields.dat time_total.dat
	rm -f rst_1/rst.*

clean: dataclean
	rm -rf $(TARGET_SPH_PICK_MODE_CHECK)* \
	$(TARGET_SPH_REF_CHECK)*

distclean: clean
