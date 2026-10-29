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

include $(MAKEDIR)/Makefile.header

SRC_SPH_INITIAL =    main_sph_initial_fld.f90
TARGET_SPH_INITIAL = $(CURRENTDIR)/sph_initial_field
#
TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd
TARGET_SPH_PICK_MODE_CHECK = $(CURRENTDIR)/sph_picked_mode_check
#
GMAKE = make
MPIRUN = mpirun
NP= 1

all: test

$(TARGET_SPH_INITIAL): $(SRC_SPH_INITIAL)
	$(F90) $(F90OPTFLAGS) -o $@ $< -L$(MAKEDIR) $(LIBS_SPH_MHD) $(F90LIBS)

targets: $(TARGET_SPH_INITIAL)
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_SPH_PICK_MODE_CHECK='$(TARGET_SPH_PICK_MODE_CHECK)' \
		$(TARGET_SPH_PICK_MODE_CHECK)

test: dataclean targets
	$(MPIRUN) -np $(NP) $(TARGET_SPH_INITIAL)
	$(MPIRUN) -np $(NP) $(TARGET_SPH_MHD)
	
	@sync
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l0_m0c  picked_mode_l0_m0c >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{0}^{0} data check'; \
		exit 1;\
	fi
	@echo 'Static scalar fields with source check is finished'


dataclean:
	rm -f *~ *.txt sph_ave_volume_s.dat picked_mode_l0_m0c.dat
	rm -f radial_point.dat time_total.dat
	rm -f rst_1/rst.*

clean: dataclean
	rm -fr $(TARGET_SPH_INITIAL)*
	rm -fr $(TARGET_SPH_PICK_MODE_CHECK)*

distclean: clean
