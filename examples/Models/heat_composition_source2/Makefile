#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
# Makefile for initial field construction
#

current_dir := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
include ../../../Makefile

KEMO_LIBS = -L/Users/matsui/src_kemo/work \
           -lkemo_MHD \
           -lkemo_viz \
           -lkemo_parallel \
           -lkemo_core \
           -lkemo_solver \
           -lkemo_c \
           -lfftpack.5d \
           -lispack3_fft \
           -lispack_fft

SRC_SPH_INITIAL =    main_sph_initial_fld.f90
TARGET_SPH_INITIAL = sph_initial_field

#
MPIRUN = mpirun
TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd
TARGET_SPH_ENE_CHECK =       $(BUILDDIR)/tests/sph_ene_check
TARGET_SPH_PICK_MODE_CHECK = $(BUILDDIR)/tests/sph_picked_mode_check
TARGET_COMPARE_PSF =         $(SRCDIR)/bin/tests/compare_psf
#

all: test

$(TARGET_SPH_INITIAL): $(SRC_SPH_INITIAL)
	$(F90) $(F90OPTFLAGS) -o $@ $< $(KEMO_LIBS) $(F90LIBS)

test: $(TARGET_SPH_INITIAL)
	echo $(current_dir)
	$(MPIRUN) -np 1 $(TARGET_SPH_INITIAL)
	$(MPIRUN) -np 1 $(TARGET_SPH_MHD)


clean:
	rm -f *.dat *.txt *~ 
	rm -f rst_1/rst.*
	rm -f monitor/*.dat monitor/*.dat.gz 
	rm -f field/out.*
	rm -fr $(TARGET_SPH_INITIAL) $(TARGET_SPH_INITIAL).dSYM

distclean: clean
