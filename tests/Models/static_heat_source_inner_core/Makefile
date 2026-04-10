#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SRCDIR = /Users/matsui/src_kemo
BUILDDIR = $(SRCDIR)/bin
#
MPIRUN = mpirun
TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd
TARGET_SPH_PICK_MODE_CHECK = $(BUILDDIR)/tests/sph_picked_mode_check
#

all: test

test:
	make clean
	$(MPIRUN) -np 1 $(TARGET_SPH_MHD)
	
	@sync
	$(TARGET_SPH_PICK_MODE_CHECK)  reference/picked_mode_l0_m0c  picked_mode_l0_m0c >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed Y_{0}^{0} data check'; \
		exit 1;\
	fi
	@echo 'Static scalar fields with inner core is finished'


clean:
	rm -f *~ *.txt sph_ave_volume_s.dat picked_mode_l0_m0c.dat
	rm -f radial_point.dat time_total.dat
	rm -f rst_1/rst.*

distclean: clean
