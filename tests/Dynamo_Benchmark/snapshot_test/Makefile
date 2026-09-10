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
BUILDDIR = $(SRCDIR)/bin
MAKEDIR = $(SRCDIR)/work
#
TARGET_SPH_SNAP =      $(BUILDDIR)/sph_snapshot

TARGET_COMPARE_PSF =   $(CURRENTDIR)/compare_psf
TARGET_PSF_2_VTK =     $(CURRENTDIR)/section_to_VTK

TARGETS= \
$(TARGET_COMPARE_PSF) \
$(TARGET_PSF_2_VTK)
#
GMAKE= make
MPIRUN= mpirun
NP = 4
#

all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_COMPARE_PSF='$(TARGET_COMPARE_PSF)' \
		TARGET_PSF_2_VTK='$(TARGET_PSF_2_VTK)' \
		$(TARGETS)

test: dataclean targets
	cd rst_4; ln -nsf ../../Dynamobench_Case2/rst_4/rst.* ./
	
	$(MPIRUN) -np $(NP) $(TARGET_SPH_SNAP)
	
	@echo 'Convert section data'
	$(TARGET_PSF_2_VTK)  'sections/psf_eq'    'sdt'     2  3  1
	$(TARGET_PSF_2_VTK)  'sections/psf_z0.3'  'sdt.gz'  2  3  1
	gunzip -f sections/zm_y0.*.vtk.gz
	
	@echo 'Convert isosurface data'
	$(TARGET_PSF_2_VTK)  'isosurfaces/iso_w10n' 'sfm.gz'  2  3  1
	$(TARGET_PSF_2_VTK)  'isosurfaces/iso_w10p' 'sfm'     2  3  1
	gunzip -f isosurfaces/iso_T0.6.*.vtk.gz
	
	@echo 'Compare sections and isosurfaces data'
	$(TARGET_COMPARE_PSF)
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed surfaceing module data output'; \
		exit 1;\
	fi
	@echo 'Snapshot program check finished'

dataclean:
	rm -f *.dat *.txt
	rm -f rst_4/rst.*
	rm -f sections/psf_* sections/zm_*
	rm -f isosurfaces/iso_*
	rm -f monitor/*.dat monitor/*.dat.gz

clean: dataclean
	rm -rf $(TARGET_COMPARE_PSF)*
	rm -rf $(TARGET_PSF_2_VTK)*

distclean: clean
