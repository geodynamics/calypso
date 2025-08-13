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
SRCDIR = /Users/matsui/src_kemo
#
MPIRUN = mpirun
TARGET_SPH_SNAP =      $(SRCDIR)/bin/sph_snapshot
TARGET_COMPARE_PSF =   $(SRCDIR)/bin/tests/compare_psf
TARGET_PSF_2_VTK =     $(SRCDIR)/bin/utilities/section_to_VTK
#

all: test

test: 
	make clean
	
	$(MPIRUN) -np 4 $(TARGET_SPH_SNAP)
	
	echo 'Convert section data'
	$(TARGET_PSF_2_VTK)  'sections/psf_eq'    'sdt'     2  3  1
	$(TARGET_PSF_2_VTK)  'sections/psf_z0.3'  'sdt.gz'  2  3  1
	gunzip -f sections/zm_y0.*.vtk.gz
	
	echo 'Convert isosurface data'
	$(TARGET_PSF_2_VTK)  'isosurfaces/iso_w10n' 'sfm.gz'  2  3  1
	$(TARGET_PSF_2_VTK)  'isosurfaces/iso_w10p' 'sfm'     2  3  1
	gunzip -f isosurfaces/iso_T0.6.*.vtk.gz
	
	echo 'Compare sections and isosurfaces data'
	$(TARGET_COMPARE_PSF)
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed surfaceing module data output'; \
		exit 1;\
	fi
	@echo 'Snapshot program check finished'

clean:
	rm -f *.dat *.txt
	rm -f sections/psf_* sections/zm_*
	rm -f isosurfaces/iso_*
	rm -f monitor/*.dat monitor/*.dat.gz

distclean: clean
