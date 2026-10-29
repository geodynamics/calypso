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
BUILDDIR= $(SRCDIR)/bin
MAKEDIR = $(SRCDIR)/work
#
TARGET_SPH_MHD =       $(BUILDDIR)/sph_mhd
TARGET_SPH_SNAP =      $(BUILDDIR)/sph_snapshot

TARGET_SPH_ENE_CHECK = $(CURRENTDIR)/sph_ene_check
TARGET_COMPARE_PSF =   $(CURRENTDIR)/compare_psf
TARGET_PSF_2_VTK =     $(CURRENTDIR)/section_to_VTK

TARGETS= \
$(TARGET_SPH_ENE_CHECK) \
$(TARGET_COMPARE_PSF) \
$(TARGET_PSF_2_VTK)
#
GMAKE = make
MPIRUN = mpirun
#
SUBDIRS= \
Dynamobench_Case0 \
Dynamobench_Case1 \
Dynamobench_Case2 \
Dynamobench_Case2_cont \
snapshot_test

#
#  -------------------------------------------------------------------------
#

all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_SPH_ENE_CHECK='$(TARGET_SPH_ENE_CHECK)' \
		TARGET_COMPARE_PSF='$(TARGET_COMPARE_PSF)' \
		TARGET_PSF_2_VTK='$(TARGET_PSF_2_VTK)' \
		$(TARGETS)

test: targets
	@for dir in $(SUBDIRS); do \
		( \
		echo '#'; \
		echo '# -----------------------------------------------------------'; \
		echo '# Test in' $${dir} 'directory...'; \
		echo '# -----------------------------------------------------------'; \
		echo '#'; \
		cd $${dir}; \
		$(GMAKE) \
			GMAKE="$(GMAKE)" \
			MPIRUN="$(MPIRUN)" \
			SRCDIR="$(SRCDIR)" \
			MAKEDIR="$(MAKEDIR)" \
			BUILDDIR="$(BUILDDIR)" \
			TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
			TARGET_SPH_SNAP="$(TARGET_SPH_SNAP)" \
			TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
			TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" \
			TARGET_PSF_2_VTK="$(TARGET_PSF_2_VTK)" \
			test \
		) \
	done
	
	rm -f *.o *.mod *~

clean:
	@for dir in $(SUBDIRS); do \
	  (cd $${dir}; $(GMAKE) clean) \
	done
	
	rm -f *.o *.mod *~
	rm -fr $(TARGET_COMPARE_PSF)*
	rm -fr $(TARGET_SPH_ENE_CHECK)*
	rm -fr $(TARGET_PSF_2_VTK)*

distclean: clean
