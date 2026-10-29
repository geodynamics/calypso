#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
BUILDDIR= $(SRCDIR)/bin
MAKEDIR = $(SRCDIR)/work
#
GMAKE = make
MPIRUN = mpirun
TARGET_SPH_MHD = $(BUILDDIR)/sph_mhd
#
SUBDIRS = \
eq_symmetry_fields \
static_heat_source_by_rst \
static_heat_source_by_ref \
perturbation_scalars_by_ref \
perturbation_scalars_by_file \
static_heat_source_inner_core \
heterogineous_temp_static \
heterogineous_temp_evolve

#
#  -------------------------------------------------------------------------
#

all: test

test:
	@for dir in $(SUBDIRS); do \
		( \
		echo '#'; \
		echo '# ------------------------------------------------------'; \
		echo '#   Test in' $${dir} 'directory...'; \
		echo '# ------------------------------------------------------'; \
		echo '#'; \
		cd $${dir}; $(GMAKE) \
			GMAKE="$(GMAKE)" \
			MPIRUN="$(MPIRUN)" \
			SRCDIR="$(SRCDIR)" \
			MAKEDIR="$(MAKEDIR)" \
			BUILDDIR="$(BUILDDIR)" \
			TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
			test \
		) \
	done
	@rm -f *.o *.mod *~
	@echo 'Tests in Model folder are finished'


clean:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		clean; \
		) \
	done
	
	rm -f *.o *.mod *~

distclean: clean
