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
BUILDDIR= $(SRCDIR)/bin
#
# Command settings
#
GMAKE= make
MPIRUN= mpirun   
TARGET_GEN_SPH_GRID= $(BUILDDIR)/utilities/gen_sph_grids
TARGET_NEWSPH=       $(BUILDDIR)/utilities/assemble_sph

TARGET_CMP_SPH_RST=  $(CURRENTDIR)/compare_sph_restart
#
#
SUBDIRS= \
ascii \
binary \
gzip \
binary_gz \
 \
merged \
merged_bin \
merged_gz \
merged_bin_gz

#
#  -------------------------------------------------------------------------
#


all: test

target:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_CMP_SPH_RST='$(TARGET_CMP_SPH_RST)' \
		$(TARGET_CMP_SPH_RST)

test:  target
	@for dir in $(SUBDIRS) ; do \
		( \
		echo '#'; \
		echo '# -------------------------------------------------------'; \
		echo '#   Test restart data assemble in' $${dir}  'directory...'; \
		echo '# -------------------------------------------------------'; \
		echo '#'; \
		cd $${dir}; $(GMAKE) \
			SRCDIR="$(SRCDIR)" \
			MAKEDIR="$(MAKEDIR)" \
			GMAKE="$(GMAKE)" \
			MPIRUN="$(MPIRUN)" \
			TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
			TARGET_NEWSPH="$(TARGET_NEWSPH)" \
			TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
			test \
		 ) \
	done
	
	rm -f *.dat
	rm -f *~ *.f90

clean:
	@for dir in $(SUBDIRS) ; do \
		( \
		cd $${dir}; \
		make clean \
		 ) \
	done
	
	rm -f *.dat
	rm -f *~ *.f90
	rm -fr $(TARGET_CMP_SPH_RST)*

distclean: clean
