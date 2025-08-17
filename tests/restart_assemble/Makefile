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
#
# Command settings
#
MPIRUN= mpirun   
TARGET_GEN_SPH_GRID= $(BUILDDIR)/utilities/gen_sph_grids
TARGET_NEWSPH=       $(BUILDDIR)/utilities/assemble_sph
TARGET_CMP_SPH_RST=  $(BUILDDIR)/tests/compare_sph_restart
#
#  -------------------------------------------------------------------------
#


all: test

test: 
	@echo "# Test restart data assemble in ascii directory..."
	@cd ascii; $(GMAKE) \
		MPIRUN="$(MPIRUN)" TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test
	
	@echo "# Test restart data assemble in binary directory..."
	@cd binary; $(GMAKE) \
		MPIRUN="$(MPIRUN)" TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test
	
	@echo "# Test restart data assemble in gzip directory..."
	@cd gzip; $(GMAKE) \
		MPIRUN="$(MPIRUN)" TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test
	
	@echo "# Test restart data assemble in binary_gz directory..."
	@cd binary_gz; $(GMAKE) \
		MPIRUN="$(MPIRUN)" TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test
	
	@echo "# Test restart data assemble in merged directory..."
	@cd merged; $(GMAKE) \
		MPIRUN="$(MPIRUN)" TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test
	
	@echo "# Test restart data assemble in merged_bin directory..."
	@cd merged_bin; $(GMAKE) \
		MPIRUN="$(MPIRUN)" TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test
	
	@echo "# Test restart data assemble in merged_gz directory..."
	@cd merged_gz; $(GMAKE) \
		MPIRUN="$(MPIRUN)" TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test
	
	@echo "# Test restart data assemble in merged_bin_gz directory..."
	@cd merged_bin_gz; $(GMAKE) \
		MPIRUN="$(MPIRUN)" TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test
	
	rm -f *.dat
	rm -f *~ *.f90

clean:
	cd ascii; make clean
	cd binary; make clean
	cd gzip; make clean
	cd binary_gz; make clean
	
	cd merged; make clean
	cd merged_bin; make clean
	cd merged_gz; make clean
	cd merged_bin_gz; make clean
	
	rm -f *.dat
	rm -f *~ *.f90

distclean: clean
