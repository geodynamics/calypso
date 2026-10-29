#
#  Makefile for Dynamo toolkit Calypso
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
NP = 6

TARGET_NEWSPH=      $(SRCDIR)/bin/utilities/assemble_sph
TARGET_CMP_SPH_RST= $(CURRENTDIR)/compare_sph_restart
#
#

all: test

target:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_CMP_SPH_RST='$(TARGET_CMP_SPH_RST)' \
		$(TARGET_CMP_SPH_RST)

test:  target
	rm -f *.dat *.txt restart/rst.*
	$(MPIRUN) -np $(NP) $(TARGET_NEWSPH)  >> log.txt
	$(MPIRUN) -np $(NP) $(TARGET_CMP_SPH_RST) >> log.txt
	@if [ `tail -n 1 flag.txt` != 0 ]; then\
		echo 'Failed gzipped BINARY Data assemble check'; \
		exit 1;\
	fi
	@echo 'Gzipped BINARY Data assemble check finished'

clean:
	rm -f *.dat *.txt restart/rst.*

distclean: clean
