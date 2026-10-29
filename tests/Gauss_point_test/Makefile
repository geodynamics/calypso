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

TESTS_DIR =      $(SRCDIR)/MHD/programs/TESTS
GAUSS_TEST_DIR = $(TESTS_DIR)/Gauss_point_test

GMAKE = make

include $(MAKEDIR)/Makefile.header

TARGET_GAUSS_TEST =     $(CURRENTDIR)/gauss_points
TARGET_CMP_GAUSS_TEST = $(CURRENTDIR)/compare_gauss_points

TARGETS = \
$(TARGET_GAUSS_TEST) \
$(TARGET_CMP_GAUSS_TEST)

all: test

targets:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_GAUSS_TEST='$(TARGET_GAUSS_TEST)' \
		TARGET_CMP_GAUSS_TEST='$(TARGET_CMP_GAUSS_TEST)' \
		$(TARGETS)

test: dataclean targets
	$(TARGET_GAUSS_TEST) 32
	$(TARGET_CMP_GAUSS_TEST)  reference/gauss_point32.dat gauss_point32.dat > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Gauss points check'; \
		exit 1;\
	fi
	echo 'Gauss points check finished'

dataclean:
	rm -f *.dat log.txt

clean: dataclean
	rm -fr $(TARGETS)

distclean: clean
