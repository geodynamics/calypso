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

TARGET_GAUSS_TEST =     $(BUILDDIR)/tests/gauss_points
TARGET_CMP_GAUSS_TEST = $(BUILDDIR)/tests/compare_gauss_points

all: test

test: 
	make clean
	$(TARGET_GAUSS_TEST) < input.txt
	$(TARGET_CMP_GAUSS_TEST) < compare.txt > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Gauss points check'; \
		exit 1;\
	fi
	echo 'Gauss points check finished'

clean:
	rm -f *.dat log.txt

distclean: clean
