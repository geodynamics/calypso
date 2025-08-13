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
MHDDIR =  $(SRCDIR)/MHD
BUILDDIR= $(SRCDIR)/bin

all: test

test: 
	rm -f *.dat log.txt
	$(BUILDDIR)/tests/gauss_points < input.txt
	$(BUILDDIR)/tests/compare_gauss_points < compare.txt > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Gauss points check'; \
		exit 1;\
	fi
	echo 'Gauss points check finished'

clean:
	rm -f *.dat

distclean: clean
