#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
BUILDDIR = $(SRCDIR)/bin
#
TARGET_CONTROL_MHD_CHECK = $(BUILDDIR)/utilities/check_control_mhd
#

all: test

test:
	make clean
	$(TARGET_CONTROL_MHD_CHECK) control_MHD > log.txt
	diff -w log.txt reference/control_checked.txt
	@echo 'Control file check test is succeessfully finished.'


clean:
	rm -f *.dat *.txt

distclean: clean
