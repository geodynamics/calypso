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
BUILDDIR= $(SRCDIR)/bin
#

all: test

test: 
	rm -f *.dat
	$(BUILDDIR)/tests/test_FFTPACK
	$(BUILDDIR)/tests/compare_fft_test < compare_FFTPACK.txt > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Gauss points check'; \
		exit 1;\
	fi
	
	$(BUILDDIR)/tests/test_FFTW3_f
	$(BUILDDIR)/tests/compare_fft_test < compare_FFTW3.txt > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed FFTW3 check'; \
		exit 1;\
	fi
	
	$(BUILDDIR)/tests/test_single_FFTW3
	$(BUILDDIR)/tests/compare_fft_test < compare_single_FFTW3.txt > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed single FFTW3 check'; \
		exit 1;\
	fi
	echo 'FFT check finished'


clean:
	rm -f *.dat log.txt

distclean: clean
