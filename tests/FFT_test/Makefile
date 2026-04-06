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
TARGET_FFTPACK_TEST =  $(BUILDDIR)/tests/test_FFTPACK
TARGET_FFW3_F_TEST =   $(BUILDDIR)/tests/test_FFTW3_f
TARGET_SGL_FFW3_TEST = $(BUILDDIR)/tests/test_single_FFTW3
TARGET_CMPR_FFT_TEST = $(BUILDDIR)/tests/compare_fft_test

all: test

test: 
	make clean
	
	$(TARGET_FFTPACK_TEST)
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'fftpack_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Gauss points check'; \
		exit 1;\
	fi
	
	$(TARGET_FFW3_F_TEST)
	$(TARGET_CMPR_FFT_TEST) 'fftpack_test.dat' 'mul_fftw_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed FFTW3 check'; \
		exit 1;\
	fi
	
	$(TARGET_SGL_FFW3_TEST)
	$(TARGET_CMPR_FFT_TEST) 'fftpack_test.dat' 'sgl_fftw_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed single FFTW3 check'; \
		exit 1;\
	fi
	echo 'FFT check finished'


clean:
	rm -f *.dat log.txt

distclean: clean
