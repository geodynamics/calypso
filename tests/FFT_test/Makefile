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

GMAKE = make

include $(MAKEDIR)/Makefile.header

TARGET_CMPR_FFT_TEST =     $(CURRENTDIR)/compare_FFT_test

TARGET_RTP_FFTPACK_TEST =  $(CURRENTDIR)/test_rtp_FFTPACK
TARGET_PRT_FFTPACK_TEST =  $(CURRENTDIR)/test_prt_FFTPACK
TARGET_RTP_FFTW_TEST =     $(CURRENTDIR)/test_rtp_FFTW
TARGET_PRT_FFTW_TEST =     $(CURRENTDIR)/test_prt_FFTW
TARGET_RTP_SGL_FFTW_TEST = $(CURRENTDIR)/test_rtp_single_FFTW

TARGETS_FFTPACK_TEST = \
		$(TARGET_RTP_FFTPACK_TEST) \
		$(TARGET_PRT_FFTPACK_TEST) \
		$(TARGET_CMPR_FFT_TEST)

TARGETS_FFTW_TEST = \
		$(TARGET_RTP_FFTW_TEST) \
		$(TARGET_PRT_FFTW_TEST) \
		$(TARGET_RTP_SGL_FFTW_TEST)

TEST_LIST= test_fftpack

ifdef FFTW3_LIBS
  TEST_LIST+= test_fftw
endif


all: test

test: $(TEST_LIST)
	@echo 'FFT check finished'

test_fftpack:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_RTP_FFTPACK_TEST='$(TARGET_RTP_FFTPACK_TEST)' \
		TARGET_PRT_FFTPACK_TEST='$(TARGET_PRT_FFTPACK_TEST)' \
		TARGET_CMPR_FFT_TEST='$(TARGET_CMPR_FFT_TEST)' \
		$(TARGETS_FFTPACK_TEST)
	
	$(TARGET_RTP_FFTPACK_TEST) control_FFTPACK
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'FFTPACK_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside FFTPACK check'; \
		exit 1;\
	fi
	rm FFTPACK_test.dat
	
	$(TARGET_PRT_FFTPACK_TEST) control_FFTPACK
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'FFTPACK_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside FFTPACK check'; \
		exit 1;\
	fi
	rm FFTPACK_test.dat

test_fftw:
	cd $(MAKEDIR); \
		$(GMAKE) \
		TARGET_RTP_FFTW_TEST='$(TARGET_RTP_FFTW_TEST)' \
		TARGET_PRT_FFTW_TEST='$(TARGET_PRT_FFTW_TEST)' \
		TARGET_RTP_SGL_FFTW_TEST='$(TARGET_RTP_SGL_FFTW_TEST)' \
		$(TARGETS_FFTW_TEST)
	
	$(TARGET_RTP_FFTW_TEST) control_FFTW
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'FFTW_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi outside FFTW3 check'; \
		exit 1;\
	fi
	rm FFTW_test.dat
	
	$(TARGET_PRT_FFTW_TEST) control_FFTW
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'FFTW_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside FFTW3 check'; \
		exit 1;\
	fi
	rm FFTW_test.dat
	
	$(TARGET_RTP_SGL_FFTW_TEST) control_FFTW
	$(TARGET_CMPR_FFT_TEST) 'reference/fftpack_test.dat' 'FFTW_test.dat' > log.txt
	@if [ `tail -n 1 log.txt` != 0 ]; then\
		echo 'Failed Phi inside FFTW check'; \
		exit 1;\
	fi
	rm FFTW_test.dat



clean:
	rm -f *.dat log.txt
	rm -fr $(TARGETS_FFTPACK_TEST) $(TARGETS_FFTW_TEST)

distclean: clean
