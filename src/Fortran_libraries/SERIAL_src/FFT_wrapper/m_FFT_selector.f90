!>@file   m_FFT_selector.f90
!!@brief  module m_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine set_fft_library_ctl(FFT_library_ctl)
!!@endverbatim
!!
      module m_FFT_selector
!
      use m_precision
!
      implicit none
!
!>      Character flag to use FFTPACK5
      character(len = kchara), parameter :: hd_FFTPACK = 'fftpack'
!>      Character flag to use FFTW3
      character(len = kchara), parameter :: hd_FFTW =     'fftw'
!>      Character flag to use FFTW3
      character(len = kchara), parameter :: hd_FFTW3 =    'fftw3'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_FFTW_F =   'fftw_field'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_FFTW3_F =  'fftw3_field'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter :: hd_FFTW_S =   'fftw_single'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter :: hd_FFTW3_S =  'fftw3_single'
!>      Character flag to use ISPACK
      character(len = kchara), parameter :: hd_ISPACK =   'ispack'
!
!>      integer flag for undefined
      integer(kind = kint), parameter :: iflag_UNDEFINED_FFT =   0
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK =     1
!>      integer flag to use FFTW3
      integer(kind = kint), parameter :: iflag_FFTW =        2
!>      integer flag to use single transforms in FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_SINGLE = 3
!>      integer flag to use FFTW3 for each component
      integer(kind = kint), parameter :: iflag_FFTW_FIELD =  4
!>      integer flag to use ISPACK
      integer(kind = kint), parameter :: iflag_ISPACK =      5
!
      integer(kind = kint) :: iflag_FFT = iflag_UNDEFINED_FFT
!
      private :: hd_FFTPACK, hd_FFTW, hd_FFTW3, hd_FFTW_S, hd_FFTW3_S
      private :: hd_ISPACK, hd_FFTW_F, hd_FFTW3_F
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine set_fft_library_ctl(FFT_library_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: FFT_library_ctl
!
!
      if     (cmp_no_case(FFT_library_ctl, hd_FFTPACK)) then
        iflag_FFT = iflag_FFTPACK
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK)) then
        iflag_FFT = iflag_ISPACK
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW)                     &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3)) then
        iflag_FFT = iflag_FFTW
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_S)                   &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_S)) then
        iflag_FFT = iflag_FFTW_SINGLE
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_F)                   &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_F)) then
        iflag_FFT = iflag_FFTW_FIELD
      end if
!
      end subroutine set_fft_library_ctl
!
! ------------------------------------------------------------------
!
      end module m_FFT_selector
