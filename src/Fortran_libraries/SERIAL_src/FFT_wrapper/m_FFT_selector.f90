!>@file   m_FFT_selector.f90
!!@brief  module m_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      integer(kind = kint) function                                   &
!!     &                    set_fft_library_ctl(FFT_library_ctl)
!!      subroutine write_elapsed_4_FFT(i_mode, etime_fft)
!!      character(len = kchara) function chosen_fft_name(i_mode)
!!@endverbatim
!!
      module m_FFT_selector
!
      use m_precision
!
      implicit none
!
!>      Character flag to use FFTPACK5
      character(len = kchara), parameter :: hd_FFTPACK = 'FFTPACK'
!>      Character flag to use FFTPACK5
      character(len = kchara), parameter                                &
     &          :: hd_FFTPACK_S = 'FFTPACK_SINGLE'
!>      Character flag to use FFTPACK5
      character(len = kchara), parameter                                &
     &          :: hd_FFTPACK_C = 'FFTPACK_COMPONENT'
!>      Character flag to use FFTPACK5
      character(len = kchara), parameter                                &
     &          :: hd_FFTPACK_D = 'FFTPACK_DOMAIN'
!
!>      Character flag to use FFTW3
      character(len = kchara), parameter :: hd_FFTW =    'FFTW'
!>      Character flag to use FFTW3
      character(len = kchara), parameter :: hd_FFTW3 =    'fftw3'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_FFTW_D =  'FFTW_DOMAIN'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_FFTW3_D = 'fftw3_domain'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter :: hd_FFTW_S =  'FFTW_SINGLE'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter :: hd_FFTW3_S = 'fftw3_single'
!>      Character flag to use FFTW3 for all components
      character(len = kchara), parameter                                &
     &          :: hd_FFTW_C =   'FFTW_COMPONENT'
!>      Character flag to use FFTW3 for all components
      character(len = kchara), parameter                                &
     &          :: hd_FFTW3_C =  'fftw3_component'
!
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_OMP_FFTW =  'OMP_FFTW'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_OMP_FFTW3 = 'omp_fftw3'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter                                &
     &          :: hd_OMP_FFTW_D =  'OMP_FFTW_DOMAIN'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter                                &
     &          :: hd_OMP_FFTW3_D = 'omp_fftw3_domain'
!
!>      Character flag to use ISPACK
      character(len = kchara), parameter :: hd_ISPACK =   'ISPACK'
!>      Character flag to use ISPACK for domain
      character(len = kchara), parameter                                &
     &          :: hd_ISPACK_D =  'ISPACK_DOMAIN'
!
!>      Character flag to use ISPACK
      character(len = kchara), parameter :: hd_ISPACK3 =  'ISPACK3'
!>      Character flag to use ISPACK for domain
      character(len = kchara), parameter                                &
     &          :: hd_ISPACK3_D =  'ISPACK3_DOMAIN'
!>      Character flag to use ISPACK for component
      character(len = kchara), parameter                                &
     &          :: hd_ISPACK3_C =  'ISPACK3_COMPONENT'
!>      Character flag to use ISPACK for component
      character(len = kchara), parameter                                &
     &          :: hd_ISPACK3_S =  'ISPACK3_SINGLE'
!
!>      Character flag to use test FFT
      character(len = kchara), parameter :: hd_FFT_TEST =  'TEST'
!
!>      integer flag for undefined
      integer(kind = kint), parameter :: iflag_UNDEFINED_FFT =   0
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK =           1
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK_SINGLE =    2
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK_COMPONENT = 3
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK_DOMAIN =    4
!
!>      integer flag to use FFTW3
      integer(kind = kint), parameter :: iflag_FFTW =        11
!>      integer flag to use single transforms in FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_SINGLE = 12
!>      integer flag to use single transforms in FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_COMPONENT = 13
!>      integer flag to use FFTW3 for each component
      integer(kind = kint), parameter :: iflag_FFTW_DOMAIN = 14
!
!>      integer flag to use FFTW3 with OopenMP at once
      integer(kind = kint), parameter :: iflag_OMP_FFTW =        41
!>      integer flag to use FFTW3 with OopenMP for domain
      integer(kind = kint), parameter :: iflag_OMP_FFTW_DOMAIN = 42
!
!>      integer flag to use ISPACK Ver.0.93
      integer(kind = kint), parameter :: iflag_ISPACK1 =        21
!>      integer flag to use ISPACK Ver.0.93 for domain
      integer(kind = kint), parameter :: iflag_ISPACK1_DOMAIN = 22
!
!>      integer flag to use ISPACK Ver. 3.01
      integer(kind = kint), parameter :: iflag_ISPACK3 =           31
!>      integer flag to use ISPACK Ver. 3.01 for domain
      integer(kind = kint), parameter :: iflag_ISPACK3_DOMAIN =    32
!>      integer flag to use ISPACK Ver. 3.01 for domain
      integer(kind = kint), parameter :: iflag_ISPACK3_COMPONENT = 33
!>      integer flag to use ISPACK Ver. 3.01 for domain
      integer(kind = kint), parameter :: iflag_ISPACK3_SINGLE =    34
!
!>      integer flag to use test FFT
      integer(kind = kint), parameter :: iflag_FFT_TEST =    99
!
      private :: hd_FFTPACK, hd_FFTPACK_S, hd_FFTPACK_C, hd_FFTPACK_D
      private :: hd_FFTW, hd_FFTW3, hd_FFTW_S, hd_FFTW3_S
      private :: hd_ISPACK, hd_ISPACK_D
      private :: hd_ISPACK3, hd_ISPACK3_D, hd_ISPACK3_C, hd_ISPACK3_S
      private :: hd_FFTW_D, hd_FFTW3_D
      private :: hd_FFTW_C, hd_FFTW3_C, hd_FFT_TEST
      private :: hd_OMP_FFTW,  hd_OMP_FFTW_D
      private :: hd_OMP_FFTW3, hd_OMP_FFTW3_D
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    set_fft_library_ctl(FFT_library_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: FFT_library_ctl
!
!
      if     (cmp_no_case(FFT_library_ctl, hd_FFTPACK)) then
        set_fft_library_ctl = iflag_FFTPACK
      else if(cmp_no_case(FFT_library_ctl, hd_FFTPACK_S)) then
        set_fft_library_ctl = iflag_FFTPACK_SINGLE
      else if(cmp_no_case(FFT_library_ctl, hd_FFTPACK_C)) then
        set_fft_library_ctl = iflag_FFTPACK_COMPONENT
      else if(cmp_no_case(FFT_library_ctl, hd_FFTPACK_D)) then
        set_fft_library_ctl = iflag_FFTPACK_DOMAIN
!
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK)) then
        set_fft_library_ctl = iflag_ISPACK1
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK_D)) then
        set_fft_library_ctl = iflag_ISPACK1_DOMAIN
!
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK3)) then
        set_fft_library_ctl = iflag_ISPACK3
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK3_D)) then
        set_fft_library_ctl = iflag_ISPACK3_DOMAIN
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK3_C)) then
        set_fft_library_ctl = iflag_ISPACK3_COMPONENT
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK3_S)) then
        set_fft_library_ctl = iflag_ISPACK3_SINGLE
!
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW)                     &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3)) then
        set_fft_library_ctl = iflag_FFTW
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_S)                   &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_S)) then
        set_fft_library_ctl = iflag_FFTW_SINGLE
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_C)                   &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_C)) then
        set_fft_library_ctl = iflag_FFTW_COMPONENT
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_D)                   &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_D)) then
        set_fft_library_ctl = iflag_FFTW_DOMAIN
!
      else if(cmp_no_case(FFT_library_ctl, hd_OMP_FFTW)                 &
     &     .or. cmp_no_case(FFT_library_ctl, hd_OMP_FFTW3)) then
        set_fft_library_ctl = iflag_OMP_FFTW
!
      else if(cmp_no_case(FFT_library_ctl, hd_OMP_FFTW_D)               &
     &     .or. cmp_no_case(FFT_library_ctl, hd_OMP_FFTW3_D)) then
        set_fft_library_ctl = iflag_OMP_FFTW_DOMAIN
!
      else if(cmp_no_case(FFT_library_ctl, hd_FFT_TEST)) then
        set_fft_library_ctl = iflag_FFT_TEST
      end if
!
      end function set_fft_library_ctl
!
! ------------------------------------------------------------------
!
      subroutine write_elapsed_4_FFT(i_mode, etime_fft)
!
      integer(kind = kint), intent(in) :: i_mode
      real(kind = kreal), intent(in) :: etime_fft
!
      if     (i_mode .eq. iflag_FFTPACK) then
        write(*,*) 'elapsed by FFTPACK                       (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTPACK_SINGLE) then
        write(*,*) 'elapsed by single FFTPACK                (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTPACK_COMPONENT) then
        write(*,*) 'elapsed by FFTPACK for all component     (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTPACK_DOMAIN) then
        write(*,*) 'elapsed by FFTPACK for domain            (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
!
      else if(i_mode .eq. iflag_FFTW) then
        write(*,*) 'elapsed by FFTW3 for each component      (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTW_SINGLE) then
        write(*,*) 'elapsed by single FFTW3                  (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTW_COMPONENT) then
        write(*,*) 'elapsed by FFTW3 for all component       (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTW_DOMAIN) then
        write(*,*) 'elapsed by FFTW3 for domain              (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
!
      else if(i_mode .eq. iflag_OMP_FFTW) then
        write(*,*) 'elapsed by FFTW3 with OpoenMP at once    (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_OMP_FFTW_DOMAIN) then
        write(*,*) 'elapsed by FFTW3 with OpoenMP for domain (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
!
      else if(i_mode .eq. iflag_ISPACK1) then
        write(*,*) 'elapsed by ISPACK V0.93                  (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK1_DOMAIN) then
        write(*,*) 'elapsed by ISPACK V0.93 for domain       (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
!
      else if(i_mode .eq. iflag_ISPACK3) then
        write(*,*) 'elapsed by ISPACK V3.0.1                 (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK3_DOMAIN) then
        write(*,*) 'elapsed by ISPACK V3.0.1 for domain      (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK3_COMPONENT) then
        write(*,*) 'elapsed by ISPACK V3.0.1 for component   (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK3_SINGLE) then
        write(*,*) 'elapsed by single ISPACK V3.0.1          (',        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      end if
!
      end subroutine write_elapsed_4_FFT
!
! ------------------------------------------------------------------
!
      character(len = kchara) function chosen_fft_name(i_mode)
!
      integer(kind = kint), intent(in) :: i_mode
!
      if     (i_mode .eq. iflag_FFTPACK) then
        chosen_fft_name = hd_FFTPACK
      else if(i_mode .eq. iflag_FFTPACK_SINGLE) then
        chosen_fft_name = hd_FFTPACK_S
      else if(i_mode .eq. iflag_FFTPACK_COMPONENT) then
        chosen_fft_name = hd_FFTPACK_C
      else if(i_mode .eq. iflag_FFTPACK_DOMAIN) then
        chosen_fft_name = hd_FFTPACK_D
!
      else if(i_mode .eq. iflag_FFTW) then
        chosen_fft_name = hd_FFTW
      else if(i_mode .eq. iflag_FFTW_SINGLE) then
        chosen_fft_name = hd_FFTW_S
      else if(i_mode .eq. iflag_FFTW_COMPONENT) then
        chosen_fft_name = hd_FFTW_C
      else if(i_mode .eq. iflag_FFTW_DOMAIN) then
        chosen_fft_name = hd_FFTW_D
!
      else if(i_mode .eq. iflag_OMP_FFTW) then
        chosen_fft_name = hd_OMP_FFTW
      else if(i_mode .eq. iflag_OMP_FFTW_DOMAIN) then
        chosen_fft_name = hd_OMP_FFTW_D
!
      else if(i_mode .eq. iflag_ISPACK1) then
        chosen_fft_name = hd_ISPACK
      else if(i_mode .eq. iflag_ISPACK1_DOMAIN) then
        chosen_fft_name = hd_ISPACK_D
!
      else if(i_mode .eq. iflag_ISPACK3) then
        chosen_fft_name = hd_ISPACK3
      else if(i_mode .eq. iflag_ISPACK3_DOMAIN) then
        chosen_fft_name = hd_ISPACK3_D
      else if(i_mode .eq. iflag_ISPACK3_COMPONENT) then
        chosen_fft_name = hd_ISPACK3_C
      else if(i_mode .eq. iflag_ISPACK3_SINGLE) then
        chosen_fft_name = hd_ISPACK3_S
!
      else if(i_mode .eq. iflag_FFT_TEST) then
        chosen_fft_name = hd_FFT_TEST
      end if
!
      end function chosen_fft_name
!
! ------------------------------------------------------------------
!
      end module m_FFT_selector
