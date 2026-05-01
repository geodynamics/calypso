!>@file   m_FFT_selector.F90
!!@brief  module m_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!   ------------------------------------------------------------------
!!      FFT Package lists
!!
!|      FFTPACK:                 FFTPACK5.11d
!!      ISPACK:                  ISPACK Ver.1
!!      ISPACK3:                 ISPACK Ver.3
!!      FFTW,     FFTW3:         FFTW3
!!      OMP_FFTW, OMP_FFTW3:     FFTW3 with OpenMP parallelization
!!      rocFFT, rocFFT_complex:  AMD rocFFT
!!      rocFFT_real:             AMD rocFFT with real data only
!!      OpenMP_rocFFT:           AMD rocFFT with OpenMP offloading
!!   ------------------------------------------------------------------
!!      FFT size flags
!!
!|      once:         Call FFT once for all data transform
!|      domain:       Call FFT once for each spherical harmonic mode
!|      component:    Call FFT once for each components
!|      single:       Call FFT for each transform
!!   ------------------------------------------------------------------
!!
!!       Current broken mode:
!!     FFT_library_ctl    'FFTW_COMPONENT'
!!     FFT_library_ctl    'FFTW_SINGLE'
!!     FFT_library_ctl    'FFTW_DOMAIN'
!!     FFT_library_ctl    'FFTPACK_COMPONENT'
!!     FFT_library_ctl    'FFTPACK_SINGLE'
!!     FFT_library_ctl    'ISPACK3_SINGLE'
!!     FFT_library_ctl    'ISPACK3_COMPONENT'
!!
!!@endverbatim
!!
      module m_FFT_selector
!
      use m_precision
      use m_constants
      use t_multi_flag_labels
!
      implicit none
!

!>      Character flag to sarch fastest FFT
      character(len = kchara), parameter                                &
     &          :: hd_search_fastest_fft = 'Search_fastest'
!
!>      Character flag to use test FFT
      character(len = kchara), parameter :: hd_FFT_TEST =  'TEST'
!
!>      Character flag to use FFTPACK
      character(len = kchara), parameter :: hd_FFTPACK =  'FFTPACK'
!>      Character flag to use FFTPACK5
      character(len = kchara), parameter :: hd_FFTPACK5 = 'FFTPACK5'
!
!>      Character flag to use FFTW3
      character(len = kchara), parameter :: hd_FFTW =     'FFTW'
!>      Character flag to use FFTW3
      character(len = kchara), parameter :: hd_FFTW3 =    'FFTW3'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_OMP_FFTW =  'OMP_FFTW'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_OMP_FFTW3 = 'OMP_FFTW3'
!
!>      Character flag to use ISPACK
      character(len = kchara), parameter :: hd_ISPACK =   'ISPACK'
!
!>     Character lables for at once FFT:  'once'
      type(multi_flag_labels), save :: at_once_FFT_flags
!>     Character lables for once FFT over domain:  'domain'
      type(multi_flag_labels), save :: domain_FFT_flags
!>     Character lables for once FFT over component:
!!                                     'component',  'comps'
      type(multi_flag_labels), save :: comp_FFT_flags
!>     Character lables for single FFT:            'single',  'sgl'
      type(multi_flag_labels), save :: single_FFT_flags
!
!
!
!>      Character flag to use single FFTPACK5
      character(len = kchara), parameter                                &
     &                            :: hd_FFTPACK_S = 'FFTPACK_SINGLE'
!>      Character flag to use FFTPACK5 for each component
      character(len = kchara), parameter                                &
     &                            :: hd_FFTPACK_C = 'FFTPACK_COMPONENT'
!>      Character flag to use FFTPACK5 for each domain
      character(len = kchara), parameter                                &
     &                            :: hd_FFTPACK_D = 'FFTPACK_DOMAIN'
!>      Character flag to use FFTPACK5 at once
      character(len = kchara), parameter                                &
     &                            :: hd_FFTPACK_O = 'FFTPACK_ONCE'
!
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
     &                               :: hd_FFTW_C =   'FFTW_COMPONENT'
!>      Character flag to use FFTW3 for all components
      character(len = kchara), parameter                                &
     &                               :: hd_FFTW3_C =  'fftw3_component'
!>      Character flag to use FFTW3 for all components
      character(len = kchara), parameter                                &
     &                               :: hd_FFTW_O =   'FFTW_ONCE'
!>      Character flag to use FFTW3 for all components
      character(len = kchara), parameter                                &
     &                               :: hd_FFTW3_O =  'FFTW3_ONCE'
!
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter                                &
     &                           :: hd_OMP_FFTW_D =  'OMP_FFTW_DOMAIN'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter                                &
     &                           :: hd_OMP_FFTW3_D = 'OMP_FFTW3_DOMAIN'
!
!>      Character flag to use ISPACK for domain
      character(len = kchara), parameter                                &
     &                               :: hd_ISPACK_D =  'ISPACK_DOMAIN'
!>      Character flag to use ISPACK at once
      character(len = kchara), parameter                                &
     &                               :: hd_ISPACK_O =  'ISPACK_ONCE'
!
!>      Character flag to use ISPACK for domain
      character(len = kchara), parameter                                &
     &                           :: hd_ISPACK3_D =  'ISPACK3_DOMAIN'
!>      Character flag to use ISPACK for component
      character(len = kchara), parameter                                &
     &                           :: hd_ISPACK3_C =  'ISPACK3_COMPONENT'
!>      Character flag to use single ISPACK
      character(len = kchara), parameter                                &
     &                           :: hd_ISPACK3_S =  'ISPACK3_SINGLE'
!>      Character flag to use ISPACK at once
      character(len = kchara), parameter                                &
     &                           :: hd_ISPACK3_O =  'ISPACK3_ONCE'
!
!
!
!
!
!!>      integer flag for undefined FFT routine
!      integer(kind = kint), parameter :: iflag_UNDEFINED_FFT =   -999
!>      integer flag for fastest FFT search
      integer(kind = kint), parameter :: iflag_SEARCH_FASTEST_FFT = -1
!
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK =      50
!>      integer flag to use FFTW3
      integer(kind = kint), parameter :: iflag_FFTW =         10
!>      integer flag to use FFTW3 with OpenMP
      integer(kind = kint), parameter :: iflag_OMP_FFTW =     40
!>      integer flag to use ISPACK Ver.0.93
      integer(kind = kint), parameter :: iflag_ISPACK0 =      20
!>      integer flag to use ISPACK Ver.3
      integer(kind = kint), parameter :: iflag_ISPACK3 =      30
!>      integer flag to use rocFFT
      integer(kind = kint), parameter :: iflag_rocFFT =       60
!>      integer flag to use rocFFT only with real value
      integer(kind = kint), parameter :: iflag_real_rocFFT =  70
!>      integer flag to use rocFFT with OpenMP
      integer(kind = kint), parameter :: iflag_OMP_rocFFT =   80
!
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_once_fft =       1
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_single_fft =     2
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_component_once = 3
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_domain_once =    4
!
!
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK_ONCE =        51
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK_SINGLE =      52
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK_COMPONENT =   53
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK_DOMAIN =      54
!
!>      integer flag to use FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_ONCE =          11
!>      integer flag to use single transforms in FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_SINGLE =        12
!>      integer flag to use single transforms in FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_COMPONENT =     13
!>      integer flag to use FFTW3 for each component
      integer(kind = kint), parameter :: iflag_FFTW_DOMAIN =        14
!
!>      integer flag to use FFTW3 with OpenMP at once
      integer(kind = kint), parameter :: iflag_OMP_FFTW_ONCE =      41
!>      integer flag to use FFTW3 with OpenMP for domain
      integer(kind = kint), parameter :: iflag_OMP_FFTW_DOMAIN =    42
!
!>      integer flag to use ISPACK Ver.0.93 at once
      integer(kind = kint), parameter :: iflag_ISPACK1_ONCE =       21
!>      integer flag to use ISPACK Ver.0.93 for domain
      integer(kind = kint), parameter :: iflag_ISPACK1_DOMAIN =     22
!
!>      integer flag to use ISPACK Ver. 3.01
      integer(kind = kint), parameter :: iflag_ISPACK3_ONCE =       31
!>      integer flag to use ISPACK Ver. 3.01 for domain
      integer(kind = kint), parameter :: iflag_ISPACK3_DOMAIN =     32
!>      integer flag to use ISPACK Ver. 3.01 for domain
      integer(kind = kint), parameter :: iflag_ISPACK3_COMPONENT =  33
!>      integer flag to use ISPACK Ver. 3.01 for domain
      integer(kind = kint), parameter :: iflag_ISPACK3_SINGLE =     34
!
!>      integer flag to use test FFT
      integer(kind = kint), parameter :: iflag_FFT_TEST =    99
!
!      private :: hd_FFTPACK
!      private :: hd_FFTW, hd_FFTW3, hd_FFTW_S, hd_FFTW3_S
!      private :: hd_FFTW_D, hd_FFTW3_D
!      private :: hd_ISPACK, hd_ISPACK3, hd_FFT_TEST
!      private :: hd_OMP_FFTW,  hd_OMP_FFTW_D
!      private :: hd_OMP_FFTW3, hd_OMP_FFTW3_D
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_each_FFT_mode_flags(base_FFT_flags,               &
     &          at_once_TGT_flags, domain_TGT_flags,                    &
     &          comp_TGT_flags, single_TGT_flags)
!
      type(multi_flag_labels), intent(in) :: base_FFT_flags
      type(multi_flag_labels), intent(inout) :: at_once_TGT_flags
      type(multi_flag_labels), intent(inout) :: domain_TGT_flags
      type(multi_flag_labels), intent(inout) :: comp_TGT_flags
      type(multi_flag_labels), intent(inout) :: single_TGT_flags
!
      type(multi_flag_labels) :: tmp_flags
      integer(kind = kint) :: icou
!
      call alloc_multi_flags(izero, at_once_TGT_flags)
      call append_multi_flag_labels(base_FFT_flags, at_once_TGT_flags)
      call init_from_two_kinds_flags(base_FFT_flags, at_once_FFT_flags, &
     &                               tmp_flags, icou)
      call append_multi_flag_labels(tmp_flags, at_once_TGT_flags)
      call dealloc_multi_flags(tmp_flags)
!
      call init_from_two_kinds_flags(base_FFT_flags, domain_FFT_flags,  &
     &                               domain_TGT_flags, icou)
      call init_from_two_kinds_flags(base_FFT_flags, comp_FFT_flags,    &
     &                               comp_TGT_flags, icou)
      call init_from_two_kinds_flags(base_FFT_flags, single_FFT_flags,  &
     &                               single_TGT_flags, icou)
!
      end subroutine init_each_FFT_mode_flags
!
! ------------------------------------------------------------------
!
      end module m_FFT_selector
