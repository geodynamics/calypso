!>@file   m_FFT_selector.F90
!!@brief  module m_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine init_FFT_loop_mode_flags()
!!      subroutine init_each_FFT_mode_flags(base_FFT_flags,             &
!!     &          at_once_TGT_flags, domain_TGT_flags,                  &
!!     &          comp_TGT_flags, single_TGT_flags)
!!        type(multi_flag_labels), intent(in) :: base_FFT_flags
!!        type(multi_flag_labels), intent(inout) :: at_once_TGT_flags
!!        type(multi_flag_labels), intent(inout) :: domain_TGT_flags
!!        type(multi_flag_labels), intent(inout) :: comp_TGT_flags
!!        type(multi_flag_labels), intent(inout) :: single_TGT_flags
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
!>      Character flag for once transform over domain
      character(len = kchara), parameter, private                       &
     &                              :: hd_once_for_mode = 'domain'
!>      flag parts for once FFT over component
      character(len = kchara), parameter, private                       &
     &          :: at_once_FFT_names(2) = (/'once   ', 'at_once'/)
!>      flag parts for once FFT over component
      character(len = kchara), parameter, private                       &
     &          :: comps_FFT_names(2)  = (/'component', 'comps    '/)
!>      flag parts for single FFT
      character(len = kchara), parameter, private                       &
     &          :: single_FFT_names(2)  = (/'single', 'sgl   '/)
!
!
!>     Character lables for at once FFT:  'once'
      type(multi_flag_labels), save :: at_once_FFT_flags
!>     Character lables for once FFT over domain:  'domain'
      type(multi_flag_labels), private, save :: domain_FFT_flags
!>     Character lables for once FFT over component:
!!                                     'component',  'comps'
      type(multi_flag_labels), private, save :: comp_FFT_flags
!>     Character lables for single FFT:            'single',  'sgl'
      type(multi_flag_labels), private, save :: single_FFT_flags
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
!>      integer flag to use test FFT
      integer(kind = kint), parameter :: iflag_FFT_TEST =    99
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_FFT_loop_mode_flags()
!
!
      call init_multi_flags_by_labels(itwo, at_once_FFT_names,          &
     &                                at_once_FFT_flags)
      call init_multi_flags_by_one_label(hd_once_for_mode,              &
     &                                   domain_FFT_flags)
      call init_multi_flags_by_labels(itwo, comps_FFT_names,            &
     &                                comp_FFT_flags)
      call init_multi_flags_by_labels(itwo, single_FFT_names,           &
     &                                single_FFT_flags)
!
      end subroutine init_FFT_loop_mode_flags
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
