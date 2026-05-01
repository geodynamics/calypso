!>@file   m_FFTPACK_labels.f90
!!@brief  module m_FFTPACK_labels
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Control labels for FFTPACK
!!
!!@verbatim
!!      subroutine init_FFTPACK_mode_flags()
!!      integer(kind = kint) function find_set_FFTPACK_flag(label)
!!        character(len = kchara), intent(in) :: label
!!      character(len = kchara) function find_FFTPACK_label(iflag_fft)
!!        integer(kind = kint), intent(in) :: iflag_fft
!!      subroutine check_FFTPACK_mode_flags(id_file)
!!        integer(kind = kint), intent(in) :: id_file
!!   ------------------------------------------------------------------
!!    FFT Package lists
!|      FFTPACK:                 FFTPACK5.11d
!!   ------------------------------------------------------------------
!!    FFT size flags
!|      once:         Call FFT once for all data transform
!|      domain:       Call FFT once for each spherical harmonic mode
!|      component:    Call FFT once for each components
!|      single:       Call FFT for each transform
!!   ------------------------------------------------------------------
!!@endverbatim
      module m_FFTPACK_labels
!
      use m_precision
      use m_FFT_selector
      use t_multi_flag_labels
!
      implicit none
!
!>      flag parts for FFTPACK
      character(len = kchara), parameter :: FFTPACK_names(2)            &
     &                               = (/'FFTPACK ', 'FFTPACK5'/)
!
!>     Character lables for at once FFTPACK5 for transform
!!        'FFTPACK', 'FFTPACK5', 'FFTPACK_once', 'FFTPACK_at_once', 
!!        'FFTPACK5_once', 'FFTPACK5_at_once', 'once_FFTPACK', 
!!        'once_FFTPACK5', 'at_once_FFTPACK', 'at_once_FFTPACK5' 
      type(multi_flag_labels), save :: at_once_FFTPACK_flags
!>     Character lables for once FFTPACK5 over domain
!!        'FFTPACK_domain', 'FFTPACK5_domain',
!!        'domain_FFTPACK', 'domain_FFTPACK5' 
      type(multi_flag_labels), save :: domain_FFTPACK_flags
!>     Character lables for once FFTPACK5 over component
!!        'FFTPACK_component',  'FFTPACK_comps', 'FFTPACK5_component',
!!        'FFTPACK5_comps', 'component_FFTPACK', 'component_FFTPACK5',
!!        'comps_FFTPACK',  'comps_FFTPACK5'
      type(multi_flag_labels), save :: comp_FFTPACK_flags
!>     Character lables for single FFTPACK5
!!        'FFTPACK_single',  'single_FFTPACK', 'FFTPACK5_single', 
!!        'single_FFTPACK5', 'FFTPACK5_sgl',   'sgl_FFTPACK5',
!!        'FFTPACK5_sgl',    'sgl_FFTPACK5'
      type(multi_flag_labels), save :: single_FFTPACK_flags
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_FFTPACK_mode_flags()
!
!>     Character lables for FFTPACK5: 'FFTPACK', 'FFTPACK5'
      type(multi_flag_labels) :: FFTPACK_flags
!
!
      call init_multi_flags_by_labels(itwo, FFTPACK_names,              &
     &                                FFTPACK_flags)
      call init_each_FFT_mode_flags(FFTPACK_flags,                      &
     &    at_once_FFTPACK_flags, domain_FFTPACK_flags,                  &
     &    comp_FFTPACK_flags, single_FFTPACK_flags)
!
      end subroutine init_FFTPACK_mode_flags
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function find_set_FFTPACK_flag(label)
!
      character(len = kchara), intent(in) :: label
!
      integer(kind = kint) :: iflag_fft
!
      if(check_mul_flags(label, at_once_FFTPACK_flags))     then
        iflag_fft = iflag_FFTPACK + iflag_once_fft
      else if(check_mul_flags(label, domain_FFTPACK_flags)) then
        iflag_fft = iflag_FFTPACK + iflag_domain_once
      else if(check_mul_flags(label, comp_FFTPACK_flags))   then
        iflag_fft = iflag_FFTPACK + iflag_component_once
      else if(check_mul_flags(label, single_FFTPACK_flags)) then
        iflag_fft = iflag_FFTPACK + iflag_single_fft
      else
        iflag_fft = -1
      end if
      find_set_FFTPACK_flag = iflag_fft
!
      end function find_set_FFTPACK_flag
!
! ----------------------------------------------------------------------
!
      character(len = kchara) function find_FFTPACK_label(iflag_fft)
!
      integer(kind = kint), intent(in) :: iflag_fft
      character(len = kchara) :: tmpchara
!
      tmpchara = 'NONE'
      if(mod(iflag_fft,10) .eq. iflag_once_fft) then
        tmpchara = at_once_FFTPACK_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_domain_once) then
        tmpchara = domain_FFTPACK_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_component_once) then
        tmpchara = comp_FFTPACK_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_single_fft) then
        tmpchara = single_FFTPACK_flags%flags(1)
      end if
      find_FFTPACK_label = tmpchara
!
      end function find_FFTPACK_label
!
! ----------------------------------------------------------------------
!
      subroutine check_FFTPACK_mode_flags(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara) :: title
!
!
      write(title,'(4x,a)') 'at_once_FFTPACK_flags'
      call write_multi_flags(id_file, title, at_once_FFTPACK_flags)
      write(title,'(4x,a)') 'domain_FFTPACK_flags'
      call write_multi_flags(id_file, title, domain_FFTPACK_flags)
      write(title,'(4x,a)') 'comp_FFTPACK_flags'
      call write_multi_flags(id_file, title, comp_FFTPACK_flags)
      write(title,'(4x,a)') 'single_FFTPACK_flags'
      call write_multi_flags(id_file, title, single_FFTPACK_flags)
!
      end subroutine check_FFTPACK_mode_flags
!
! ----------------------------------------------------------------------
!
      end module m_FFTPACK_labels
