!>@file   m_ISPACK1_labels.f90
!!@brief  module m_ISPACK1_labels
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Control labels for ISPACK
!!
!!@verbatim
!!      subroutine init_ISPACK1_mode_flags()
!!      integer(kind = kint) function find_set_ISPACK1_flag(label)
!!        character(len = kchara), intent(in) :: label
!!      character(len = kchara) function find_ISPACK1_label(iflag_fft)
!!        integer(kind = kint), intent(in) :: iflag_fft
!!      subroutine check_ISPACK1_mode_flags(id_file)
!!        integer(kind = kint), intent(in) :: id_file
!!   ------------------------------------------------------------------
!!    FFT Package lists
!!      ISPACK:                  ISPACK Ver.1
!!   ------------------------------------------------------------------
!!    FFT size flags
!|      once:         Call FFT once for all data transform
!|      domain:       Call FFT once for each spherical harmonic mode
!|      component:    Call FFT once for each components
!|      single:       Call FFT for each transform
!!   ------------------------------------------------------------------
!!@endverbatim
      module m_ISPACK1_labels
!
      use m_precision
      use m_FFT_selector
      use t_multi_flag_labels
!
      implicit none
!
!>      flag parts for ISPACK 0.97
      character(len = kchara), parameter, private                       &
     &   :: ISPACK_names(3) = (/'ISPACK   ', 'ISPACK1  ', 'ISPACK097'/)
!
!>     Character lables for at once ISPACKv0.97 for transform
!!        'ISPACK',          'ISPACK1',         'ISPACK097',
!!        'ISPACK_once',     'ISPACK_at_once',  'ISPACK1_once', 
!!        'ISPACK1_at_once', 'ISPACK097_once',  'ISPACK097_at_once',
!!        'once_ISPACK',     'once_ISPACK1',    'once_ISPACK097',
!!        'at_once_ISPACK',  'at_once_ISPACK1', 'at_once_ISPACK097' 
      type(multi_flag_labels), save :: at_once_ISPACK1_flags
!>     Character lables for once ISPACKv0.97 over domain
!!         'ISPACK_domain', 'ISPACK1_domain', 'ISPACK097_domain', 
!!         'domain_ISPACK', 'domain_ISPACK1', 'domain_ISPACK097' 
      type(multi_flag_labels), save :: domain_ISPACK1_flags
!>     Character lables for once ISPACKv0.97 over component
!!        'ISPACK_component', 'ISPACK_comps',      'ISPACK1_component',
!!        'ISPACK1_comps',    'ISPACK097_component', 'ISPACK097_comps',
!!        'component_ISPACK',    'component_ISPACK1', 
!!        'component_ISPACK097', 'comps_ISPACK',
!!        'comps_ISPACK1',       'comps_ISPACK097' 
      type(multi_flag_labels), save :: comp_ISPACK1_flags
!>     Character lables for single ISPACKv0.97
!!        'ISPACK_single', 'ISPACK_sgl',       'ISPACK1_single', 
!!        'ISPACK1_sgl',   'ISPACK097_single', 'ISPACK097_sgl', 
!!        'single_ISPACK', 'single_ISPACK1',   'single_ISPACK097',
!!        'sgl_ISPACK',    'sgl_ISPACK1',      'sgl_ISPACK097' 
      type(multi_flag_labels), save :: single_ISPACK1_flags
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_ISPACK1_mode_flags()
!
!>     Character lables for ISPACK 0.97: 'ISPACK', 'ISPACK097'
      type(multi_flag_labels), save :: ISPACK1_flags
!
      call init_multi_flags_by_labels(ithree, ISPACK_names,             &
     &                                ISPACK1_flags)
      call init_each_FFT_mode_flags(ISPACK1_flags,                      &
     &    at_once_ISPACK1_flags, domain_ISPACK1_flags,                  &
     &    comp_ISPACK1_flags, single_ISPACK1_flags)
!
      end subroutine init_ISPACK1_mode_flags
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function find_set_ISPACK1_flag(label)
!
      character(len = kchara), intent(in) :: label
      integer(kind = kint) :: iflag_fft
!
      if(check_mul_flags(label, at_once_ISPACK1_flags))     then
        iflag_fft = iflag_ISPACK0 + iflag_once_fft
      else if(check_mul_flags(label, domain_ISPACK1_flags)) then
        iflag_fft = iflag_ISPACK0 + iflag_domain_once
      else if(check_mul_flags(label, comp_ISPACK1_flags))   then
        iflag_fft = iflag_ISPACK0 + iflag_component_once
      else if(check_mul_flags(label, single_ISPACK1_flags)) then
        iflag_fft = iflag_ISPACK0 + iflag_single_fft
      else
        iflag_fft = -1
      end if
      find_set_ISPACK1_flag = iflag_fft
!
      end function find_set_ISPACK1_flag
!
! ----------------------------------------------------------------------
!
      character(len = kchara) function find_ISPACK1_label(iflag_fft)
!
      integer(kind = kint), intent(in) :: iflag_fft
      character(len = kchara) :: tmpchara
!
      tmpchara = 'NONE'
      if(mod(iflag_fft,10) .eq. iflag_once_fft) then
        tmpchara = at_once_ISPACK1_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_domain_once) then
        tmpchara = domain_ISPACK1_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_component_once) then
        tmpchara = comp_ISPACK1_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_single_fft) then
        tmpchara = single_ISPACK1_flags%flags(1)
      end if
      find_ISPACK1_label = tmpchara
!
      end function find_ISPACK1_label
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_ISPACK1_mode_flags(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara) :: title
!
      write(title,'(4x,a)') 'at_once_ISPACK1_flags'
      call write_multi_flags(id_file, title, at_once_ISPACK1_flags)
      write(title,'(4x,a)') 'domain_ISPACK1_flags'
      call write_multi_flags(id_file, title, domain_ISPACK1_flags)
      write(title,'(4x,a)') 'comp_ISPACK1_flags'
      call write_multi_flags(id_file, title, comp_ISPACK1_flags)
      write(title,'(4x,a)') 'single_ISPACK1_flags'
      call write_multi_flags(id_file, title, single_ISPACK1_flags)
!
      end subroutine check_ISPACK1_mode_flags
!
! ----------------------------------------------------------------------
!
      end module m_ISPACK1_labels
