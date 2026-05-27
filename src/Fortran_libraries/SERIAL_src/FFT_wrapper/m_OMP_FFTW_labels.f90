!>@file   m_OMP_FFTW_labels.f90
!!@brief  module m_OMP_FFTW_labels
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Control labels for FFTW
!!
!!@verbatim
!!      subroutine init_OMP_FFTW_flags(FFTW_flags)
!!        type(multi_flag_labels), intent(in) :: FFTW_flags
!!      integer(kind = kint) function find_set_OMP_FFTW_flag(label)
!!        character(len = kchara), intent(in) :: label
!!      character(len = kchara) function find_OMP_FFTW_label(iflag_fft)
!!        integer(kind = kint), intent(in) :: iflag_fft
!!      subroutine check_OMP_FFTW_mode_flags(id_file)
!!        integer(kind = kint), intent(in) :: id_file
!!   ------------------------------------------------------------------
!!    FFT Package lists
!!      OMP_FFTW, OMP_FFTW3:     FFTW3 with OpenMP parallelization
!!   ------------------------------------------------------------------
!!    FFT size flags
!|      once:         Call FFT once for all data transform
!|      domain:       Call FFT once for each spherical harmonic mode
!|      component:    Call FFT once for each components
!|      single:       Call FFT for each transform
!!   ------------------------------------------------------------------
!!@endverbatim
      module m_OMP_FFTW_labels
!
      use m_precision
      use m_FFT_selector
      use t_multi_flag_labels
!
      implicit none
!
!>      flag parts for OpenMP
      character(len = kchara), parameter, private :: OpenMP_names(2)    &
     &                               = (/'OpenMP', 'OMP   '/)
!
!>       Character lables for at once FFTW3 with OpenMP for transform
!!        'OpenMP_FFTW', 'OpenMP_FFTW3', 'OMP_FFTW', 'OMP_FFTW3',
!!        'FFTW_OpenMP', 'FFTW_OMP', 'FFTW3_OpenMP', 'FFTW3_OMP',
!!        'OpenMP_FFTW_once', 'OpenMP_FFTW_at_once', 
!!        'OpenMP_FFTW3_once', 'OpenMP_FFTW3_at_once', 
!!        'OMP_FFTW_once', 'OMP_FFTW_at_once', 'OMP_FFTW3_once',
!!        'OMP_FFTW3_at_once', 'FFTW_OpenMP_once',
!!        'FFTW_OpenMP_at_once', 'FFTW_OMP_once', 'FFTW_OMP_at_once',
!!        'FFTW3_OpenMP_once', 'FFTW3_OpenMP_at_once', 'FFTW3_OMP_once',
!!        'FFTW3_OMP_at_once', 'once_OpenMP_FFTW', 'once_OpenMP_FFTW3',
!!        'once_OMP_FFTW', 'once_OMP_FFTW3', 'once_FFTW_OpenMP',
!!        'once_FFTW_OMP', 'once_FFTW3_OpenMP', 'once_FFTW3_OMP',
!!        'at_once_OpenMP_FFTW', 'at_once_OpenMP_FFTW3',
!!        'at_once_OMP_FFTW', 'at_once_OMP_FFTW3', 
!!        'at_once_FFTW_OpenMP', 'at_once_FFTW_OMP', 
!!        'at_once_FFTW3_OpenMP', 'at_once_FFTW3_OMP' 
      type(multi_flag_labels), save :: at_once_OMP_FFTW_flags
!>       Character lables for once FFTW3 with OpenMP over domain
!!        'OpenMP_FFTW_domain', 'OpenMP_FFTW3_domain',
!!        'OMP_FFTW_domain', 'OMP_FFTW3_domain', 'FFTW_OpenMP_domain',
!!        'FFTW_OMP_domain', 'FFTW3_OpenMP_domain', 'FFTW3_OMP_domain',
!!        'domain_OpenMP_FFTW', 'domain_OpenMP_FFTW3',
!!        'domain_OMP_FFTW', 'domain_OMP_FFTW3', 'domain_FFTW_OpenMP',
!!        'domain_FFTW_OMP', 'domain_FFTW3_OpenMP', 'domain_FFTW3_OMP'
      type(multi_flag_labels), save :: domain_OMP_FFTW_flags
!>       Character lables for once FFTW3 with OpenMP over component
!!        'OpenMP_FFTW_component', 'OpenMP_FFTW_comps',
!!        'OpenMP_FFTW3_component', 'OpenMP_FFTW3_comps',
!!        'OMP_FFTW_component', 'OMP_FFTW_comps', 'OMP_FFTW3_component',
!!        'OMP_FFTW3_comps', 'FFTW_OpenMP_component',
!!        'FFTW_OpenMP_comps', 'FFTW_OMP_component', 'FFTW_OMP_comps',
!!        'FFTW3_OpenMP_component', 'FFTW3_OpenMP_comps',
!!        'FFTW3_OMP_component', 'FFTW3_OMP_comps',
!!        'component_OpenMP_FFTW', 'component_OpenMP_FFTW3',
!!        'component_OMP_FFTW', 'component_OMP_FFTW3',
!!        'component_FFTW_OpenMP', 'component_FFTW_OMP',
!!        'component_FFTW3_OpenMP', 'component_FFTW3_OMP',
!!        'comps_OpenMP_FFTW', 'comps_OpenMP_FFTW3', 'comps_OMP_FFTW',
!!        'comps_OMP_FFTW3', 'comps_FFTW_OpenMP', 'comps_FFTW_OMP',
!!        'comps_FFTW3_OpenMP', 'comps_FFTW3_OMP'
      type(multi_flag_labels), save :: comp_OMP_FFTW_flags
!>       Character lables for single FFTW3 with OpenMP
!!        'OpenMP_FFTW_single',  'OpenMP_FFTW_sgl', 
!!        'OpenMP_FFTW3_single', 'OpenMP_FFTW3_sgl', 'OMP_FFTW_single',
!!        'OMP_FFTW_sgl', 'OMP_FFTW3_single', 'OMP_FFTW3_sgl',
!!        'FFTW_OpenMP_single', 'FFTW_OpenMP_sgl', 'FFTW_OMP_single',
!!        'FFTW_OMP_sgl', 'FFTW3_OpenMP_single', 'FFTW3_OpenMP_sgl',
!!        'FFTW3_OMP_single', 'FFTW3_OMP_sgl', 'single_OpenMP_FFTW',
!!        'single_OpenMP_FFTW3', 'single_OMP_FFTW', 'single_OMP_FFTW3',
!!        'single_FFTW_OpenMP', 'single_FFTW_OMP', 
!!        'single_FFTW3_OpenMP', 'single_FFTW3_OMP', 'sgl_OpenMP_FFTW',
!!        'sgl_OpenMP_FFTW3', 'sgl_OMP_FFTW', 'sgl_OMP_FFTW3',
!!        'sgl_FFTW_OpenMP', 'sgl_FFTW_OMP',
!!        'sgl_FFTW3_OpenMP', 'sgl_FFTW3_OMP' 
      type(multi_flag_labels), save :: single_OMP_FFTW_flags
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_OMP_FFTW_flags(FFTW_flags)
!
      type(multi_flag_labels), intent(in) :: FFTW_flags
!
!>     Character lables for OpenMP FFTW3:    'FFTW',    'FFTW3'
      type(multi_flag_labels) :: OMP_FFTW_flags
      type(multi_flag_labels) :: tmp_flags
!
      integer(kind = kint) :: icou
!
      call init_multi_flags_by_labels(itwo, OpenMP_names, tmp_flags)
      call init_from_two_kinds_flags(tmp_flags, FFTW_flags,             &
     &                               OMP_FFTW_flags, icou)
      call dealloc_multi_flags(tmp_flags)
!
      call init_each_FFT_mode_flags(OMP_FFTW_flags,                     &
     &    at_once_OMP_FFTW_flags, domain_OMP_FFTW_flags,                &
     &    comp_OMP_FFTW_flags, single_OMP_FFTW_flags)
      call dealloc_multi_flags(OMP_FFTW_flags)
!
      end subroutine init_OMP_FFTW_flags
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function find_set_OMP_FFTW_flag(label)
!
      character(len = kchara), intent(in) :: label
      integer(kind = kint) :: iflag_fft
!
      if(check_mul_flags(label, at_once_OMP_FFTW_flags))     then
        iflag_fft = iflag_OMP_FFTW + iflag_once_fft
      else if(check_mul_flags(label, domain_OMP_FFTW_flags)) then
        iflag_fft = iflag_OMP_FFTW + iflag_domain_once
      else if(check_mul_flags(label, comp_OMP_FFTW_flags))   then
        iflag_fft = iflag_OMP_FFTW + iflag_component_once
      else if(check_mul_flags(label, single_OMP_FFTW_flags)) then
        iflag_fft = iflag_OMP_FFTW + iflag_single_fft
      else
        iflag_fft = -1
      end if
      find_set_OMP_FFTW_flag = iflag_fft
!
      end function find_set_OMP_FFTW_flag
!
! ----------------------------------------------------------------------
!
      character(len = kchara) function find_OMP_FFTW_label(iflag_fft)
!
      integer(kind = kint), intent(in) :: iflag_fft
      character(len = kchara) :: tmpchara
!
      tmpchara = 'NONE'
      if(mod(iflag_fft,10) .eq. iflag_once_fft) then
        tmpchara = at_once_OMP_FFTW_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_domain_once) then
        tmpchara = domain_OMP_FFTW_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_component_once) then
        tmpchara = comp_OMP_FFTW_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_single_fft) then
        tmpchara = single_OMP_FFTW_flags%flags(1)
      end if
      find_OMP_FFTW_label = tmpchara
!
      end function find_OMP_FFTW_label
!
! ----------------------------------------------------------------------
!
      subroutine check_OMP_FFTW_mode_flags(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara) :: title
!
!
      write(title,'(4x,a)') 'at_once_OMP_FFTW_flags'
      call write_multi_flags(id_file, title, at_once_OMP_FFTW_flags)
      write(title,'(4x,a)') 'domain_OMP_FFTW_flags'
      call write_multi_flags(id_file, title, domain_OMP_FFTW_flags)
      write(title,'(4x,a)') 'comp_OMP_FFTW_flags'
      call write_multi_flags(id_file, title, comp_OMP_FFTW_flags)
      write(title,'(4x,a)') 'single_OMP_FFTW_flags'
      call write_multi_flags(id_file, title, single_OMP_FFTW_flags)
!
      end subroutine check_OMP_FFTW_mode_flags
!
! ----------------------------------------------------------------------
!
      end module m_OMP_FFTW_labels
