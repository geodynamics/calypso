!>@file   m_FFTW_labels.F90
!!@brief  module m_FFTW_labels
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Control labels for FFTW
!!
!!@verbatim
!!      subroutine init_all_FFTW_flags()
!!      integer(kind = kint) function find_set_all_FFTW_flag(label)
!!        character(len = kchara), intent(in) :: label
!!      character(len = kchara) function find_all_FFTW_label(iflag_fft)
!!        integer(kind = kint), intent(in) :: iflag_fft
!!      subroutine check_all_FFTW_mode_flags(id_file)
!!        integer(kind = kint), intent(in) :: id_file
!!   ------------------------------------------------------------------
!!    FFT Package lists
!!      FFTW,     FFTW3:         FFTW3
!!      OMP_FFTW, OMP_FFTW3:     FFTW3 with OpenMP parallelization
!!   ------------------------------------------------------------------
!!    FFT size flags
!|      once:         Call FFT once for all data transform
!|      domain:       Call FFT once for each spherical harmonic mode
!|      component:    Call FFT once for each components
!|      single:       Call FFT for each transform
!!   ------------------------------------------------------------------
!!@endverbatim
      module m_FFTW_labels
!
      use m_precision
      use t_multi_flag_labels
      use m_FFT_selector
#ifdef OMP_FFTW3
      use m_OMP_FFTW_labels
#endif
!
      implicit none
!
!>      flag parts for FFTW3
      character(len = kchara), parameter, private :: FFTW_names(2)      &
     &                               = (/'FFTW ', 'FFTW3'/)
!
!>     Character lables for FFTW3:    'FFTW',    'FFTW3'
      type(multi_flag_labels), save :: FFTW_flags
!
!>       Character lables for at once FFTW3 for transform
!!        'FFTW', 'FFTW3', 'FFTPACK_once', 'FFTPACK_at_once', 
!!        'FFTPACK5_once', 'FFTPACK5_at_once', 'once_FFTPACK', 
!!        'once_FFTPACK5', 'at_once_FFTPACK', 'at_once_FFTPACK5' 
      type(multi_flag_labels), save :: at_once_FFTW_flags
!>       Character lables for once FFTW3 over domain
!!         'FFTW_domain', 'FFTW3_domain', 'domain_FFTW', 'domain_FFTW3'
      type(multi_flag_labels), save :: domain_FFTW_flags
!>       Character lables for once FFTW3 over component
!!        'FFTW_component', 'FFTW_comps',     'FFTW3_component',
!!        'FFTW3_comps',    'component_FFTW', 'component_FFTW3',
!!        'comps_FFTW',     'comps_FFTW3' 
      type(multi_flag_labels), save :: comp_FFTW_flags
!>       Character lables for single FFTW3
!!        'FFTW_single', 'FFTW_sgl',     'FFTW3_single', 'FFTW3_sgl',
!!        'single_FFTW', 'single_FFTW3', 'sgl_FFTW',     'sgl_FFTW3' 
      type(multi_flag_labels), save :: single_FFTW_flags
!
      private :: init_FFTW_flags, check_FFTW_mode_flags
      private :: find_set_FFTW_flag, find_FFTW_label
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_all_FFTW_flags()
!
      type(multi_flag_labels) :: FFTW_flags
!
      call init_multi_flags_by_labels(itwo, FFTW_names, FFTW_flags)
      call init_each_FFT_mode_flags(FFTW_flags,                         &
     &    at_once_FFTW_flags, domain_FFTW_flags,                        &
     &    comp_FFTW_flags, single_FFTW_flags)
!
#ifdef OMP_FFTW3
      call init_OMP_FFTW_flags(FFTW_flags)
#endif
!
      end subroutine init_all_FFTW_flags
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function find_set_all_FFTW_flag(label)
!
      character(len = kchara), intent(in) :: label
!
      integer(kind = kint) :: iflag_fft
!
      iflag_fft = find_set_FFTW_flag(label)
#ifdef OMP_FFTW3
      if(iflag_fft .lt. 0) iflag_fft = find_set_OMP_FFTW_flag(label)
#endif
      find_set_all_FFTW_flag = iflag_fft
!
      end function find_set_all_FFTW_flag
!
! ----------------------------------------------------------------------
!
      character(len = kchara) function find_all_FFTW_label(iflag_fft)
!
      integer(kind = kint), intent(in) :: iflag_fft
!
      character(len = kchara) :: tmpchara
!
      tmpchara = 'NONE'
      if((iflag_fft/10) .eq. (iflag_FFTW/10)) then
        tmpchara = find_FFTW_label(iflag_fft)
#ifdef OMP_FFTW3
      else if((iflag_fft/10) .eq. (iflag_OMP_FFTW/10)) then
        tmpchara = find_OMP_FFTW_label(iflag_fft)
#endif
      end if
      find_all_FFTW_label = tmpchara
!
      end function find_all_FFTW_label
!
! ----------------------------------------------------------------------
!
      subroutine check_all_FFTW_mode_flags(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call check_FFTW_mode_flags(id_file)
      write(id_file,'(a)') ''
#ifdef OMP_FFTW3
      call check_OMP_FFTW_mode_flags(id_file)
      write(id_file,'(a)') ''
#endif
!
      end subroutine check_all_FFTW_mode_flags
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_FFTW_flags(FFTW_flags)
!
      type(multi_flag_labels), intent(in) :: FFTW_flags
!
      call init_each_FFT_mode_flags(FFTW_flags,                         &
     &    at_once_FFTW_flags, domain_FFTW_flags,                        &
     &    comp_FFTW_flags, single_FFTW_flags)
!
      end subroutine init_FFTW_flags
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function find_set_FFTW_flag(label)
!
      character(len = kchara), intent(in) :: label
!
      integer(kind = kint) :: iflag_fft
!
      if(check_mul_flags(label, at_once_FFTW_flags))     then
        iflag_fft = iflag_FFTW + iflag_once_fft
      else if(check_mul_flags(label, domain_FFTW_flags)) then
        iflag_fft = iflag_FFTW + iflag_domain_once
      else if(check_mul_flags(label, comp_FFTW_flags))   then
        iflag_fft = iflag_FFTW + iflag_component_once
      else if(check_mul_flags(label, single_FFTW_flags)) then
        iflag_fft = iflag_FFTW + iflag_single_fft
      else
        iflag_fft = -1
      end if
      find_set_FFTW_flag = iflag_fft
!
      end function find_set_FFTW_flag
!
! ----------------------------------------------------------------------
!
      character(len = kchara) function find_FFTW_label(iflag_fft)
!
      integer(kind = kint), intent(in) :: iflag_fft
      character(len = kchara) :: tmpchara
!
      tmpchara = 'NONE'
      if(mod(iflag_fft,10) .eq. iflag_once_fft) then
        tmpchara = at_once_FFTW_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_domain_once) then
        tmpchara = domain_FFTW_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_component_once) then
        tmpchara = comp_FFTW_flags%flags(1)
      else if(mod(iflag_fft,10) .eq. iflag_single_fft) then
        tmpchara = single_FFTW_flags%flags(1)
      end if
      find_FFTW_label = tmpchara
!
      end function find_FFTW_label
!
! ----------------------------------------------------------------------
!
      subroutine check_FFTW_mode_flags(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara) :: title
!
!
      write(title,'(4x,a)') 'at_once_FFTW_flags'
      call write_multi_flags(id_file, title, at_once_FFTW_flags)
      write(title,'(4x,a)') 'domain_FFTW_flags'
      call write_multi_flags(id_file, title, domain_FFTW_flags)
      write(title,'(4x,a)') 'comp_FFTW_flags'
      call write_multi_flags(id_file, title, comp_FFTW_flags)
      write(title,'(4x,a)') 'single_FFTW_flags'
      call write_multi_flags(id_file, title, single_FFTW_flags)
!
      end subroutine check_FFTW_mode_flags
!
! ----------------------------------------------------------------------
!
      end module m_FFTW_labels
