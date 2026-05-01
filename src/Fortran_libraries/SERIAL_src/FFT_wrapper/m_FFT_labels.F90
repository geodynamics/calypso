!>@file   m_FFT_labels.F90
!!@brief  module m_FFT_labels
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Control labels for rocFFT
!!
!!@verbatim
!!      subroutine init_FFT_mode_flags()
!!      integer(kind = kint) function find_set_FFT_flag(label)
!!        character(len = kchara), intent(in) :: label
!!      character(len = kchara) function find_FFT_label(iflag_fft)
!!        integer(kind = kint), intent(in) :: iflag_fft
!!      subroutine check_rocFFT_mode_flags(id_file)
!!        integer(kind = kint), intent(in) :: id_file
!!
!!      subroutine write_elapsed_4_FFT(i_mode, etime_fft)
!!        integer(kind = kint), intent(in) :: i_mode
!!        real(kind = kreal), intent(in) :: etime_fft
!!      integer(kind = kint) function                                   &
!!     &            set_fft_library_ctl(iflag_ctl, FFT_library_ctl)
!!        integer(kind = kint), intent(in) :: iflag_ctl
!!        character(len = kchara), intent(in) :: FFT_library_ctl
!!   ------------------------------------------------------------------
!!    FFT Package lists
!|      FFTPACK:                 FFTPACK5.11d
!!      ISPACK:                  ISPACK Ver.1
!!      ISPACK3:                 ISPACK Ver.3
!!      FFTW,     FFTW3:         FFTW3
!!      OMP_FFTW, OMP_FFTW3:     FFTW3 with OpenMP parallelization
!!      rocFFT, rocFFT_complex:  AMD rocFFT
!!      rocFFT_real:             AMD rocFFT with real data only
!!      OpenMP_rocFFT:           AMD rocFFT with OpenMP offloading
!!   ------------------------------------------------------------------
!!    FFT size flags
!!
!|      once:         Call FFT once for all data transform
!|      domain:       Call FFT once for each spherical harmonic mode
!|      component:    Call FFT once for each components
!|      single:       Call FFT for each transform
!!   ------------------------------------------------------------------
!!@endverbatim
      module m_FFT_labels
!
      use m_precision
      use t_multi_flag_labels
      use m_FFT_selector
      use m_FFTPACK_labels
!      use m_ISPACK1_labels
!      use m_ISPACK3_labels
!
#ifdef FFTW3
      use m_FFTW_labels
#endif
#ifdef OMP_FFTW3
      use m_OMP_FFTW_labels
#endif
!#ifdef _AMD_ROCM_
!      use m_rocFFT_labels
!#endif
!
      implicit none
!
!>      Character flag for at once transeform
      character(len = kchara), parameter, private                       &
     &                              :: hd_at_once =       'once'
!>      Character flag for once transform over component
      character(len = kchara), parameter, private                       &
     &                              :: hd_once_for_comp = 'component'
!>      Character flag for once transform over domain
      character(len = kchara), parameter, private                       &
     &                              :: hd_once_for_mode = 'domain'
!>      Character flag single transform
      character(len = kchara), parameter, private                       &
     &                              :: hd_single_FFT =    'single'
!
!
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
      private :: init_FFT_loop_mode_flags
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_FFT_mode_flags()
!
      type(multi_flag_labels) :: rocFFT_base_flags
!
!
      if(allocated(at_once_FFT_flags%flags)) return
!
      call init_FFT_loop_mode_flags()
!
      call init_FFTPACK_mode_flags()
!      call init_ISPACK1_mode_flags()
!      call init_ISPACK3_mode_flags()
!
#ifdef FFTW3
      call init_all_FFTW_flags()
#endif
!
!#ifdef _AMD_ROCM_
!      call init_rocFFT_mode_flags()
!#endif
!
      end subroutine init_FFT_mode_flags
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function find_set_FFT_flag(label)
!
      character(len = kchara), intent(in) :: label
!
      integer(kind = kint) :: iflag_fft
!
      iflag_fft = -1
      iflag_fft = find_set_FFTPACK_flag(label)
!      if(iflag_fft .lt. 0) iflag_fft = find_set_ISPACK1_flag(label)
!      if(iflag_fft .lt. 0) iflag_fft = find_set_ISPACK3_flag(label)
!
#ifdef FFTW3
      if(iflag_fft .lt. 0) iflag_fft = find_set_all_FFTW_flag(label)
#endif
!
!#ifdef _AMD_ROCM_
!      if(iflag_fft .lt. 0) iflag_fft = find_set_rocFFT_flag(label)
!#endif
!
      find_set_FFT_flag = iflag_fft
!
      end function find_set_FFT_flag
!
! ----------------------------------------------------------------------
!
      character(len = kchara) function find_FFT_label(iflag_fft)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: iflag_fft
!
      character(len = kchara) :: tmpchara
!
      tmpchara = 'NONE'
      find_FFT_label = tmpchara
      if((iflag_fft/10) .eq. (iflag_FFTPACK/10)) then
        tmpchara = find_FFTPACK_label(iflag_fft)
!      else if((iflag_fft/10) .eq. (iflag_ISPACK0/10)) then
!        tmpchara = find_ISPACK1_label(iflag_fft)
!      else if((iflag_fft/10) .eq. (iflag_ISPACK3/10)) then
!        tmpchara = find_ISPACK3_label(iflag_fft)
      end if
!
#ifdef FFTW3
      if(cmp_no_case(tmpchara, 'NONE')) then
        tmpchara = find_all_FFTW_label(iflag_fft)
      end if
#endif
!
#ifdef _AMD_ROCM_
!      if(cmp_no_case(tmpchara, 'NONE')) then
!        tmpchara = find_rocFFT_label(iflag_fft)
!      end if
#endif
      find_FFT_label = tmpchara
!
      end function find_FFT_label
!
! ----------------------------------------------------------------------
!
      subroutine check_FFT_mode_flags(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call check_FFTPACK_mode_flags(id_file)
      write(id_file,*) ''
#ifdef FFTW3
      call check_all_FFTW_mode_flags(id_file)
      write(id_file,*) ''
#endif
!      call check_ISPACK1_mode_flags(id_file)
!      write(id_file,*) ''
!      call check_ISPACK3_mode_flags(id_file)
!      write(id_file,*) ''
!#ifdef _AMD_ROCM_
!      call check_rocFFT_mode_flags(id_file)
!      write(id_file,*) ''
!#endif
!
      end subroutine check_FFT_mode_flags
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_FFT_loop_mode_flags()
!
      type(multi_flag_labels) :: tmp_flags
      integer(kind = kint) :: icou
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
!
      end subroutine init_FFT_loop_mode_flags
!
! ------------------------------------------------------------------
!
      subroutine write_elapsed_4_FFT(i_mode, etime_fft)
!
      integer(kind = kint), intent(in) :: i_mode
      real(kind = kreal), intent(in) :: etime_fft
!
      character(len = kchara) :: FFT_name, FFT_type, tmpchara
!
!
      tmpchara = find_FFT_label(i_mode)
!
      if     (i_mode .eq. iflag_FFTPACK_ONCE) then
        write(*,*) 'elapsed by FFTPACK at once               (',        &
     &            trim(tmpchara), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTPACK_SINGLE) then
        write(*,*) 'elapsed by single FFTPACK                (',        &
     &            trim(tmpchara), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTPACK_COMPONENT) then
        write(*,*) 'elapsed by FFTPACK for all component     (',        &
     &            trim(tmpchara), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTPACK_DOMAIN) then
        write(*,*) 'elapsed by FFTPACK for domain            (',        &
     &            trim(tmpchara), '): ', etime_fft
!
      else if(i_mode .eq. iflag_FFTW_ONCE) then
        write(*,*) 'elapsed by FFTW3 for at once             (',        &
     &            trim(tmpchara), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTW_SINGLE) then
        write(*,*) 'elapsed by single FFTW3                  (',        &
     &            trim(tmpchara), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTW_COMPONENT) then
        write(*,*) 'elapsed by FFTW3 for all component       (',        &
     &            trim(tmpchara), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTW_DOMAIN) then
        write(*,*) 'elapsed by FFTW3 for domain              (',        &
     &            trim(tmpchara), '): ', etime_fft
!
      else if(i_mode .eq. iflag_OMP_FFTW_ONCE) then
        write(*,*) 'elapsed by FFTW3 with OpoenMP at once    (',        &
     &            trim(tmpchara), '): ', etime_fft
      else if(i_mode .eq. iflag_OMP_FFTW_DOMAIN) then
        write(*,*) 'elapsed by FFTW3 with OpoenMP for domain (',        &
     &            trim(tmpchara), '): ', etime_fft
!
      else if(i_mode .eq. iflag_ISPACK1_ONCE) then
        write(*,*) 'elapsed by ISPACK V0.93                  (',        &
     &            trim(tmpchara), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK1_DOMAIN) then
        write(*,*) 'elapsed by ISPACK V0.93 for domain       (',        &
     &            trim(tmpchara), '): ', etime_fft
!
      else if(i_mode .eq. iflag_ISPACK3_ONCE) then
        write(*,*) 'elapsed by ISPACK V3.0.1                 (',        &
     &            trim(tmpchara), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK3_DOMAIN) then
        write(*,*) 'elapsed by ISPACK V3.0.1 for domain      (',        &
     &            trim(tmpchara), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK3_COMPONENT) then
        write(*,*) 'elapsed by ISPACK V3.0.1 for component   (',        &
     &            trim(tmpchara), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK3_SINGLE) then
        write(*,*) 'elapsed by single ISPACK V3.0.1          (',        &
     &            trim(tmpchara), '): ', etime_fft
      end if
!
      end subroutine write_elapsed_4_FFT
!
! ------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &            set_fft_library_ctl(iflag_ctl, FFT_library_ctl)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: iflag_ctl
      character(len = kchara), intent(in) :: FFT_library_ctl
      integer(kind = kint) :: iflag
!
!
      call init_FFT_mode_flags()
!
#ifdef FFTW3
      iflag = iflag_FFTW_SINGLE
#else
      iflag = iflag_FFTPACK_ONCE
#endif
      if(iflag_ctl .eq. 0) then
        set_fft_library_ctl = iflag
        return
      end if
!
      if(cmp_no_case(FFT_library_ctl, hd_search_fastest_fft)) then
        iflag = iflag_SEARCH_FASTEST_FFT
      else
        iflag = find_set_FFT_flag(FFT_library_ctl)
      end if
      set_fft_library_ctl = iflag
!
      end function set_fft_library_ctl
!
! ------------------------------------------------------------------
!
      end module m_FFT_labels
