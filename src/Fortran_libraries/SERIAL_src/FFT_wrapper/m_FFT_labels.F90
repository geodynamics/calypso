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
      use m_ISPACK1_labels
!
#ifdef FFTW3
      use m_FFTW_labels
#endif
#ifdef OMP_FFTW3
      use m_OMP_FFTW_labels
#endif
!
      implicit none
!
!>      Character flag to sarch fastest FFT
      character(len = kchara), parameter, private                       &
     &               :: hd_search_fastest_fft = 'Search_fastest'
!>      Character flag to use test FFT
      character(len = kchara), parameter, private                       &
     &               :: hd_FFT_TEST = 'TEST'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_FFT_mode_flags()
!
!
      if(allocated(at_once_FFT_flags%flags)) return
!
      call init_FFT_loop_mode_flags()
!
      call init_FFTPACK_mode_flags()
      call init_ISPACK1_mode_flags()
!
#ifdef FFTW3
      call init_all_FFTW_flags()
#endif
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
      if(iflag_fft .lt. 0) iflag_fft = find_set_ISPACK1_flag(label)
!
#ifdef FFTW3
      if(iflag_fft .lt. 0) iflag_fft = find_set_all_FFTW_flag(label)
#endif
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
      else if((iflag_fft/10) .eq. (iflag_ISPACK0/10)) then
        tmpchara = find_ISPACK1_label(iflag_fft)
      end if
!
#ifdef FFTW3
      if(cmp_no_case(tmpchara, 'NONE')) then
        tmpchara = find_all_FFTW_label(iflag_fft)
      end if
#endif
!
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
      call check_ISPACK1_mode_flags(id_file)
      write(id_file,*) ''
!
      end subroutine check_FFT_mode_flags
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_elapsed_4_FFT(i_mode, etime_fft)
!
      integer(kind = kint), intent(in) :: i_mode
      real(kind = kreal), intent(in) :: etime_fft
!
      character(len = 20) :: size_label
      character(len = kchara) :: tmpchara
      integer(kind = kint) :: iflag_FFT, iflag_size
!
!
      iflag_size = mod(i_mode,10)
      iflag_FFT =  i_mode - iflag_size
!
      tmpchara = find_FFT_label(i_mode)
!
      if     (iflag_size .eq. iflag_once_fft) then
        write(size_label,'(a20)') 'at once:            '
      else if(iflag_size .eq. iflag_single_fft) then
        write(size_label,'(a20)') 'for each transform: '
      else if(iflag_size .eq. iflag_component_once) then
        write(size_label,'(a20)') 'for all component:  '
      else if(iflag_size .eq. iflag_domain_once) then
        write(size_label,'(a20)') 'for each component: '
      else
        write(size_label,'(a20)') 'for unknown size:   '
      end if
!
      if     (iflag_FFT .eq. iflag_FFTPACK) then
        write(*,'(a,a20)',ADVANCE='NO')                                 &
     &         'Elapsed by FFTPACK ', size_label, '              ('
      else if(iflag_FFT .eq. iflag_ISPACK0) then
        write(*,'(a,a20)',ADVANCE='NO')                                 &
     &         'Elapsed by ISPACK V0.93 ', size_label, '         ('
      else if(iflag_FFT .eq. iflag_FFTW) then
        write(*,'(a,a20)',ADVANCE='NO')                                 &
     &         'Elapsed by FFTW Ver.3 ', size_label, '           ('
      else if(iflag_FFT .eq. iflag_OMP_FFTW) then
        write(*,'(a,a20)',ADVANCE='NO')                                 &
     &         'Elapsed by FFTW V.3 with OpenMP ', size_label, ' ('
      else
        write(*,'(a,a20)',ADVANCE='NO')                                 &
     &         'Elapsed by unknown ', size_label, '              ('
      end if
!
      write(*,'(2a)',ADVANCE='NO') trim(tmpchara), '): '
      write(*,*) etime_fft
!
      end subroutine write_elapsed_4_FFT
!
! ------------------------------------------------------------------
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
      iflag = iflag_FFTW + iflag_single_fft
#else
      iflag = iflag_FFTPACK + iflag_once_fft
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
