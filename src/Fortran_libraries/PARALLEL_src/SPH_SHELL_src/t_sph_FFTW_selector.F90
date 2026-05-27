!>@file   t_sph_FFTW_selector.F90
!!@brief  module t_sph_FFTW_selector
!!
!!@author H. Matsui
!!@date Programmed in May, 2026
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine sel_finalize_prt_FFTW(iflag_sph_FFT, iflag_size,     &
!!     &                                 WKs_FFTW, flag_FFT)
!!      subroutine sel_finalize_rtp_FFTW(iflag_sph_FFT, iflag_size,     &
!!     &                                 WKs_FFTW, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
!!        type(works_sph_FFTW), intent(inout) :: WKs_FFTW
!!        logical, intent(inout) :: flag_FFT
!!
!!      subroutine sel_finalize_sph_FFTW_smp(iflag_size,                &
!!     &                                     WKs_FFTW, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_size
!!        type(works_sph_FFTW), intent(inout) :: WKs_FFTW
!!        logical, intent(inout) :: flag_FFT
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for FFT in ISPACK
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \sin (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for backward FFT
!!
!!   x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!!          (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!!       i = 1:     a_{0}
!!       i = 2:     a_{Nfft/2}
!!       i = 3:     a_{1}
!!       i = 4:     b_{1}
!!       ...
!!       i = 2*k+1: a_{k}
!!       i = 2*k+2: b_{k}
!!       ...
!!       i = Nfft-1:   a_{Nfft/2-1}
!!       i = Nfft:     b_{Nfft/2-1}
!!
!! ------------------------------------------------------------------
!!@endverbatim
      module t_sph_FFTW_selector
!
      use m_precision
      use m_machine_parameter
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use m_FFT_selector
!
      use t_sph_single_FFTW
      use t_sph_field_FFTW
      use t_sph_component_FFTW
!
#ifdef OMP_FFTW3
      use t_sph_OMP_FFTW
      use t_sph_field_OMP_FFTW
#endif
      implicit none
!
!>      Structure for work area of FFTW
      type works_sph_FFTW
!>        Structure to use FFTW
        type(work_for_field_FFTW) :: sph_fld_FFTW
!>        Structure to use FFTW for each component and meridinal point
        type(work_for_sgl_FFTW) :: sph_sgl_FFTW
!>        Structure to use FFTW for each component
        type(work_for_comp_FFTW) :: sph_comp_FFTW
!
#ifdef OMP_FFTW3
!>        Structure to use FFTW with OpenMP
        type(work_for_OpenMP_FFTW) :: sph_OMP_FFTW
!>        Structure to use FFTW with OpenMP
        type(work_for_domain_OMP_FFTW) :: sph_domain_OMP_FFTW
#endif
      end type works_sph_FFTW
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine sel_finalize_prt_FFTW(iflag_sph_FFT, iflag_size,       &
     &                                 WKs_FFTW, flag_FFT)
!
      use sph_prt_domain_FFTW
!
      integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
!
      type(works_sph_FFTW), intent(inout) :: WKs_FFTW
      logical, intent(inout) :: flag_FFT
!
!
      if(iflag_sph_FFT .eq. iflag_FFTW) then
        call sel_finalize_sph_FFTW_smp(iflag_size, WKs_FFTW, flag_FFT)
!
#ifdef OMP_FFTW3
      else if(iflag_sph_FFT .eq. iflag_OMP_FFTW) then
        if(     (iflag_size .eq. iflag_once_fft)                        &
     &     .or. (iflag_size .eq. iflag_domain_once)) then
          if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
          call finalize_sph_field_FFTW(WKs_FFTW%sph_fld_FFTW, flag_fft)
        end if
#endif
      end if
!
      end subroutine sel_finalize_prt_FFTW
!
! ------------------------------------------------------------------
!
      subroutine sel_finalize_rtp_FFTW(iflag_sph_FFT, iflag_size,       &
     &                                 WKs_FFTW, flag_FFT)
!
      integer(kind = kint), intent(in) :: iflag_sph_FFT, iflag_size
!
      type(works_sph_FFTW), intent(inout) :: WKs_FFTW
      logical, intent(inout) :: flag_FFT
!
!
      if(iflag_sph_FFT .eq. iflag_FFTW) then
        call sel_finalize_sph_FFTW_smp(iflag_size, WKs_FFTW, flag_FFT)
!
#ifdef OMP_FFTW3
      else if(iflag_sph_FFT .eq. iflag_OMP_FFTW) then
        if(iflag_size .eq. iflag_once_fft) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &          'Finalize at once OpenMP FFTW'
          call finalize_rtp_OMP_FFTW(WKs_FFTW%sph_OMP_FFTW, flag_fft)
        else if(iflag_size .eq. iflag_domain_once) then
          if(iflag_debug .eq. 0) write(*,*)                             &
     &        'Finalize rtp OpenMP FFTW for domain'
          call finalize_sph_domain_OMP_FFTW                             &
     &       (WKs_FFTW%sph_domain_OMP_FFTW, flag_fft)
        end if
#endif
      end if
!
      end subroutine sel_finalize_rtp_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_finalize_sph_FFTW_smp(iflag_size,                  &
     &                                     WKs_FFTW, flag_FFT)
!
      integer(kind = kint), intent(in) :: iflag_size
!
      type(works_sph_FFTW), intent(inout) :: WKs_FFTW
      logical, intent(inout) :: flag_FFT
!
!
      if     ((iflag_size .eq. iflag_once_fft)                          &
     &   .or. (iflag_size .eq. iflag_domain_once))  then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
        call finalize_sph_field_FFTW(WKs_FFTW%sph_fld_FFTW, flag_fft)
      else if(iflag_size .eq. iflag_component_once) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW for all comps'
        call finalize_sph_component_FFTW(WKs_FFTW%sph_comp_FFTW,        &
     &                                   flag_fft)
      else if(iflag_size .eq. iflag_single_fft) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTW'
        call finalize_sph_single_FFTW(WKs_FFTW%sph_sgl_FFTW, flag_fft)
      end if
!
      end subroutine sel_finalize_sph_FFTW_smp
!
! ------------------------------------------------------------------
!
      end module t_sph_FFTW_selector
