!>@file   t_sph_FFTPACK_selector.f90
!!@brief  module t_sph_FFTPACK_selector
!!
!!@author H. Matsui
!!@date Programmed in May, 2026
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine sel_finalize_sph_FFTPACK(iflag_size,                 &
!!     &                                    WKs_FFTPACK, flag_FFT)
!!        integer(kind = kint), intent(in) :: iflag_size
!!        type(works_sph_FFTPACK), intent(inout) :: WKs_FFTPACK
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
!
      module t_sph_FFTPACK_selector
!
      use m_precision
      use m_machine_parameter
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      use m_FFT_selector
!
      use t_sph_FFTPACK5
      use t_sph_component_FFTPACK5
      use t_sph_domain_FFTPACK5
      use t_sph_single_FFTPACK5
!
      implicit none
!
!>      Structure for work area of FFTPACK
      type works_sph_FFTPACK
!>        Structure to use FFTPACK
        type(work_for_fftpack) :: sph_FFTPACK
!>        Structure to use single FFTPACK
        type(work_for_sgl_fftpack) :: sph_sgl_FFTPACK
!>        Structure to use single FFTPACK
        type(work_for_comp_fftpack) :: sph_comp_FFTPACK
!>        Structure to use single FFTPACK
        type(work_for_domain_fftpack) :: sph_domain_FFTPACK
      end type works_sph_FFTPACK
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine sel_finalize_sph_FFTPACK(iflag_size,                   &
     &                                    WKs_FFTPACK, flag_FFT)
!
      use sph_prt_FFTPACK5
      use sph_prt_domain_FFTPACK5
!
      integer(kind = kint), intent(in) :: iflag_size
!
      type(works_sph_FFTPACK), intent(inout) :: WKs_FFTPACK
      logical, intent(inout) :: flag_FFT
!
!
      if     (iflag_size .eq. iflag_once_fft) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTPACK'
        call finalize_sph_FFTPACK5(WKs_FFTPACK%sph_FFTPACK, flag_fft)
      else if(iflag_size .eq. iflag_domain_once) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                     'Finalize FFTPACK for domain'
        call finalize_sph_domain_FFTPACK5                               &
     &     (WKs_FFTPACK%sph_domain_FFTPACK, flag_fft)
      else if(iflag_size .eq. iflag_component_once) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                     'Finalize FFTPACK for all comp'
        call finalize_sph_comp_FFTPACK5(WKs_FFTPACK%sph_comp_FFTPACK,   &
     &                                  flag_fft)
      else if(iflag_size .eq. iflag_single_fft) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTPACK'
        call finalize_sph_single_FFTPACK5(WKs_FFTPACK%sph_sgl_FFTPACK,  &
     &                                    flag_fft)
      end if
!
      end subroutine sel_finalize_sph_FFTPACK
!
! ------------------------------------------------------------------
!
      end module t_sph_FFTPACK_selector
