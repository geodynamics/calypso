!>@file   t_FFT_selector.f90
!!@brief  module t_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!      module t_FFT_selector
!
!
!>@brief  Selector of Fourier transform using structure
!!
!!@verbatim
!!      subroutine initialize_FFT_sel_t(my_rank, Nsmp, Nstacksmp, Nfft, &
!!     &          WKS)
!!      subroutine finalize_FFT_sel_t(Nsmp, Nstacksmp, WKS)
!!      subroutine verify_FFT_sel_t(Nsmp, Nstacksmp, Nfft, WKS)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine forward_FFT_sel_t(Nsmp, Nstacksmp, M, Nfft, X, WKS)
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for FFT in ISPACK
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine backward_FFT_sel_t(Nsmp, Nstacksmp, M, Nfft, X, WKS)
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
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!!@n @param WKS         Work structure for ISPACK
!
      module t_FFT_selector
!
      use m_precision
      use m_machine_parameter
      use t_FFTPACK5_wrapper
!
      use t_FFTW_wrapper
!
      implicit none
!
!>      structure for working data for FFT
      type working_FFTs
        type(working_FFTPACK) :: WK_FFTPACK
        type(working_FFTW) ::    WK_FFTW
      end type working_FFTs
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine initialize_FFT_sel_t(my_rank, Nsmp, Nstacksmp, Nfft,   &
     &          WKS)
!
      use FFT_selector
!
      integer(kind = kint), intent(in) ::  my_rank, Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_FFTs), intent(inout) :: WKS
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        if(my_rank .eq. 0) write(*,*) 'Use FFTW'
        call init_FFTW_type(Nstacksmp(Nsmp), Nfft, WKS%WK_FFTW)
        return
      end if
#endif
!
        if(my_rank .eq. 0) write(*,*) 'Use FFTPACK'
        call init_WK_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WKS%WK_FFTPACK)
!
      end subroutine initialize_FFT_sel_t
!
! ------------------------------------------------------------------
!
      subroutine finalize_FFT_sel_t(Nsmp, Nstacksmp, WKS)
!
      use FFT_selector
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      type(working_FFTs), intent(inout) :: WKS
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
        call finalize_FFTW_type(Nstacksmp(Nsmp), WKS%WK_FFTW)
        return
      end if
#endif
!
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTPACK'
        call finalize_WK_FFTPACK_t(WKS%WK_FFTPACK)
!
      end subroutine finalize_FFT_sel_t
!
! ------------------------------------------------------------------
!
      subroutine verify_FFT_sel_t(Nsmp, Nstacksmp, Nfft, WKS)
!
      use FFT_selector
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_FFTs), intent(inout) :: WKS
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW'
        call verify_wk_FFTW_type(Nstacksmp(Nsmp), Nfft, WKS%WK_FFTW)
        return
      end if
#endif
!
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTPACK'
        call verify_wk_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WKS%WK_FFTPACK)
!
      end subroutine verify_FFT_sel_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine forward_FFT_sel_t(Nsmp, Nstacksmp, M, Nfft, X, WKS)
!
      use FFT_selector
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      type(working_FFTs), intent(inout) :: WKS
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        call FFTW_forward_type(Nsmp, Nstacksmp, M, Nfft, X,             &
     &      WKS%WK_FFTW)
        return
      end if
#endif
!
        call CALYPSO_RFFTMF_t(Nsmp, Nstacksmp, M, Nfft, X,              &
     &      WKS%WK_FFTPACK)
!
      end subroutine forward_FFT_sel_t
!
! ------------------------------------------------------------------
!
      subroutine backward_FFT_sel_t(Nsmp, Nstacksmp, M, Nfft, X, WKS)
!
      use FFT_selector
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      type(working_FFTs), intent(inout) :: WKS
!
!
#ifdef FFTW3
      if(iflag_FFT .eq. iflag_FFTW) then
        call FFTW_backward_type(Nsmp, Nstacksmp, M, Nfft, X,            &
     &      WKS%WK_FFTW)
        return
      end if
#endif
!
        call CALYPSO_RFFTMB_t(Nsmp, Nstacksmp, M, Nfft, X,              &
     &      WKS%WK_FFTPACK)
!
      end subroutine backward_FFT_sel_t
!
! ------------------------------------------------------------------
!
      end module t_FFT_selector
