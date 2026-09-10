!>@file   calypso_single_FFTW3.f90
!!@brief  module calypso_single_FFTW3
!!
!!@author H. Matsui
!!@date Programmed on Apr., 2013
!
!
!>@brief  Fourier transform with work structures for ISPACK
!!
!!@verbatim
!!   wrapper subroutine for initierize FFT by FFTW
!!      subroutine init_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
!!      subroutine finalize_FFTW_type(Nsmp, WK)
!!      subroutine verify_wk_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
!!        integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Nfft
!!        type(working_FFTW), intent(inout) :: WK
!!
!! ------------------------------------------------------------------
!!
!!      subroutine calypso_single_pin_fwd_FFTW3(Ncomp, Nfft, X, WK,     &
!!     &                                       elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!        type(working_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine FFTW_forward_type(Ncomp, Nfft, X, WK,                &
!!     &                             elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
!!        type(working_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
!!
!!   a_{k} = \frac{2}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \cos (\frac{2\pi j k}{Nfft})]
!!   b_{k} = \frac{2}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \sin (\frac{2\pi j k}{Nfft})]
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \cos (\frac{2\pi j k}{Nfft})]
!!
!! ------------------------------------------------------------------
!!
!!      subroutine calypso_single_pin_bwd_FFTW3(Ncomp, Nfft, X, WK,     &
!!     &                                       elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!        type(working_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine FFTW_backward_type(Ncomp, Nfft, X, WK,               &
!!     &                              elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!        type(working_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by FFTW3
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
      module calypso_single_FFTW3
!
      use m_precision
      use m_constants
      use t_single_FFTW_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Nfft
!
      type(working_FFTW), intent(inout) :: WK
!
!
      call alloc_work_4_FFTW_t(Nsmp, Nfft, WK)
      call init_single_FFTW_smp(Nsmp, Nstacksmp, Nfft, WK%Nfft_c,       &
     &                          WK%plan_forward, WK%plan_backward,      &
     &                          WK%istack_sFFTW, WK%Mmax_smp,           &
     &                          WK%X_FFTW, WK%C_FFTW)
!
      end subroutine init_FFTW_type
!
! ------------------------------------------------------------------
!
      subroutine finalize_FFTW_type(Nsmp, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp
!
      type(working_FFTW), intent(inout) :: WK
!
!
      call destroy_single_FFTW_smp                                      &
     &   (Nsmp, WK%plan_forward, WK%plan_backward)
      call dealloc_work_4_FFTW_t(WK)
!
      end subroutine finalize_FFTW_type
!
! ------------------------------------------------------------------
!
      subroutine verify_wk_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) ::  Nfft
!
      type(working_FFTW), intent(inout) :: WK
!
!
      if(WK%iflag_fft_len .lt. 0) then
        call init_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
        return
      end if
!
      if( WK%iflag_fft_len .ne. Nfft*Nsmp) then
        call finalize_FFTW_type(Nsmp, WK)
        call init_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
      end if
!
      end subroutine verify_wk_FFTW_type
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_single_pin_fwd_FFTW3(Ncomp, Nfft, X, WK,       &
     &                                       elapsed_fft, elapsed_cpy)
!
      use single_pin_FFTW3_smp
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      type(working_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call single_pin_fwd_FFTW3_smp                                     &
     &   (WK%plan_forward, WK%Nplan_sFFTW, WK%istack_sFFTW,             &
     &    Ncomp, Nfft, WK%aNfft, WK%Nfft_c, X, WK%C_FFTW,               &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine calypso_single_pin_fwd_FFTW3
!
! ------------------------------------------------------------------
!
      subroutine calypso_single_pin_bwd_FFTW3(Ncomp, Nfft, X, WK,       &
     &                                       elapsed_fft, elapsed_cpy)
!
      use single_pin_FFTW3_smp
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      type(working_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call single_pin_bwd_FFTW3_smp                                     &
     &   (WK%plan_backward, WK%Nplan_sFFTW, WK%istack_sFFTW,            &
     &    Ncomp, Nfft, WK%Nfft_c, X, WK%C_FFTW,                         &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine calypso_single_pin_bwd_FFTW3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine FFTW_forward_type(Ncomp, Nfft, X, WK,                  &
     &                             elapsed_fft, elapsed_cpy)
!
      use single_pout_FFTW3_smp
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
      type(working_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call single_pout_fwd_FFTW3_smp                                    &
     &   (WK%plan_forward, WK%Nplan_sFFTW, WK%istack_sFFTW,             &
     &    Ncomp, Nfft, WK%aNfft, WK%NFFT_c, X, WK%X_FFTW, WK%C_FFTW,    &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine FFTW_forward_type
!
! ------------------------------------------------------------------
!
      subroutine FFTW_backward_type(Ncomp, Nfft, X, WK,                 &
     &                              elapsed_fft, elapsed_cpy)
!
      use single_pout_FFTW3_smp
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      type(working_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call single_pout_bwd_FFTW3_smp                                    &
     &   (WK%plan_backward, WK%Nplan_sFFTW, WK%istack_sFFTW,            &
     &    Ncomp, Nfft, WK%NFFT_c, X, WK%X_FFTW, WK%C_FFTW,              &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine FFTW_backward_type
!
! ------------------------------------------------------------------
!
      end module calypso_single_FFTW3
