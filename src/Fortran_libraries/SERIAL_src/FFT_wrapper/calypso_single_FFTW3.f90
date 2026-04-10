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
!! ------------------------------------------------------------------
!!
!!      subroutine calypso_single_pin_fwd_FFTW3(Nsmp, Nstacksmp,        &
!!     &          Ncomp, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!        type(working_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine FFTW_forward_type(Nsmp, Nstacksmp, Ncomp, Nfft,      &
!!     &                             X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
!!        type(working_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
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
!!      subroutine calypso_single_pin_bwd_FFTW3(Nsmp, Nstacksmp,        &
!!     &          Ncomp, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!        type(working_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine FFTW_backward_type(Nsmp, Nstacksmp, Ncomp, Nfft,     &
!!     &                              X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
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
      use t_FFTW_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine calypso_single_pin_fwd_FFTW3(Nsmp, Nstacksmp,          &
     &          Ncomp, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!
      use single_pin_FFTW3_smp
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      type(working_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call single_pin_fwd_FFTW3_smp(WK%plan_forward, Nsmp, Nstacksmp,   &
     &    Ncomp, Nfft, WK%aNfft, WK%Nfft_c, X, WK%X_FFTW, WK%C_FFTW,    &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine calypso_single_pin_fwd_FFTW3
!
! ------------------------------------------------------------------
!
      subroutine calypso_single_pin_bwd_FFTW3(Nsmp, Nstacksmp,          &
     &          Ncomp, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!
      use single_pin_FFTW3_smp
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      type(working_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call single_pin_bwd_FFTW3_smp(WK%plan_backward, Nsmp, Nstacksmp,  &
     &    Ncomp, Nfft, WK%Nfft_c, X, WK%X_FFTW, WK%C_FFTW,              &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine calypso_single_pin_bwd_FFTW3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine FFTW_forward_type(Nsmp, Nstacksmp, Ncomp, Nfft,        &
     &                             X, WK, elapsed_fft, elapsed_cpy)
!
      use FFTW3_wrapper
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
      type(working_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call FFTW_forward_SMP(WK%plan_forward, Nsmp, Nstacksmp,           &
     &    Ncomp, Nfft, WK%aNfft, WK%NFFT_c, X, WK%X_FFTW, WK%C_FFTW,    &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine FFTW_forward_type
!
! ------------------------------------------------------------------
!
      subroutine FFTW_backward_type(Nsmp, Nstacksmp, Ncomp, Nfft,       &
     &                              X, WK, elapsed_fft, elapsed_cpy)
!
      use FFTW3_wrapper
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      type(working_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call FFTW_backward_SMP(WK%plan_backward, Nsmp, Nstacksmp,         &
     &    Ncomp, Nfft, WK%NFFT_c, X, WK%X_FFTW, WK%C_FFTW,              &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine FFTW_backward_type
!
! ------------------------------------------------------------------
!
      end module calypso_single_FFTW3
