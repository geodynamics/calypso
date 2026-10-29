!>@file   multi_pin_FFTW3_smp.F90
!!@brief  module multi_pin_FFTW3_smp
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine multi_pin_fwd_FFTW3(Ncomp, Nfft, X, WK,              &
!!     &                               elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        type(working_mul_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
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
!!      subroutine multi_pin_bwd_FFTW3(Ncomp, Nfft, X, WK,              &
!!     &                               elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        type(working_mul_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
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
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param Ncomp           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(Ncomp, Nfft)  Data for Fourier transform
!!
!!@n @param plan_forward   FFTW plan for forward transform
!!@n @param plan_backward  FFTW plan for backward transform
!!@n @param aNfft       normalization parameter for FFTW (= 1 / Nfft)
!!@n @param X_FFTW      real data for multiple Fourier transform
!!@n @param C_FFTW      spectrum data for multiple Fourier transform
!
      module multi_pin_FFTW3_smp
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_fftw_parameters
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine multi_pin_fwd_FFTW3(Ncomp, Nfft, X, WK,                &
     &                               elapsed_fft, elapsed_cpy)
!
      use t_multi_FFTW_wrapper
      use calypso_multi_FFTW3
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      type(working_mul_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_fwd_FFTW3_smp                                          &
     &   (WK%plan_mul_fwd, WK%Nplan_FFTW, WK%istack_FFTW,               &
     &    Ncomp, Nfft, X, WK%Nfft_c, WK%C_FFTW_mul)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
!   normalization
      start = OMP_GET_WTIME()
      call normalize_fwd_OMP_FFTW(WK%aNfft, Ncomp, WK%Nfft_c,           &
     &                            WK%C_FFTW_mul(1,1))
      call copy_from_prt_fwd_OMP_FFTW                                   &
     &   (Ncomp, WK%Nfft_c, WK%C_FFTW_mul(1,1), Ncomp, Nfft, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pin_fwd_FFTW3
!
! ------------------------------------------------------------------
!
      subroutine multi_pin_bwd_FFTW3(Ncomp, Nfft, X, WK,                &
     &                               elapsed_fft, elapsed_cpy)
!
      use t_multi_FFTW_wrapper
      use calypso_multi_FFTW3
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      type(working_mul_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
!   normalization
      start = OMP_GET_WTIME()
      call norm_copy_to_prt_bwd_OMP_FFTW(Ncomp, Nfft, X(1,1),           &
     &    Ncomp, WK%Nfft_c, WK%C_FFTW_mul(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_bwd_FFTW3_smp                                          &
     &   (WK%plan_mul_bwd, WK%Nplan_FFTW, WK%istack_FFTW,               &
     &    Ncomp,  WK%Nfft_c, WK%C_FFTW_mul, Nfft, X)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      end subroutine multi_pin_bwd_FFTW3
!
! ------------------------------------------------------------------
!
      end module multi_pin_FFTW3_smp
