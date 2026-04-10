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
!!      subroutine multi_pin_fwd_FFTW3_smp(plan_forward_smp,            &
!!     &          Nsmp, Nstacksmp, Ncomp, Nfft, aNfft, Nfft_c,          &
!!     &          X, C_FFTW, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, Nfft_c
!!        integer(kind = fftw_plan), intent(in) :: plan_forward_smp(Nsmp)
!!        real(kind = kreal), intent(in) :: aNfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!        complex(kind = fftw_complex), intent(inout)                   &
!!     &                                  :: C_FFTW(Nfft_c,Ncomp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine multi_pin_bwd_FFTW3_smp(plan_backward_smp,           &
!!     &          Nsmp, Nstacksmp, Ncomp, Nfft, Nfft_c,                 &
!!     &          X, C_FFTW, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, Nfft_c
!!        integer(kind=fftw_plan), intent(in) :: plan_backward_smp(Nsmp)
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!        complex(kind = fftw_complex), intent(inout)                   &
!!     &                                  :: C_FFTW(Nfft_c,Ncomp)
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
      subroutine multi_pin_fwd_FFTW3_smp(plan_forward_smp,              &
     &          Nsmp, Nstacksmp, Ncomp, Nfft, aNfft, Nfft_c,            &
     &          X, C_FFTW, elapsed_fft, elapsed_cpy)
!
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft, Nfft_c
      integer(kind = fftw_plan), intent(in) :: plan_forward_smp(Nsmp)
      real(kind = kreal), intent(in) :: aNfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &                                  :: C_FFTW(Nfft_c,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st_c, ed_c, st_f, ed_f
      integer(kind = kint) :: ip, ist, num
!
!
      ed_c = 0.0d0
      ed_f = 0.0d0
!$omp parallel do private(ip,ist,num,st_c,st_f)                         &
!$omp&            reduction(+:ed_c,ed_f)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1)
        num = Nstacksmp(ip  ) - ist
!
        st_f = OMP_GET_WTIME()
        call dfftw_execute_dft_r2c(plan_forward_smp(ip),                &
     &                             X(1,ist+1), C_FFTW(1,ist+1))
        ed_f = ed_f + OMP_GET_WTIME() - st_f
!
!   normalization
        ed_c = OMP_GET_WTIME() - st_c
        call norm_copy_from_prt_fwd_FFTW(num, NFFT_c, C_FFTW(1,ist+1),  &
     &                                   Nfft, aNfft, X(1,ist+1))
        ed_c = ed_c + OMP_GET_WTIME() - st_c
      end do
!$omp end parallel do
!
      elapsed_fft = elapsed_fft + ed_f / dble(Nsmp)
      elapsed_cpy = elapsed_cpy + ed_c / dble(Nsmp)
!
      end subroutine multi_pin_fwd_FFTW3_smp
!
! ------------------------------------------------------------------
!
      subroutine multi_pin_bwd_FFTW3_smp(plan_backward_smp,             &
     &          Nsmp, Nstacksmp, Ncomp, Nfft, Nfft_c,                   &
     &          X, C_FFTW, elapsed_fft, elapsed_cpy)
!
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft, Nfft_c
      integer(kind = fftw_plan), intent(in) :: plan_backward_smp(Nsmp)
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &                                  :: C_FFTW(Nfft_c,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st_c, ed_c, st_f, ed_f
      integer(kind = kint) :: i, ip, ist, num
!
!
      ed_c = 0.0d0
      ed_f = 0.0d0
!$omp parallel do private(i,ip,ist,num,st_c,st_f)                       &
!$omp&            reduction(+:ed_c,ed_f)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1)
        num = Nstacksmp(ip) - ist
!   normalization
        st_c = OMP_GET_WTIME()
        call norm_copy_to_prt_bwd_FFTW(num, Nfft, X(1,ist+1),           &
     &                                 NFFT_c, C_FFTW(1,ist+1))
        ed_c = ed_c + OMP_GET_WTIME() - st_c
!
        st_f = OMP_GET_WTIME()
        call dfftw_execute_dft_c2r(plan_backward_smp(ip),               &
     &                             C_FFTW(1,ist+1), X(1,ist+1))
        ed_f = ed_f + OMP_GET_WTIME() - st_f
      end do
!$omp end parallel do
!
      elapsed_fft = elapsed_fft + ed_f / dble(Nsmp)
      elapsed_cpy = elapsed_cpy + ed_c / dble(Nsmp)
!
      end subroutine multi_pin_bwd_FFTW3_smp
!
! ------------------------------------------------------------------
!
      end module multi_pin_FFTW3_smp
