!>@file   FFTW3_wrapper.F90
!!@brief  module FFTW3_wrapper
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_4_FFTW_smp(Ncomp, Nfft,                         &
!!     &          plan_forward, plan_backward, X_FFTW, C_FFTW)
!!
!!   wrapper subroutine for initierize FFTW plans
!! ------------------------------------------------------------------
!!      subroutine destroy_FFTW_smp(Ncomp, plan_forward, plan_backward)
!!        CAUTION!!  dfftw_destroy_plan oftern makes SEGMENTAION FAULT!!
!!
!!
!!   wrapper subroutine for clear FFTW plans
!! ------------------------------------------------------------------
!!
!!      subroutine FFTW_forward_SMP(plan_forward, Nsmp, Nstacksmp,      &
!!     &          Ncomp, Nfft, aNfft, NFFT_c, X, X_FFTW, C_FFTW,        &
!!     &          elapsed_fft, elapsed_cpy)
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
!!      subroutine FFTW_backward_SMP(plan_backward, Nsmp, Nstacksmp,    &
!!     &          Ncomp, Nfft, NFFT_c, X, X_FFTW, C_FFTW,               &
!!     &          elapsed_fft, elapsed_cpy)
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
      module FFTW3_wrapper
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
      subroutine init_4_FFTW_smp(Ncomp, Nfft, NFFT_c,                   &
     &          plan_forward, plan_backward, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) ::  Nfft, Nfft_c
      integer(kind = kint), intent(in) ::  Ncomp
!
      integer(kind = fftw_plan), intent(inout) :: plan_forward(Ncomp)
      integer(kind = fftw_plan), intent(inout) :: plan_backward(Ncomp)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &                                  :: C_FFTW(Nfft_c,Ncomp)
!
      integer(kind = kint) :: j
      integer :: Nfft4
!
!
      Nfft4 = int(Nfft)
      do j = 1, Ncomp
        call dfftw_plan_dft_r2c_1d(plan_forward(j), Nfft4,              &
     &      X_FFTW(1,j), C_FFTW(1,j) , FFTW_KEMO_EST)
        call dfftw_plan_dft_c2r_1d(plan_backward(j), Nfft4,             &
     &      C_FFTW(1,j), X_FFTW(1,j) , FFTW_KEMO_EST)
      end do
!
      end subroutine init_4_FFTW_smp
!
! ------------------------------------------------------------------
!
      subroutine destroy_FFTW_smp(Ncomp, plan_forward, plan_backward)
!
      integer(kind = kint), intent(in) ::  Ncomp
!
      integer(kind = fftw_plan), intent(in) :: plan_forward(Ncomp)
      integer(kind = fftw_plan), intent(in) :: plan_backward(Ncomp)
!
      integer(kind = kint) :: j
!
!
      do j = 1, Ncomp
        call dfftw_destroy_plan(plan_forward(j))
        call dfftw_destroy_plan(plan_backward(j))
        call dfftw_cleanup
      end do
!
      end subroutine destroy_FFTW_smp
!
! ------------------------------------------------------------------
!
      subroutine FFTW_forward_SMP(plan_forward, Nsmp, Nstacksmp,        &
     &          Ncomp, Nfft, aNfft, NFFT_c, X, X_FFTW, C_FFTW,          &
     &          elapsed_fft, elapsed_cpy)
!
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      integer(kind = fftw_plan), intent(in) :: plan_forward(Ncomp)
      real(kind = kreal), intent(in) :: aNfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &                                  :: C_FFTW(NFFT_c,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st_c, ed_c, st_f, ed_f
      integer(kind = kint) ::  j, ip, ist, ied
!
!
      ed_c = 0.0d0
      ed_f = 0.0d0
!$omp parallel do private(j,ist,ied,st_c,st_f) reduction(+:ed_c,ed_f)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip) 
!
        st_c = OMP_GET_WTIME()
        do j = ist, ied
          X_FFTW(1:Nfft,j) = X(j,1:Nfft)
        end do
        ed_c = OMP_GET_WTIME() - st_c
!
        st_f = OMP_GET_WTIME()
        do j = ist, ied
          call dfftw_execute(plan_forward(j))
        end do
        ed_f = OMP_GET_WTIME() - st_f
!
!   normalization
        st_c = OMP_GET_WTIME()
        call normalize_fwd_r2c_fft_SMP(ist, ied, Ncomp, NFFT_c, C_FFTW, &
     &                                 Nfft, aNfft, X)
        ed_c = ed_c + OMP_GET_WTIME() - st_c
      end do
!$omp end parallel do
!
      elapsed_fft = elapsed_fft + ed_f / dble(Nsmp)
      elapsed_cpy = elapsed_cpy + ed_c / dble(Nsmp)
!
      end subroutine FFTW_forward_SMP
!
! ------------------------------------------------------------------
!
      subroutine FFTW_backward_SMP(plan_backward, Nsmp, Nstacksmp,      &
     &          Ncomp, Nfft, NFFT_c, X, X_FFTW, C_FFTW,                 &
     &          elapsed_fft, elapsed_cpy)
!
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      integer(kind = fftw_plan), intent(in) :: plan_backward(Ncomp)
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &                                  :: C_FFTW(NFFT_c,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st_c, ed_c, st_f, ed_f
      integer(kind = kint) :: i, j, ip, ist, ied
!
!
      ed_c = 0.0d0
      ed_f = 0.0d0
!$omp parallel do private(i,j,ist,ied,st_c,st_f) reduction(+:ed_c,ed_f)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
!
!   normalization
        st_c = OMP_GET_WTIME()
        call normalize_bwd_c2r_FFT_SMP(ist, ied, Ncomp, Nfft, X,        &
     &                                 NFFT_c, C_FFTW)
        ed_c = OMP_GET_WTIME() - st_c
!
        st_f = OMP_GET_WTIME()
        do j = ist, ied
          call dfftw_execute(plan_backward(j))
        end do
        ed_f = OMP_GET_WTIME() - st_f
!
        st_c = OMP_GET_WTIME()
        do i = 1, Nfft
          X(ist:ied,i) = X_FFTW(i,ist:ied)
        end do
        ed_c = OMP_GET_WTIME() - st_c
      end do
!$omp end parallel do
!
      elapsed_fft = elapsed_fft + ed_f / dble(Nsmp)
      elapsed_cpy = elapsed_cpy + ed_c / dble(Nsmp)
!
      end subroutine FFTW_backward_SMP
!
! ------------------------------------------------------------------
!
      end module FFTW3_wrapper
