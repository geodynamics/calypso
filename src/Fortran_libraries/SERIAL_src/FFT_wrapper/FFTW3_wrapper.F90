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
!!     &          plan_forward, plan_backward,  aNfft, X_FFTW, C_FFTW)
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
!!     &          Ncomp, Nfft, aNfft, X, X_FFTW, C_FFTW)
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
!!     &          Ncomp, Nfft, X, X_FFTW, C_FFTW)
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
      use m_precision
      use m_constants
!
      implicit none
!
!>      plan ID for fftw
      integer, parameter :: fftw_plan =    8
!>      data size of complex for FFTW3
      integer, parameter :: fftw_complex = 8
!
!>      Unit imaginary number
      complex(kind = fftw_complex), parameter :: iu = (0.0d0,1.0d0)
!
!>      estimation flag for FFTW
      integer(kind = 4), parameter :: FFTW_ESTIMATE = 64
!>      Meajor flag for FFTW
      integer(kind = 4), parameter :: FFTW_MEASURE = 0
!
      real(kind = kreal) :: elapsed_fftw(3) = (/0.0,0.0,0.0/)
!
      private :: iu
      private :: FFTW_ESTIMATE
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_4_FFTW_smp(Ncomp, Nfft,                           &
     &          plan_forward, plan_backward,  aNfft, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Ncomp
!
      integer(kind = fftw_plan), intent(inout) :: plan_forward(Ncomp)
      integer(kind = fftw_plan), intent(inout) :: plan_backward(Ncomp)
      real(kind = kreal), intent(inout) :: aNfft
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Ncomp)
!
      integer(kind = kint) :: j
      integer :: Nfft4
!
!
      Nfft4 = int(Nfft)
      do j = 1, Ncomp
#ifdef FFTW3_C
        call kemo_fftw_plan_dft_r2c_1d(plan_forward(j), Nfft4,          &
     &      X_FFTW(1,j), C_FFTW(1,j) , FFTW_ESTIMATE)
        call kemo_fftw_plan_dft_c2r_1d(plan_backward(j), Nfft4,         &
     &      C_FFTW(1,j), X_FFTW(1,j) , FFTW_ESTIMATE)
#else
        call dfftw_plan_dft_r2c_1d(plan_forward(j), Nfft4,              &
     &      X_FFTW(1,j), C_FFTW(1,j) , FFTW_ESTIMATE)
        call dfftw_plan_dft_c2r_1d(plan_backward(j), Nfft4,             &
     &      C_FFTW(1,j), X_FFTW(1,j) , FFTW_ESTIMATE)
#endif
      end do
      aNfft = one / dble(Nfft)
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
#ifdef FFTW3_C
        call kemo_fftw_destroy_plan(plan_forward(j))
        call kemo_fftw_destroy_plan(plan_backward(j))
        call kemo_fftw_cleanup
#else
        call dfftw_destroy_plan(plan_forward(j))
        call dfftw_destroy_plan(plan_backward(j))
        call dfftw_cleanup
#endif
      end do
!
      end subroutine destroy_FFTW_smp
!
! ------------------------------------------------------------------
!
      subroutine FFTW_forward_SMP(plan_forward, Nsmp, Nstacksmp,        &
     &          Ncomp, Nfft, aNfft, X, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = fftw_plan), intent(in) :: plan_forward(Ncomp)
      real(kind = kreal), intent(in) :: aNfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Ncomp)
!
      integer(kind = kint) ::  i, j, ip, ist, ied
      real :: dummy(Nsmp,3), rtmp(Nsmp,3)
!
!
!$omp parallel do private(i,j,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip) 
!
!        call cpu_time(dummy(ip,1))
        do j = ist, ied
          X_FFTW(1:Nfft,j) = X(j,1:Nfft)
        end do
!        call cpu_time(rtmp(ip,1))
!
!        call cpu_time(dummy(ip,2))
        do j = ist, ied
#ifdef FFTW3_C
          call kemo_fftw_execute(plan_forward(j))
#else
          call dfftw_execute(plan_forward(j))
#endif
        end do
!        call cpu_time(rtmp(ip,2))
!
!   normalization
!        call cpu_time(dummy(ip,3))
        do j = ist, ied
          X(j,1) = aNfft * real(C_FFTW(1,j))
          do i = 2, (Nfft+1)/2
            X(j,2*i-1) = two * aNfft * real(C_FFTW(i,j))
            X(j,2*i  ) = two * aNfft * real(C_FFTW(i,j)*iu)
          end do 
          i = (Nfft+1)/2 + 1
          X(j,2) = two * aNfft * real(C_FFTW(i,j))
        end do
!        call cpu_time(rtmp(ip,3))
      end do
!$omp end parallel do
!
!      do ip = 1, Nsmp
!        elapsed_fftw(1:3) = elapsed_fftw(1:3)                          &
!     &                     + rtmp(ip,1:3) - dummy(ip,1:3)
!      end do
!
      end subroutine FFTW_forward_SMP
!
! ------------------------------------------------------------------
!
      subroutine FFTW_backward_SMP(plan_backward, Nsmp, Nstacksmp,      &
     &          Ncomp, Nfft, X, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = fftw_plan), intent(in) :: plan_backward(Ncomp)
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Ncomp)
!
      integer(kind = kint) :: i, j, ip, ist, ied
      real :: dummy(Nsmp,3), rtmp(Nsmp,3)
!
!
!$omp parallel do private(i,j,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
!
!   normalization
!        call cpu_time(dummy(ip,3))
!   normalization
        do j = ist, ied
          C_FFTW(1,j) = cmplx(X(j,1), zero, kind(0d0))
          do i = 2, (Nfft+1)/2
            C_FFTW(i,j) = half * cmplx(X(j,2*i-1), -X(j,2*i),kind(0d0))
          end do
          i = (Nfft+1)/2 + 1
          C_FFTW(i,j) = half * cmplx(X(j,2), zero, kind(0d0))
        end do
!        call cpu_time(rtmp(ip,3))
!
!        call cpu_time(dummy(ip,2))
        do j = ist, ied
#ifdef FFTW3_C
          call kemo_fftw_execute(plan_backward(j))
#else
          call dfftw_execute(plan_backward(j))
#endif
        end do
!        call cpu_time(rtmp(ip,2))
!
!        call cpu_time(dummy(ip,1))
        do j = ist, ied
          X(j,1:Nfft) = X_FFTW(1:Nfft,j)
        end do
!        call cpu_time(rtmp(ip,1))
      end do
!$omp end parallel do
!
!      do ip = 1, Nsmp
!        elapsed_fftw(1:3) = elapsed_fftw(1:3)                          &
!     &                     + rtmp(ip,1:3) - dummy(ip,1:3)
!      end do
!
      end subroutine FFTW_backward_SMP
!
! ------------------------------------------------------------------
!
      end module FFTW3_wrapper
