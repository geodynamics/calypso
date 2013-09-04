!>@file   FFTW_wrapper.f90
!!@brief  module FFTW_wrapper
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_4_FFTW_smp(Nsmp, Nfft,                          &
!!     &          plan_forward, plan_backward,  aNfft, X_FFTW, C_FFTW)
!!
!!   wrapper subroutine for initierize FFTW plans
!! ------------------------------------------------------------------
!!      subroutine destroy_FFTW_smp(Nsmp, plan_forward, plan_backward)
!!
!!   wrapper subroutine for clear FFTW plans
!! ------------------------------------------------------------------
!!
!!      subroutine FFTW_forward_SMP(plan_forward, Nsmp, Nstacksmp,      &
!!     &          M, Nfft, aNfft, X, X_FFTW, C_FFTW)
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
!!     &          M, Nfft, X, X_FFTW, C_FFTW)
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
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!!
!!@n @param plan_forward   FFTW plan for forward transform
!!@n @param plan_backward  FFTW plan for backward transform
!!@n @param aNfft       normalization parameter for FFTW (= 1 / Nfft)
!!@n @param X_FFTW      real data for multiple Fourier transform
!!@n @param C_FFTW      spectrum data for multiple Fourier transform
!
      module FFTW_wrapper
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
      subroutine init_4_FFTW_smp(Nsmp, Nfft,                            &
     &          plan_forward, plan_backward,  aNfft, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp
!
      integer(kind = fftw_plan), intent(inout) :: plan_forward(Nsmp)
      integer(kind = fftw_plan), intent(inout) :: plan_backward(Nsmp)
      real(kind = kreal), intent(inout) :: aNfft
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Nsmp)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Nsmp)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, Nsmp
        call dfftw_plan_dft_r2c_1d(plan_forward(ip), Nfft,              &
     &      X_FFTW(1,ip), C_FFTW(1,ip) , FFTW_ESTIMATE)
        call dfftw_plan_dft_c2r_1d(plan_backward(ip), Nfft,             &
     &      C_FFTW(1,ip), X_FFTW(1,ip) , FFTW_ESTIMATE)
      end do
      aNfft = one / dble(Nfft)
!
      end subroutine init_4_FFTW_smp
!
! ------------------------------------------------------------------
!
      subroutine destroy_FFTW_smp(Nsmp, plan_forward, plan_backward)
!
      integer(kind = kint), intent(in) ::  Nsmp
!
      integer(kind = fftw_plan), intent(in) :: plan_forward(Nsmp)
      integer(kind = fftw_plan), intent(in) :: plan_backward(Nsmp)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, Nsmp
        call dfftw_destroy_plan(plan_forward(ip))
        call dfftw_destroy_plan(plan_backward(ip))
       end do
!
      end subroutine destroy_FFTW_smp
!
! ------------------------------------------------------------------
!
      subroutine FFTW_forward_SMP(plan_forward, Nsmp, Nstacksmp,        &
     &          M, Nfft, aNfft, X, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
      integer(kind = fftw_plan), intent(in) :: plan_forward(Nsmp)
      real(kind = kreal), intent(in) :: aNfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Nsmp)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Nsmp)
!
      integer(kind = kint) ::  i, j, ip, ist, num, inum
!
!
!$omp parallel do private(i,j,ist,num,inum)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1)
        num = Nstacksmp(ip) - Nstacksmp(ip-1)
        do inum = 1, num
          j = ist + inum
!
          do i = 1, Nfft
            X_FFTW(i,ip) = X(j,i)
          end do
!
          call dfftw_execute_dft_r2c(plan_forward(ip),                  &
     &        X_FFTW(1,ip), C_FFTW(1,ip) )
!
!   normalization
!
          X(j,1) = aNfft * real(C_FFTW(1,ip))
          do i = 2, (Nfft+1)/2
            X(j,2*i-1) = two * aNfft * real(C_FFTW(i,ip))
            X(j,2*i  ) = two * aNfft * real(C_FFTW(i,ip)*iu)
          end do
          i = (Nfft+1)/2 + 1
          X(j,2) = two * aNfft * real(C_FFTW(i,ip))
        end do
!
      end do
!$omp end parallel do
!
      end subroutine FFTW_forward_SMP
!
! ------------------------------------------------------------------
!
      subroutine FFTW_backward_SMP(plan_backward, Nsmp, Nstacksmp,      &
     &          M, Nfft, X, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
      integer(kind = fftw_plan), intent(in) :: plan_backward(Nsmp)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Nsmp)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Nsmp)
!
      integer(kind = kint) ::  i, j, ip, ist, inum, num
!
!
!$omp parallel do private(i,j,ist,num,inum)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1)
        num = Nstacksmp(ip) - Nstacksmp(ip-1)
        do inum = 1, num
          j = ist + inum
!
!   normalization
!
          C_FFTW(1,ip) = cmplx(X(j,1), zero, kind(0d0))
          do i = 2, (Nfft+1)/2
            C_FFTW(i,ip) = half                                         &
     &                     * cmplx(X(j,2*i-1), -X(j,2*i  ), kind(0d0))
          end do
          i = (Nfft+1)/2 + 1
          C_FFTW(i,ip) = half * cmplx(X(j,2), zero, kind(0d0))
!
          call dfftw_execute_dft_c2r(plan_backward(ip),                 &
     &        C_FFTW(1,ip),  X_FFTW(1,ip) )
!
          do i = 1, Nfft
            X(j,i) = X_FFTW(i,ip)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine FFTW_backward_SMP
!
! ------------------------------------------------------------------
!
      end module FFTW_wrapper
