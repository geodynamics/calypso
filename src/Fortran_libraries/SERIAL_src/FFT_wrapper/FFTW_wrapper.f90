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
!!      subroutine init_4_FFTW_smp(Ncomp, Nfft,                         &
!!     &          plan_forward, plan_backward,  aNfft, X_FFTW, C_FFTW)
!!      subroutine init_4_FFTW_mul_smp(Nsmp, Nstacksmp, Ncomp, Nfft,    &
!!     &          plan_forward_smp, plan_backward_smp,                  &
!!     &          aNfft, X_FFTW, C_FFTW)
!!
!!   wrapper subroutine for initierize FFTW plans
!! ------------------------------------------------------------------
!!      subroutine destroy_FFTW_smp(Ncomp, plan_forward, plan_backward)
!!
!!   wrapper subroutine for clear FFTW plans
!! ------------------------------------------------------------------
!!
!!      subroutine FFTW_forward_SMP(plan_forward, Nsmp, Nstacksmp,      &
!!     &          Ncomp, Nfft, aNfft, X, X_FFTW, C_FFTW)
!!      subroutine FFTW_mul_forward_SMP(plan_forward_smp,               &
!!     &          Nsmp, Nstacksmp, Ncomp, Nfft, aNfft, X, X_FFTW, C_FFTW)
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
!!      subroutine FFTW_mul_backward_SMP(plan_backward_smp,             &
!!     &          Nsmp, Nstacksmp, Ncomp, Nfft, X, X_FFTW, C_FFTW)
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
      module FFTW_wrapper
!
      use m_precision
      use m_constants
      use m_work_time
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
      real(kind = kreal) :: elapsed_fftw(3)
!
      private :: iu
      private :: FFTW_ESTIMATE
!
      private :: copy_fftw_real_from_rtp_smp
      private :: copy_fftw_spec_to_rtm_smp
      private :: copy_fftw_spec_from_rtm_smp
      private :: copy_fftw_real_to_rtp_smp
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
!
!
      do j = 1, Ncomp
        call dfftw_plan_dft_r2c_1d(plan_forward(j), Nfft,               &
     &      X_FFTW(1,j), C_FFTW(1,j) , FFTW_ESTIMATE)
        call dfftw_plan_dft_c2r_1d(plan_backward(j), Nfft,              &
     &      C_FFTW(1,j), X_FFTW(1,j) , FFTW_ESTIMATE)
      end do
      aNfft = one / dble(Nfft)
!
      end subroutine init_4_FFTW_smp
!
! ------------------------------------------------------------------
!
      subroutine init_4_FFTW_mul_smp(Nsmp, Nstacksmp, Ncomp, Nfft,      &
     &          plan_forward_smp, plan_backward_smp,                    &
     &          aNfft, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Nfft, Ncomp
!
      integer(kind = fftw_plan), intent(inout)                          &
     &                          :: plan_forward_smp(Nsmp)
      integer(kind = fftw_plan), intent(inout)                          &
     &                          :: plan_backward_smp(Nsmp)
      real(kind = kreal), intent(inout) :: aNfft
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Ncomp)
!
      integer(kind = kint) :: ip, ist, howmany, inembed, istride
      integer(kind = kint) :: idist_r, idist_c
!
!
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        howmany = Nstacksmp(ip  ) - Nstacksmp(ip-1)
        inembed = 0
        istride = 1
        idist_r = Nfft
        idist_c = Nfft/2+1
!
        call dfftw_plan_many_dft_r2c                                    &
     &     (plan_forward_smp(ip), ione, Nfft, howmany,                  &
            X_FFTW(1,ist), inembed, istride, idist_r,                   &
            C_FFTW(1,ist), inembed, istride, idist_c, FFTW_ESTIMATE)
        call dfftw_plan_many_dft_c2r                                    &
     &     (plan_backward_smp(ip), ione, Nfft, howmany,                 &
            C_FFTW(1,ist), inembed, istride, idist_c,                   &
     &      X_FFTW(1,ist), inembed, istride, idist_r, FFTW_ESTIMATE)
      end do
      aNfft = one / dble(Nfft)
!
      end subroutine init_4_FFTW_mul_smp
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
      integer(kind = kint) ::  j, ip, ist, ied
      real :: dummy(Nsmp,3), rtmp(Nsmp,3)
!
!
!$omp parallel do private(j,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip) 
!
        call cpu_time(dummy(ip,1))
        call copy_fftw_real_from_rtp_smp(Ncomp, Nfft, ist, ied,         &
     &      X, X_FFTW)
        call cpu_time(rtmp(ip,1))
!
        call cpu_time(dummy(ip,2))
        do j = ist, ied
          call dfftw_execute(plan_forward(j))
        end do
        call cpu_time(rtmp(ip,2))
!
!   normalization
        call cpu_time(dummy(ip,3))
        call copy_fftw_spec_to_rtm_smp(Ncomp, Nfft, ist, ied,           &
     &      aNfft, C_FFTW, X)
        call cpu_time(rtmp(ip,3))
      end do
!$omp end parallel do
!
      do ip = 1, Nsmp
        elapsed_fftw(1:3) = elapsed_fftw(1:3)                           &
     &                     + rtmp(ip,1:3) - dummy(ip,1:3)
      end do
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
      integer(kind = kint) ::  j, ip, ist, ied
      real :: dummy(Nsmp,3), rtmp(Nsmp,3)
!
!
!$omp parallel do private(j,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
!
!   normalization
        call cpu_time(dummy(ip,3))
        call copy_fftw_spec_from_rtm_smp(Ncomp, Nfft, ist, ied,         &
     &      X, C_FFTW)
        call cpu_time(rtmp(ip,3))
!
        call cpu_time(dummy(ip,2))
        do j = ist, ied
          call dfftw_execute(plan_backward(j))
        end do
        call cpu_time(rtmp(ip,2))
!
        call cpu_time(dummy(ip,1))
        call copy_fftw_real_to_rtp_smp(Ncomp, Nfft, ist, ied,           &
     &      X_FFTW, X)
        call cpu_time(rtmp(ip,1))
      end do
!$omp end parallel do
!
      do ip = 1, Nsmp
        elapsed_fftw(1:3) = elapsed_fftw(1:3)                           &
     &                     + rtmp(ip,1:3) - dummy(ip,1:3)
      end do
!
      end subroutine FFTW_backward_SMP
!
! ------------------------------------------------------------------
!
      subroutine FFTW_mul_forward_SMP(plan_forward_smp,                 &
     &          Nsmp, Nstacksmp, Ncomp, Nfft, aNfft, X, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = fftw_plan), intent(in) :: plan_forward_smp(Nsmp)
      real(kind = kreal), intent(in) :: aNfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Ncomp)
!
      integer(kind = kint) ::  ip, ist, ied
      real :: dummy(3), rtmp(3)
!
!
      call cpu_time(dummy(1))
!$omp parallel do private(ip,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
        call copy_fftw_real_from_rtp_smp(Ncomp, Nfft, ist, ied,         &
     &      X, X_FFTW)
      end do
!$omp end parallel do
      call cpu_time(rtmp(1))
!
      call cpu_time(dummy(2))
!$omp parallel do private(ip)
      do ip = 1, Nsmp
        call dfftw_execute(plan_forward_smp(ip))
      end do
!$omp end parallel do
      call cpu_time(rtmp(2))
!
!   normalization
      call cpu_time(dummy(3))
!$omp parallel do private(ip,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
        call copy_fftw_spec_to_rtm_smp(Ncomp, Nfft, ist, ied,           &
     &      aNfft, C_FFTW, X)
      end do
!$omp end parallel do
      call cpu_time(rtmp(3))
      elapsed_fftw(1:3) = elapsed_fftw(1:3) + rtmp(1:3) - dummy(1:3)
!
      end subroutine FFTW_mul_forward_SMP
!
! ------------------------------------------------------------------
!
      subroutine FFTW_mul_backward_SMP(plan_backward_smp,               &
     &          Nsmp, Nstacksmp, Ncomp, Nfft, X, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = fftw_plan), intent(in) :: plan_backward_smp(Nsmp)
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Ncomp)
!
      integer(kind = kint) :: ip, ist, ied
      real :: dummy(3), rtmp(3)
!
!
!   normalization
      call cpu_time(dummy(3))
!$omp parallel do private(ip,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
        call copy_fftw_spec_from_rtm_smp(Ncomp, Nfft, ist, ied,         &
     &      X, C_FFTW)
      end do
!$omp end parallel do
      call cpu_time(rtmp(3))
!
      call cpu_time(dummy(2))
!$omp parallel do private(ip)
      do ip = 1, Nsmp
        call dfftw_execute(plan_backward_smp(ip))
      end do
!$omp end parallel do
      call cpu_time(rtmp(2))
!
      call cpu_time(dummy(1))
!$omp parallel do private(ip,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
        call copy_fftw_real_to_rtp_smp(Ncomp, Nfft, ist, ied,           &
     &      X_FFTW, X)
      end do
!$omp end parallel do
      call cpu_time(rtmp(1))
!
      elapsed_fftw(1:3) = elapsed_fftw(1:3) + rtmp(1:3) - dummy(1:3)
!
      end subroutine FFTW_mul_backward_SMP
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_fftw_real_from_rtp_smp(Ncomp, Nfft,               &
     &          ist_fld, ied_fld, X, X_FFTW)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = kint), intent(in) :: ist_fld, ied_fld
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
!
      integer(kind = kint) ::  j
!
!
      do j = ist_fld, ied_fld
        X_FFTW(1:Nfft,j) = X(j,1:Nfft)
      end do
!
      end subroutine copy_fftw_real_from_rtp_smp
!
! ------------------------------------------------------------------
!
      subroutine copy_fftw_spec_to_rtm_smp(Ncomp, Nfft,                 &
     &          ist_fld, ied_fld, aNfft, C_FFTW, X)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = kint), intent(in) :: ist_fld, ied_fld
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = fftw_complex), intent(in)                          &
     &              :: C_FFTW(Nfft/2+1,Ncomp)
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
      integer(kind = kint) ::  i, j
!
!   normalization
      do j = ist_fld, ied_fld
        X(j,1) = aNfft * real(C_FFTW(1,j))
        do i = 2, (Nfft+1)/2
          X(j,2*i-1) = two * aNfft * real(C_FFTW(i,j))
          X(j,2*i  ) = two * aNfft * real(C_FFTW(i,j)*iu)
        end do 
        i = (Nfft+1)/2 + 1
        X(j,2) = two * aNfft * real(C_FFTW(i,j))
      end do
!
      end subroutine copy_fftw_spec_to_rtm_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_fftw_spec_from_rtm_smp(Ncomp, Nfft,               &
     &          ist_fld, ied_fld, X, C_FFTW)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = kint), intent(in) :: ist_fld, ied_fld
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Ncomp)
!
      integer(kind = kint) ::  i, j
!
!   normalization
      do j = ist_fld, ied_fld
        C_FFTW(1,j) = cmplx(X(j,1), zero, kind(0d0))
        do i = 2, (Nfft+1)/2
          C_FFTW(i,j) = half * cmplx(X(j,2*i-1), -X(j,2*i  ),kind(0d0))
        end do
        i = (Nfft+1)/2 + 1
        C_FFTW(i,j) = half * cmplx(X(j,2), zero, kind(0d0))
      end do
!
      end subroutine copy_fftw_spec_from_rtm_smp
!
! ------------------------------------------------------------------
!
      subroutine copy_fftw_real_to_rtp_smp(Ncomp, Nfft,                 &
     &          ist_fld, ied_fld, X_FFTW, X)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = kint), intent(in) :: ist_fld, ied_fld
      real(kind = kreal), intent(in) :: X_FFTW(Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
      integer(kind = kint) ::  j
!
!
      do j = ist_fld, ied_fld
        X(j,1:Nfft) = X_FFTW(1:Nfft,j)
      end do
!
      end subroutine copy_fftw_real_to_rtp_smp
!
! ------------------------------------------------------------------
!
      end module FFTW_wrapper
