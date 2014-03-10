!>@file   m_FFTW_wrapper.f90
!!@brief  module m_FFTW_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_4_FFTW(Nsmp, Nstacksmp, Nfft)
!!      subroutine finalize_4_FFTW(Nsmp, Nstacksmp)
!!      subroutine verify_work_4_FFTW(Nsmp, Nstacksmp, Nfft)
!!
!!      subroutine init_FFTW_mul(Nsmp, Nstacksmp, Nfft)
!!      subroutine finalize_FFTW_mul(Nsmp)
!!      subroutine verify_work_FFTW_mul(Nsmp, Nstacksmp, Nfft)
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine FFTW_forward(Nsmp, Nstacksmp, Ncomp, Nfft, X)
!!      subroutine FFTW_mul_forward(Nsmp, Nstacksmp, Ncomp, Nfft, X)
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
!!      subroutine FFTW_backward(Nsmp, Nstacksmp, Ncomp, Nfft, X)
!!      subroutine FFTW_mul_backward(Nsmp, Nstacksmp, Ncomp, Nfft, X)
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
!
      module m_FFTW_wrapper
!
      use m_precision
      use m_constants
!
      use FFTW_wrapper
!
      implicit none
!
!>      plan ID for backward transform
      integer(kind = fftw_plan), allocatable :: plan_backward(:)
!>      plan ID for forward transform
      integer(kind = fftw_plan), allocatable :: plan_forward(:)
!
!>      normalization parameter for FFTW (= 1 / Nfft)
      real(kind = kreal) :: aNfft
!>      real data for multiple Fourier transform
      real(kind = kreal), allocatable :: X_FFTW(:,:)
!>      spectrum data for multiple Fourier transform
      complex(kind = fftw_complex), allocatable :: C_FFTW(:,:)
!>      flag for number of components for Fourier transform
      integer(kind = kint) :: iflag_fft_len =  -1
!
!
      private :: iflag_fft_len
      private :: plan_backward, plan_forward
      private :: X_FFTW, C_FFTW, aNfft
!
      private :: allocate_work_4_FFTW, deallocate_work_4_FFTW
!
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_4_FFTW(Nsmp, Nstacksmp, Nfft)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) ::  Nfft
!
!
      call allocate_work_4_FFTW(Nstacksmp(Nsmp), Nstacksmp(Nsmp), Nfft)
      call init_4_FFTW_smp(Nstacksmp(Nsmp), Nfft,                       &
     &    plan_forward, plan_backward, aNfft, X_FFTW, C_FFTW)
!
      end subroutine init_4_FFTW
!
! ------------------------------------------------------------------
!
      subroutine finalize_4_FFTW(Nsmp, Nstacksmp)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
!
      call destroy_FFTW_smp(Nstacksmp(Nsmp),                            &
     &    plan_forward, plan_backward)
      call deallocate_work_4_FFTW
!
      end subroutine finalize_4_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_work_4_FFTW(Nsmp, Nstacksmp, Nfft)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) ::  Nfft
!
!
      if( iflag_fft_len .lt. 0) then
        call init_4_FFTW(Nsmp, Nstacksmp, Nfft)
        return
      end if
!
      if( iflag_fft_len .ne. Nfft*Nstacksmp(Nsmp)) then
        call finalize_4_FFTW(Nsmp, Nstacksmp)
        call init_4_FFTW(Nsmp, Nstacksmp, Nfft)
      end if
!
      end subroutine verify_work_4_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine FFTW_forward(Nsmp, Nstacksmp, Ncomp, Nfft, X)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
!
!
      call FFTW_forward_SMP(plan_forward, Nsmp, Nstacksmp,              &
     &          Ncomp, Nfft, aNfft, X, X_FFTW, C_FFTW)
!
      end subroutine FFTW_forward
!
! ------------------------------------------------------------------
!
      subroutine FFTW_backward(Nsmp, Nstacksmp, Ncomp, Nfft, X)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
!
      call FFTW_backward_SMP(plan_backward, Nsmp, Nstacksmp,            &
     &    Ncomp, Nfft, X, X_FFTW, C_FFTW)
!
      end subroutine FFTW_backward
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine init_FFTW_mul(Nsmp, Nstacksmp, Nfft)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) ::  Nfft
!
!
      call allocate_work_4_FFTW(Nsmp, Nstacksmp(Nsmp), Nfft)
      call init_4_FFTW_mul_smp(Nsmp, Nstacksmp, Nstacksmp(Nsmp), Nfft,  &
     &    plan_forward, plan_backward, aNfft, X_FFTW, C_FFTW)
!
      end subroutine init_FFTW_mul
!
! ------------------------------------------------------------------
!
      subroutine finalize_FFTW_mul(Nsmp)
!
      integer(kind = kint), intent(in) ::  Nsmp
!
!
      call destroy_FFTW_smp(Nsmp, plan_forward, plan_backward)
      call deallocate_work_4_FFTW
!
      end subroutine finalize_FFTW_mul
!
! ------------------------------------------------------------------
!
      subroutine verify_work_FFTW_mul(Nsmp, Nstacksmp, Nfft)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) ::  Nfft
!
!
      if( iflag_fft_len .lt. 0) then
        call init_FFTW_mul(Nsmp, Nstacksmp, Nfft)
        return
      end if
!
      if( iflag_fft_len .ne. Nfft*Nstacksmp(Nsmp)) then
        call finalize_FFTW_mul(Nsmp)
        call init_FFTW_mul(Nsmp, Nstacksmp, Nfft)
      end if
!
      end subroutine verify_work_FFTW_mul
!
! ------------------------------------------------------------------
!
      subroutine FFTW_mul_forward(Nsmp, Nstacksmp, Ncomp, Nfft, X)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
!
!
      call FFTW_mul_forward_SMP(plan_forward, Nsmp, Nstacksmp,          &
     &    Ncomp, Nfft, aNfft, X, X_FFTW, C_FFTW)
!
      end subroutine FFTW_mul_forward
!
! ------------------------------------------------------------------
!
      subroutine FFTW_mul_backward(Nsmp, Nstacksmp, Ncomp, Nfft, X)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
!
      call FFTW_mul_backward_SMP(plan_backward, Nsmp, Nstacksmp,        &
     &    Ncomp, Nfft, X, X_FFTW, C_FFTW)
!
      end subroutine FFTW_mul_backward
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine allocate_work_4_FFTW(Nplan, Ncomp, Nfft)
!
      integer(kind = kint), intent(in) :: Nplan, Ncomp, Nfft
!
!
      allocate(plan_forward(Nplan))
      allocate(plan_backward(Nplan))
!
      iflag_fft_len = Nfft*Ncomp
      allocate( X_FFTW(Nfft,Ncomp) )
      allocate( C_FFTW(Nfft/2+1,Ncomp) )
      X_FFTW = 0.0d0
      C_FFTW = 0.0d0
!
      end subroutine allocate_work_4_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine deallocate_work_4_FFTW
!
      deallocate(x_FFTW, C_FFTW)
      deallocate(plan_forward, plan_backward)
      iflag_fft_len = 0
!
      end subroutine deallocate_work_4_FFTW
!
! ------------------------------------------------------------------
!
      end module m_FFTW_wrapper
