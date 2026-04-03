!>@file   normalize_for_FFTW.f90
!!@brief  module normalize_for_FFTW
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine normalize_fwd_r2c_fft_SMP(ist_nd, ied_nd,            &
!!     &          Ncomp, NFFT_c, C_FFT, Nfft, aNfft, X)
!!        integer(kind = kint), intent(in) :: ist_nd, ied_nd
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: aNfft
!!        complex(kind = kreal), intent(in) :: C_FFT(NFFT_c,Ncomp)
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!
!!   wrapper subroutine for initierize FFTW plans
!! ------------------------------------------------------------------
!!      subroutine destroy_FFTW_mul_smp                                 &
!!     &         (Nsmp, plan_backward_smp, plan_backward)
!!        CAUTION!!  dfftw_destroy_plan oftern makes SEGMENTAION FAULT!!
!!
!!
!!   wrapper subroutine for clear FFTW plans
!! ------------------------------------------------------------------
!!
!!      subroutine FFTW_mul_forward_SMP(plan_forward_smp,               &
!!     &          Nsmp, Nstacksmp, Ncomp, Nfft, aNfft, Nfft_c,          &
!!     &          X, X_FFTW, C_FFTW, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, Nfft_c
!!        integer(kind = fftw_plan), intent(in) :: plan_forward_smp(Nsmp)
!!        real(kind = kreal), intent(in) :: aNfft
!!        real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
!!        real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
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
!!      subroutine normalize_bwd_c2r_FFT_SMP(ist_nd, ied_nd,            &
!!     &          Ncomp, Nfft, X, NFFT_c, C_FFT)
!!        integer(kind = kint), intent(in) :: ist_nd, ied_nd
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!!        complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c,Ncomp)
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
      module normalize_for_FFTW
!
      use m_precision
      use m_constants
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine normalize_fwd_r2c_fft_SMP(ist_nd, ied_nd,              &
     &          Ncomp, NFFT_c, C_FFT, Nfft, aNfft, X)
!
      integer(kind = kint), intent(in) :: ist_nd, ied_nd
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = kreal), intent(in) :: C_FFT(NFFT_c,Ncomp)
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
      integer(kind = kint) :: i
!
!
      X(ist_nd:ied_nd,1) = aNfft * real(C_FFT(1,     ist_nd:ied_nd))
      X(ist_nd:ied_nd,2) = aNfft * real(C_FFT(NFFT_c,ist_nd:ied_nd))
      do i = 2, NFFT_c - 1
        X(ist_nd:ied_nd,2*i-1)                                          &
     &     =  two * aNfft * real(C_FFT(i,ist_nd:ied_nd))
        X(ist_nd:ied_nd,2*i  )                                          &
     &     = -two * aNfft * imag(C_FFT(i,ist_nd:ied_nd))
      end do 
!
      end subroutine normalize_fwd_r2c_fft_SMP
!
! ------------------------------------------------------------------
!
      subroutine normalize_bwd_c2r_FFT_SMP(ist_nd, ied_nd,              &
     &          Ncomp, Nfft, X, NFFT_c, C_FFT)
!
      integer(kind = kint), intent(in) :: ist_nd, ied_nd
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!
      complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c,Ncomp)
!
      integer(kind = kint) :: i, nd
!
!
      do nd = ist_nd, ied_nd
        C_FFT(1,nd) = cmplx(X(nd,1), zero, kind(0d0))
        do i = 2, NFFT_c - 1
          C_FFT(i,nd) = half * cmplx(X(nd,2*i-1), -X(nd,2*i),kind(0d0))
        end do
        C_FFT(NFFT_c,nd) = cmplx(X(nd,2), zero, kind(0d0))
      end do
!
      end subroutine normalize_bwd_c2r_FFT_SMP
!
! ------------------------------------------------------------------
!
      end module normalize_for_FFTW
