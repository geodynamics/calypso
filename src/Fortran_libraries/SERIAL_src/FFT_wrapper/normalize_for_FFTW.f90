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
!!      subroutine normalize_fwd_FFTW(aNfft, Ncomp_smp, NFFT_c, C_FFT)
!!      subroutine normalize_fwd_OMP_FFTW(aNfft, Ncomp_smp, NFFT_c,     &
!!     &                                  C_FFT)
!!        integer(kind = kint), intent(in) :: Ncomp_smp, NFFT_c
!!        real(kind = kreal), intent(in) :: aNfft
!!        complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c*Ncomp_smp)
!!
!!      subroutine swap_from_fwd_single_FFTW(nd8, Ncomp, NFFT_c, C_FFT, &
!!     &                                     Nfft, X)
!!        integer(kind = kint_gl), intent(in) :: nd8
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        complex(kind = kreal), intent(in) :: C_FFT(NFFT_c)
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!      subroutine copy_from_prt_fwd_FFTW(NFFT_c, C_FFT, Nfft, X)
!!        integer(kind = kint), intent(in) :: Nfft, NFFT_c
!!        complex(kind = kreal), intent(in) :: C_FFT(NFFT_c)
!!        real(kind = kreal), intent(inout) :: X(Nfft)
!!      subroutine copy_from_prt_fwd_OMP_FFTW(Ncomp_c, NFFT_c, C_FFT,   &
!!     &                                      Ncomp, Nfft, X)
!!        integer(kind = kint), intent(in) :: Ncomp_c, NFFT_c
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        complex(kind = kreal), intent(in) :: C_FFT(NFFT_c,Ncomp_c)
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!
!!      subroutine norm_prt_bwd_single_FFTW(nd8, Ncomp, Nfft, X,        &
!!     &                                    NFFT_c, C_FFT)
!!        integer(kind = kint_gl), intent(in) :: nd8
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!!        complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c)
!!      subroutine norm_copy_to_prt_bwd_FFTW(Nfft, X, NFFT_c, C_FFT)
!!        integer(kind = kint), intent(in) :: Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: X(Nfft)
!!        complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c)
!!      subroutine norm_copy_to_prt_bwd_OMP_FFTW(Ncomp, Nfft, X,        &
!!     &                                         Ncomp_c, NFFT_c, C_FFT)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        integer(kind = kint), intent(in) :: Ncomp_c, NFFT_c
!!        real(kind = kreal), intent(in) :: X(Nfft,Ncomp)
!!        complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c,Ncomp_c)
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
      subroutine normalize_fwd_FFTW(aNfft, Ncomp_smp, NFFT_c, C_FFT)
!
      integer(kind = kint), intent(in) :: Ncomp_smp, NFFT_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c*Ncomp_smp)
!
      C_FFT(1:NFFT_c*Ncomp_smp) = aNfft * C_FFT(1:NFFT_c*Ncomp_smp)
!
      end subroutine normalize_fwd_FFTW
!
! ------------------------------------------------------------------
!
      subroutine normalize_fwd_OMP_FFTW(aNfft, Ncomp_smp, NFFT_c,       &
     &                                  C_FFT)
!
      integer(kind = kint), intent(in) :: Ncomp_smp, NFFT_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c*Ncomp_smp)
!
!$omp parallel workshare
      C_FFT(1:NFFT_c*Ncomp_smp) = aNfft * C_FFT(1:NFFT_c*Ncomp_smp)
!$omp end parallel workshare
!
      end subroutine normalize_fwd_OMP_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine swap_from_fwd_single_FFTW(nd8, Ncomp, NFFT_c, C_FFT,   &
     &                                     Nfft, X)
!
      integer(kind = kint_gl), intent(in) :: nd8
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      complex(kind = kreal), intent(in) :: C_FFT(NFFT_c)
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
      integer(kind = kint) :: i
!
!
      X(nd8,1) = real(C_FFT(1))
      X(nd8,2) = real(C_FFT(NFFT_c))
      do i = 2, NFFT_c - 1
        X(nd8,2*i-1) =  two * real(C_FFT(i))
        X(nd8,2*i  ) = -two * imag(C_FFT(i))
      end do
!
      end subroutine swap_from_fwd_single_FFTW
!
! ------------------------------------------------------------------
!
      subroutine copy_from_prt_fwd_FFTW(NFFT_c, C_FFT, Nfft, X)
!
      integer(kind = kint), intent(in) :: Nfft, NFFT_c
      complex(kind = kreal), intent(in) :: C_FFT(NFFT_c)
!
      real(kind = kreal), intent(inout) :: X(Nfft)
!
      integer(kind = kint) :: i
!
!
      X(1) = real(C_FFT(1))
      X(2) = real(C_FFT(NFFT_c))
      do i = 2, NFFT_c - 1
        X(2*i-1) =  two * real(C_FFT(i))
        X(2*i  ) = -two * imag(C_FFT(i))
      end do
!
      end subroutine copy_from_prt_fwd_FFTW
!
! ------------------------------------------------------------------
!
      subroutine copy_from_prt_fwd_OMP_FFTW(Ncomp_c, NFFT_c, C_FFT,     &
     &                                      Ncomp, Nfft, X)
!
      integer(kind = kint), intent(in) :: Ncomp_c, NFFT_c
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      complex(kind = kreal), intent(in) :: C_FFT(NFFT_c,Ncomp_c)
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!
      integer(kind = kint) :: i, nd
!
!
!$omp parallel do private(nd,i)
      do nd = 1, Ncomp_c
        X(1,nd) = real(C_FFT(1,     nd))
        X(2,nd) = real(C_FFT(NFFT_c,nd))
        do i = 2, NFFT_c - 1
          X(2*i-1,nd) =  two * real(C_FFT(i,nd))
          X(2*i,  nd) = -two * imag(C_FFT(i,nd))
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_from_prt_fwd_OMP_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine norm_prt_bwd_single_FFTW(nd8, Ncomp, Nfft, X,          &
     &                                    NFFT_c, C_FFT)
!
      integer(kind = kint_gl), intent(in) :: nd8
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!
      complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c)
!
      integer(kind = kint) :: i
!
!
      C_FFT(1) = cmplx(X(nd8,1), zero, kind(0d0))
      do i = 2, NFFT_c - 1
        C_FFT(i) = half * cmplx(X(nd8,2*i-1), -X(nd8,2*i), kind(0d0))
      end do
      C_FFT(NFFT_c) = cmplx(X(nd8,2), zero, kind(0d0))
!
      end subroutine norm_prt_bwd_single_FFTW
!
! ------------------------------------------------------------------
!
      subroutine norm_copy_to_prt_bwd_FFTW(Nfft, X, NFFT_c, C_FFT)
!
      integer(kind = kint), intent(in) :: Nfft, NFFT_c
      real(kind = kreal), intent(in) :: X(Nfft)
!
      complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c)
!
      integer(kind = kint) :: i
!
!
      C_FFT(1) = cmplx(X(1), zero, kind(0d0))
      do i = 2, NFFT_c - 1
        C_FFT(i) = half * cmplx(X(2*i-1), -X(2*i),kind(0d0))
      end do
      C_FFT(NFFT_c) = cmplx(X(2), zero, kind(0d0))
!
      end subroutine norm_copy_to_prt_bwd_FFTW
!
! ------------------------------------------------------------------
!
      subroutine norm_copy_to_prt_bwd_OMP_FFTW(Ncomp, Nfft, X,          &
     &                                         Ncomp_c, NFFT_c, C_FFT)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = kint), intent(in) :: Ncomp_c, NFFT_c
      real(kind = kreal), intent(in) :: X(Nfft,Ncomp)
!
      complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c,Ncomp_c)
!
      integer(kind = kint) :: i, nd
!
!
!$omp parallel do private(nd,i)
      do nd = 1, Ncomp_c
        C_FFT(1,nd) = cmplx(X(1,nd), zero, kind(0d0))
        do i = 2, NFFT_c - 1
          C_FFT(i,nd) = half * cmplx(X(2*i-1,nd),-X(2*i,nd),kind(0d0))
        end do
        C_FFT(NFFT_c,nd) = cmplx(X(2,nd), zero, kind(0d0))
      end do
!$omp end parallel do
!
      end subroutine norm_copy_to_prt_bwd_OMP_FFTW
!
! ------------------------------------------------------------------
!
      end module normalize_for_FFTW
