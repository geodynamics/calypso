!>@file   normalize_for_OMP_FFTW.f90
!!@brief  module normalize_for_OMP_FFTW
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine norm_rtp_from_fwd_OMP_FFTW                           &
!!     &         (Ncomp, aNfft, NFFT_c, C_FFT, Nfft, X)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: aNfft
!!        complex(kind = kreal), intent(in) :: C_FFT(Ncomp,Nfft_c)
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!      subroutine norm_prt_from_fwd_OMP_FFTW(Ncomp, aNfft,             &
!!     &                                      NFFT_c, C_FFT, Nfft, X)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: aNfft
!!        complex(kind = kreal), intent(in) :: C_FFT(Nfft_c,Ncomp)
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!
!!      subroutine norm_rtp_to_bwd_OMP_FFTW(Ncomp, Nfft, X,             &
!!     &                                    NFFT_c, C_FFT)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!!        complex(kind = kreal), intent(inout) :: C_FFT(Ncomp,Nfft_c)
!!      subroutine norm_prt_to_bwd_OMP_FFTW(Ncomp, Nfft, X,             &
!!     &                                    NFFT_c, C_FFT)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: X(Nfft,Ncomp)
!!        complex(kind = kreal), intent(inout) :: C_FFT(Nfft_c,Ncomp)
!! ------------------------------------------------------------------
!!@endverbatim
      module normalize_for_OMP_FFTW
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
      subroutine norm_rtp_from_fwd_OMP_FFTW                             &
     &         (Ncomp, aNfft, NFFT_c, C_FFT, Nfft, X)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = kreal), intent(in) :: C_FFT(Ncomp,Nfft_c)
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
      integer(kind = kint) :: i
!
!$omp parallel
!$omp workshare
      X(1:Ncomp,1) = aNfft * real(C_FFT(1:Ncomp,1     ))
      X(1:Ncomp,2) = aNfft * real(C_FFT(1:Ncomp,Nfft_c))
!$omp end workshare nowait
      do i = 2, Nfft_c-1
!$omp workshare
        X(1:Ncomp,2*i-1) =  two * aNfft * real(C_FFT(1:Ncomp,i))
        X(1:Ncomp,2*i  ) = -two * aNfft * imag(C_FFT(1:Ncomp,i))
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine norm_rtp_from_fwd_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine norm_prt_from_fwd_OMP_FFTW(Ncomp, aNfft,               &
     &                                      NFFT_c, C_FFT, Nfft, X)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = kreal), intent(in) :: C_FFT(Nfft_c,Ncomp)
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!
      integer(kind = kint) :: i, nd
!
!$omp parallel do private(i,nd)
      do nd = 1, Ncomp
        X(1,nd) = aNfft * real(C_FFT(1,     nd))
        X(2,nd) = aNfft * real(C_FFT(Nfft_c,nd))
        do i = 2, Nfft_c-1
          X(2*i-1,nd) =  two * aNfft * real(C_FFT(i,nd))
          X(2*i,  nd) = -two * aNfft * imag(C_FFT(i,nd))
        end do
      end do
!$omp end parallel do
!
      end subroutine norm_prt_from_fwd_OMP_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine norm_rtp_to_bwd_OMP_FFTW(Ncomp, Nfft, X,               &
     &                                    NFFT_c, C_FFT)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!
      complex(kind = kreal), intent(inout) :: C_FFT(Ncomp,Nfft_c)
!
      integer(kind = kint) :: i
!
!$omp parallel
!$omp workshare
      C_FFT(1:Ncomp,1     ) = cmplx(X(1:Ncomp,1), zero, kind(0d0))
!$omp end workshare nowait
      do i = 2, Nfft_c-1
!$omp workshare
        C_FFT(1:Ncomp,i) = half * cmplx( X(1:Ncomp,2*i-1),              &
     &                                  -X(1:Ncomp,2*i  ),kind(0d0))
!$omp end workshare nowait
!$omp workshare
        C_FFT(1:Ncomp,Nfft_c) = cmplx(X(1:Ncomp,2), zero, kind(0d0))
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine norm_rtp_to_bwd_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine norm_prt_to_bwd_OMP_FFTW(Ncomp, Nfft, X,               &
     &                                    NFFT_c, C_FFT)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      real(kind = kreal), intent(in) :: X(Nfft,Ncomp)
!
      complex(kind = kreal), intent(inout) :: C_FFT(Nfft_c,Ncomp)
!
      integer(kind = kint) :: i, nd
!
!$omp parallel do private(i,nd)
      do nd = 1, Ncomp
        C_FFT(1,     nd) = cmplx(X(1,nd), zero, kind(0d0))
        do i = 2, Nfft_c-1
          C_FFT(i,nd) = half * cmplx( X(2*i-1,nd),                      &
     &                               -X(2*i,  nd),kind(0d0))
        end do
        C_FFT(Nfft_c,nd) = cmplx(X(2,nd), zero, kind(0d0))
      end do
!$omp end parallel do
!
      end subroutine norm_prt_to_bwd_OMP_FFTW
!
! ------------------------------------------------------------------
!
      end module normalize_for_OMP_FFTW
