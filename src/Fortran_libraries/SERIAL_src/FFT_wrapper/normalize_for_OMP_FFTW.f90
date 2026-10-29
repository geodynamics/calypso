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
!!      subroutine norm_rtp_from_fwd_OMP_FFTW(Ncomp_c, NFFT_c, C_FFT,   &
!!     &                                      ist_c, Ncomp, Nfft, X)
!!        integer(kind = kint), intent(in) :: Ncomp_c, NFFT_c
!!        complex(kind = kreal), intent(in) :: C_FFT(Ncomp_c,Nfft_c)
!!        integer(kind = kint), intent(in) :: ist_c, Ncomp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!      subroutine norm_prt_from_fwd_OMP_FFTW(Ncomp, NFFT_c, C_FFT,     &
!!     &                                      Nfft, X)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        complex(kind = kreal), intent(in) :: C_FFT(Nfft_c,Ncomp)
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!
!!      subroutine norm_rtp_to_bwd_OMP_FFTW(ist_comp, Ncomp, Nfft, X,   &
!!     &                                    Ncomp_c, NFFT_c, C_FFT)
!!        integer(kind = kint), intent(in) :: ist_comp, Ncomp, Nfft
!!        integer(kind = kint), intent(in) :: Ncomp_c, NFFT_c
!!        real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!!        complex(kind = kreal), intent(inout) :: C_FFT(Ncomp_c,Nfft_c)
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
      subroutine norm_rtp_from_fwd_OMP_FFTW(Ncomp_c, NFFT_c, C_FFT,     &
     &                                      ist_c, Ncomp, Nfft, X)
!
      integer(kind = kint), intent(in) :: Ncomp_c, NFFT_c
      complex(kind = kreal), intent(in) :: C_FFT(Ncomp_c,Nfft_c)
      integer(kind = kint), intent(in) :: ist_c, Ncomp, Nfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
      integer(kind = kint) :: i
!
!$omp parallel
!$omp workshare
      X(ist_c:ist_c+Ncomp_c-1,1) = real(C_FFT(1:Ncomp_c,1     ))
      X(ist_c:ist_c+Ncomp_c-1,2) = real(C_FFT(1:Ncomp_c,Nfft_c))
!$omp end workshare nowait
      do i = 2, Nfft_c-1
!$omp workshare
        X(ist_c:ist_c+Ncomp_c-1,2*i-1) = two * real(C_FFT(1:Ncomp_c,i))
        X(ist_c:ist_c+Ncomp_c-1,2*i  ) =-two * imag(C_FFT(1:Ncomp_c,i))
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine norm_rtp_from_fwd_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine norm_prt_from_fwd_OMP_FFTW(Ncomp, NFFT_c, C_FFT,       &
     &                                      Nfft, X)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      complex(kind = kreal), intent(in) :: C_FFT(Nfft_c,Ncomp)
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!
      integer(kind = kint) :: i, nd
!
!$omp parallel do private(i,nd)
      do nd = 1, Ncomp
        X(1,nd) = real(C_FFT(1,     nd))
        X(2,nd) = real(C_FFT(Nfft_c,nd))
        do i = 2, Nfft_c-1
          X(2*i-1,nd) =  two * real(C_FFT(i,nd))
          X(2*i,  nd) = -two * imag(C_FFT(i,nd))
        end do
      end do
!$omp end parallel do
!
      end subroutine norm_prt_from_fwd_OMP_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine norm_rtp_to_bwd_OMP_FFTW(ist_comp, Ncomp, Nfft, X,     &
     &                                    Ncomp_c, NFFT_c, C_FFT)
!
      integer(kind = kint), intent(in) :: ist_comp, Ncomp, Nfft
      integer(kind = kint), intent(in) :: Ncomp_c, NFFT_c
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!
      complex(kind = kreal), intent(inout) :: C_FFT(Ncomp_c,Nfft_c)
!
      integer(kind = kint) :: i
!
!$omp parallel
!$omp workshare
      C_FFT(1:Ncomp_c,1     )                                           &
     &     = cmplx(X(ist_comp:ist_comp+Ncomp_c-1,1), zero, kind(0d0))
!$omp end workshare nowait
      do i = 2, Nfft_c-1
!$omp workshare
        C_FFT(1:Ncomp_c,i) = half                                       &
     &         * cmplx( X(ist_comp:ist_comp+Ncomp_c-1,2*i-1),           &
     &                 -X(ist_comp:ist_comp+Ncomp_c-1,2*i  ),kind(0d0))
!$omp end workshare nowait
      end do
!$omp workshare
        C_FFT(1:Ncomp_c,Nfft_c)                                         &
     &    = cmplx(X(ist_comp:ist_comp+Ncomp_c-1,2), zero, kind(0d0))
!$omp end workshare
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
