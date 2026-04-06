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
!!      subroutine norm_swap_from_prt_fwd_FFT(ist_nd, ied_nd,           &
!!     &          Ncomp, NFFT_c, C_FFT, Nfft, aNfft, X)
!!        integer(kind = kint), intent(in) :: ist_nd, ied_nd
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: aNfft
!!        complex(kind = kreal), intent(in) :: C_FFT(NFFT_c,Ncomp)
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!      subroutine normalize_to_rtp_fwd_FFT(Ncomp, aNfft, NFFT_c, C_FFT,&
!!     &                                    Nfft, X)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: aNfft
!!        complex(kind = kreal), intent(in) :: C_FFT(Ncomp,Nfft_c)
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!
!!      subroutine norm_swap_to_prt_bwd_FFT(ist_nd, ied_nd,             &
!!     &          Ncomp, Nfft, X, NFFT_c, C_FFT)
!!        integer(kind = kint), intent(in) :: ist_nd, ied_nd
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!!        complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c,Ncomp)
!!      subroutine normalize_to_rtp_bwd_FFT(Ncomp, Nfft, X,             &
!!     &                                    NFFT_c, C_FFT)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!!        complex(kind = kreal), intent(inout) :: C_FFT(Ncomp,Nfft_c)
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
      subroutine norm_swap_from_prt_fwd_FFT(ist_nd, ied_nd,             &
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
      end subroutine norm_swap_from_prt_fwd_FFT
!
! ------------------------------------------------------------------
!
      subroutine normalize_to_rtp_fwd_FFT(Ncomp, aNfft, NFFT_c, C_FFT,  &
     &                                    Nfft, X)
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
      end subroutine normalize_to_rtp_fwd_FFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine norm_swap_to_prt_bwd_FFT(ist_nd, ied_nd,               &
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
      end subroutine norm_swap_to_prt_bwd_FFT
!
! ------------------------------------------------------------------
!
      subroutine normalize_to_rtp_bwd_FFT(Ncomp, Nfft, X,               &
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
      C_FFT(1:Ncomp,Nfft_c) = cmplx(X(1:Ncomp,2), zero, kind(0d0))
!$omp end workshare nowait
      do i = 2, Nfft_c-1
!$omp workshare
        C_FFT(1:Ncomp,i) = half * cmplx(X(1:Ncomp,2*i-1),               &
     &                                  -X(1:Ncomp,2*i  ),kind(0d0))
!$omp end workshare nowait
      end do
!$omp end parallel
!
      end subroutine normalize_to_rtp_bwd_FFT
!
! ------------------------------------------------------------------
!
      end module normalize_for_FFTW
