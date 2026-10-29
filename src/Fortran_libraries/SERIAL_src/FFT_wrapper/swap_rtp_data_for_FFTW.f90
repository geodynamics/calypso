!>@file   swap_rtp_data_for_FFTW.f90
!!@brief  module swap_rtp_data_for_FFTW
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine swap_to_rtp_fwd_FFTW(ist_comp, Ncomp, Nfft, X,       &
!!     &                                Ncomp_r, Nfft_r, X_FFTW)
!!        integer(kind = kint), intent(in) :: Ncomp_r, Nfft_r
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, ist_comp
!!        real(kind = kreal), intent(in) :: X(Ncomp, Nfft)
!!        real(kind = kreal), intent(inout) :: X_FFTW(Nfft_r,Ncomp_r)
!!      subroutine swap_from_rtp_fwd_OMP_FFTW(Ncomp_c, NFFT_c, C_FFT,   &
!!     &                                      Ncomp, Nfft, ist_comp, X)
!!        integer(kind = kint), intent(in) :: Ncomp_c, NFFT_c
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, ist_comp
!!        complex(kind = kreal), intent(in) :: C_FFT(NFFT_c,Ncomp)
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!
!!      subroutine swap_to_rtp_bwd_OMP_FFTW                             &
!!     &         (Ncomp, Nfft, ist_comp, X, Ncomp_c, NFFT_c, C_FFT)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, ist_comp
!!        integer(kind = kint), intent(in) :: Ncomp_c, NFFT_c
!!        real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!!        complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c,Ncomp)
!!      subroutine swap_from_rtp_bwd_FFTW(Ncomp_r, Nfft_r, X_FFTW,      &
!!     &                                  ist_comp, Ncomp, Nfft, X)
!!        integer(kind = kint), intent(in) :: Ncomp_r, Nfft_r
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, ist_comp
!!        real(kind = kreal), intent(in) :: X_FFTW(Nfft_r,Ncomp_r)
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!! ------------------------------------------------------------------
!!@endverbatim
      module swap_rtp_data_for_FFTW
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
      subroutine swap_to_rtp_fwd_FFTW(ist_comp, Ncomp, Nfft, X,         &
     &                                Ncomp_r, Nfft_r, X_FFTW)
!
      integer(kind = kint), intent(in) :: Ncomp_r, Nfft_r
      integer(kind = kint), intent(in) :: Ncomp, Nfft, ist_comp
      real(kind = kreal), intent(in) :: X(Ncomp, Nfft)
!
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft_r,Ncomp_r)
!
      integer(kind = kint) :: j
!
!
!$omp parallel do
      do j = 1, Ncomp_r
        X_FFTW(1:Nfft,j) = X(ist_comp+j-1,1:Nfft)
      end do
!$omp end parallel do
!
      end subroutine swap_to_rtp_fwd_FFTW
!
! ------------------------------------------------------------------
!
      subroutine swap_from_rtp_fwd_OMP_FFTW(Ncomp_c, NFFT_c, C_FFT,     &
     &                                      Ncomp, Nfft, ist_comp, X)
!
      integer(kind = kint), intent(in) :: Ncomp_c, NFFT_c
      integer(kind = kint), intent(in) :: Ncomp, Nfft, ist_comp
      complex(kind = kreal), intent(in) :: C_FFT(NFFT_c,Ncomp)
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
      integer(kind = kint) :: i
!
!
      X(ist_comp:ist_comp+Ncomp_c-1,1) = real(C_FFT(1,     1:Ncomp_c))
      X(ist_comp:ist_comp+Ncomp_c-1,2) = real(C_FFT(NFFT_c,1:Ncomp_c))
      do i = 2, NFFT_c - 1
        X(ist_comp:ist_comp+Ncomp_c-1,2*i-1)                            &
     &          =  two * real(C_FFT(i,1:Ncomp_c))
        X(ist_comp:ist_comp+Ncomp_c-1,2*i  )                            &
     &          = -two * imag(C_FFT(i,1:Ncomp_c))
      end do 
!
      end subroutine swap_from_rtp_fwd_OMP_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine swap_to_rtp_bwd_OMP_FFTW                               &
     &         (Ncomp, Nfft, ist_comp, X, Ncomp_c, NFFT_c, C_FFT)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft, ist_comp
      integer(kind = kint), intent(in) :: Ncomp_c, NFFT_c
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!
      complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c,Ncomp)
!
      integer(kind = kint) :: i, nd, icomp
!
!
!$omp parallel do private(nd,i,icomp)
      do nd = 1, Ncomp_c
        icomp = nd + ist_comp - 1
        C_FFT(1,nd) = cmplx(X(icomp,1), zero, kind(0d0))
        do i = 2, NFFT_c - 1
          C_FFT(i,nd) = half * cmplx( X(icomp,2*i-1),                   &
     &                               -X(icomp,2*i), kind(0d0))
        end do
        C_FFT(NFFT_c,nd) = cmplx(X(icomp,2), zero, kind(0d0))
      end do
!$omp end parallel do
!
      end subroutine swap_to_rtp_bwd_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine swap_from_rtp_bwd_FFTW(Ncomp_r, Nfft_r, X_FFTW,        &
     &                                  ist_comp, Ncomp, Nfft, X)
!
      integer(kind = kint), intent(in) :: Ncomp_r, Nfft_r
      integer(kind = kint), intent(in) :: Ncomp, Nfft, ist_comp
      real(kind = kreal), intent(in) :: X_FFTW(Nfft_r,Ncomp_r)
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
      integer(kind = kint) :: i
!
!
!$omp parallel do
      do i = 1, Nfft
        X(ist_comp:ist_comp+Ncomp_r-1,i) = X_FFTW(i,1:Ncomp_r)
      end do
!$omp end parallel do
!
      end subroutine swap_from_rtp_bwd_FFTW
!
! ------------------------------------------------------------------
!
      end module swap_rtp_data_for_FFTW
