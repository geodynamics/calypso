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
!!      subroutine norm_swap_from_prt_fwd_FFTW(ist_smp, num_smp,        &
!!     &          Ncomp, NFFT_c, C_FFT, Nfft, aNfft, X)
!!        integer(kind = kint), intent(in) :: ist_smp, num_smp
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: aNfft
!!        complex(kind = kreal), intent(in) :: C_FFT(NFFT_c,Ncomp)
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!      subroutine norm_copy_from_prt_fwd_FFTW(Ncomp_smp, NFFT_c, C_FFT,&
!!     &                                       Nfft, aNfft, X)
!!        integer(kind = kint), intent(in) :: Ncomp_smp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: aNfft
!!        complex(kind = kreal), intent(in) :: C_FFT(NFFT_c,Ncomp_smp)
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp_smp)
!!      subroutine normalize_to_rtp_fwd_FFTW                            &
!!     &         (Ncomp, aNfft, NFFT_c, C_FFT, Nfft, X)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: aNfft
!!        complex(kind = kreal), intent(in) :: C_FFT(Ncomp,Nfft_c)
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!
!!      subroutine norm_swap_to_prt_bwd_FFTW(ist_smp, num_smp,          &
!!     &          Ncomp, Nfft, X, NFFT_c, C_FFT)
!!        integer(kind = kint), intent(in) :: ist_smp, num_smp
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!!        complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c,num_smp)
!!      subroutine norm_copy_to_prt_bwd_FFTW(Ncomp_smp, Nfft, X,        &
!!     &                                     NFFT_c, C_FFT)
!!        integer(kind = kint), intent(in) :: Ncomp_smp, Nfft, NFFT_c
!!        real(kind = kreal), intent(in) :: X(Nfft,Ncomp_smp)
!!        complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c,Ncomp_smp)
!!      subroutine normalize_to_rtp_bwd_FFTW(Ncomp, Nfft, X,            &
!!     &                                     NFFT_c, C_FFT)
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
      subroutine norm_swap_from_prt_fwd_FFTW(ist_smp, num_smp,          &
     &          Ncomp, NFFT_c, C_FFT, Nfft, aNfft, X)
!
      integer(kind = kint), intent(in) :: ist_smp, num_smp
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = kreal), intent(in) :: C_FFT(NFFT_c,Ncomp)
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
      integer(kind = kint) :: i
!
!
      X(ist_smp+1:ist_smp+num_smp,1)                                    &
     &            = aNfft * real(C_FFT(1,     1:num_smp))
      X(ist_smp+1:ist_smp+num_smp,2)                                    &
     &            = aNfft * real(C_FFT(NFFT_c,1:num_smp))
      do i = 2, NFFT_c - 1
        X(ist_smp+1:ist_smp+num_smp,2*i-1)                              &
     &            =  two * aNfft * real(C_FFT(i,1:num_smp))
        X(ist_smp+1:ist_smp+num_smp,2*i  )                              &
     &            = -two * aNfft * imag(C_FFT(i,1:num_smp))
      end do 
!
      end subroutine norm_swap_from_prt_fwd_FFTW
!
! ------------------------------------------------------------------
!
      subroutine norm_copy_from_prt_fwd_FFTW(Ncomp_smp, NFFT_c, C_FFT,  &
     &                                       Nfft, aNfft, X)
!
      integer(kind = kint), intent(in) :: Ncomp_smp, Nfft, NFFT_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = kreal), intent(in) :: C_FFT(NFFT_c,Ncomp_smp)
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp_smp)
!
      integer(kind = kint) :: i, nd
!
!
      do nd = 1, Ncomp_smp
        X(1,nd) = aNfft * real(C_FFT(1,     nd))
        X(2,nd) = aNfft * real(C_FFT(NFFT_c,nd))
        do i = 2, NFFT_c - 1
          X(2*i-1,nd) =  two * aNfft * real(C_FFT(i,nd))
          X(2*i,  nd) = -two * aNfft * imag(C_FFT(i,nd))
        end do
      end do
!
      end subroutine norm_copy_from_prt_fwd_FFTW
!
! ------------------------------------------------------------------
!
      subroutine normalize_to_rtp_fwd_FFTW                              &
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
      end subroutine normalize_to_rtp_fwd_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine norm_swap_to_prt_bwd_FFTW(ist_smp, num_smp,            &
     &          Ncomp, Nfft, X, NFFT_c, C_FFT)
!
      integer(kind = kint), intent(in) :: ist_smp, num_smp
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_c
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!
      complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c,num_smp)
!
      integer(kind = kint) :: i, inum, nd
!
!
      do inum = 1, num_smp
        nd = ist_smp + inum
        C_FFT(1,inum) = cmplx(X(nd,1), zero, kind(0d0))
        do i = 2, NFFT_c - 1
          C_FFT(i,inum) = half * cmplx( X(nd,2*i-1),                    &
     &                                 -X(nd,2*i), kind(0d0))
        end do
        C_FFT(NFFT_c,inum) = cmplx(X(nd,2), zero, kind(0d0))
      end do
!
      end subroutine norm_swap_to_prt_bwd_FFTW
!
! ------------------------------------------------------------------
!
      subroutine norm_copy_to_prt_bwd_FFTW(Ncomp_smp, Nfft, X,          &
     &                                     NFFT_c, C_FFT)
!
      integer(kind = kint), intent(in) :: Ncomp_smp, Nfft, NFFT_c
      real(kind = kreal), intent(in) :: X(Nfft,Ncomp_smp)
!
      complex(kind = kreal), intent(inout) :: C_FFT(NFFT_c,Ncomp_smp)
!
      integer(kind = kint) :: i, nd
!
!
      do nd = 1, Ncomp_smp
        C_FFT(1,nd) = cmplx(X(1,nd), zero, kind(0d0))
        do i = 2, NFFT_c - 1
          C_FFT(i,nd) = half * cmplx(X(2*i-1,nd), -X(2*i,nd),kind(0d0))
        end do
        C_FFT(NFFT_c,nd) = cmplx(X(2,nd), zero, kind(0d0))
      end do
!
      end subroutine norm_copy_to_prt_bwd_FFTW
!
! ------------------------------------------------------------------
!
      subroutine normalize_to_rtp_bwd_FFTW(Ncomp, Nfft, X,              &
     &                                     NFFT_c, C_FFT)
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
      end subroutine normalize_to_rtp_bwd_FFTW
!
! ------------------------------------------------------------------
!
      end module normalize_for_FFTW
