!>@file   normalize_for_FFTPACK.f90
!!@brief  module normalize_for_FFTPACK
!!
!!@author H. Matsui
!!@date Programmed in 20026
!!
!!
!>@brief  Normalization and data copy for FFTPACK
!!
!!@verbatim
!!      subroutine copy_rtp_spectr_from_RFFTMF_smp(ist_smp, nnod_smp,   &
!!     &          Nfft, Mmax_smp, X_FFTPACK, M, X)
!!        integer(kind = kint), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X_FFTPACK(Mmax_smp*Nfft)
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!      subroutine swap_prt_spectr_from_RFFTMF_smp(ist_smp, nnod_smp,   &
!!     &          Nfft, Mmax_smp, X_FFTPACK, M, X)
!!        integer(kind = kint), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X_FFTPACK(Mmax_smp*Nfft)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!
!!      subroutine copy_rtp_fld_to_RFFTMF_smp(ist_smp, nnod_smp,        &
!!     &          Nfft, M, X, Mmax_smp, X_FFTPACK)
!!        integer(kind = kint), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X(M,Nfft)
!!        real(kind = kreal), intent(inout) :: X_FFTPACK(Mmax_smp*Nfft)
!!      subroutine swap_prt_fld_to_RFFTMF_smp(ist_smp, nnod_smp,        &
!!     &          Nfft, M, X, Mmax_smp, X_FFTPACK)
!!        integer(kind = kint), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X(Nfft,M)
!!        real(kind = kreal), intent(inout) :: X_FFTPACK(Mmax_smp*Nfft)
!!
!!      subroutine copy_rtp_spectr_to_RFFTMB_smp(ist_smp, nnod_smp,     &
!!     &          Nfft, M, X, Mmax_smp, X_FFTPACK)
!!        integer(kind = kint), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X(M,Nfft)
!!        real(kind = kreal), intent(inout) :: X_FFTPACK(Mmax_smp*Nfft)
!!      subroutine swap_prt_spectr_to_RFFTMB_smp(ist_smp, nnod_smp,     &
!!     &          Nfft, M, X, Mmax_smp, X_FFTPACK)
!!        integer(kind = kint), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X(Nfft,M)
!!        real(kind = kreal), intent(inout) :: X_FFTPACK(Mmax_smp*Nfft)
!!
!!      subroutine copy_rtp_fld_from_RFFTMB_smp(ist_smp, nnod_smp,      &
!!     &          Nfft, Mmax_smp, X_FFTPACK, M, X)
!!        integer(kind = kint), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X_FFTPACK(Mmax_smp*Nfft)
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!      subroutine swap_prt_fld_from_RFFTMB_smp(ist_smp, nnod_smp,      &
!!     &          Nfft, Mmax_smp, X_FFTPACK, M, X)
!!        integer(kind = kint), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X_FFTPACK(Mmax_smp*Nfft)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!@endverbatim
!
      module normalize_for_FFTPACK
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
      subroutine copy_rtp_spectr_from_RFFTMF_smp(ist_smp, nnod_smp,     &
     &          Nfft, Mmax_smp, X_FFTPACK, M, X)
!
      integer(kind = kint), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X_FFTPACK(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
      integer(kind = kint) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        j = ist_smp + inum
        X(j,1) = X_FFTPACK(inum  )
      end do
      do i = 2, Nfft-1
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + (i-1) * nnod_smp
          X(j,i+1) = X_FFTPACK(inod_c)
        end do
      end do
      do inum = 1, nnod_smp
        j = ist_smp + inum
        inod_c = inum + (Nfft-1) * nnod_smp
        X(j,2) = X_FFTPACK(inod_c)
      end do
!
      end subroutine copy_rtp_spectr_from_RFFTMF_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_spectr_from_RFFTMF_smp(ist_smp, nnod_smp,     &
     &          Nfft, Mmax_smp, X_FFTPACK, M, X)
!
      integer(kind = kint), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X_FFTPACK(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
!
      integer(kind = kint) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        j = ist_smp + inum
        X(1,j) = X_FFTPACK(inum  )
      end do
      do i = 2, Nfft-1
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + (i-1) * nnod_smp
          X(i+1,j) = X_FFTPACK(inod_c)
        end do
      end do
      do inum = 1, nnod_smp
        j = ist_smp + inum
        inod_c = inum + (Nfft-1) * nnod_smp
        X(2,j) = X_FFTPACK(inod_c)
      end do
!
      end subroutine swap_prt_spectr_from_RFFTMF_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_rtp_fld_to_RFFTMF_smp(ist_smp, nnod_smp,          &
     &          Nfft, M, X, Mmax_smp, X_FFTPACK)
!
      integer(kind = kint), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X(M,Nfft)
!
      real(kind = kreal), intent(inout) :: X_FFTPACK(Mmax_smp*Nfft)
!
      integer(kind = kint) ::  i, j, inum, inod_c
!
!
      do i = 1, Nfft
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + (i-1) * nnod_smp
          X_FFTPACK(inod_c) = X(j,i)
        end do
      end do
!
      end subroutine copy_rtp_fld_to_RFFTMF_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_fld_to_RFFTMF_smp(ist_smp, nnod_smp,          &
     &          Nfft, M, X, Mmax_smp, X_FFTPACK)
!
      integer(kind = kint), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X(Nfft,M)
!
      real(kind = kreal), intent(inout) :: X_FFTPACK(Mmax_smp*Nfft)
!
      integer(kind = kint) ::  i, j, inum, inod_c
!
!
      do i = 1, Nfft
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + (i-1) * nnod_smp
          X_FFTPACK(inod_c) = X(i,j)
        end do
      end do
!
      end subroutine swap_prt_fld_to_RFFTMF_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_rtp_spectr_to_RFFTMB_smp(ist_smp, nnod_smp,       &
     &          Nfft, M, X, Mmax_smp, X_FFTPACK)
!
      integer(kind = kint), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X(M,Nfft)
!
      real(kind = kreal), intent(inout) :: X_FFTPACK(Mmax_smp*Nfft)
!
      integer(kind = kint) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        j = ist_smp + inum
        X_FFTPACK(inum  ) = X(j,1)
      end do
      do i = 2, Nfft-1
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + (i-1) * nnod_smp
          X_FFTPACK(inod_c) = X(j,i+1)
        end do
      end do
      do inum = 1, nnod_smp
        j = ist_smp + inum
        inod_c = inum + (Nfft-1) * nnod_smp
        X_FFTPACK(inod_c) = X(j,2)
      end do
!
      end subroutine copy_rtp_spectr_to_RFFTMB_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_spectr_to_RFFTMB_smp(ist_smp, nnod_smp,       &
     &          Nfft, M, X, Mmax_smp, X_FFTPACK)
!
      integer(kind = kint), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X(Nfft,M)
!
      real(kind = kreal), intent(inout) :: X_FFTPACK(Mmax_smp*Nfft)
!
      integer(kind = kint) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        j = ist_smp + inum
        X_FFTPACK(inum  ) = X(1,j)
      end do
      do i = 2, Nfft-1
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + (i-1) * nnod_smp
          X_FFTPACK(inod_c) = X(i+1,j)
        end do
      end do
      do inum = 1, nnod_smp
        j = ist_smp + inum
        inod_c = inum + (Nfft-1) * nnod_smp
        X_FFTPACK(inod_c) = X(2,j)
      end do
!
      end subroutine swap_prt_spectr_to_RFFTMB_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_rtp_fld_from_RFFTMB_smp(ist_smp, nnod_smp,        &
     &          Nfft, Mmax_smp, X_FFTPACK, M, X)
!
      integer(kind = kint), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X_FFTPACK(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
      integer(kind = kint) ::  i, inod_c
!
!
      do i = 1, Nfft
        inod_c = (i-1) * nnod_smp
        X(ist_smp+1:ist_smp+nnod_smp,i)                                 &
     &     = X_FFTPACK(inod_c+1:inod_c+nnod_smp)
      end do
!
      end subroutine copy_rtp_fld_from_RFFTMB_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_fld_from_RFFTMB_smp(ist_smp, nnod_smp,        &
     &          Nfft, Mmax_smp, X_FFTPACK, M, X)
!
      integer(kind = kint), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X_FFTPACK(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
!
      integer(kind = kint) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        j = ist_smp + inum
        do i = 1, Nfft
          inod_c = inum + (i-1) * nnod_smp
          X(i,j) = X_FFTPACK(inod_c)
        end do
      end do
!
      end subroutine swap_prt_fld_from_RFFTMB_smp
!
! ------------------------------------------------------------------
!
      end module normalize_for_FFTPACK
