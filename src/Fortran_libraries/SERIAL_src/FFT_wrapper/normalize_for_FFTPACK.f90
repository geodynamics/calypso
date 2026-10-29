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
!!      subroutine copy_rtp_spectr_from_RFFTMF                          &
!!     &         (Nsmp, Nstacksmp, Mmax_smp, Nfft, X_FFTPACK5, M, X)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: X(M, Nfft)
!!      subroutine copy_rtp_fld_to_RFFTMF(Nsmp, Nstacksmp, Mmax_smp,    &
!!     &                                  Nfft, M, X, X_FFTPACK5)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(in) :: X(M, Nfft)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!!      subroutine copy_rtp_spectr_to_RFFTMB(Nsmp, Nstacksmp, Mmax_smp, &
!!     &                                     Nfft, M, X, X_FFTPACK5)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(in) :: X(M,Nfft)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!!      subroutine copy_rtp_fld_from_RFFTMB(Nsmp, Nstacksmp, Mmax_smp,  &
!!     &                                    Nfft, X_FFTPACK5, M, X)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint), intent(in) :: M, Nfft
!!      real(kind = kreal), intent(in)                                  &
!!     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!@endverbatim
!
      module normalize_for_FFTPACK
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: copy_rtp_fld_to_RFFTMF_smp
      private :: copy_rtp_spec_from_RFFTMF_smp
      private :: copy_rtp_spec_to_RFFTMB_smp
      private :: copy_rtp_fld_from_RFFTMB_smp
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine copy_rtp_spectr_from_RFFTMF                            &
     &         (Nsmp, Nstacksmp, Mmax_smp, Nfft, X_FFTPACK5, M, X)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in)                                    &
     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
!
      integer(kind = kint) :: ip
      integer(kind = kint_gl) :: ist, num
!
!
!$omp parallel do private(ip,ist,num)
      do ip = 1, Nsmp
        num = Nstacksmp(ip) - Nstacksmp(ip-1)
        if(num .le. 0) cycle
!
        ist = Nstacksmp(ip-1)
        call copy_rtp_spec_from_RFFTMF_smp(ist, num, Nfft, Mmax_smp,    &
     &                                     X_FFTPACK5(1,ip), M, X)
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_spectr_from_RFFTMF
!
! ------------------------------------------------------------------
!
      subroutine copy_rtp_fld_to_RFFTMF(Nsmp, Nstacksmp, Mmax_smp,      &
     &                                  Nfft, M, X, X_FFTPACK5)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X(M, Nfft)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint) :: ip
      integer(kind = kint_gl) :: ist, num
!
!
!$omp parallel do private(ip,ist,num)
      do ip = 1, Nsmp
        num = Nstacksmp(ip) - Nstacksmp(ip-1)
        if(num .le. 0) cycle
!
        ist = Nstacksmp(ip-1)
        call copy_rtp_fld_to_RFFTMF_smp(ist, num, Nfft, M, X,           &
     &                                  Mmax_smp, X_FFTPACK5(1,ip))
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_fld_to_RFFTMF
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_rtp_spectr_to_RFFTMB(Nsmp, Nstacksmp, Mmax_smp,   &
     &                                     Nfft, M, X, X_FFTPACK5)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X(M,Nfft)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint) :: ip
      integer(kind = kint_gl) :: ist, num
!
!
!$omp parallel do private(ip,ist,num)
      do ip = 1, Nsmp
        num = Nstacksmp(ip) - Nstacksmp(ip-1)
        if(num .le. 0) cycle
!
        ist = Nstacksmp(ip-1)
        call copy_rtp_spec_to_RFFTMB_smp(ist, num, Nfft, M, X,          &
     &                                   Mmax_smp, X_FFTPACK5(1,ip))
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_spectr_to_RFFTMB
!
! ------------------------------------------------------------------
!
      subroutine copy_rtp_fld_from_RFFTMB(Nsmp, Nstacksmp, Mmax_smp,    &
     &                                    Nfft, X_FFTPACK5, M, X)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in)                                    &
     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
      integer(kind = kint) :: ip
      integer(kind = kint_gl) :: ist, num
!
!
!$omp parallel do private(ip,ist,num)
      do ip = 1, Nsmp
        num = Nstacksmp(ip) - Nstacksmp(ip-1)
        if(num .le. 0) cycle
!
        ist = Nstacksmp(ip-1)
        call copy_rtp_fld_from_RFFTMB_smp(ist, num, Nfft, Mmax_smp,     &
     &                                    X_FFTPACK5(1,ip), M, X)
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_fld_from_RFFTMB
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_rtp_spec_from_RFFTMF_smp(ist_smp, nnod_smp,       &
     &          Nfft, Mmax_smp, X_FFTPACK, M, X)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X_FFTPACK(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
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
      end subroutine copy_rtp_spec_from_RFFTMF_smp
!
! ------------------------------------------------------------------
!
      subroutine copy_rtp_fld_to_RFFTMF_smp(ist_smp, nnod_smp,          &
     &          Nfft, M, X, Mmax_smp, X_FFTPACK)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X(M,Nfft)
!
      real(kind = kreal), intent(inout) :: X_FFTPACK(Mmax_smp*Nfft)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
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
      subroutine copy_rtp_spec_to_RFFTMB_smp(ist_smp, nnod_smp,         &
     &          Nfft, M, X, Mmax_smp, X_FFTPACK)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X(M,Nfft)
!
      real(kind = kreal), intent(inout) :: X_FFTPACK(Mmax_smp*Nfft)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
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
      end subroutine copy_rtp_spec_to_RFFTMB_smp
!
! ------------------------------------------------------------------
!
      subroutine copy_rtp_fld_from_RFFTMB_smp(ist_smp, nnod_smp,        &
     &          Nfft, Mmax_smp, X_FFTPACK, M, X)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X_FFTPACK(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
      integer(kind = kint_gl) ::  i, inod_c
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
      end module normalize_for_FFTPACK
