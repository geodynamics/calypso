!>@file   swap_prt_data_for_FFTPACK.f90
!!@brief  module swap_prt_data_for_FFTPACK
!!
!!@author H. Matsui
!!@date Programmed in 20026
!!
!!
!>@brief  Swap FFT data array for FFTPACK
!!
!!@verbatim
!!      subroutine swap_prt_fld_to_RFFTMF(Nsmp, Nstacksmp, Mmax_smp,    &
!!     &                                  Nfft, M, X, X_FFTPACK5)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(in) :: X(Nfft,M)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!!      subroutine swap_prt_spectr_from_RFFTMF                          &
!!     &         (Nsmp, Nstacksmp, Mmax_smp, Nfft, X_FFTPACK5, M, X)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!
!!      subroutine swap_prt_spectr_to_RFFTMB(Nsmp, Nstacksmp, Mmax_smp, &
!!     &                                     Nfft, M, X, X_FFTPACK5)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(in) :: X(Nfft,M)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!!      subroutine swap_prt_fld_from_RFFTMB(Nsmp, Nstacksmp, Mmax_smp,  &
!!     &                                    Nfft, X_FFTPACK5, M, X)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!@endverbatim
!
      module swap_prt_data_for_FFTPACK
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: swap_prt_fld_to_RFFTMF_smp
      private :: swap_prt_spec_from_RFFTMF_smp
      private :: swap_prt_spec_to_RFFTMB_smp
      private :: swap_prt_fld_from_RFFTMB_smp
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_fld_to_RFFTMF(Nsmp, Nstacksmp, Mmax_smp,      &
     &                                  Nfft, M, X, X_FFTPACK5)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X(Nfft,M)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ismp, ist, num
!
!
!$omp parallel do private(ist,num)
      do ismp = 1, Nsmp
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num .le. 0) cycle
!
        ist = Nstacksmp(ismp-1) - Nstacksmp(0)
        call swap_prt_fld_to_RFFTMF_smp(ist, num, Nfft, M, X,           &
     &                                  Mmax_smp, X_FFTPACK5(1,ismp))
      end do
!$omp end parallel do
!
      end subroutine swap_prt_fld_to_RFFTMF
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine swap_prt_spectr_from_RFFTMF                            &
     &         (Nsmp, Nstacksmp, Mmax_smp, Nfft, X_FFTPACK5, M, X)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in)                                    &
     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
!
      integer(kind = kint_gl) :: ismp, ist, num
!
!
!$omp parallel do private(ist,num)
      do ismp = 1, Nsmp
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num .le. 0) cycle
!
        ist = Nstacksmp(ismp-1) - Nstacksmp(0)
        call swap_prt_spec_from_RFFTMF_smp(ist, num, Nfft, Mmax_smp,    &
     &                                     X_FFTPACK5(1,ismp), M, X)
      end do
!$omp end parallel do
!
      end subroutine swap_prt_spectr_from_RFFTMF
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_spectr_to_RFFTMB(Nsmp, Nstacksmp, Mmax_smp,   &
     &                                     Nfft, M, X, X_FFTPACK5)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(in) :: X(Nfft,M)
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ismp, ist, num
!
!
!$omp parallel do private(ist,num)
      do ismp = 1, Nsmp
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num .le. 0) cycle
!
        ist = Nstacksmp(ismp-1) - Nstacksmp(0)
        call swap_prt_spec_to_RFFTMB_smp(ist, num, Nfft, M, X,          &
     &                                   Mmax_smp, X_FFTPACK5(1,ismp))
      end do
!$omp end parallel do
!
      end subroutine swap_prt_spectr_to_RFFTMB
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_fld_from_RFFTMB(Nsmp, Nstacksmp, Mmax_smp,    &
     &                                    Nfft, X_FFTPACK5, M, X)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in)                                    &
     &                   :: X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
!
      integer(kind = kint_gl) ::  ismp, ist, num
!
!
!$omp parallel do private(ist,num)
      do ismp = 1, Nsmp
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num .le. 0) cycle
!
        ist = Nstacksmp(ismp-1) - Nstacksmp(0)
        call swap_prt_fld_from_RFFTMB_smp(ist, num, Nfft, Mmax_smp,     &
     &                                    X_FFTPACK5(1,ismp), M, X)
      end do
!$omp end parallel do
!
      end subroutine swap_prt_fld_from_RFFTMB
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine swap_prt_fld_to_RFFTMF_smp(ist_smp, nnod_smp,          &
     &          Nfft, M, X, Mmax_smp, X_FFTPACK)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X(Nfft,M)
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
          X_FFTPACK(inod_c) = X(i,j)
        end do
      end do
!
      end subroutine swap_prt_fld_to_RFFTMF_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_spec_from_RFFTMF_smp(ist_smp, nnod_smp,       &
     &          Nfft, Mmax_smp, X_FFTPACK, M, X)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X_FFTPACK(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
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
      end subroutine swap_prt_spec_from_RFFTMF_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_spec_to_RFFTMB_smp(ist_smp, nnod_smp,         &
     &          Nfft, M, X, Mmax_smp, X_FFTPACK)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X(Nfft,M)
!
      real(kind = kreal), intent(inout) :: X_FFTPACK(Mmax_smp*Nfft)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
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
      end subroutine swap_prt_spec_to_RFFTMB_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_fld_from_RFFTMB_smp(ist_smp, nnod_smp,        &
     &          Nfft, Mmax_smp, X_FFTPACK, M, X)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X_FFTPACK(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
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
      end module swap_prt_data_for_FFTPACK
