!>@file   swap_prt_data_for_ISPACK.f90
!!@brief  module swap_prt_data_for_ISPACK
!!
!!@author H. Matsui
!!@date Programmed in 2026
!!
!!
!>@brief  Swap FFT dataarray for ISPACK
!!
!!@verbatim
!!      subroutine swap_prt_fld_to_FXRTFA(Nsmp, Nstacksmp, Mmax_smp,    &
!!     &                                  Nfft, M, X, X_ispack)
!!        integer(kind = kint), intent(in) ::  Nsmp
!!        integer(kind = kint_gl), intent(in) ::  Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X(Nfft,M)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!      subroutine swap_prt_spectr_from_FXRTFA(Nsmp, Nstacksmp,         &
!!     &          Mmax_smp, Nfft, X_ispack, M, X)
!!        integer(kind = kint), intent(in) ::  Nsmp
!!        integer(kind = kint_gl), intent(in) ::  Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!
!!      subroutine swap_prt_spectr_to_FXRTBA(Nsmp, Nstacksmp, Mmax_smp, &
!!     &                                     Nfft, M, X, X_ispack)
!!        integer(kind = kint), intent(in) ::  Nsmp
!!        integer(kind = kint_gl), intent(in) ::  Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X(Nfft,M)
!!      subroutine swap_prt_fld_from_FXRTBA(Nsmp, Nstacksmp, Mmax_smp,  &
!!     &                                    Nfft, X_ispack, M, X)
!!        integer(kind = kint), intent(in) ::  Nsmp
!!        integer(kind = kint_gl), intent(in) ::  Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!@endverbatim
!
      module swap_prt_data_for_ISPACK
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: swap_prt_fld_to_FXRTFA_smp
      private :: swap_prt_spectr_from_FXRTFA_smp
      private :: swap_prt_spectr_to_FXRTBA_smp
      private :: swap_prt_fld_from_FXRTBA_smp
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_fld_to_FXRTFA(Nsmp, Nstacksmp, Mmax_smp,      &
     &                                  Nfft, M, X, X_ispack)
!
      integer(kind = kint), intent(in) ::  Nsmp
      integer(kind = kint_gl), intent(in) ::  Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X(Nfft,M)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_ispack(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ismp, ist, num8
!
!
!$omp parallel do private(ist,num8)
      do ismp = 1, Nsmp
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        ist = Nstacksmp(ismp-1) - Nstacksmp(0)
        call swap_prt_fld_to_FXRTFA_smp(ist, num8, Nfft, M, X,          &
     &                                  Mmax_smp, X_ispack(1,ismp))
      end do
!$omp end parallel do
!
      end subroutine swap_prt_fld_to_FXRTFA
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_spectr_from_FXRTFA(Nsmp, Nstacksmp,           &
     &          Mmax_smp, Nfft, X_ispack, M, X)
!
      integer(kind = kint), intent(in) ::  Nsmp
      integer(kind = kint_gl), intent(in) ::  Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
!
      integer(kind = kint_gl) :: ismp, ist, num8
!
!
!$omp parallel do private(ist,num8)
      do ismp = 1, Nsmp
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        ist = Nstacksmp(ismp-1) - Nstacksmp(0)
        call swap_prt_spectr_from_FXRTFA_smp(ist, num8, Nfft, Mmax_smp, &
     &                                       X_ispack(1,ismp), M, X)
      end do
!$omp end parallel do
!
      end subroutine swap_prt_spectr_from_FXRTFA
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine swap_prt_spectr_to_FXRTBA(Nsmp, Nstacksmp, Mmax_smp,   &
     &                                     Nfft, M, X, X_ispack)
!
      integer(kind = kint), intent(in) ::  Nsmp
      integer(kind = kint_gl), intent(in) ::  Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X(Nfft,M)
!
      real(kind = kreal), intent(inout)                                 &
     &                              :: X_ispack(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ismp, ist, num8
!
!
!$omp parallel do private(ist,num8)
      do ismp = 1, Nsmp
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        ist =  Nstacksmp(ismp-1) - Nstacksmp(0)
        call swap_prt_spectr_to_FXRTBA_smp(ist, num8, Nfft, M, X,       &
     &                                     Mmax_smp, X_ispack(1,ismp))
      end do
!$omp end parallel do
!
      end subroutine swap_prt_spectr_to_FXRTBA
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_fld_from_FXRTBA(Nsmp, Nstacksmp, Mmax_smp,    &
     &                                    Nfft, X_ispack, M, X)
!
      integer(kind = kint), intent(in) ::  Nsmp
      integer(kind = kint_gl), intent(in) ::  Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
!
      integer(kind = kint_gl) :: ismp, ist, num8
!
!
!$omp parallel do private(ist,num8)
      do ismp = 1, Nsmp
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        ist = Nstacksmp(ismp-1) - Nstacksmp(0)
        call swap_prt_fld_from_FXRTBA_smp(ist, num8, Nfft, Mmax_smp,    &
     &                                    X_ispack(1,ismp), M, X)
      end do
!$omp end parallel do
!
      end subroutine swap_prt_fld_from_FXRTBA
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine swap_prt_fld_to_FXRTFA_smp(ist_smp, nnod_smp,          &
     &          Nfft, M, X, Mmax_smp, X_ispack)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X(Nfft,M)
!
      real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
!
!
      do i = 1, Nfft/2
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + 2*(i-1) * nnod_smp
          X_ispack(inod_c         ) = X(2*i-1,j)
          X_ispack(inod_c+nnod_smp) = X(2*i,  j)
        end do
      end do
!
      end subroutine swap_prt_fld_to_FXRTFA_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_spectr_from_FXRTFA_smp(ist_smp, nnod_smp,     &
     &          Nfft, Mmax_smp, X_ispack, M, X)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        j = ist_smp + inum
        inod_c = inum
        X(1,j) = X_ispack(inod_c)
        X(2,j) = X_ispack(inod_c+nnod_smp)
        do i = 2, Nfft/2
          inod_c = inum + (2*i-2) * nnod_smp
          X(2*i-1,j) =   two * X_ispack(inod_c         )
          X(2*i,  j) = - two * X_ispack(inod_c+nnod_smp)
        end do
      end do
!
      end subroutine swap_prt_spectr_from_FXRTFA_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine swap_prt_spectr_to_FXRTBA_smp(ist_smp, nnod_smp,       &
     &          Nfft, M, X, Mmax_smp, X_ispack)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X(Nfft,M)
!
      real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        j = ist_smp + inum
        inod_c = inum
        X_ispack(inod_c         ) = X(1,j)
        X_ispack(inod_c+nnod_smp) = X(2,j)
      end do
      do i = 2, Nfft/2
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + (2*i-2) * nnod_smp
          X_ispack(inod_c         ) =  half * X(2*i-1,j)
          X_ispack(inod_c+nnod_smp) = -half * X(2*i  ,j)
        end do
      end do
!
      end subroutine swap_prt_spectr_to_FXRTBA_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_fld_from_FXRTBA_smp(ist_smp, nnod_smp,        &
     &          Nfft, Mmax_smp, X_ispack, M, X)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        do i = 1, Nfft/2
          j = ist_smp + inum
          inod_c = inum + 2*(i-1) * nnod_smp
          X(2*i-1,j) = X_ispack(inod_c         )
          X(2*i,  j) = X_ispack(inod_c+nnod_smp)
        end do
      end do
!
      end subroutine swap_prt_fld_from_FXRTBA_smp
!
! ------------------------------------------------------------------
!
      end module swap_prt_data_for_ISPACK
