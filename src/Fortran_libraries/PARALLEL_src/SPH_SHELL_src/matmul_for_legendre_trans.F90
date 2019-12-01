!>@file   matmul_for_legendre_trans.F90
!!@brief  module matmul_for_legendre_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Matrix products for Legendre transforms
!!
!!@verbatim
!!      subroutine matmul_fwd_leg_trans(iflag_matmul, nkr, n_jk, nl_rtm,&
!!     &          V_kl, P_lj, S_kj)
!!      subroutine matmul_bwd_leg_trans(iflag_matmul, nl_rtm, nkr, n_jk,&
!!     &          P_lj, S_jk, V_lk)
!!
!!      subroutine matmul_fwd_leg_trans_Pjl(iflag_matmul,               &
!!     &          n_jk, nkr, nl_rtm, P_jl, V_lk, S_jk)
!!      subroutine matmul_bwd_leg_trans_Pjl(iflag_matmul,               &
!!     &          nkr, nl_rtm, n_jk, S_kj, P_jl, V_kl)
!!
!!      subroutine add_matmul_fwd_leg_trans(iflag_matmul,               &
!!     &          nkr, n_jk, nl_rtm, V_kl, P_lj, coef, S_kj)
!!      subroutine add_matmul_bwd_leg_trans(iflag_matmul,               &
!!     &          nl_rtm, nkr, n_jk, P_lj, S_jk, coef, V_lk)
!!
!!      subroutine matmat_leg_trans(np1, np2, nab, Amat, Bmat, Prod)
!!@endverbatim
!!
!!@param   nkr     Number of radial grid and field
!!@param   n_jk    Number of spherical harmonic degree for transform
!!@param   nl_rtm  Number of meridional grids
!!
!!@param   P_lj    Matrix for Legendre polynomials
!!@param   V_kl    field data @f$ f(r,\theta,m) @f$ with V_kl(r,theta)
!!@param   S_kj    spectrum data @f$ f(r,l,m) @f$ with S_kj(r,l)
!!@param   V_lk    field data @f$ f(r,\theta,m) @f$ with V_kl(theta,r)
!!@param   S_jk    spectrum data @f$ f(r,l,m) @f$ with S_jk(l,r)
!
      module matmul_for_legendre_trans
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), parameter :: iflag_INTRINSIC = 1
      integer(kind = kint), parameter :: iflag_DGEMM =     2
      integer(kind = kint), parameter :: iflag_MATPROD =   3
!
      private :: add_matmat_leg_trans
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine matmul_fwd_leg_trans(iflag_matmul, nkr, n_jk, nl_rtm,  &
     &          V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer :: n_jk4, nkr4, nl_rtm4
!
!
      if(n_jk*nkr .eq. 0) return
      if(iflag_matmul .eq. iflag_INTRINSIC) then
        S_kj = matmul(V_kl,P_lj)
#ifdef BLAS
      else if(iflag_matmul .eq. iflag_DGEMM) then
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        nl_rtm4 = int(nl_rtm)
        call DGEMM('N', 'N', nkr4, n_jk4, nl_rtm4, one,                 &
     &      V_kl, nkr4, P_lj, nl_rtm4, zero, S_kj, nkr4)
#endif
      else
        call matmat_leg_trans(nkr, n_jk, nl_rtm,                        &
     &      V_kl, P_lj, S_kj)
      end if
!
      end subroutine matmul_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine matmul_bwd_leg_trans(iflag_matmul, nl_rtm, nkr, n_jk,  &
     &          P_lj, S_jk, V_lk)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!
      real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
!
      integer :: n_jk4, nkr4, nl_rtm4
!
!
      if(nkr .eq. 0) return
      if(n_jk .eq. 0) then
        V_lk = 0.0d0
      else if(iflag_matmul .eq. iflag_INTRINSIC) then
        V_lk = matmul(P_lj,S_jk)
!
#ifdef BLAS
      else if(iflag_matmul .eq. iflag_DGEMM) then
        nl_rtm4 = int(nl_rtm)
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        call DGEMM('N', 'N', nl_rtm4, nkr4, n_jk4, one,                 &
     &      P_lj, nl_rtm4, S_jk, n_jk4, zero, V_lk, nl_rtm4)
#endif
      else
        call matmat_leg_trans(nl_rtm, nkr, n_jk, P_lj, S_jk, V_lk)
      end if
!
      end subroutine matmul_bwd_leg_trans
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine matmul_fwd_leg_trans_Pjl(iflag_matmul,                 &
     &          n_jk, nkr, nl_rtm, P_jl, V_lk, S_jk)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: P_jl(n_jk,nl_rtm)
      real(kind = kreal), intent(in) :: V_lk(nl_rtm,nkr)
!
      real(kind = kreal), intent(inout) :: S_jk(n_jk,nkr)
!
      integer :: n_jk4, nkr4, nl_rtm4
!
      if(n_jk*nkr .eq. 0) return
      if(iflag_matmul .eq. iflag_INTRINSIC) then
        S_jk = matmul(P_jl,V_lk)
#ifdef BLAS
      else if(iflag_matmul .eq. iflag_DGEMM) then
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        nl_rtm4 = int(nl_rtm)
        call DGEMM('N', 'N', n_jk4, nkr4, nl_rtm4, one,                 &
     &      P_jl, n_jk4, V_lk, nl_rtm4, zero, S_jk, n_jk4)
#endif
      else
        call matmat_leg_trans(n_jk, nkr, nl_rtm, V_lk, P_jl, S_jk)
      end if
!
      end subroutine matmul_fwd_leg_trans_Pjl
!
! ----------------------------------------------------------------------
!
      subroutine matmul_bwd_leg_trans_Pjl(iflag_matmul,                 &
     &          nkr, nl_rtm, n_jk, S_kj, P_jl, V_kl)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: S_kj(nkr,n_jk)
      real(kind = kreal), intent(in) :: P_jl(n_jk,nl_rtm)
!
      real(kind = kreal), intent(inout) :: V_kl(nkr,nl_rtm)
!
      integer :: n_jk4, nkr4, nl_rtm4
!
!
      if(nkr .eq. 0) return
      if(n_jk .eq. 0) then
        V_kl = 0.0d0
      else if(iflag_matmul .eq. iflag_INTRINSIC) then
        V_kl = matmul(S_kj,P_jl)
!
#ifdef BLAS
      else if(iflag_matmul .eq. iflag_DGEMM) then
        nl_rtm4 = int(nl_rtm)
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        call DGEMM('N', 'N', nkr4, nl_rtm4, n_jk4, one,                 &
     &      S_kj, nkr4, P_jl, n_jk4, zero, V_kl, nkr4)
#endif
      else
        call matmat_leg_trans(nkr, nl_rtm, n_jk, S_kj, P_jl, V_kl)
      end if
!
      end subroutine matmul_bwd_leg_trans_Pjl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_matmul_fwd_leg_trans(iflag_matmul,                 &
     &          nkr, n_jk, nl_rtm, V_kl, P_lj, coef, S_kj)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer :: n_jk4, nkr4, nl_rtm4
!
!
      if(n_jk*nkr .eq. 0) return
      if(iflag_matmul .eq. iflag_INTRINSIC) then
        S_kj(1:nkr,1:n_jk) = coef * S_kj(1:nkr,1:n_jk)                  &
     &         +  matmul(V_kl(1:nkr,1:nl_rtm), P_lj(1:nl_rtm,1:n_jk))
#ifdef BLAS
      else if(iflag_matmul .eq. iflag_DGEMM) then
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        nl_rtm4 = int(nl_rtm)
        call DGEMM('N', 'N', nkr4, n_jk4, nl_rtm4, one,                 &
     &      V_kl, nkr4, P_lj, nl_rtm4, coef, S_kj, nkr4)
#endif
      else
        call add_matmat_leg_trans(nkr, n_jk, nl_rtm,                    &
     &      V_kl, P_lj, coef, S_kj)
      end if
!
      end subroutine add_matmul_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine add_matmul_bwd_leg_trans(iflag_matmul,                 &
     &          nl_rtm, nkr, n_jk, P_lj, S_jk, coef, V_lk)
!
      integer(kind = kint), intent(in) :: iflag_matmul
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!
      real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
!
      integer :: n_jk4, nkr4, nl_rtm4
!
!
      if(nkr .eq. 0) return
      if(n_jk .eq. 0) then
        V_lk = 0.0d0
      else if(iflag_matmul .eq. iflag_INTRINSIC) then
        V_lk(1:nl_rtm,1:nkr) = coef * V_lk(1:nl_rtm,1:nkr)              &
     &       + matmul(P_lj(1:nl_rtm,1:n_jk), S_jk(1:n_jk,1:nkr))
#ifdef BLAS
      else if(iflag_matmul .eq. iflag_DGEMM) then
        nl_rtm4 = int(nl_rtm)
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        call DGEMM('N', 'N', nl_rtm4, nkr4, n_jk4, one,                 &
     &      P_lj, nl_rtm4, S_jk, n_jk4, coef, V_lk, nl_rtm4)
#endif
      else
        call add_matmat_leg_trans(nl_rtm, nkr, n_jk,                    &
     &      P_lj, S_jk, coef, V_lk)
      end if
!
      end subroutine add_matmul_bwd_leg_trans
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine matmat_leg_trans(np1, np2, nab, Amat, Bmat, Prod)
!
      integer(kind = kint), intent(in) :: nab, np2, np1
      real(kind = kreal), intent(in) :: Amat(np1,nab)
      real(kind = kreal), intent(in) :: Bmat(nab,np2)
!
      real(kind = kreal), intent(inout) :: Prod(np1,np2)
!
      integer(kind = kint) :: jj, kk, ll
      real(kind = kreal) :: s
!
!
      do kk = 1, np2
        do ll = 1, np1
          s = 0.0d0
          do jj = 1, nab
            s = s + Amat(ll,jj) * Bmat(jj,kk)
          end do
          Prod(ll,kk) = s
        end do
      end do
!
      end subroutine matmat_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine add_matmat_leg_trans(np1, np2, nab,                    &
     &          Amat, Bmat, coef, Prod)
!
      integer(kind = kint), intent(in) :: nab, np2, np1
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: Amat(np1,nab)
      real(kind = kreal), intent(in) :: Bmat(nab,np2)
!
      real(kind = kreal), intent(inout) :: Prod(np1,np2)
!
      integer(kind = kint) :: jj, kk, ll
      real(kind = kreal) :: s
!
!
      do kk = 1, np2
        do ll = 1, np1
          s = 0.0d0
          do jj = 1, nab
            s = s + Amat(ll,jj) * Bmat(jj,kk)
          end do
          Prod(ll,kk) = coef * Prod(ll,kk) + s
        end do
      end do
!
      end subroutine add_matmat_leg_trans
!
! ----------------------------------------------------------------------
!
      end module matmul_for_legendre_trans
