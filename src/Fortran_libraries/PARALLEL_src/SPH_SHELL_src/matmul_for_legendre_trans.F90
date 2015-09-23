!>@file   matmul_for_legendre_trans.F90
!!@brief  module matmul_for_legendre_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Matrix products for Legendre transforms
!!
!!@verbatim
!!      subroutine matmat_fwd_leg_trans(nkr, n_jk, nl_rtm,              &
!!     &          V_kl, P_lj, S_kj)
!!      subroutine matmul_fwd_leg_trans(nkr, n_jk, nl_rtm,              &
!!     &          V_kl, P_lj, S_kj)
!!      subroutine add_matmat_fwd_leg_trans(nkr, n_jk, nl_rtm,          &
!!     &          V_kl, P_lj, coef, S_kj)
!!      subroutine dgemm_fwd_leg_trans(nkr, n_jk, nl_rtm,               &
!!     &          V_kl, P_lj, coef, S_kj)
!!
!!      subroutine matmat_bwd_leg_trans(nl_rtm, nkr, n_jk,              &
!!     &          P_lj, S_jk, V_lk)
!!      subroutine matmul_bwd_leg_trans(nl_rtm, nkr, n_jk,              &
!!     &          P_lj, S_jk, V_lk)
!!      subroutine add_matmat_bwd_leg_trans(nl_rtm, nkr, n_jk,          &
!!     &          P_lj, S_jk, coef, V_lk)
!!      subroutine dgemm_bwd_leg_trans(nl_rtm, nkr, n_jk,               &
!!     &          P_lj, S_jk, coef, V_lk)
!!
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine matmat_fwd_leg_trans(nkr, n_jk, nl_rtm,                &
     &          V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk, ll
      real(kind = kreal) :: s
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          s = 0.0d0
          do ll = 1, nl_rtm
            s = s + P_lj(ll,jj) * V_kl(kk,ll)
          end do
          S_kj(kk,jj) = s
        end do
      end do
!
      end subroutine matmat_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine matmul_fwd_leg_trans(nkr, n_jk, nl_rtm,                &
     &          V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
!
      if(nkr .eq. 0) return
      S_kj = matmul(V_kl,P_lj)
!
      end subroutine matmul_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine add_matmat_fwd_leg_trans(nkr, n_jk, nl_rtm,            &
     &          V_kl, P_lj, coef, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk, ll
      real(kind = kreal) :: s
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          s = coef * S_kj(kk,jj)
          do ll = 1, nl_rtm
            s = s + P_lj(ll,jj) * V_kl(kk,ll)
          end do
          S_kj(kk,jj) = s
        end do
      end do
!
      end subroutine add_matmat_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine add_matmul_fwd_leg_trans(nkr, n_jk, nl_rtm,            &
     &          V_kl, P_lj, coef, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: coef
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
!
      if(nkr .eq. 0) return
      S_kj(1:nkr,1:n_jk) = coef * S_kj(1:nkr,1:n_jk)                    &
     &         +  matmul(V_kl(1:nkr,1:nl_rtm), P_lj(1:nl_rtm,1:n_jk))
!
      end subroutine add_matmul_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine dgemm_fwd_leg_trans(nkr, n_jk, nl_rtm,                 &
     &          V_kl, P_lj, coef, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: V_kl(nkr,nl_rtm)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer :: n_jk4, nkr4, nl_rtm4
!
!
      if(nkr .eq. 0) return
#ifdef BLAS
      nkr4 =    int(nkr)
      n_jk4 =   int(n_jk)
      nl_rtm4 = int(nl_rtm)
      call DGEMM('N', 'N', nkr4, n_jk4, nl_rtm4, one,                   &
     &    V_kl, nkr4, P_lj, nl_rtm4, coef, S_kj, nkr4)
#else
      call add_matmat_fwd_leg_trans(nkr, n_jk, nl_rtm,                  &
     &    V_kl, P_lj, coef, S_kj)
#endif
!
      end subroutine dgemm_fwd_leg_trans
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine matmat_bwd_leg_trans(nl_rtm, nkr, n_jk,                &
     &          P_lj, S_jk, V_lk)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!
      real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
!
      integer(kind = kint) :: jj, kk, ll
      real(kind = kreal) :: s
!
!
      do kk = 1, nkr
        do ll = 1, nl_rtm
          s = 0.0d0
          do jj = 1, n_jk
            s = s + P_lj(ll,jj) * S_jk(jj,kk)
          end do
          V_lk(ll,kk) = s
        end do
      end do
!
      end subroutine matmat_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine matmul_bwd_leg_trans(nl_rtm, nkr, n_jk,                &
     &          P_lj, S_jk, V_lk)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!
      real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
!
!
      if(nkr .eq. 0) return
      V_lk = matmul(P_lj,S_jk)
!
      end subroutine matmul_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine add_matmat_bwd_leg_trans(nl_rtm, nkr, n_jk,            &
     &          P_lj, S_jk, coef, V_lk)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!
      real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
!
      integer(kind = kint) :: jj, kk, ll
      real(kind = kreal) :: s
!
!
      do kk = 1, nkr
        do ll = 1, nl_rtm
          s = 0.0d0
          do jj = 1, n_jk
            s = s + P_lj(ll,jj) * S_jk(jj,kk)
          end do
          V_lk(ll,kk) = coef * V_lk(ll,kk) + s
        end do
      end do
!
      end subroutine add_matmat_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine add_matmul_bwd_leg_trans(nl_rtm, nkr, n_jk,            &
     &          P_lj, S_jk, coef, V_lk)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!
      real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
!
!
      if(nkr .eq. 0) return
      V_lk(1:nl_rtm,1:nkr) = coef * V_lk(1:nl_rtm,1:nkr)                &
     &       + matmul(P_lj(1:nl_rtm,1:n_jk), S_jk(1:n_jk,1:nkr))
!
      end subroutine add_matmul_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine dgemm_bwd_leg_trans(nl_rtm, nkr, n_jk,                 &
     &          P_lj, S_jk, coef, V_lk)
!
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
#ifdef BLAS
      if(n_jk .eq. 0) then
        V_lk = 0.0d0
      else
        nl_rtm4 = int(nl_rtm)
        nkr4 =    int(nkr)
        n_jk4 =   int(n_jk)
        call DGEMM('N', 'N', nl_rtm4, nkr4, n_jk4, one,                 &
     &      P_lj, nl_rtm4, S_jk, n_jk4, coef, V_lk, nl_rtm4)
      end if
#else
      call add_matmat_bwd_leg_trans(nl_rtm, nkr, n_jk,                  &
     &    P_lj, S_jk, coef, V_lk)
#endif
!
      end subroutine dgemm_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
      end module matmul_for_legendre_trans
