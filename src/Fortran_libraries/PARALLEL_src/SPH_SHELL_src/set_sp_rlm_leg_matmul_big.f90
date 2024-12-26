!>@file   set_sp_rlm_leg_matmul_big.f90
!!@brief  module set_sp_rlm_leg_matmul_big
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform using matmulti
!!
!!@verbatim
!!      subroutine set_sp_rlm_vec_sym_matmul_big(nnod_rlm, nidx_rlm,    &
!!     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,  &
!!     &          kst, nkr, jst, n_jk_e, n_jk_o, ncomp, nvector,        &
!!     &          irev_sr_rlm, n_WR, WR,  pol_e, tor_e, pol_o, tor_o)
!!      subroutine set_sp_rlm_scl_sym_matmul_big                        &
!!     &         (nnod_rlm, nidx_rlm, istep_rlm, kst, nkr, jst,         &
!!     &          n_jk_e, n_jk_o, ncomp, nvector, nscalar, irev_sr_rlm, &
!!     &          n_WR, WR, scl_e, scl_o)
!!        integer(kind = kint), intent(in) :: nnod_rlm
!!        integer(kind = kint), intent(in) :: nidx_rlm(2)
!!        integer(kind = kint), intent(in) :: istep_rlm(2)
!!        integer(kind = kint), intent(in)                              &
!!     &                     :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
!!        real(kind = kreal), intent(in) :: a_r_1d_rlm_r(nidx_rlm(1))
!!        real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!!        integer(kind = kint), intent(in) :: kst, nkr
!!        integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
!!        integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!!        integer(kind = kint), intent(in) :: n_WR
!!        integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
!!        real(kind = kreal), intent(in) :: WR(n_WR)
!!        real(kind = kreal), intent(inout):: pol_e(n_jk_e,ncomp*nkr)
!!        real(kind = kreal), intent(inout):: tor_e(n_jk_e,2*nvector*nkr)
!!        real(kind = kreal), intent(inout):: pol_o(n_jk_o,ncomp*nkr)
!!        real(kind = kreal), intent(inout):: tor_o(n_jk_o,2*nvector*nkr)
!!        real(kind = kreal), intent(inout):: scl_e(n_jk_e,ncomp*nkr)
!!        real(kind = kreal), intent(inout):: scl_o(n_jk_o,ncomp*nkr)
!!@endverbatim
!!
!
      module set_sp_rlm_leg_matmul_big
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
      subroutine set_sp_rlm_vec_sym_matmul_big(nnod_rlm, nidx_rlm,      &
     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,    &
     &          kst, nkr, jst, n_jk_e, n_jk_o, ncomp, nvector,          &
     &          irev_sr_rlm, n_WR, WR,  pol_e, tor_e, pol_o, tor_o)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in)                                  &
     &                     :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
      real(kind = kreal), intent(in) :: a_r_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: pol_e(n_jk_e,ncomp*nkr)
      real(kind = kreal), intent(inout) :: tor_e(n_jk_e,2*nvector*nkr)
      real(kind = kreal), intent(inout) :: pol_o(n_jk_o,ncomp*nkr)
      real(kind = kreal), intent(inout) :: tor_o(n_jk_o,2*nvector*nkr)
!
      integer(kind = kint) :: jj, kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: j_rlm, i_rlm, i_recv, nkrv
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
      real(kind = kreal) :: g3, gm
!
!
      nkrv = nkr * nvector
      do kk = 1, nkrv
        kr_nd = kk + kst*nvector
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        a1r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)
        a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
!   even l-m
        do jj = 1, n_jk_e
          j_rlm = 2*jj + jst - 1
          g3 = g_sph_rlm(j_rlm,3)
          gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
          i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                          &
     &              + (k_rlm-1) * istep_rlm(1)
          i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
          pol_e(jj,kk) =        WR(i_recv-2) * a2r_1d_rlm_r * g3
          tor_e(jj,kk+nkrv) =   WR(i_recv-1) * a1r_1d_rlm_r
          pol_e(jj,kk+2*nkrv) = WR(i_recv-1) * a1r_1d_rlm_r * gm
          tor_e(jj,kk) =        WR(i_recv  ) * a1r_1d_rlm_r
          pol_e(jj,kk+nkrv) =   WR(i_recv  ) * a1r_1d_rlm_r * gm
        end do
!   odd l-m
        do jj = 1, n_jk_o
          j_rlm = 2*jj + jst
          g3 = g_sph_rlm(j_rlm,3)
          gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
          i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                          &
     &              + (k_rlm-1) * istep_rlm(1)
          i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
          pol_o(jj,kk) =        WR(i_recv-2) * a2r_1d_rlm_r * g3
          tor_o(jj,kk+nkrv) =   WR(i_recv-1) * a1r_1d_rlm_r
          pol_o(jj,kk+2*nkrv) = WR(i_recv-1) * a1r_1d_rlm_r * gm
          tor_o(jj,kk) =        WR(i_recv  ) * a1r_1d_rlm_r
          pol_o(jj,kk+nkrv) =   WR(i_recv  ) * a1r_1d_rlm_r * gm
        end do
      end do
!
      end subroutine set_sp_rlm_vec_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_scl_sym_matmul_big                          &
     &         (nnod_rlm, nidx_rlm, istep_rlm, kst, nkr, jst,           &
     &          n_jk_e, n_jk_o, ncomp, nvector, nscalar, irev_sr_rlm,   &
     &          n_WR, WR, scl_e, scl_o)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: scl_e(n_jk_e,ncomp*nkr)
      real(kind = kreal), intent(inout) :: scl_o(n_jk_o,ncomp*nkr)
!
      integer(kind = kint) :: jj, kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: i_rlm, i_recv, nkrv
!
!
      nkrv = nkr * nvector
      do kk = 1, nkr*nscalar
        kr_nd = kk + kst*nscalar
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!   even l-m
        do jj = 1, n_jk_e
          i_rlm = 1 + (2*jj + jst - 2) * istep_rlm(2)                   &
     &              + (k_rlm-1) *        istep_rlm(1)
          i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
          scl_e(jj,kk+3*nkrv) = WR(i_recv)
        end do
!   odd l-m
        do jj = 1, n_jk_o
          i_rlm = 1 + (2*jj + jst - 1) * istep_rlm(2)                   &
     &              + (k_rlm-1) *        istep_rlm(1)
          i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
          scl_o(jj,kk+3*nkrv) = WR(i_recv)
        end do
      end do
!
      end subroutine set_sp_rlm_scl_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      end module set_sp_rlm_leg_matmul_big
