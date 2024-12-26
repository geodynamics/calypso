!>@file   set_sp_rlm_leg_sym_matmul.f90
!!@brief  module set_sp_rlm_leg_sym_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Set spectrum data for backward Legendre transform
!!
!!@verbatim
!!      subroutine set_sp_rlm_vector_sym_matmul(nnod_rlm, nidx_rlm,     &
!!     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,  &
!!     &          kst, nkr, jst,  n_jk_e, n_jk_o,                       &
!!     &          ncomp, irev_sr_rlm, n_WR, WR,                         &
!!     &          pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e,        &
!!     &          pol_o, dpoldt_o, dpoldp_o, dtordt_o, dtordp_o)
!!      subroutine set_sp_rlm_scalar_sym_matmul                         &
!!     &         (nnod_rlm, nidx_rlm, istep_rlm,                        &
!!     &          kst, nkr, jst, n_jk_e, n_jk_o,                        &
!!     &          ncomp, nvector, irev_sr_rlm, n_WR, WR, scl_e, scl_o)
!!        integer(kind = kint), intent(in) :: nnod_rlm
!!        integer(kind = kint), intent(in) :: nidx_rlm(2)
!!        integer(kind = kint), intent(in) :: istep_rlm(2)
!!        integer(kind = kint), intent(in)                              &
!!     &                     :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
!!        real(kind = kreal), intent(in) :: a_r_1d_rlm_r(nidx_rlm(1))
!!        real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!!        integer(kind = kint), intent(in) :: kst, nkr
!!        integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
!!        integer(kind = kint), intent(in) :: ncomp, nvector
!!        integer(kind = kint), intent(in) :: n_WR
!!        integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
!!        real(kind = kreal), intent(in) :: WR(n_WR)
!!        real(kind = kreal), intent(inout) :: pol_e(n_jk_e,nkr)
!!        real(kind = kreal), intent(inout) :: dpoldt_e(n_jk_e,nkr)
!!        real(kind = kreal), intent(inout) :: dpoldp_e(n_jk_e,nkr)
!!        real(kind = kreal), intent(inout) :: dtordt_e(n_jk_e,nkr)
!!        real(kind = kreal), intent(inout) :: dtordp_e(n_jk_e,nkr)
!!        real(kind = kreal), intent(inout) :: pol_o(n_jk_o,nkr)
!!        real(kind = kreal), intent(inout) :: dpoldt_o(n_jk_o,nkr)
!!        real(kind = kreal), intent(inout) :: dpoldp_o(n_jk_o,nkr)
!!        real(kind = kreal), intent(inout) :: dtordt_o(n_jk_o,nkr)
!!        real(kind = kreal), intent(inout) :: dtordp_o(n_jk_o,nkr)
!!        real(kind = kreal), intent(inout) :: scl_e(n_jk_e,nkr)
!!        real(kind = kreal), intent(inout) :: scl_o(n_jk_o,nkr)
!!@endverbatim
!!
      module set_sp_rlm_leg_sym_matmul
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
      subroutine set_sp_rlm_vector_sym_matmul(nnod_rlm, nidx_rlm,       &
     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,    &
     &          kst, nkr, jst,  n_jk_e, n_jk_o,                         &
     &          ncomp, irev_sr_rlm, n_WR, WR,                           &
     &          pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e,          &
     &          pol_o, dpoldt_o, dpoldp_o, dtordt_o, dtordp_o)
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
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: pol_e(n_jk_e,nkr)
      real(kind = kreal), intent(inout) :: dpoldt_e(n_jk_e,nkr)
      real(kind = kreal), intent(inout) :: dpoldp_e(n_jk_e,nkr)
      real(kind = kreal), intent(inout) :: dtordt_e(n_jk_e,nkr)
      real(kind = kreal), intent(inout) :: dtordp_e(n_jk_e,nkr)
      real(kind = kreal), intent(inout) :: pol_o(n_jk_o,nkr)
      real(kind = kreal), intent(inout) :: dpoldt_o(n_jk_o,nkr)
      real(kind = kreal), intent(inout) :: dpoldp_o(n_jk_o,nkr)
      real(kind = kreal), intent(inout) :: dtordt_o(n_jk_o,nkr)
      real(kind = kreal), intent(inout) :: dtordp_o(n_jk_o,nkr)
!
      integer(kind = kint) :: jj, kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: j_rlm, i_rlm, i_recv
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
      real(kind = kreal) :: g3, gm
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
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
          pol_e(jj,kk) =    WR(i_recv-2) * a2r_1d_rlm_r * g3
          dpoldt_e(jj,kk) = WR(i_recv-1) * a1r_1d_rlm_r
          dpoldp_e(jj,kk) = WR(i_recv-1) * a1r_1d_rlm_r * gm
          dtordt_e(jj,kk) = WR(i_recv  ) * a1r_1d_rlm_r
          dtordp_e(jj,kk) = WR(i_recv  ) * a1r_1d_rlm_r * gm
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
          pol_o(jj,kk) =    WR(i_recv-2) * a2r_1d_rlm_r * g3
          dpoldt_o(jj,kk) = WR(i_recv-1) * a1r_1d_rlm_r
          dpoldp_o(jj,kk) = WR(i_recv-1) * a1r_1d_rlm_r * gm
          dtordt_o(jj,kk) = WR(i_recv  ) * a1r_1d_rlm_r
          dtordp_o(jj,kk) = WR(i_recv  ) * a1r_1d_rlm_r * gm
        end do
      end do
!
      end subroutine set_sp_rlm_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_scalar_sym_matmul                           &
     &         (nnod_rlm, nidx_rlm, istep_rlm,                          &
     &          kst, nkr, jst, n_jk_e, n_jk_o,                          &
     &          ncomp, nvector, irev_sr_rlm, n_WR, WR, scl_e, scl_o)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real(kind = kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: scl_e(n_jk_e,nkr)
      real(kind = kreal), intent(inout) :: scl_o(n_jk_o,nkr)
!
      integer(kind = kint) :: jj, kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: i_rlm, i_recv
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!   even l-m
        do jj = 1, n_jk_e
          i_rlm = 1 + (2*jj + jst - 2) * istep_rlm(2)                   &
     &              + (k_rlm-1) *        istep_rlm(1)
          i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
          scl_e(jj,kk) = WR(i_recv)
        end do
!   odd l-m
        do jj = 1, n_jk_o
          i_rlm = 1 + (2*jj + jst - 1) * istep_rlm(2)                   &
     &              + (k_rlm-1) *        istep_rlm(1)
          i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
          scl_o(jj,kk) = WR(i_recv)
        end do
      end do
!
      end subroutine set_sp_rlm_scalar_sym_matmul
!
! -----------------------------------------------------------------------
!
      end module set_sp_rlm_leg_sym_matmul
