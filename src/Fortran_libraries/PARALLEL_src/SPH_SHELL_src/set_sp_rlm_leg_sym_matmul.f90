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
!!
!!      subroutine cal_sp_rlm_vector_sym_matmul(nnod_rlm, nidx_rlm,     &
!!     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,&
!!     &         kst, nkr, jst, n_jk_o, n_jk_e,                         &
!!     &         pol_e, pol_o, dpoldt_e, dpoldp_e, dpoldt_o, dpoldp_o,  &
!!     &         dtordt_e, dtordp_e, dtordt_o, dtordp_o,                &
!!     &         ncomp, irev_sr_rlm, n_WS, WS)
!!      subroutine cal_sp_rlm_scalar_sym_matmul                         &
!!     &         (nnod_rlm, nidx_rlm, istep_rlm, g_sph_rlm,             &
!!     &          kst, nkr, jst, n_jk_o, n_jk_e, scl_e, scl_o,          &
!!     &          ncomp, nvector, irev_sr_rlm, n_WS, WS)
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
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
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
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_vector_sym_matmul(nnod_rlm, nidx_rlm,       &
     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,  &
     &         kst, nkr, jst, n_jk_o, n_jk_e,                           &
     &         pol_e, pol_o, dpoldt_e, dpoldp_e, dpoldt_o, dpoldp_o,    &
     &         dtordt_e, dtordp_e, dtordt_o, dtordp_o,                  &
     &         ncomp, irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in)                                  &
     &            :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
      real(kind = kreal), intent(in) :: radius_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(inout) :: pol_e(nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: pol_o(nkr,n_jk_o)
      real(kind = kreal), intent(inout) :: dpoldt_e(nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: dpoldp_e(nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: dpoldt_o(nkr,n_jk_o)
      real(kind = kreal), intent(inout) :: dpoldp_o(nkr,n_jk_o)
      real(kind = kreal), intent(inout) :: dtordt_e(nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: dtordp_e(nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: dtordt_o(nkr,n_jk_o)
      real(kind = kreal), intent(inout) :: dtordp_o(nkr,n_jk_o)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: nd, jj, i_kj
      real(kind = kreal) :: g7, gm, r1_1d_rlm_r, r2_1d_rlm_r
!
!
      do jj = 1, n_jk_e
        g7 = g_sph_rlm(2*jj+jst-1,7)
        gm = dble(idx_gl_1d_rlm_j(2*jj+jst-1,3))
        do kk = 1, nkr
          i_kj = kk + (jj-1) * nkr
          k_rlm = 1 + mod((kk+kst-1),nidx_rlm(1))
          r1_1d_rlm_r = radius_1d_rlm_r(k_rlm)
          r2_1d_rlm_r = r1_1d_rlm_r * r1_1d_rlm_r
!
          pol_e(kk,jj) =    pol_e(kk,jj) *    r2_1d_rlm_r * g7
          dpoldt_e(kk,jj) = dpoldt_e(kk,jj) * r1_1d_rlm_r * g7
          dpoldp_e(kk,jj) = dpoldp_e(kk,jj) * r1_1d_rlm_r * g7 * gm
          dtordt_e(kk,jj) = dtordt_e(kk,jj) * r1_1d_rlm_r * g7
          dtordp_e(kk,jj) = dtordp_e(kk,jj) * r1_1d_rlm_r * g7 * gm
        end do
      end do
      do jj = 1, n_jk_o
        g7 = g_sph_rlm(2*jj+jst,7)
        gm = dble(idx_gl_1d_rlm_j(2*jj+jst,3))
        do kk = 1, nkr
          k_rlm = 1 + mod((kk+kst-1),nidx_rlm(1))
          r1_1d_rlm_r = radius_1d_rlm_r(k_rlm)
          r2_1d_rlm_r = r1_1d_rlm_r * r1_1d_rlm_r
          i_kj = kk + (jj-1) * nkr
!
          pol_o(kk,jj) =    pol_o(kk,jj) *    r2_1d_rlm_r * g7
          dpoldt_o(kk,jj) = dpoldt_o(kk,jj) * r1_1d_rlm_r * g7
          dpoldp_o(kk,jj) = dpoldp_o(kk,jj) * r1_1d_rlm_r * g7 * gm
          dtordt_o(kk,jj) = dtordt_o(kk,jj) * r1_1d_rlm_r * g7
          dtordp_o(kk,jj) = dtordp_o(kk,jj) * r1_1d_rlm_r * g7 * gm
        end do
      end do
!
      do jj = 1, n_jk_o
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          i_kj = kk + (jj-1) * nkr
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          io_rlm = 1 + (2*jj+jst-1) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp
          io_send = 3*nd + (irev_sr_rlm(io_rlm) - 1) * ncomp
!
!  even l-m
          WS(ie_send-2) = WS(ie_send-2) + pol_e(kk,jj)
          WS(ie_send-1) = WS(ie_send-1)                                 &
     &                   - dpoldp_e(kk,jj) + dpoldt_e(kk,jj)
          WS(ie_send  ) = WS(ie_send  )                                 &
     &                   - dtordp_e(kk,jj) - dtordt_e(kk,jj)
!  odd l-m
          WS(io_send-2) = WS(io_send-2) + pol_o(kk,jj)
          WS(io_send-1) = WS(io_send-1)                                 &
     &                   - dpoldp_o(kk,jj) + dpoldt_o(kk,jj)
          WS(io_send  ) = WS(io_send  )                                 &
     &                   - dtordp_o(kk,jj) - dtordt_o(kk,jj)
        end do
      end do
!
      do jj = n_jk_o+1, n_jk_e
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          i_kj = kk + (jj-1) * nkr
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp
!
          WS(ie_send-2) = WS(ie_send-2) + pol_e(kk,jj)
          WS(ie_send-1) = WS(ie_send-1)                                 &
     &                   - dpoldp_e(kk,jj) + dpoldt_e(kk,jj)
          WS(ie_send  ) = WS(ie_send  )                                 &
     &                   - dtordp_e(kk,jj) - dtordt_e(kk,jj)
        end do
      end do
!
      end subroutine cal_sp_rlm_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_scalar_sym_matmul                           &
     &         (nnod_rlm, nidx_rlm, istep_rlm, g_sph_rlm,               &
     &          kst, nkr, jst, n_jk_o, n_jk_e, scl_e, scl_o,            &
     &          ncomp, nvector, irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(inout) :: scl_e(nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: scl_o(nkr,n_jk_o)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: nd, jj
      real(kind = kreal) :: g6
!
!
      do jj = 1, n_jk_e
        g6 = g_sph_rlm(2*jj+jst-1,6)
        do kk = 1, nkr
          scl_e(kk,jj) = scl_e(kk,jj) * g6
        end do
      end do
      do jj = 1, n_jk_o
        g6 = g_sph_rlm(2*jj+jst,6)
        do kk = 1, nkr
          scl_o(kk,jj) = scl_o(kk,jj) * g6
        end do
      end do
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do jj = 1, n_jk_o
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          io_rlm = 1 + (2*jj+jst-1) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = nd + 3*nvector + (irev_sr_rlm(ie_rlm) - 1) * ncomp
          io_send = nd + 3*nvector + (irev_sr_rlm(io_rlm) - 1) * ncomp
!
          WS(ie_send) = WS(ie_send) + scl_e(kk,jj)
          WS(io_send) = WS(io_send) + scl_o(kk,jj)
        end do
!
        do jj = n_jk_o+1, n_jk_e
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = nd + 3*nvector + (irev_sr_rlm(ie_rlm) - 1) * ncomp
          WS(ie_send) = WS(ie_send) + scl_e(kk,jj)
        end do
      end do
!
      end subroutine cal_sp_rlm_scalar_sym_matmul
!
! -----------------------------------------------------------------------
!
      end module set_sp_rlm_leg_sym_matmul
