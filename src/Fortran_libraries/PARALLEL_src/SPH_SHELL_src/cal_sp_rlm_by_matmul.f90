!>@file   cal_sp_rlm_by_matmul.f90
!!@brief  module cal_sp_rlm_by_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform using matmulti
!!
!!@verbatim
!!      subroutine cal_sp_rlm_vector_matmul(nnod_rlm, nidx_rlm,         &
!!     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,&
!!     &         kst, nkr, jst, nj_rlm, nvec_jk,                        &
!!     &         pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e,         &
!!     &         ncomp, irev_sr_rlm, n_WS, WS)
!!      subroutine cal_sp_rlm_scalar_matmul(nnod_rlm, nidx_rlm,         &
!!     &          istep_rlm, g_sph_rlm, kst, nkr, jst, nj_rlm,          &
!!     &          nscl_jk, scl_e, ncomp, nvector, irev_sr_rlm, n_WS, WS)
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
!!
!!      subroutine cal_sp_rlm_vec_sym_matmul_big(nnod_rlm, nidx_rlm,    &
!!     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,&
!!     &         kst, nkr, jst, n_jk_o, n_jk_e, pol_e, pol_o,           &
!!     &         tor_e, tor_o, ncomp, nvector, irev_sr_rlm, n_WS, WS)
!!      subroutine cal_sp_rlm_scl_sym_matmul_big                        &
!!     &         (nnod_rlm, nidx_rlm, istep_rlm, g_sph_rlm,             &
!!     &          kst, nkr, jst, n_jk_o, n_jk_e, scl_e, scl_o,          &
!!     &          ncomp, nvector, nscalar, irev_sr_rlm, n_WS, WS)
!!@endverbatim
!!
!
      module cal_sp_rlm_by_matmul
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
      subroutine cal_sp_rlm_vector_matmul(nnod_rlm, nidx_rlm,           &
     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,  &
     &         kst, nkr, jst, nj_rlm, nvec_jk,                          &
     &         pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e,           &
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
      integer(kind = kint), intent(in) :: nvec_jk
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, nj_rlm
!
      real(kind = kreal), intent(inout) :: pol_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dpoldt_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dpoldp_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dtordt_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dtordp_e(nvec_jk)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, kk, nd, k_rlm
      integer(kind = kint) :: i_rlm, i_send, i_kj, j_rlm, jj
      real(kind = kreal) :: g7, gm, r1_1d_rlm_r, r2_1d_rlm_r
!
!
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        g7 = g_sph_rlm(jj+jst,7)
        gm = dble(idx_gl_1d_rlm_j(jj+jst,3))
        do kk = 1, nkr
          i_kj = kk + (jj-1) * nkr
          k_rlm = 1 + mod((kk+kst-1),nidx_rlm(1))
          r1_1d_rlm_r = radius_1d_rlm_r(k_rlm)
          r2_1d_rlm_r = r1_1d_rlm_r * r1_1d_rlm_r
          pol_e(i_kj) =    pol_e(i_kj) *    r2_1d_rlm_r * g7
          dpoldt_e(i_kj) = dpoldt_e(i_kj) * r1_1d_rlm_r * g7
          dpoldp_e(i_kj) = dpoldp_e(i_kj) * r1_1d_rlm_r * g7 * gm
          dtordt_e(i_kj) = dtordt_e(i_kj) * r1_1d_rlm_r * g7
          dtordp_e(i_kj) = dtordp_e(i_kj) * r1_1d_rlm_r * g7 * gm
        end do
      end do
!
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          i_kj = kk + (jj-1) * nkr
          i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                          &
     &              + (k_rlm-1) * istep_rlm(1)
          i_send = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
          WS(i_send-2) = WS(i_send-2) + pol_e(i_kj)
          WS(i_send-1) = WS(i_send-1) - dpoldp_e(i_kj) + dpoldt_e(i_kj)
          WS(i_send  ) = WS(i_send  ) - dtordp_e(i_kj) - dtordt_e(i_kj)
        end do
      end do
!
      end subroutine cal_sp_rlm_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_scalar_matmul(nnod_rlm, nidx_rlm,           &
     &          istep_rlm, g_sph_rlm, kst, nkr, jst, nj_rlm,            &
     &          nscl_jk, scl_e, ncomp, nvector, irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, nj_rlm
!
      integer(kind = kint), intent(in) :: nscl_jk
      real(kind = kreal), intent(in) :: scl_e(nscl_jk)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, kk, nd, k_rlm
      integer(kind = kint) :: i_rlm, i_send, i_kj, j_rlm, jj
      real(kind = kreal) :: g6
!
!
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        g6 = g_sph_rlm(j_rlm,6)
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                          &
     &              + (k_rlm-1) * istep_rlm(1)
          i_send = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
          i_kj = kk + (jj-1) * nkr
!
          WS(i_send) = WS(i_send) + scl_e(i_kj) * g6
        end do
      end do
!
      end subroutine cal_sp_rlm_scalar_matmul
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
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_vec_sym_matmul_big(nnod_rlm, nidx_rlm,      &
     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,  &
     &         kst, nkr, jst, n_jk_o, n_jk_e, pol_e, pol_o,             &
     &         tor_e, tor_o, ncomp, nvector, irev_sr_rlm, n_WS, WS)
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
      real(kind = kreal), intent(inout) :: pol_e(ncomp*nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: pol_o(ncomp*nkr,n_jk_o)
      real(kind = kreal), intent(inout) :: tor_e(2*nvector*nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: tor_o(2*nvector*nkr,n_jk_o)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nkrv
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: nd, jj
      real(kind = kreal) :: g7, gm, r1, r2
!
!
      nkrv = nkr * nvector
      do jj = 1, n_jk_e
        g7 = g_sph_rlm(2*jj+jst-1,7)
        gm = dble(idx_gl_1d_rlm_j(2*jj+jst-1,3))
        do kk = 1, nkrv
          kr_nd = kk + nvector*kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
!
          pol_e(kk,jj) =        pol_e(kk,jj) *        r2 * g7
          tor_e(kk+nkrv,jj) =   tor_e(kk+nkrv,jj) *   r1 * g7
          pol_e(kk+2*nkrv,jj) = pol_e(kk+2*nkrv,jj) * r1 * g7 * gm
          tor_e(kk,jj) =        tor_e(kk,jj) *        r1 * g7
          pol_e(kk+nkrv,jj) =   pol_e(kk+nkrv,jj) *   r1 * g7 * gm
        end do
      end do
      do jj = 1, n_jk_o
        g7 = g_sph_rlm(2*jj+jst,7)
        gm = dble(idx_gl_1d_rlm_j(2*jj+jst,3))
        do kk = 1, nkrv
          kr_nd = kk + nvector*kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
!
          pol_o(kk,jj) =        pol_o(kk,jj) *        r2 * g7
          tor_o(kk+nkrv,jj) =   tor_o(kk+nkrv,jj) *   r1 * g7
          pol_o(kk+2*nkrv,jj) = pol_o(kk+2*nkrv,jj) * r1 * g7 * gm
          tor_o(kk,jj) =        tor_o(kk,jj) *        r1 * g7
          pol_o(kk+nkrv,jj) =   pol_o(kk+nkrv,jj) *   r1 * g7 * gm
        end do
      end do
!
      do jj = 1, n_jk_o
        do kk = 1, nkrv
          kr_nd = kk + nvector*kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
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
     &                   - pol_e(kk+2*nkrv,jj) + tor_e(kk+nkrv,jj)
          WS(ie_send  ) = WS(ie_send  )                                 &
     &                   - pol_e(kk+nkrv,jj) -   tor_e(kk,jj)
!  odd l-m
          WS(io_send-2) = WS(io_send-2) + pol_o(kk,jj)
          WS(io_send-1) = WS(io_send-1)                                 &
     &                   - pol_o(kk+2*nkrv,jj) + tor_o(kk+nkrv,jj)
          WS(io_send  ) = WS(io_send  )                                 &
     &                   - pol_o(kk+nkrv,jj) -   tor_o(kk,jj)
        end do
      end do
!
      do jj = n_jk_o+1, n_jk_e
        do kk = 1, nkrv
          kr_nd = kk + nvector*kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp
!
          WS(ie_send-2) = WS(ie_send-2) + pol_e(kk,jj)
          WS(ie_send-1) = WS(ie_send-1)                                 &
     &                   - pol_e(kk+2*nkrv,jj) + tor_e(kk+nkrv,jj)
          WS(ie_send  ) = WS(ie_send  )                                 &
     &                   - pol_e(kk+nkrv,jj) -   tor_e(kk,jj)
        end do
      end do
!
      end subroutine cal_sp_rlm_vec_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_scl_sym_matmul_big                          &
     &         (nnod_rlm, nidx_rlm, istep_rlm, g_sph_rlm,               &
     &          kst, nkr, jst, n_jk_o, n_jk_e, scl_e, scl_o,            &
     &          ncomp, nvector, nscalar, irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(inout) :: scl_e(ncomp*nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: scl_o(ncomp*nkr,n_jk_o)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nkrv
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: nd, jj
      real(kind = kreal) :: g6
!
!
      nkrv = nkr * nvector
      do jj = 1, n_jk_e
        g6 = g_sph_rlm(2*jj+jst-1,6)
        do kk = 1, nkr*nscalar
          scl_e(kk+3*nkrv,jj) = scl_e(kk+3*nkrv,jj) * g6
        end do
      end do
      do jj = 1, n_jk_o
        g6 = g_sph_rlm(2*jj+jst,6)
        do kk = 1, nkr*nscalar
          scl_o(kk+3*nkrv,jj) = scl_o(kk+3*nkrv,jj) * g6
        end do
      end do
!
      do kk = 1, nkr*nscalar
        kr_nd = kk + kst*nscalar
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
          WS(ie_send) = WS(ie_send) + scl_e(kk+3*nkrv,jj)
          WS(io_send) = WS(io_send) + scl_o(kk+3*nkrv,jj)
        end do
!
        do jj = n_jk_o+1, n_jk_e
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          ie_send = nd + 3*nvector + (irev_sr_rlm(ie_rlm) - 1) * ncomp
          WS(ie_send) = WS(ie_send) + scl_e(kk+3*nkrv,jj)
        end do
      end do
!
      end subroutine cal_sp_rlm_scl_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      end module cal_sp_rlm_by_matmul
