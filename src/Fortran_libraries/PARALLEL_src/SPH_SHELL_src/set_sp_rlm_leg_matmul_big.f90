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
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
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
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(inout) :: pol_e(ncomp*nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: pol_o(ncomp*nkr,n_jk_o)
      real(kind = kreal), intent(inout) :: tor_e(2*nvector*nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: tor_o(2*nvector*nkr,n_jk_o)
!
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
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(inout) :: scl_e(ncomp*nkr,n_jk_e)
      real(kind = kreal), intent(inout) :: scl_o(ncomp*nkr,n_jk_o)
!
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
      end module set_sp_rlm_leg_matmul_big
