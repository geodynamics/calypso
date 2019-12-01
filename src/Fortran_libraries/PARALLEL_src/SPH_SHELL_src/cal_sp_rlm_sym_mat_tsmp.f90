!>@file   cal_sp_rlm_sym_mat_tsmp.f90
!!@brief  module cal_sp_rlm_sym_mat_tsmp
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (Blocked loop version)
!!
!!@verbatim
!!      subroutine cal_sp_rlm_sym_mat_rout(nnod_rlm, nidx_rlm,          &
!!     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,&
!!     &         jst, n_jk_o, n_jk_e, pol_e, pol_o, tor_e, tor_o,       &
!!     &         ncomp_send, nvector, nscalar, irev_sr_rlm, n_WS, WS)
!!      subroutine cal_sp_rlm_sym_mat_rin(nnod_rlm, nidx_rlm,           &
!!     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,&
!!     &         jst, n_jk_o, n_jk_e, pol_e, pol_o, tor_e, tor_o,       &
!!     &         ncomp_send, nvector, nscalar, irev_sr_rlm, n_WS, WS)
!!@endverbatim
!!
      module cal_sp_rlm_sym_mat_tsmp
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
!
      implicit none
!
      integer, external :: omp_get_max_threads
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_sym_mat_rout(nnod_rlm, nidx_rlm,            &
     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,  &
     &         jst, n_jk_o, n_jk_e, pol_e, pol_o, tor_e, tor_o,         &
     &         ncomp_send, nvector, nscalar, irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in)                                  &
     &            :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
      real(kind = kreal), intent(in) :: radius_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: nvector, nscalar
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(inout)                                 &
     &         :: pol_e(n_jk_e,nidx_rlm(1),3*nvector+nscalar)
      real(kind = kreal), intent(inout)                                 &
     &         :: pol_o(n_jk_o,nidx_rlm(1),3*nvector+nscalar)
      real(kind = kreal), intent(inout)                                 &
     &         :: tor_e(n_jk_e,nidx_rlm(1),2*nvector)
      real(kind = kreal), intent(inout)                                 &
     &         :: tor_o(n_jk_o,nidx_rlm(1),2*nvector)
!
      integer(kind = kint), intent(in) :: ncomp_send
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, k_rlm
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: nd, jj
      real(kind = kreal) :: g6, g7, gm, r1, r2
!
!
!$omp parallel do private(jj,kr_nd,nd,k_rlm,g6,g7,gm,r1,r2)
      do kr_nd = 1, nvector*nidx_rlm(1)
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
          do jj = 1, n_jk_e
            g6 = g_sph_rlm(2*jj+jst-1,6)
            g7 = g_sph_rlm(2*jj+jst-1,7)
            gm = dble(idx_gl_1d_rlm_j(2*jj+jst-1,3))
!
            pol_e(jj,k_rlm,3*nd-2) = pol_e(jj,k_rlm,3*nd-2) * r2*g7
            tor_e(jj,k_rlm,2*nd  ) = tor_e(jj,k_rlm,2*nd  ) * r1*g7
            pol_e(jj,k_rlm,3*nd  ) = pol_e(jj,k_rlm,3*nd  ) * r1*g7*gm
            tor_e(jj,k_rlm,2*nd-1) = tor_e(jj,k_rlm,2*nd-1) * r1*g7
            pol_e(jj,k_rlm,3*nd-1) = pol_e(jj,k_rlm,3*nd-1) * r1*g7*gm
          end do
      end do
!$omp end parallel do
!
!$omp parallel do private(jj,kr_nd,nd,k_rlm,g6,g7,gm,r1,r2)
      do kr_nd = 1, nscalar*nidx_rlm(1)
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
          do jj = 1, n_jk_e
            g6 = g_sph_rlm(2*jj+jst-1,6)
            g7 = g_sph_rlm(2*jj+jst-1,7)
            gm = dble(idx_gl_1d_rlm_j(2*jj+jst-1,3))
!
            pol_e(jj,k_rlm,nd+3*nvector)                                &
     &            = pol_e(jj,k_rlm,nd+3*nvector) * g6
          end do
      end do
!$omp end parallel do
!
!$omp parallel do private(jj,kr_nd,nd,k_rlm,g6,g7,gm,r1,r2)
      do kr_nd = 1, nvector*nidx_rlm(1)
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
          do jj = 1, n_jk_o
            g6 = g_sph_rlm(2*jj+jst,6)
            g7 = g_sph_rlm(2*jj+jst,7)
            gm = dble(idx_gl_1d_rlm_j(2*jj+jst,3))
!
            pol_o(jj,k_rlm,3*nd-2) = pol_o(jj,k_rlm,3*nd-2) * r2*g7
            tor_o(jj,k_rlm,2*nd  ) = tor_o(jj,k_rlm,2*nd  ) * r1*g7
            pol_o(jj,k_rlm,3*nd  ) = pol_o(jj,k_rlm,3*nd  ) * r1*g7*gm
            tor_o(jj,k_rlm,2*nd-1) = tor_o(jj,k_rlm,2*nd-1) * r1*g7
            pol_o(jj,k_rlm,3*nd-1) = pol_o(jj,k_rlm,3*nd-1) * r1*g7*gm
          end do
      end do
!$omp end parallel do
!
!$omp parallel do private(jj,kr_nd,nd,k_rlm,g6,g7,gm,r1,r2)
      do kr_nd = 1, nscalar*nidx_rlm(1)
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
          do jj = 1, n_jk_o
            g6 = g_sph_rlm(2*jj+jst,6)
            g7 = g_sph_rlm(2*jj+jst,7)
            gm = dble(idx_gl_1d_rlm_j(2*jj+jst,3))
!
            pol_o(jj,k_rlm,nd+3*nvector)                                &
     &            = pol_o(jj,k_rlm,nd+3*nvector) * g6
          end do
      end do
!$omp end parallel do
!
!
!$omp parallel do private(kr_nd,k_rlm,nd,jj,ie_rlm,io_rlm,ie_send,io_send)
      do kr_nd = 1, nvector*nidx_rlm(1)
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          do jj = 1, n_jk_o
            ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                    &
     &               + (k_rlm-1) *    istep_rlm(1)
            io_rlm = 1 + (2*jj+jst-1) * istep_rlm(2)                    &
     &               + (k_rlm-1) *    istep_rlm(1)
!
            ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp_send
            io_send = 3*nd + (irev_sr_rlm(io_rlm) - 1) * ncomp_send
!
!  even l-m
            WS(ie_send-2) = WS(ie_send-2) + pol_e(jj,k_rlm,3*nd-2)
            WS(ie_send-1) = WS(ie_send-1) - pol_e(jj,k_rlm,3*nd  )      &
     &                                    + tor_e(jj,k_rlm,2*nd  )
            WS(ie_send  ) = WS(ie_send  ) - pol_e(jj,k_rlm,3*nd-1)      &
     &                                    - tor_e(jj,k_rlm,2*nd-1)
!  odd l-m
            WS(io_send-2) = WS(io_send-2) + pol_o(jj,k_rlm,3*nd-2)
            WS(io_send-1) = WS(io_send-1) - pol_o(jj,k_rlm,3*nd  )      &
     &                                    + tor_o(jj,k_rlm,2*nd  )
            WS(io_send  ) = WS(io_send  ) - pol_o(jj,k_rlm,3*nd-1)      &
     &                                    - tor_o(jj,k_rlm,2*nd-1)
          end do
      end do
!$omp end parallel do
!
!$omp parallel do private(kr_nd,k_rlm,nd,jj,ie_rlm,io_rlm,ie_send,io_send)
      do kr_nd = 1, nscalar*nidx_rlm(1)
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          do jj = 1, n_jk_o
            ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                    &
     &               + (k_rlm-1) *    istep_rlm(1)
            io_rlm = 1 + (2*jj+jst-1) * istep_rlm(2)                    &
     &               + (k_rlm-1) *    istep_rlm(1)
!
            ie_send = nd + 3*nvector                                    &
     &               + (irev_sr_rlm(ie_rlm) - 1) * ncomp_send
            io_send = nd + 3*nvector                                    &
     &               + (irev_sr_rlm(io_rlm) - 1) * ncomp_send
!
            WS(ie_send) = WS(ie_send) + pol_e(jj,k_rlm,nd+3*nvector)
            WS(io_send) = WS(io_send) + pol_o(jj,k_rlm,nd+3*nvector)
          end do
      end do
!$omp end parallel do
!
      do jj = n_jk_o+1, n_jk_e
!$omp parallel do private(kr_nd,k_rlm,nd,ie_rlm,ie_send)
        do kr_nd = 1, nvector*nidx_rlm(1)
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
            ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                    &
     &                 + (k_rlm-1) *    istep_rlm(1)
!
            ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp_send
!
            WS(ie_send-2) = WS(ie_send-2) + pol_e(jj,k_rlm,3*nd-2)
            WS(ie_send-1) = WS(ie_send-1) - pol_e(jj,k_rlm,3*nd  )      &
     &                                    + tor_e(jj,k_rlm,2*nd  )
            WS(ie_send  ) = WS(ie_send  ) - pol_e(jj,k_rlm,3*nd-1)      &
     &                                    - tor_e(jj,k_rlm,2*nd-1)
        end do
!$omp end parallel do
!
!$omp parallel do private(kr_nd,k_rlm,nd,ie_rlm,ie_send)
        do kr_nd = 1, nscalar*nidx_rlm(1)
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
            ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                    &
     &               + (k_rlm-1) *    istep_rlm(1)
            ie_send = nd + 3*nvector                                    &
     &               + (irev_sr_rlm(ie_rlm) - 1) * ncomp_send
            WS(ie_send) = WS(ie_send) + pol_e(jj,k_rlm,nd+3*nvector)
        end do
!$omp end parallel do
      end do
!
      end subroutine cal_sp_rlm_sym_mat_rout
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_sym_mat_rin(nnod_rlm, nidx_rlm,             &
     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,  &
     &         jst, n_jk_o, n_jk_e, pol_e, pol_o, tor_e, tor_o,         &
     &         ncomp_send, nvector, nscalar, irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in)                                  &
     &            :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
      real(kind = kreal), intent(in) :: radius_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: nvector, nscalar
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(inout)                                 &
     &         :: pol_e(3*nvector+nscalar,nidx_rlm(1),n_jk_e)
      real(kind = kreal), intent(inout)                                 &
     &         :: pol_o(3*nvector+nscalar,nidx_rlm(1),n_jk_o)
      real(kind = kreal), intent(inout)                                 &
     &         :: tor_e(2*nvector,nidx_rlm(1),n_jk_e)
      real(kind = kreal), intent(inout)                                 &
     &         :: tor_o(2*nvector,nidx_rlm(1),n_jk_o)
!
      integer(kind = kint), intent(in) :: ncomp_send
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, k_rlm
      integer(kind = kint) :: ie_rlm, io_rlm, ie_send, io_send
      integer(kind = kint) :: nd, jj
      real(kind = kreal) :: g6e, g6o, g7e, g7o, gme, gmo, r1, r2
!
!
!$omp parallel do                                                       &
!$omp& private(jj,kr_nd,nd,k_rlm,g6e,g6o,g7e,g7o,gme,gmo,r1,r2)
      do jj = 1, n_jk_o
        g6e = g_sph_rlm(2*jj+jst-1,6)
        g6o = g_sph_rlm(2*jj+jst,  6)
        g7e = g_sph_rlm(2*jj+jst-1,7)
        g7o = g_sph_rlm(2*jj+jst,  7)
        gme = dble(idx_gl_1d_rlm_j(2*jj+jst-1,3))
        gmo = dble(idx_gl_1d_rlm_j(2*jj+jst,  3))
        do kr_nd = 1, nvector*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nvector)
          k_rlm = 1 + (kr_nd - nd) / nvector
!
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
!
          pol_e(3*nd-2,k_rlm,jj) = pol_e(3*nd-2,k_rlm,jj) * r2*g7e
          tor_e(2*nd,  k_rlm,jj) = tor_e(2*nd,  k_rlm,jj) * r1*g7e
          pol_e(3*nd,  k_rlm,jj) = pol_e(3*nd,  k_rlm,jj) * r1*g7e*gme
          tor_e(2*nd-1,k_rlm,jj) = tor_e(2*nd-1,k_rlm,jj) * r1*g7e
          pol_e(3*nd-1,k_rlm,jj) = pol_e(3*nd-1,k_rlm,jj) * r1*g7e*gme
!
          pol_o(3*nd-2,k_rlm,jj) = pol_o(3*nd-2,k_rlm,jj) * r2*g7o
          tor_o(2*nd,  k_rlm,jj) = tor_o(2*nd,  k_rlm,jj) * r1*g7o
          pol_o(3*nd,  k_rlm,jj) = pol_o(3*nd,  k_rlm,jj) * r1*g7o*gmo
          tor_o(2*nd-1,k_rlm,jj) = tor_o(2*nd-1,k_rlm,jj) * r1*g7o
          pol_o(3*nd-1,k_rlm,jj) = pol_o(3*nd-1,k_rlm,jj) * r1*g7o*gmo
        end do
!
        do kr_nd = 1, nscalar*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nscalar)
          k_rlm = 1 + (kr_nd - nd) / nscalar
!
          pol_e(nd+3*nvector,k_rlm,jj)                                  &
     &            = pol_e(nd+3*nvector,k_rlm,jj) * g6e
          pol_o(nd+3*nvector,k_rlm,jj)                                  &
     &            = pol_o(nd+3*nvector,k_rlm,jj) * g6o
        end do
      end do
!$omp end parallel do
!
!$omp parallel do                                                       &
!$omp& private(kr_nd,k_rlm,nd,jj,ie_rlm,io_rlm,ie_send,io_send)
      do jj = 1, n_jk_o
        do kr_nd = 1, nvector*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nvector)
          k_rlm = 1 + (kr_nd - nd) / nvector
!
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          io_rlm = 1 + (2*jj+jst-1) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
!
          ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp_send
          io_send = 3*nd + (irev_sr_rlm(io_rlm) - 1) * ncomp_send
!
!  even l-m
          WS(ie_send-2) = WS(ie_send-2) + pol_e(3*nd-2,k_rlm,jj)
          WS(ie_send-1) = WS(ie_send-1) - pol_e(3*nd,  k_rlm,jj)        &
     &                                  + tor_e(2*nd,  k_rlm,jj)
          WS(ie_send  ) = WS(ie_send  ) - pol_e(3*nd-1,k_rlm,jj)        &
     &                                  - tor_e(2*nd-1,k_rlm,jj)
!  odd l-m
          WS(io_send-2) = WS(io_send-2) + pol_o(3*nd-2,k_rlm,jj)
          WS(io_send-1) = WS(io_send-1) - pol_o(3*nd,  k_rlm,jj)        &
     &                                  + tor_o(2*nd,  k_rlm,jj)
          WS(io_send  ) = WS(io_send  ) - pol_o(3*nd-1,k_rlm,jj)        &
     &                                  - tor_o(2*nd-1,k_rlm,jj)
        end do
!
        do kr_nd = 1, nscalar*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nscalar)
          k_rlm = 1 + (kr_nd - nd) / nscalar
!
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
          io_rlm = 1 + (2*jj+jst-1) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
!
          ie_send = nd + 3*nvector                                      &
     &             + (irev_sr_rlm(ie_rlm) - 1) * ncomp_send
          io_send = nd + 3*nvector                                      &
     &             + (irev_sr_rlm(io_rlm) - 1) * ncomp_send
!
          WS(ie_send) = WS(ie_send) + pol_e(nd+3*nvector,k_rlm,jj)
          WS(io_send) = WS(io_send) + pol_o(nd+3*nvector,k_rlm,jj)
        end do
      end do
!$omp end parallel do
!
      do jj = n_jk_o+1, n_jk_e
        g6e = g_sph_rlm(2*jj+jst-1,6)
        g6o = g_sph_rlm(2*jj+jst,  6)
        g7e = g_sph_rlm(2*jj+jst-1,7)
        g7o = g_sph_rlm(2*jj+jst,  7)
        gme = dble(idx_gl_1d_rlm_j(2*jj+jst-1,3))
        gmo = dble(idx_gl_1d_rlm_j(2*jj+jst,  3))
!$omp parallel do private(kr_nd,nd,k_rlm,r1,r2)
        do kr_nd = 1, nvector*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nvector)
          k_rlm = 1 + (kr_nd - nd) / nvector
!
          r1 = radius_1d_rlm_r(k_rlm)
          r2 = r1 * r1
!
          pol_e(3*nd-2,k_rlm,jj) = pol_e(3*nd-2,k_rlm,jj) * r2*g7e
          tor_e(2*nd,  k_rlm,jj) = tor_e(2*nd,  k_rlm,jj) * r1*g7e
          pol_e(3*nd,  k_rlm,jj) = pol_e(3*nd,  k_rlm,jj) * r1*g7e*gme
          tor_e(2*nd-1,k_rlm,jj) = tor_e(2*nd-1,k_rlm,jj) * r1*g7e
          pol_e(3*nd-1,k_rlm,jj) = pol_e(3*nd-1,k_rlm,jj) * r1*g7e*gme
        end do
!$omp end parallel do
!
!$omp parallel do private(kr_nd,nd,k_rlm,r1,r2)
        do kr_nd = 1, nscalar*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nscalar)
          k_rlm = 1 + (kr_nd - nd) / nscalar
!
          pol_e(nd+3*nvector,k_rlm,jj)                                  &
     &            = pol_e(nd+3*nvector,k_rlm,jj) * g6e
        end do
!$omp end parallel do
!
!$omp parallel do private(kr_nd,k_rlm,nd,ie_rlm,ie_send)
        do kr_nd = 1, nvector*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nvector)
          k_rlm = 1 + (kr_nd - nd) / nvector
!
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
!
          ie_send = 3*nd + (irev_sr_rlm(ie_rlm) - 1) * ncomp_send
!
          WS(ie_send-2) = WS(ie_send-2) + pol_e(3*nd-2,k_rlm,jj)
          WS(ie_send-1) = WS(ie_send-1) - pol_e(3*nd,  k_rlm,jj)        &
     &                                  + tor_e(2*nd,  k_rlm,jj)
          WS(ie_send  ) = WS(ie_send  ) - pol_e(3*nd-1,k_rlm,jj)        &
     &                                  - tor_e(2*nd-1,k_rlm,jj)
        end do
!$omp end parallel do
!
!$omp parallel do private(kr_nd,k_rlm,nd,ie_rlm,ie_send)
        do kr_nd = 1, nscalar*nidx_rlm(1)
          nd = 1 + mod((kr_nd-1),nscalar)
          k_rlm = 1 + (kr_nd - nd) / nscalar
!
          ie_rlm = 1 + (2*jj+jst-2) * istep_rlm(2)                      &
     &               + (k_rlm-1) *    istep_rlm(1)
!
          ie_send = nd + 3*nvector                                      &
     &                 + (irev_sr_rlm(ie_rlm) - 1) * ncomp_send
          WS(ie_send) = WS(ie_send) + pol_e(nd+3*nvector,k_rlm,jj)
        end do
!$omp end parallel do
      end do
!
      end subroutine cal_sp_rlm_sym_mat_rin
!
! -----------------------------------------------------------------------
!
      end module cal_sp_rlm_sym_mat_tsmp
