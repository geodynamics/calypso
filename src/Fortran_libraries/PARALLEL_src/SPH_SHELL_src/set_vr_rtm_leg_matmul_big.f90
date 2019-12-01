!>@file   set_vr_rtm_leg_matmul_big.f90
!!@brief  module set_vr_rtm_leg_matmul_big
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform using matrix multi
!!
!!@verbatim
!!      subroutine set_vr_rtm_vec_sym_matmul_big(nnod_rtm, nidx_rtm,    &
!!     &         istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,    &
!!     &         kst, nkr, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,            &
!!     &         ncomp, nvector, irev_sr_rtm, n_WR, WR,                 &
!!     &         symp_r, asmp_p, asmp_r, symp_p)
!!      subroutine set_vr_rtm_scl_sym_matmul_big                        &
!!     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, weight_rtm,  &
!!     &          kst, nkr, mp_rlm,  nle_rtm, nlo_rtm,                  &
!!     &          ncomp, nvector, nscalar, irev_sr_rtm,                 &
!!     &          n_WR, WR, symp, asmp)
!!
!!      subroutine cal_vr_rtm_vec_sym_matmul_big(nnod_rtm, nidx_rtm,    &
!!     &          istep_rtm, nidx_rlm, asin_theta_1d_rtm,               &
!!     &          kst, nkr, mp_rlm, mn_rlm, nl_rtm, symp_r, asmp_p,     &
!!     &          asmp_r, symp_p, ncomp, nvector, irev_sr_rtm, n_WS, WS)
!!      subroutine cal_vr_rtm_scl_sym_matmul_big                        &
!!     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, kst, nkr,    &
!!     &          mp_rlm, nl_rtm, symp, asmp, ncomp, nvector, nscalar,  &
!!     &          irev_sr_rtm, n_WS, WS)
!!@endverbatim
!!
      module set_vr_rtm_leg_matmul_big
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
      subroutine set_vr_rtm_vec_sym_matmul_big(nnod_rtm, nidx_rtm,      &
     &         istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,      &
     &         kst, nkr, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,              &
     &         ncomp, nvector, irev_sr_rtm, n_WR, WR,                   &
     &         symp_r, asmp_p, asmp_r, symp_p)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind=kreal), intent(inout) :: symp_r(ncomp*nkr,nle_rtm)
      real(kind=kreal), intent(inout) :: asmp_p(2*nkr*nvector,nle_rtm)
      real(kind=kreal), intent(inout) :: asmp_r(ncomp*nkr,nle_rtm)
      real(kind=kreal), intent(inout) :: symp_p(2*nkr*nvector,nle_rtm)
!
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      real(kind = kreal) :: wp_rtm, asin_rtm
!
!
      nkrv = nkr * nvector
      do lp_rtm = 1, nlo_rtm
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm =   weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do kk = 1, nkrv
          kr_nd = kk + nvector*kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          ipn_recv = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
          inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
          inn_recv = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
          symp_r(kk,lp_rtm) =      (WR(ipp_recv-2) + WR(ipn_recv-2))    &
     &                             * wp_rtm
          symp_p(kk+nkrv,lp_rtm) = (WR(ipp_recv-1) + WR(ipn_recv-1))    &
     &                             * wp_rtm
          symp_p(kk,lp_rtm) =      (WR(ipp_recv  ) + WR(ipn_recv  ))    &
     &                             * wp_rtm
!
          asmp_r(kk,lp_rtm) = (WR(ipp_recv-2) - WR(ipn_recv-2))*wp_rtm
          asmp_p(kk+nkrv,lp_rtm) = (WR(ipp_recv-1) - WR(ipn_recv-1))    &
     &                             * wp_rtm
          asmp_p(kk,lp_rtm) = (WR(ipp_recv  ) - WR(ipn_recv  ))*wp_rtm
!
          symp_r(kk+nkrv,lp_rtm) =   (WR(inp_recv-1) + WR(inn_recv-1))  &
     &                             * wp_rtm * asin_rtm
          symp_r(kk+2*nkrv,lp_rtm) = (WR(inp_recv  ) + WR(inn_recv  ))  &
     &                             * wp_rtm * asin_rtm
!
          asmp_r(kk+nkrv,lp_rtm) =   (WR(inp_recv-1) - WR(inn_recv-1))  &
     &                             * wp_rtm * asin_rtm
          asmp_r(kk+2*nkrv,lp_rtm) = (WR(inp_recv  ) - WR(inn_recv  ))  &
     &                             * wp_rtm * asin_rtm
        end do
      end do
!   Equator (if necessary)
      do lp_rtm = nlo_rtm+1, nle_rtm
        wp_rtm = weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do kk = 1, nkrv
          kr_nd = kk + nvector*kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
          symp_r(kk,lp_rtm) =      WR(ipp_recv-2) * wp_rtm
          symp_p(kk+nkrv,lp_rtm) = WR(ipp_recv-1) * wp_rtm
          symp_p(kk,lp_rtm) =      WR(ipp_recv  ) * wp_rtm
!
          asmp_r(kk,lp_rtm) =      0.0d0
          asmp_p(kk+nkrv,lp_rtm) = 0.0d0
          asmp_p(kk,lp_rtm) =      0.0d0
!
          symp_r(kk+nkrv,lp_rtm) =   WR(inp_recv-1) * wp_rtm * asin_rtm
          symp_r(kk+2*nkrv,lp_rtm) = WR(inp_recv  ) * wp_rtm * asin_rtm
!
          asmp_r(kk+nkrv,lp_rtm) =   0.0d0
          asmp_r(kk+2*nkrv,lp_rtm) = 0.0d0
        end do
      end do
!
      end subroutine set_vr_rtm_vec_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_scl_sym_matmul_big                          &
     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, weight_rtm,    &
     &          kst, nkr, mp_rlm,  nle_rtm, nlo_rtm,                    &
     &          ncomp, nvector, nscalar, irev_sr_rtm,                   &
     &          n_WR, WR, symp, asmp)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: symp(ncomp*nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: asmp(ncomp*nkr,nle_rtm)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_recv, ipn_recv
      real(kind = kreal) :: wp_rtm
!
!
      nkrv = nkr * nvector
      do lp_rtm = 1, nlo_rtm
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm = weight_rtm(lp_rtm)
        do kk = 1, nkr*nscalar
          kr_nd = kk + kst*nscalar
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_recv = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          ipn_recv = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
          symp(kk+3*nkrv,lp_rtm) = (WR(ipp_recv) + WR(ipn_recv))        &
     &                            * wp_rtm
          asmp(kk+3*nkrv,lp_rtm) = (WR(ipp_recv) - WR(ipn_recv))        &
     &                            * wp_rtm
        end do
      end do
!   Equator (if necessary)
      do lp_rtm = nlo_rtm+1, nle_rtm
        wp_rtm = weight_rtm(lp_rtm)
        do kk = 1, nkr*nscalar
          kr_nd = kk + kst*nscalar
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                   &
     &                + (k_rlm-1) *  istep_rtm(1)                   &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_recv = nd + 3*nvector                                 &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
          symp(kk+3*nkrv,lp_rtm) = WR(ipp_recv) * wp_rtm
          asmp(kk+3*nkrv,lp_rtm) = 0.0d0
        end do
      end do
!
      end subroutine set_vr_rtm_scl_sym_matmul_big
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_vec_sym_matmul_big(nnod_rtm, nidx_rtm,      &
     &          istep_rtm, nidx_rlm, asin_theta_1d_rtm,                 &
     &          kst, nkr, mp_rlm, mn_rlm, nl_rtm, symp_r, asmp_p,       &
     &          asmp_r, symp_p, ncomp, nvector, irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nl_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout) :: symp_r(nl_rtm,ncomp*nkr)
      real(kind = kreal), intent(in) ::    asmp_p(nl_rtm,2*nvector*nkr)
      real(kind = kreal), intent(inout) :: asmp_r(nl_rtm,ncomp*nkr)
      real(kind = kreal), intent(in) ::    symp_p(nl_rtm,2*nvector*nkr)
!
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
!
!
      nkrv = nkr * nvector
      do kk = 1, nkrv
        do lp_rtm = 1, nl_rtm
          symp_r(lp_rtm,kk+nkrv)                                        &
     &         = - symp_r(lp_rtm,kk+nkrv) *   asin_theta_1d_rtm(lp_rtm)
          symp_r(lp_rtm,kk+2*nkrv)                                      &
     &         = - symp_r(lp_rtm,kk+2*nkrv) * asin_theta_1d_rtm(lp_rtm)
          asmp_r(lp_rtm,kk+nkrv)                                        &
     &         = - asmp_r(lp_rtm,kk+nkrv) *   asin_theta_1d_rtm(lp_rtm)
          asmp_r(lp_rtm,kk+2*nkrv)                                      &
     &         = - asmp_r(lp_rtm,kk+2*nkrv) * asin_theta_1d_rtm(lp_rtm)
        end do
      end do
!
      do kk = 1, nkrv
        kr_nd = kk + kst*nvector
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do lp_rtm = 1, nidx_rtm(2)/2
          ln_rtm =  nidx_rtm(2) - lp_rtm + 1
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
          ipn_send = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
          inn_send = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
          WS(ipp_send-2) = WS(ipp_send-2)                               &
     &            + symp_r(lp_rtm,kk) +     asmp_r(lp_rtm,kk)
          WS(ipp_send-1) = WS(ipp_send-1)                               &
     &            + asmp_p(lp_rtm,kk+nkrv) + symp_p(lp_rtm,kk+nkrv)
          WS(ipp_send  ) = WS(ipp_send  )                               &
     &            - asmp_p(lp_rtm,kk) -     symp_p(lp_rtm,kk)
!
          WS(inp_send-1) = WS(inp_send-1)                               &
     &            + symp_r(lp_rtm,kk+nkrv) +   asmp_r(lp_rtm,kk+nkrv)
          WS(inp_send  ) = WS(inp_send  )                               &
     &            + symp_r(lp_rtm,kk+2*nkrv) + asmp_r(lp_rtm,kk+2*nkrv)
!
!
          WS(ipn_send-2) = WS(ipn_send-2)                               &
     &            + symp_r(lp_rtm,kk) -     asmp_r(lp_rtm,kk)
          WS(ipn_send-1) = WS(ipn_send-1)                               &
     &            - asmp_p(lp_rtm,kk+nkrv) + symp_p(lp_rtm,kk+nkrv)
          WS(ipn_send  ) = WS(ipn_send  )                               &
     &            + asmp_p(lp_rtm,kk) -     symp_p(lp_rtm,kk)
!
          WS(inn_send-1) = WS(inn_send-1)                               &
     &            + symp_r(lp_rtm,kk+nkrv) -   asmp_r(lp_rtm,kk+nkrv)
          WS(inn_send  ) = WS(inn_send  )                               &
     &            + symp_r(lp_rtm,kk+2*nkrv) - asmp_r(lp_rtm,kk+2*nkrv)
        end do
!
        do lp_rtm = nidx_rtm(2)/2+1, nl_rtm
          ln_rtm =  nidx_rtm(2) - nidx_rtm(2)/2-1 + 1
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
!
          ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
          WS(ipp_send-2) = WS(ipp_send-2) + symp_r(lp_rtm,kk)
          WS(ipp_send-1) = WS(ipp_send-1) + symp_p(lp_rtm,kk+nkrv)
          WS(ipp_send  ) = WS(ipp_send  ) - symp_p(lp_rtm,kk)
!
          WS(inp_send-1) = WS(inp_send-1) + symp_r(lp_rtm,kk+nkrv)
          WS(inp_send  ) = WS(inp_send  ) + symp_r(lp_rtm,kk+2*nkrv)
        end do
      end do
!
      end subroutine cal_vr_rtm_vec_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_scl_sym_matmul_big                          &
     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, kst, nkr,      &
     &          mp_rlm, nl_rtm, symp, asmp, ncomp, nvector, nscalar,    &
     &          irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nl_rtm
      real(kind = kreal), intent(in) :: symp(nl_rtm,ncomp*nkr)
      real(kind = kreal), intent(in) :: asmp(nl_rtm,ncomp*nkr)
!
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_send, ipn_send
!
!
      nkrv = nkr * nvector
      do kk = 1, nkr*nscalar
        kr_nd = kk + kst*nscalar
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do lp_rtm = 1, nidx_rtm(2)/2
          ln_rtm =  nidx_rtm(2) - lp_rtm + 1
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
!
          ipp_send = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          ipn_send = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
          WS(ipp_send) = WS(ipp_send)                                   &
     &                + symp(lp_rtm,kk+3*nkrv) + asmp(lp_rtm,kk+3*nkrv)
          WS(ipn_send) = WS(ipn_send)                                   &
     &                + symp(lp_rtm,kk+3*nkrv) - asmp(lp_rtm,kk+3*nkrv)
        end do
!
        do lp_rtm = nidx_rtm(2)/2+1, nl_rtm
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_send = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
          WS(ipp_send) = WS(ipp_send) + symp(lp_rtm,kk+3*nkrv)
        end do
      end do
!
      end subroutine cal_vr_rtm_scl_sym_matmul_big
!
! -----------------------------------------------------------------------
!
      end module set_vr_rtm_leg_matmul_big
