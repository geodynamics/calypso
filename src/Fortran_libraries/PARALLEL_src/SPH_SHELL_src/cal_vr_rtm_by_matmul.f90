!>@file   cal_vr_rtm_by_matmul.f90
!!@brief  module cal_vr_rtm_by_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Backward Legendre transform after mat multi
!!
!!@verbatim
!!      subroutine cal_vr_rtm_vector_matmul(nnod_rtm, nidx_rtm,         &
!!     &        istep_rtm, nidx_rlm, asin_theta_1d_rtm, kst, nkr,       &
!!     &        mp_rlm, mn_rlm, nvec_lk, symp_r, asmp_t, asmp_p,        &
!!     &        symn_t, symn_p, ncomp, irev_sr_rtm, n_WS, WS)
!!      subroutine cal_vr_rtm_scalar_matmul                             &
!!     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm,              &
!!     &          kst, nkr, mp_rlm, nscl_lk, symp, ncomp, nvector,      &
!!     &          irev_sr_rtm, n_WS, WS)
!!
!!      subroutine cal_vr_rtm_vector_sym_matmul(nnod_rtm, nidx_rtm,     &
!!     &        istep_rtm, nidx_rlm, asin_theta_1d_rtm,                 &
!!     &        kst, nkr, mp_rlm, mn_rlm, nl_rtm,                       &
!!     &        symp_r, asmp_t, asmp_p, symn_t, symn_p,                 &
!!     &        asmp_r, symp_t, symp_p, asmn_t, asmn_p,                 &
!!     &        ncomp, irev_sr_rtm, n_WS, WS)
!!      subroutine cal_vr_rtm_scalar_sym_matmul                         &
!!     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, kst, nkr,    &
!!     &          mp_rlm, nl_rtm, symp, asmp, ncomp, nvector,           &
!!     &          irev_sr_rtm, n_WS, WS)
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
!
      module cal_vr_rtm_by_matmul
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
      subroutine cal_vr_rtm_vector_matmul(nnod_rtm, nidx_rtm,           &
     &        istep_rtm, nidx_rlm, asin_theta_1d_rtm, kst, nkr,         &
     &        mp_rlm, mn_rlm, nvec_lk, symp_r, asmp_t, asmp_p,          &
     &        symn_t, symn_p, ncomp, irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nvec_lk
      real(kind = kreal), intent(in) ::    symp_r(nvec_lk)
      real(kind = kreal), intent(in) ::    asmp_t(nvec_lk)
      real(kind = kreal), intent(in) ::    asmp_p(nvec_lk)
      real(kind = kreal), intent(inout) :: symn_t(nvec_lk)
      real(kind = kreal), intent(inout) :: symn_p(nvec_lk)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd, l_rtm, i_lk
      integer(kind = kint) :: ip_rtm, in_rtm, ip_send, in_send
!
!
      do kk = 1, nkr
        do l_rtm = 1, nidx_rtm(2)
          i_lk = l_rtm + (kk-1) * nidx_rtm(2)
          symn_t(i_lk) = - symn_t(i_lk) * asin_theta_1d_rtm(l_rtm)
          symn_p(i_lk) = - symn_p(i_lk) * asin_theta_1d_rtm(l_rtm)
        end do
      end do
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do l_rtm = 1, nidx_rtm(2)
          i_lk = l_rtm + (kk-1) * nidx_rtm(2)
          ip_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                        &
     &               + (k_rlm-1) *  istep_rtm(1)                        &
     &               + (mp_rlm-1) * istep_rtm(3)
          in_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                        &
     &               + (k_rlm-1) *  istep_rtm(1)                        &
     &               + (mn_rlm-1) * istep_rtm(3)
!
          ip_send = 3*nd + (irev_sr_rtm(ip_rtm) - 1) * ncomp
          in_send = 3*nd + (irev_sr_rtm(in_rtm) - 1) * ncomp
!
          WS(ip_send-2) = WS(ip_send-2) + symp_r(i_lk)
          WS(ip_send-1) = WS(ip_send-1) + asmp_t(i_lk)
          WS(ip_send  ) = WS(ip_send  ) - asmp_p(i_lk)
!
          WS(in_send-1) = WS(in_send-1) + symn_t(i_lk)
          WS(in_send  ) = WS(in_send  ) + symn_p(i_lk)
        end do
      end do
!
      end subroutine cal_vr_rtm_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_scalar_matmul                               &
     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm,                &
     &          kst, nkr, mp_rlm, nscl_lk, symp, ncomp, nvector,        &
     &          irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nscl_lk
      real(kind = kreal), intent(in) :: symp(nscl_lk)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: l_rtm, i_lk, ip_rtm, ip_send
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do l_rtm = 1, nidx_rtm(2)
          i_lk = l_rtm + (kk-1) * nidx_rtm(2)
          ip_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                        &
     &               + (k_rlm-1) *  istep_rtm(1)                        &
     &               + (mp_rlm-1) * istep_rtm(3)
          ip_send = nd + 3*nvector + (irev_sr_rtm(ip_rtm) - 1) * ncomp
          WS(ip_send) = WS(ip_send) + symp(i_lk)
        end do
      end do
!
      end subroutine cal_vr_rtm_scalar_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_vector_sym_matmul(nnod_rtm, nidx_rtm,       &
     &        istep_rtm, nidx_rlm, asin_theta_1d_rtm,                   &
     &        kst, nkr, mp_rlm, mn_rlm, nl_rtm,                         &
     &        symp_r, asmp_t, asmp_p, symn_t, symn_p,                   &
     &        asmp_r, symp_t, symp_p, asmn_t, asmn_p,                   &
     &        ncomp, irev_sr_rtm, n_WS, WS)
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
      real(kind = kreal), intent(in) ::    symp_r(nl_rtm,nkr)
      real(kind = kreal), intent(in) ::    asmp_t(nl_rtm,nkr)
      real(kind = kreal), intent(in) ::    asmp_p(nl_rtm,nkr)
      real(kind = kreal), intent(inout) :: symn_t(nl_rtm,nkr)
      real(kind = kreal), intent(inout) :: symn_p(nl_rtm,nkr)
      real(kind = kreal), intent(in) ::    asmp_r(nl_rtm,nkr)
      real(kind = kreal), intent(in) ::    symp_t(nl_rtm,nkr)
      real(kind = kreal), intent(in) ::    symp_p(nl_rtm,nkr)
      real(kind = kreal), intent(inout) :: asmn_t(nl_rtm,nkr)
      real(kind = kreal), intent(inout) :: asmn_p(nl_rtm,nkr)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
!
!
      do kk = 1, nkr
        do lp_rtm = 1, nl_rtm
          symn_t(lp_rtm,kk)                                             &
     &          = - symn_t(lp_rtm,kk) * asin_theta_1d_rtm(lp_rtm)
          symn_p(lp_rtm,kk)                                             &
     &          = - symn_p(lp_rtm,kk) * asin_theta_1d_rtm(lp_rtm)
          asmn_t(lp_rtm,kk)                                             &
     &          = - asmn_t(lp_rtm,kk) * asin_theta_1d_rtm(lp_rtm)
          asmn_p(lp_rtm,kk)                                             &
     &          = - asmn_p(lp_rtm,kk) * asin_theta_1d_rtm(lp_rtm)
        end do
      end do
!
      do kk = 1, nkr
        kr_nd = kk + kst
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
     &                    + symp_r(lp_rtm,kk) + asmp_r(lp_rtm,kk)
          WS(ipp_send-1) = WS(ipp_send-1)                               &
     &                    + asmp_t(lp_rtm,kk) + symp_t(lp_rtm,kk)
          WS(ipp_send  ) = WS(ipp_send  )                               &
     &                    - asmp_p(lp_rtm,kk) - symp_p(lp_rtm,kk)
!
          WS(inp_send-1) = WS(inp_send-1)                               &
     &                    + symn_t(lp_rtm,kk) + asmn_t(lp_rtm,kk)
          WS(inp_send  ) = WS(inp_send  )                               &
     &                    + symn_p(lp_rtm,kk) + asmn_p(lp_rtm,kk)
!
!
          WS(ipn_send-2) = WS(ipn_send-2)                               &
     &                    + symp_r(lp_rtm,kk) - asmp_r(lp_rtm,kk)
          WS(ipn_send-1) = WS(ipn_send-1)                               &
     &                    - asmp_t(lp_rtm,kk) + symp_t(lp_rtm,kk)
          WS(ipn_send  ) = WS(ipn_send  )                               &
     &                    + asmp_p(lp_rtm,kk) - symp_p(lp_rtm,kk)
!
          WS(inn_send-1) = WS(inn_send-1)                               &
     &                    + symn_t(lp_rtm,kk) - asmn_t(lp_rtm,kk)
          WS(inn_send  ) = WS(inn_send  )                               &
     &                    + symn_p(lp_rtm,kk) - asmn_p(lp_rtm,kk)
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
          WS(ipp_send-1) = WS(ipp_send-1) + symp_t(lp_rtm,kk)
          WS(ipp_send  ) = WS(ipp_send  ) - symp_p(lp_rtm,kk)
!
          WS(inp_send-1) = WS(inp_send-1) + symn_t(lp_rtm,kk)
          WS(inp_send  ) = WS(inp_send  ) + symn_p(lp_rtm,kk)
        end do
      end do
!
      end subroutine cal_vr_rtm_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_scalar_sym_matmul                           &
     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, kst, nkr,      &
     &          mp_rlm, nl_rtm, symp, asmp, ncomp, nvector,             &
     &          irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nl_rtm
      real(kind = kreal), intent(in) :: symp(nl_rtm,nkr)
      real(kind = kreal), intent(in) :: asmp(nl_rtm,nkr)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_send, ipn_send
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
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
     &                  + symp(lp_rtm,kk) + asmp(lp_rtm,kk)
          WS(ipn_send) = WS(ipn_send)                                   &
     &                  + symp(lp_rtm,kk) - asmp(lp_rtm,kk)
        end do
!
        do lp_rtm = nidx_rtm(2)/2+1, nl_rtm
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_send = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
          WS(ipp_send) = WS(ipp_send) + symp(lp_rtm,kk)
        end do
      end do
!
      end subroutine cal_vr_rtm_scalar_sym_matmul
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
      end module cal_vr_rtm_by_matmul
