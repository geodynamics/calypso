!>@file   set_vr_rtm_for_leg_matmul.f90
!!@brief  module set_vr_rtm_for_leg_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform using matrix multi
!!
!!@verbatim
!!      subroutine set_vr_rtm_vector_matmul(nnod_rtm, nidx_rtm,         &
!!     &         istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,    &
!!     &         kst, nkr, mp_rlm, mn_rlm, ncomp, irev_sr_rtm, n_WR, WR,&
!!     &         nvec_kl, symp_r, asmp_t, asmp_p, symn_t, symn_p)
!!      subroutine set_vr_rtm_scalar_matmul                             &
!!     &        (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, weight_rtm,   &
!!     &         kst, nkr, mp_rlm, ncomp, nvector, irev_sr_rtm,         &
!!     &         n_WR, WR, nscl_lk, symp)
!!
!!      subroutine set_vr_rtm_vector_sym_matmul(nnod_rtm, nidx_rtm,     &
!!     &          istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,   &
!!     &          kst, nkr, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,           &
!!     &          ncomp, irev_sr_rtm, n_WR, WR,                         &
!!     &          symp_r, asmp_t, asmp_p, symn_t, symn_p,               &
!!     &          asmp_r, symp_t, symp_p, asmn_t, asmn_p)
!!      subroutine set_vr_rtm_scalar_sym_matmul                         &
!!     &        (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, weight_rtm,   &
!!     &         kst, nkr, mp_rlm, nle_rtm, nlo_rtm,                    &
!!     &         ncomp, nvector, irev_sr_rtm, n_WR, WR, symp, asmp)
!!
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
!!@endverbatim
!!
      module set_vr_rtm_for_leg_matmul
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
      subroutine set_vr_rtm_vector_matmul(nnod_rtm, nidx_rtm,           &
     &         istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,      &
     &         kst, nkr, mp_rlm, mn_rlm, ncomp, irev_sr_rtm, n_WR, WR,  &
     &         nvec_kl, symp_r, asmp_t, asmp_p, symn_t, symn_p)
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
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nvec_kl
      real(kind = kreal), intent(inout) :: symp_r(nvec_kl)
      real(kind = kreal), intent(inout) :: asmp_t(nvec_kl)
      real(kind = kreal), intent(inout) :: asmp_p(nvec_kl)
      real(kind = kreal), intent(inout) :: symn_t(nvec_kl)
      real(kind = kreal), intent(inout) :: symn_p(nvec_kl)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: l_rtm, i_kl, ip_rtm, in_rtm
      integer(kind = kint) :: ip_recv, in_recv
      real(kind = kreal) :: wp_rtm, asin_rtm
!
!
      do l_rtm = 1, nidx_rtm(2)
        wp_rtm =   weight_rtm(l_rtm)
        asin_rtm = asin_theta_1d_rtm(l_rtm)
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          ip_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                        &
     &               + (k_rlm-1) *  istep_rtm(1)                        &
     &               + (mp_rlm-1) * istep_rtm(3)
          in_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                        &
     &               + (k_rlm-1) *  istep_rtm(1)                        &
     &               + (mn_rlm-1) * istep_rtm(3)
          ip_recv = 3*nd + (irev_sr_rtm(ip_rtm) - 1) * ncomp
          in_recv = 3*nd + (irev_sr_rtm(in_rtm) - 1) * ncomp
          i_kl = kk + (l_rtm-1) * nkr
!
          symp_r(i_kl) =  WR(ip_recv-2) * wp_rtm
!
          asmp_t(i_kl) =  WR(ip_recv-1) * wp_rtm
          asmp_p(i_kl) =  WR(ip_recv  ) * wp_rtm
!
          symn_t(i_kl) =  WR(in_recv-1) * wp_rtm*asin_rtm
          symn_p(i_kl) =  WR(in_recv  ) * wp_rtm*asin_rtm
        end do
      end do
!
      end subroutine set_vr_rtm_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_scalar_matmul                               &
     &        (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, weight_rtm,     &
     &         kst, nkr, mp_rlm, ncomp, nvector, irev_sr_rtm,           &
     &         n_WR, WR, nscl_lk, symp)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nscl_lk
      real(kind = kreal), intent(inout) :: symp(nscl_lk)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: l_rtm, i_kl
      integer(kind = kint) :: ip_rtm, i_recv
      real(kind = kreal) :: wp_rtm
!
!
      do l_rtm = 1, nidx_rtm(2)
        wp_rtm =   weight_rtm(l_rtm)
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          ip_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                        &
     &               + (k_rlm-1) *  istep_rtm(1)                        &
     &               + (mp_rlm-1) * istep_rtm(3)
          i_recv = nd + 3*nvector + (irev_sr_rtm(ip_rtm) - 1) * ncomp
          i_kl = kk + (l_rtm-1) * nkr
!
          symp(i_kl) =  WR(i_recv) * wp_rtm
        end do
      end do
!
      end subroutine set_vr_rtm_scalar_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_vector_sym_matmul(nnod_rtm, nidx_rtm,       &
     &          istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,     &
     &          kst, nkr, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,             &
     &          ncomp, irev_sr_rtm, n_WR, WR,                           &
     &          symp_r, asmp_t, asmp_p, symn_t, symn_p,                 &
     &          asmp_r, symp_t, symp_p, asmn_t, asmn_p)
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
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: symp_r(nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: asmp_t(nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: asmp_p(nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: symn_t(nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: symn_p(nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: asmp_r(nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: symp_t(nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: symp_p(nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: asmn_t(nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: asmn_p(nkr,nle_rtm)
!
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      real(kind = kreal) :: wp_rtm, asin_rtm
!
!
      do lp_rtm = 1, nlo_rtm
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm =   weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do kk = 1, nkr
          kr_nd = kk + kst
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
          symp_r(kk,lp_rtm) = (WR(ipp_recv-2) + WR(ipn_recv-2))*wp_rtm
          symp_t(kk,lp_rtm) = (WR(ipp_recv-1) + WR(ipn_recv-1))*wp_rtm
          symp_p(kk,lp_rtm) = (WR(ipp_recv  ) + WR(ipn_recv  ))*wp_rtm
!
          asmp_r(kk,lp_rtm) = (WR(ipp_recv-2) - WR(ipn_recv-2))*wp_rtm
          asmp_t(kk,lp_rtm) = (WR(ipp_recv-1) - WR(ipn_recv-1))*wp_rtm
          asmp_p(kk,lp_rtm) = (WR(ipp_recv  ) - WR(ipn_recv  ))*wp_rtm
!
          symn_t(kk,lp_rtm) = (WR(inp_recv-1) + WR(inn_recv-1))         &
     &                  * wp_rtm * asin_rtm
          symn_p(kk,lp_rtm) = (WR(inp_recv  ) + WR(inn_recv  ))         &
     &                  * wp_rtm * asin_rtm
!
          asmn_t(kk,lp_rtm) = (WR(inp_recv-1) - WR(inn_recv-1))         &
     &                  * wp_rtm * asin_rtm
          asmn_p(kk,lp_rtm) = (WR(inp_recv  ) - WR(inn_recv  ))         &
     &                  * wp_rtm * asin_rtm
        end do
      end do
!   Equator (if necessary)
      do lp_rtm = nlo_rtm+1, nle_rtm
        wp_rtm = weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do kk = 1, nkr
          kr_nd = kk + kst
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
          symp_r(kk,lp_rtm) = WR(ipp_recv-2) * wp_rtm
          symp_t(kk,lp_rtm) = WR(ipp_recv-1) * wp_rtm
          symp_p(kk,lp_rtm) = WR(ipp_recv  ) * wp_rtm
!
          asmp_r(kk,lp_rtm) = 0.0d0
          asmp_t(kk,lp_rtm) = 0.0d0
          asmp_p(kk,lp_rtm) = 0.0d0
!
          symn_t(kk,lp_rtm) = WR(inp_recv-1) * wp_rtm * asin_rtm
          symn_p(kk,lp_rtm) = WR(inp_recv  ) * wp_rtm * asin_rtm
!
          asmn_t(kk,lp_rtm) = 0.0d0
          asmn_p(kk,lp_rtm) = 0.0d0
        end do
      end do
!
      end subroutine set_vr_rtm_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_scalar_sym_matmul                           &
     &        (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, weight_rtm,     &
     &         kst, nkr, mp_rlm, nle_rtm, nlo_rtm,                      &
     &         ncomp, nvector, irev_sr_rtm, n_WR, WR, symp, asmp)
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
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: symp(nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: asmp(nkr,nle_rtm)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_recv, ipn_recv
      real(kind = kreal) :: wp_rtm
!
!
      do lp_rtm = 1, nlo_rtm
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm = weight_rtm(lp_rtm)
        do kk = 1, nkr
          kr_nd = kk + kst
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
          symp(kk,lp_rtm) = (WR(ipp_recv) + WR(ipn_recv)) * wp_rtm
          asmp(kk,lp_rtm) = (WR(ipp_recv) - WR(ipn_recv)) * wp_rtm
        end do
      end do
!   Equator (if necessary)
      do lp_rtm = nlo_rtm+1, nle_rtm
        wp_rtm = weight_rtm(lp_rtm)
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                   &
     &                + (k_rlm-1) *  istep_rtm(1)                   &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_recv = nd + 3*nvector                                 &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
          symp(kk,lp_rtm) = WR(ipp_recv) * wp_rtm
          asmp(kk,lp_rtm) = 0.0d0
        end do
      end do
!
      end subroutine set_vr_rtm_scalar_sym_matmul
!
! -----------------------------------------------------------------------
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
!
      end module set_vr_rtm_for_leg_matmul
