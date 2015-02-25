!>@file   set_vr_rtm_for_leg_vecprod.f90
!!@brief  module set_vr_rtm_for_leg_vecprod
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform using dot products
!!
!!@verbatim
!!
!!      subroutine set_vr_rtm_vector_blocked                            &
!!     &         (nd, k_rlm, mp_rlm, mn_rlm, asin_rtm, lst, nle_rtm,    &
!!     &          ncomp, irev_sr_rtm, n_WR, WR,                         &
!!     &          symp_r, asmp_t, asmp_p, symn_t, symn_p)
!!      subroutine set_vr_rtm_scalar_blocked                            &
!!     &         (nd, k_rlm, mp_rlm, lst, nle_rtm,                      &
!!     &          ncomp, nvector, irev_sr_rtm, n_WR, WR, symp)
!!
!!      subroutine set_vr_rtm_vector_symmetry                           &
!!     &         (nd, k_rlm, mp_rlm, mn_rlm, asin_rtm, lst,             &
!!     &          nle_rtm, nlo_rtm, ncomp, irev_sr_rtm, n_WR, WR,       &
!!     &          symp_r, asmp_t, asmp_p, symn_t, symn_p, asmp_r,       &
!!     &          symp_t, symp_p, asmn_t, asmn_p)
!!      subroutine set_vr_rtm_scalar_symmetry                           &
!!     &         (nd, k_rlm, mp_rlm, lst, nle_rtm, nlo_rtm,             &
!!     &          ncomp, nvector, irev_sr_rtm, n_WR, WR, symp, asmp)
!!@endverbatim
!!
      module set_vr_rtm_for_leg_vecprod
!
      use m_precision
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_vector_blocked                              &
     &         (nd, k_rlm, mp_rlm, mn_rlm, asin_rtm, lst, nle_rtm,      &
     &          ncomp, irev_sr_rtm, n_WR, WR,                           &
     &          symp_r, asmp_t, asmp_p, symn_t, symn_p)
!
      integer(kind = kint), intent(in) :: nd, k_rlm
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: lst, nle_rtm
      real(kind = kreal), intent(in) :: asin_rtm(nle_rtm)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: symp_r(nle_rtm)
      real(kind = kreal), intent(inout) :: asmp_t(nle_rtm)
      real(kind = kreal), intent(inout) :: asmp_p(nle_rtm)
      real(kind = kreal), intent(inout) :: symn_t(nle_rtm)
      real(kind = kreal), intent(inout) :: symn_p(nle_rtm)
!
!
      integer(kind = kint) :: ll, lp_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm
      integer(kind = kint) :: ipp_recv, inp_recv
      real(kind = kreal) :: wp_rtm
!
!
      do ll = 1, nle_rtm
        lp_rtm = ll + lst
        wp_rtm =   weight_rtm(lp_rtm)
!
        ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mp_rlm-1) * istep_rtm(3)
        in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mn_rlm-1) * istep_rtm(3)
        ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
        inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
        symp_r(ll) = WR(ipp_recv-2) * wp_rtm
        asmp_t(ll) = WR(ipp_recv-1) * wp_rtm
        asmp_p(ll) = WR(ipp_recv  ) * wp_rtm
!
        symn_t(ll) = WR(inp_recv-1) * wp_rtm * asin_rtm(ll)
        symn_p(ll) = WR(inp_recv  ) * wp_rtm * asin_rtm(ll)
      end do
!
      end subroutine set_vr_rtm_vector_blocked
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_scalar_blocked                              &
     &         (nd, k_rlm, mp_rlm, lst, nle_rtm,                        &
     &          ncomp, nvector, irev_sr_rtm, n_WR, WR, symp)
!
      integer(kind = kint), intent(in) :: nd, k_rlm
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nle_rtm, lst
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: symp(nle_rtm)
!
!
      integer(kind = kint) :: ll, l_rtm, ip_rtpm
      integer(kind = kint) :: ipp_recv
!
!
      do ll = 1, nle_rtm
        l_rtm = ll + lst
        ip_rtpm = 1 + (l_rtm-1) *  istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mp_rlm-1) * istep_rtm(3)
        ipp_recv = nd + 3*nvector + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
        symp(ll) = WR(ipp_recv) * weight_rtm(l_rtm)
      end do
!
      end subroutine set_vr_rtm_scalar_blocked
!
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_vector_symmetry                             &
     &         (nd, k_rlm, mp_rlm, mn_rlm, asin_rtm, lst,               &
     &          nle_rtm, nlo_rtm, ncomp, irev_sr_rtm, n_WR, WR,         &
     &          symp_r, asmp_t, asmp_p, symn_t, symn_p, asmp_r,         &
     &          symp_t, symp_p, asmn_t, asmn_p)
!
      integer(kind = kint), intent(in) :: nd, k_rlm
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: lst, nle_rtm, nlo_rtm
      real(kind = kreal), intent(in) :: asin_rtm(nle_rtm)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: symp_r(nle_rtm)
      real(kind = kreal), intent(inout) :: asmp_t(nle_rtm)
      real(kind = kreal), intent(inout) :: asmp_p(nle_rtm)
      real(kind = kreal), intent(inout) :: symn_t(nle_rtm)
      real(kind = kreal), intent(inout) :: symn_p(nle_rtm)
      real(kind = kreal), intent(inout) :: asmp_r(nle_rtm)
      real(kind = kreal), intent(inout) :: symp_t(nle_rtm)
      real(kind = kreal), intent(inout) :: symp_p(nle_rtm)
      real(kind = kreal), intent(inout) :: asmn_t(nle_rtm)
      real(kind = kreal), intent(inout) :: asmn_p(nle_rtm)
!
!
      integer(kind = kint) :: ll, lp_rtm, ln_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      real(kind = kreal) :: wp_rtm
!
!
      do ll = 1, nlo_rtm
        lp_rtm = ll + lst
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm =   weight_rtm(lp_rtm)
!
        ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mp_rlm-1) * istep_rtm(3)
        ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mp_rlm-1) * istep_rtm(3)
        in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mn_rlm-1) * istep_rtm(3)
        in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mn_rlm-1) * istep_rtm(3)
        ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
        ipn_recv = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
        inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
        inn_recv = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
        symp_r(ll) = (WR(ipp_recv-2) + WR(ipn_recv-2)) * wp_rtm
        symp_t(ll) = (WR(ipp_recv-1) + WR(ipn_recv-1)) * wp_rtm
        symp_p(ll) = (WR(ipp_recv  ) + WR(ipn_recv  )) * wp_rtm
!
        asmp_r(ll) = (WR(ipp_recv-2) - WR(ipn_recv-2)) * wp_rtm
        asmp_t(ll) = (WR(ipp_recv-1) - WR(ipn_recv-1)) * wp_rtm
        asmp_p(ll) = (WR(ipp_recv  ) - WR(ipn_recv  )) * wp_rtm
!
        symn_t(ll) = (WR(inp_recv-1) + WR(inn_recv-1))                  &
     &                  * wp_rtm * asin_rtm(ll)
        symn_p(ll) = (WR(inp_recv  ) + WR(inn_recv  ))                  &
     &                  * wp_rtm * asin_rtm(ll)
!
        asmn_t(ll) = (WR(inp_recv-1) - WR(inn_recv-1))                  &
     &                  * wp_rtm * asin_rtm(ll)
        asmn_p(ll) = (WR(inp_recv  ) - WR(inn_recv  ))                  &
     &                  * wp_rtm * asin_rtm(ll)
      end do
!   Equator (if necessary)
      do ll = nlo_rtm+1, nle_rtm
        lp_rtm = ll + lst
        wp_rtm = weight_rtm(lp_rtm)
!
        ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                         &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
        in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                         &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
        ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
        inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
        symp_r(ll) = WR(ipp_recv-2) * wp_rtm
        symp_t(ll) = WR(ipp_recv-1) * wp_rtm
        symp_p(ll) = WR(ipp_recv  ) * wp_rtm
!
        asmp_r(ll) = 0.0d0
        asmp_t(ll) = 0.0d0
        asmp_p(ll) = 0.0d0
!
        symn_t(ll) = WR(inp_recv-1) * wp_rtm * asin_rtm(ll)
        symn_p(ll) = WR(inp_recv  ) * wp_rtm * asin_rtm(ll)
!
        asmn_t(ll) = 0.0d0
        asmn_p(ll) = 0.0d0
      end do
!
      end subroutine set_vr_rtm_vector_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_scalar_symmetry                             &
     &         (nd, k_rlm, mp_rlm, lst, nle_rtm, nlo_rtm,               &
     &          ncomp, nvector, irev_sr_rtm, n_WR, WR, symp, asmp)
!
      integer(kind = kint), intent(in) :: nd, k_rlm
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: lst, nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: symp(nle_rtm)
      real(kind = kreal), intent(inout) :: asmp(nle_rtm)
!
!
      integer(kind = kint) :: ll, l_rtm, ip_rtpm, ip_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv
!
!
      do ll = 1, nlo_rtm
        l_rtm = ll + lst
        ip_rtpm = 1 + (l_rtm-1) *  istep_rtm(2)                         &
     &              + (k_rlm-1) *  istep_rtm(1)                         &
     &              + (mp_rlm-1) * istep_rtm(3)
        ip_rtnm = 1 + (nidx_rtm(2) - l_rtm) *  istep_rtm(2)             &
     &              + (k_rlm-1) *              istep_rtm(1)             &
     &              + (mp_rlm-1) *             istep_rtm(3)
        ipp_recv = nd + 3*nvector + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
        ipn_recv = nd + 3*nvector + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
        symp(ll) = (WR(ipp_recv) + WR(ipn_recv)) * weight_rtm(l_rtm)
        asmp(ll) = (WR(ipp_recv) - WR(ipn_recv)) * weight_rtm(l_rtm)
      end do
!   Equator (if necessary)
      do ll = nlo_rtm+1, nle_rtm
        l_rtm = ll + lst
        ip_rtpm = 1 + (l_rtm-1) *  istep_rtm(2)               &
     &              + (k_rlm-1) *  istep_rtm(1)               &
     &              + (mp_rlm-1) * istep_rtm(3)
        ipp_recv = nd + 3*nvector                   &
     &                + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
        symp(ll) = WR(ipp_recv) * weight_rtm(l_rtm)
        asmp(ll) = 0.0d0
      end do
!
      end subroutine set_vr_rtm_scalar_symmetry
!
! -----------------------------------------------------------------------
!
      end module set_vr_rtm_for_leg_vecprod
