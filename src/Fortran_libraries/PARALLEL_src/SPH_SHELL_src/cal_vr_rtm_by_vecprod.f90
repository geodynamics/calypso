!>@file   cal_vr_rtm_by_vecprod.f90
!!@brief  module cal_vr_rtm_by_vecprod
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2014
!
!>@brief  Backward Legendre transform after vector product
!!
!!@verbatim
!!      subroutine cal_vr_rtm_dydtheta_vector                           &
!!     &      (nj_rlm, Pg3_je, dPdt_je, pol_e, dpoldt_e, dtordt_e, vr_pp)
!!      subroutine cal_vr_rtm_dydphi_vector                             &
!!     &         (nj_rlm, Pg3_je, asin_t, dpoldp_e, dtordp_e, vr_np)
!!      subroutine cal_vr_rtm_scalar_blocked(nj_rlm, P_je, scl_e, vr_p)
!!
!!      subroutine cal_vr_rtm_dydtheta_symmetry                         &
!!     &       (nj_rlm, Pg3_je, dPdt_je, Pg3_jo, dPdt_jo,               &
!!     &        pol_e, dpoldt_e, dtordt_e, pol_o, dpoldt_o, dtordt_o,   &
!!     &        vr_pp, vr_pn)
!!      subroutine cal_vr_rtm_dydphi_symmetry                           &
!!     &       (nj_rlm, Pg3_je, Pg3_jo, asin_t,                         &
!!     &        dpoldp_e, dtordp_e, dpoldp_o, dtordp_o, vr_np, vr_nn)
!!      subroutine cal_vr_rtm_scalar_symmetry(nj_rlm, P_je, P_jo,       &
!!     &          scl_e, scl_o, vr_p, vr_n)
!!
!!      subroutine cal_vr_rtm_dydtheta_equator(nj_rlm, Pg3_je, dPdt_jo, &
!!     &          pol_e, dpoldt_o, dtordt_o, vr_pp)
!!      subroutine cal_vr_rtm_dydphi_equator                            &
!!     &       (nj_rlm, Pg3_je, dpoldp_e, dtordp_e, vr_np)
!!      subroutine cal_vr_rtm_scalar_equator(nj_rlm, P_je, scl_e, vr_p)
!!@endverbatim
!!
!
      module cal_vr_rtm_by_vecprod
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
      subroutine cal_vr_rtm_dydtheta_vector                             &
     &      (nj_rlm, Pg3_je, dPdt_je, pol_e, dpoldt_e, dtordt_e, vr_pp)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(in) :: Pg3_je(nj_rlm)
      real(kind = kreal), intent(in) :: dPdt_je(nj_rlm)
!
      real(kind = kreal), intent(in) :: pol_e(nj_rlm)
      real(kind = kreal), intent(in) :: dpoldt_e(nj_rlm)
      real(kind = kreal), intent(in) :: dtordt_e(nj_rlm)
!
      real(kind = kreal), intent(inout) :: vr_pp(3)
!
      real(kind = kreal) :: symp_r, asmp_t, asmp_p
!
!
      symp_r = DOT_PRODUCT(pol_e, Pg3_je)
      asmp_t = DOT_PRODUCT(dpoldt_e, dPdt_je)
      asmp_p = DOT_PRODUCT(dtordt_e, dPdt_je)
!
      vr_pp(1) = vr_pp(1) + symp_r
      vr_pp(2) = vr_pp(2) + asmp_t
      vr_pp(3) = vr_pp(3) - asmp_p
!
      end subroutine cal_vr_rtm_dydtheta_vector
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_dydphi_vector                               &
     &         (nj_rlm, Pg3_je, asin_t, dpoldp_e, dtordp_e, vr_np)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(in) :: Pg3_je(nj_rlm)
!
      real(kind = kreal), intent(in) :: dpoldp_e(nj_rlm)
      real(kind = kreal), intent(in) :: dtordp_e(nj_rlm)
      real(kind = kreal), intent(in) :: asin_t
!
      real(kind = kreal), intent(inout) :: vr_np(3)
!
      real(kind = kreal) :: symn_t, symn_p
!
!
      symn_t = DOT_PRODUCT(dtordp_e, Pg3_je)
      symn_p = DOT_PRODUCT(dpoldp_e, Pg3_je)
!
      vr_np(2) = vr_np(2) - symn_t * asin_t
      vr_np(3) = vr_np(3) - symn_p * asin_t
!
      end subroutine cal_vr_rtm_dydphi_vector
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_scalar_blocked(nj_rlm, P_je, scl_e, vr_p)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(in) :: P_je(nj_rlm)
      real(kind = kreal), intent(in) :: scl_e(nj_rlm)
!
      real(kind = kreal), intent(inout) :: vr_p
!
      real(kind = kreal) :: symp_r
!
!
      symp_r = DOT_PRODUCT(scl_e, P_je)
      vr_p = vr_p + symp_r
!
      end subroutine cal_vr_rtm_scalar_blocked
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_dydtheta_symmetry                           &
     &       (nj_rlm, Pg3_je, dPdt_je, Pg3_jo, dPdt_jo,                 &
     &        pol_e, dpoldt_e, dtordt_e, pol_o, dpoldt_o, dtordt_o,     &
     &        vr_pp, vr_pn)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(in) :: Pg3_je((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dPdt_je((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: Pg3_jo(nj_rlm/2)
      real(kind = kreal), intent(in) :: dPdt_jo(nj_rlm/2)
!
      real(kind = kreal), intent(in) :: pol_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dpoldt_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dtordt_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: pol_o(nj_rlm/2)
      real(kind = kreal), intent(in) :: dpoldt_o(nj_rlm/2)
      real(kind = kreal), intent(in) :: dtordt_o(nj_rlm/2)
!
      real(kind = kreal), intent(inout) :: vr_pp(3), vr_pn(3)
!
      real(kind = kreal) :: symp_r, asmp_t, asmp_p
      real(kind = kreal) :: asmp_r, symp_t, symp_p
!
!
!   even l-m
      symp_r = DOT_PRODUCT(pol_e, Pg3_je)
      asmp_t = DOT_PRODUCT(dpoldt_e, dPdt_je)
      asmp_p = DOT_PRODUCT(dtordt_e, dPdt_je)
!
!   odd l-m
      asmp_r = DOT_PRODUCT(pol_o, Pg3_jo)
      symp_t = DOT_PRODUCT(dpoldt_o, dPdt_jo)
      symp_p = DOT_PRODUCT(dtordt_o, dPdt_jo)
!
      vr_pp(1) = vr_pp(1) + symp_r + asmp_r
      vr_pp(2) = vr_pp(2) + asmp_t + symp_t
      vr_pp(3) = vr_pp(3) - asmp_p - symp_p
!
      vr_pn(1) = vr_pn(1) + symp_r - asmp_r
      vr_pn(2) = vr_pn(2) - asmp_t + symp_t
      vr_pn(3) = vr_pn(3) + asmp_p - symp_p
!
      end subroutine cal_vr_rtm_dydtheta_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_dydphi_symmetry                             &
     &       (nj_rlm, Pg3_je, Pg3_jo, asin_t,                           &
     &        dpoldp_e, dtordp_e, dpoldp_o, dtordp_o, vr_np, vr_nn)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(in) :: Pg3_je((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: Pg3_jo(nj_rlm/2)
!
      real(kind = kreal), intent(in) :: dpoldp_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dtordp_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dpoldp_o(nj_rlm/2)
      real(kind = kreal), intent(in) :: dtordp_o(nj_rlm/2)
      real(kind = kreal), intent(in) :: asin_t
!
      real(kind = kreal), intent(inout) :: vr_np(3), vr_nn(3)
!
      real(kind = kreal) :: symn_t, symn_p, asmn_t, asmn_p
!
!
!   even l-m
      symn_t = DOT_PRODUCT(dtordp_e, Pg3_je)
      symn_p = DOT_PRODUCT(dpoldp_e, Pg3_je)
      symn_t = symn_t * asin_t
      symn_p = symn_p * asin_t
!
!   odd l-m
      asmn_t = DOT_PRODUCT(dtordp_o, Pg3_jo)
      asmn_p = DOT_PRODUCT(dpoldp_o, Pg3_jo)
      asmn_t = asmn_t * asin_t
      asmn_p = asmn_p * asin_t
!
      vr_np(2) = vr_np(2) - symn_t - asmn_t
      vr_np(3) = vr_np(3) - symn_p - asmn_p
!
      vr_nn(2) = vr_nn(2) - symn_t + asmn_t
      vr_nn(3) = vr_nn(3) - symn_p + asmn_p
!
      end subroutine cal_vr_rtm_dydphi_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_scalar_symmetry(nj_rlm, P_je, P_jo,         &
     &          scl_e, scl_o, vr_p, vr_n)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(in) :: P_je((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: P_jo(nj_rlm/2)
!
      real(kind = kreal), intent(in) :: scl_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: scl_o(nj_rlm/2)
!
      real(kind = kreal), intent(inout) :: vr_p, vr_n
!
      real(kind = kreal) :: symp_r, asmp_r
!
!
!   even l-m
      symp_r = DOT_PRODUCT(scl_e, P_je)
!   odd l-m
      asmp_r = DOT_PRODUCT(scl_o, P_jo)
      vr_n = vr_n + symp_r - asmp_r
      vr_p = vr_p + symp_r + asmp_r
!
      end subroutine cal_vr_rtm_scalar_symmetry
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_dydtheta_equator(nj_rlm, Pg3_je, dPdt_jo,   &
     &          pol_e, dpoldt_o, dtordt_o, vr_pp)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(in) :: Pg3_je((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dPdt_jo(nj_rlm/2)
!
      real(kind = kreal), intent(in) :: pol_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dpoldt_o(nj_rlm/2)
      real(kind = kreal), intent(in) :: dtordt_o(nj_rlm/2)
!
      real(kind = kreal), intent(inout) :: vr_pp(3)
!
      real(kind = kreal) :: symp_r, symp_t, symp_p
!
!
!   even l-m
      symp_r = DOT_PRODUCT(pol_e, Pg3_je)
!
!   odd l-m
      symp_t = DOT_PRODUCT(dpoldt_o, dPdt_jo)
      symp_p = DOT_PRODUCT(dtordt_o, dPdt_jo)
!
      vr_pp(1) = vr_pp(1) + symp_r
      vr_pp(2) = vr_pp(2) + symp_t
      vr_pp(3) = vr_pp(3) - symp_p
!
      end subroutine cal_vr_rtm_dydtheta_equator
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_dydphi_equator                              &
     &       (nj_rlm, Pg3_je, dpoldp_e, dtordp_e, vr_np)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(in) :: Pg3_je((nj_rlm+1)/2)
!
      real(kind = kreal), intent(in) :: dpoldp_e((nj_rlm+1)/2)
      real(kind = kreal), intent(in) :: dtordp_e((nj_rlm+1)/2)
!
      real(kind = kreal), intent(inout) :: vr_np(3)
!
      real(kind = kreal) :: symn_t, symn_p
!
!
!   even l-m
      symn_t = DOT_PRODUCT(dtordp_e, Pg3_je)
      symn_p = DOT_PRODUCT(dpoldp_e, Pg3_je)
!
      vr_np(2) = vr_np(2) - symn_t
      vr_np(3) = vr_np(3) - symn_p
!
      end subroutine cal_vr_rtm_dydphi_equator
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_scalar_equator(nj_rlm, P_je, scl_e, vr_p)
!
      integer(kind = kint), intent(in) :: nj_rlm
      real(kind = kreal), intent(in) :: P_je((nj_rlm+1)/2)
!
      real(kind = kreal), intent(in) :: scl_e((nj_rlm+1)/2)
!
      real(kind = kreal), intent(inout) :: vr_p
!
      real(kind = kreal) :: symp_r
!
!
!   even l-m
      symp_r = DOT_PRODUCT(scl_e, P_je)
      vr_p = vr_p + symp_r
!
      end subroutine cal_vr_rtm_scalar_equator
!
! -----------------------------------------------------------------------
!
      end module cal_vr_rtm_by_vecprod
