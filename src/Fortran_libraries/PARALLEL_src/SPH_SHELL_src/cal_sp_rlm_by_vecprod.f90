!>@file   cal_sp_rlm_by_vecprod.f90
!!@brief  module cal_sp_rlm_by_vecprod
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform useing vector products
!!
!!@verbatim
!!      subroutine cal_vector_sp_rlm_dotprod(nth, g7, gm,               &
!!     &          r1_1d_rlm_r, r2_1d_rlm_r, Pvw_le, dPvw_le,            &
!!     &          symp_r, asmp_t, asmp_p, symn_t, symn_p, sp_rlm)
!!      subroutine cal_scalar_sp_rlm_dotprod                            &
!!     &          (nth, g6, Pws_le, symp, sp_rlm)
!!@endverbatim
!!
!
      module cal_sp_rlm_by_vecprod
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
      subroutine cal_vector_sp_rlm_dotprod(nth, g7, gm,                 &
     &          r1_1d_rlm_r, r2_1d_rlm_r, Pvw_le, dPvw_le,              &
     &          symp_r, asmp_t, asmp_p, symn_t, symn_p, sp_rlm)
!
      integer(kind = kint), intent(in) :: nth
      real (kind=kreal), intent(in) :: Pvw_le(nth)
      real (kind=kreal), intent(in) :: dPvw_le(nth)
!
      real (kind=kreal), intent(in) :: symp_r(nth)
      real (kind=kreal), intent(in) :: asmp_t(nth)
      real (kind=kreal), intent(in) :: asmp_p(nth)
      real (kind=kreal), intent(in) :: symn_t(nth)
      real (kind=kreal), intent(in) :: symn_p(nth)
!
      real (kind=kreal), intent(in) :: g7, gm
      real (kind=kreal), intent(in) :: r1_1d_rlm_r, r2_1d_rlm_r
!
      real (kind=kreal), intent(inout) :: sp_rlm(3)
!
      real(kind = kreal) :: pol_e, dpoldt_e, dpoldp_e
      real(kind = kreal) :: dtordt_e, dtordp_e
!
!
      pol_e =    DOT_PRODUCT(symp_r, Pvw_le)
      dpoldt_e = DOT_PRODUCT(asmp_t, dPvw_le)
      dpoldp_e = DOT_PRODUCT(symn_p, Pvw_le)
      dtordp_e = DOT_PRODUCT(symn_t, Pvw_le)
      dtordt_e = DOT_PRODUCT(asmp_p, dPvw_le)
!
      sp_rlm(1) = sp_rlm(1) + pol_e * r2_1d_rlm_r * g7
      sp_rlm(2) = sp_rlm(2)                                             &
     &                 + (dpoldt_e - dpoldp_e*gm) * r1_1d_rlm_r*g7
      sp_rlm(3) = sp_rlm(3)                                             &
     &                 - (dtordp_e*gm + dtordt_e) * r1_1d_rlm_r*g7
!
      end subroutine cal_vector_sp_rlm_dotprod
!
! -----------------------------------------------------------------------
!
      subroutine cal_scalar_sp_rlm_dotprod                              &
     &          (nth, g6, Pws_le, symp, sp_rlm)
!
      integer(kind = kint), intent(in) :: nth
      real (kind=kreal), intent(in) :: Pws_le(nth)
      real (kind=kreal), intent(in) :: symp(nth)
      real (kind=kreal), intent(in) :: g6
!
      real (kind=kreal), intent(inout) :: sp_rlm
!
      real(kind = kreal) :: pol_e
!
!
      pol_e = DOT_PRODUCT(symp, Pws_le)
      sp_rlm = sp_rlm + pol_e * g6
!
      end subroutine cal_scalar_sp_rlm_dotprod
!
! -----------------------------------------------------------------------
!
      end module cal_sp_rlm_by_vecprod
