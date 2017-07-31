!>@file   legendre_transform_org.f90
!!@brief  module legendre_transform_org
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transforms
!!       (Original version)
!!
!!
!!@verbatim
!!      subroutine leg_backward_trans_org(ncomp, nvector, nscalar,      &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg,            &
!!     &          n_WR, n_WS, WR, WS, WK_spin)
!!      subroutine leg_backward_trans_blocked(ncomp, nvector, nscalar,  &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!      subroutine leg_backward_trans_sym_org(ncomp, nvector, nscalar,  &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_sml)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!!
!!    Forward transforms
!!      subroutine leg_forwawd_trans_org(ncomp, nvector, nscalar,       &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_spin)
!!      subroutine leg_forwawd_trans_blocked(ncomp, nvector, nscalar,   &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!      subroutine leg_forward_trans_sym_org(ncomp, nvector, nscalar,   &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_sml)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!!        type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!!        type(leg_trns_spin_work), intent(inout) :: WK_spin
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_org
!
      use m_precision
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_work_4_sph_trans_spin
      use t_legendre_work_matmul
      use t_legendre_work_sym_matmul
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_org(ncomp, nvector, nscalar,        &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_spin)
!
      use legendre_bwd_trans_org
      use spherical_SRs_N
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_spin_work), intent(inout) :: WK_spin
!
!
      call calypso_sph_from_recv_N(ncomp, sph_rlm%nnod_rlm,             &
     &    comm_rlm, n_WR, WR, WK_spin%sp_rlm_wk(1))
      call clear_bwd_legendre_work(ncomp, sph_rtm%nnod_rtm, WK_spin)
!
      call legendre_b_trans_vector_org                                  &
     &   (ncomp, nvector, sph_rlm, sph_rtm, idx_trns,                   &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%P_jl, leg%dPdt_jl,         &
     &    WK_spin%sp_rlm_wk(1), WK_spin%vr_rtm_wk(1))
      call legendre_b_trans_scalar_org                                  &
     &   (ncomp, nvector, nscalar, sph_rlm, sph_rtm, idx_trns,          &
     &    leg%P_jl, WK_spin%sp_rlm_wk(1), WK_spin%vr_rtm_wk(1))
!
      call calypso_sph_to_send_N(ncomp, sph_rtm%nnod_rtm,               &
     &    comm_rtm, n_WS, WK_spin%vr_rtm_wk(1), WS)
!
      end subroutine leg_backward_trans_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_forwawd_trans_org(ncomp, nvector, nscalar,         &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_spin)
!
      use legendre_fwd_trans_org
      use spherical_SRs_N
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_spin_work), intent(inout) :: WK_spin
!
!
      call calypso_sph_from_recv_N(ncomp, sph_rtm%nnod_rtm,             &
     &    comm_rtm, n_WR, WR, WK_spin%vr_rtm_wk(1))
      call clear_fwd_legendre_work(ncomp, sph_rlm%nnod_rlm, WK_spin)
!
      call legendre_f_trans_vector_org                                  &
     &   (ncomp, nvector, sph_rtm, sph_rlm, idx_trns,                   &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%weight_rtm,                &
     &    leg%P_rtm, leg%dPdt_rtm, WK_spin%vr_rtm_wk(1),                &
     &    WK_spin%sp_rlm_wk(1))
      call legendre_f_trans_scalar_org                                  &
     &   (ncomp, nvector, nscalar, sph_rtm, sph_rlm, idx_trns,          &
     &    leg%g_sph_rlm, leg%weight_rtm, leg%P_rtm,                     &
     &    WK_spin%vr_rtm_wk(1), WK_spin%sp_rlm_wk(1))
!
      call calypso_sph_to_send_N(ncomp, sph_rlm%nnod_rlm,               &
     &    comm_rlm, n_WS, WK_spin%sp_rlm_wk(1), WS)
!
      end subroutine leg_forwawd_trans_org
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_blocked(ncomp, nvector, nscalar,    &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use legendre_bwd_trans_blocked
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
!
      call leg_b_trans_vector_blocked(ncomp, nvector,                   &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%P_jl, leg%dPdt_jl,         &
     &    n_WR, n_WS, WR, WS, WK_l_mtl)
      call leg_b_trans_scalar_blocked(ncomp, nvector, nscalar,          &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns, leg%P_jl,     &
     &    n_WR, n_WS, WR, WS, WK_l_mtl)
!
      end subroutine leg_backward_trans_blocked
!
! -----------------------------------------------------------------------
!
      subroutine leg_forwawd_trans_blocked(ncomp, nvector, nscalar,     &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use legendre_fwd_trans_blocked
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!
!
      call leg_f_trans_vector_blocked(ncomp, nvector,                  &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,              &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%weight_rtm,               &
     &    leg%P_rtm, leg%dPdt_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
      call leg_f_trans_scalar_blocked(ncomp, nvector, nscalar,         &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,              &
     &    leg%g_sph_rlm, leg%weight_rtm, leg%P_rtm,                    &
     &    n_WR, n_WS, WR, WS, WK_l_mtl)
!
      end subroutine leg_forwawd_trans_blocked
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_sym_org(ncomp, nvector, nscalar,    &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_sml)
!
      use legendre_bwd_trans_symmetry
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      call leg_bwd_trans_vector_sym_org(ncomp, nvector,                 &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, n_WR, n_WS, WR, WS, WK_l_sml)
      call leg_bwd_trans_scalar_sym_org(ncomp, nvector, nscalar,        &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    n_WR, n_WS, WR, WS, WK_l_sml)
!
      end subroutine leg_backward_trans_sym_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_sym_org(ncomp, nvector, nscalar,     &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_sml)
!
      use legendre_fwd_trans_symmetry
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
!
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(leg_trns_sym_mul_work), intent(inout) :: WK_l_sml
!
!
      call leg_fwd_trans_vector_sym_org(ncomp, nvector,                 &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%weight_rtm,                &
     &    n_WR, n_WS, WR, WS, WK_l_sml)
      call leg_fwd_trans_scalar_sym_org(ncomp, nvector, nscalar,        &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,               &
     &    leg%g_sph_rlm, leg%weight_rtm, n_WR, n_WS, WR, WS, WK_l_sml)
!
      end subroutine leg_forward_trans_sym_org
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_org
