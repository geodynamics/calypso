!>@file   legendre_transform_spin.f90
!!@brief  module legendre_transform_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transforms
!!       (innermost loop is spherical harmonics)
!!
!!
!!@verbatim
!!    Backward transforms
!!      subroutine leg_backward_trans_spin(ncomp, nvector, nscalar,     &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forward_trans_spin(ncomp, nvector, nscalar,      &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!      subroutine leg_backward_trans_sym_spin(ncomp, nvector, nscalar, &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_sml)
!!      subroutine leg_forward_trans_sym_spin(ncomp, nvector, nscalar,  &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_sml)
!!
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!        type(leg_trns_matmul_work), intent(inout) :: WK_l_mtl
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_spin
!
      use m_precision
      use m_work_time
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
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
      subroutine leg_backward_trans_spin(ncomp, nvector, nscalar,       &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use legendre_bwd_trans_spin
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
        call legendre_b_trans_vector_spin(ncomp, nvector,               &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,             &
     &      leg%asin_t_rtm, leg%g_sph_rlm, leg%P_jl, leg%dPdt_jl,       &
     &      n_WR, n_WS, WR, WS, WK_l_mtl)
        call legendre_b_trans_scalar_spin(ncomp, nvector, nscalar,      &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns, leg%P_jl,   &
     &      n_WR, n_WS, WR, WS, WK_l_mtl)
!
      end subroutine leg_backward_trans_spin
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_spin(ncomp, nvector, nscalar,        &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use legendre_fwd_trans_spin
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
        call legendre_f_trans_vector_spin(ncomp, nvector,               &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,             &
     &      leg%asin_t_rtm, leg%g_sph_rlm, leg%weight_rtm,              &
     &      leg%P_rtm, leg%dPdt_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
        call legendre_f_trans_scalar_spin(ncomp, nvector, nscalar,      &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,             &
     &      leg%g_sph_rlm, leg%weight_rtm, leg%P_rtm,                   &
     &      n_WR, n_WS, WR, WS, WK_l_mtl)
!
      end subroutine leg_forward_trans_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_sym_spin(ncomp, nvector, nscalar,   &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_sml)
!
      use legendre_bwd_trans_sym_spin
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
      call leg_bwd_trans_vector_sym_spin(ncomp, nvector,                &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, n_WR, n_WS, WR, WS, WK_l_sml)
      call leg_bwd_trans_scalar_sym_spin(ncomp, nvector, nscalar,       &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    n_WR, n_WS, WR, WS, WK_l_sml)
!
      end subroutine leg_backward_trans_sym_spin
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_sym_spin(ncomp, nvector, nscalar,    &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_sml)
!
      use legendre_fwd_trans_sym_spin
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
      call leg_fwd_trans_vector_sym_spin(ncomp, nvector,                &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%weight_rtm,                &
     &    n_WR, n_WS, WR, WS, WK_l_sml)
      call leg_fwd_trans_scalar_sym_spin(ncomp, nvector, nscalar,       &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,               &
     &    leg%g_sph_rlm, leg%weight_rtm, n_WR, n_WS, WR, WS, WK_l_sml)
!
      end subroutine leg_forward_trans_sym_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_spin

