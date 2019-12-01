!>@file   legendre_trans_sym_matmul.f90
!!@brief  module legendre_trans_sym_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transforms
!!       (longest loop version)
!!
!!
!!@verbatim
!!      subroutine leg_backward_trans_sym_matmul                        &
!!     &         (iflag_matmul, ncomp, nvector, nscalar,                &
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
!!
!!    Forward transforms
!!      subroutine leg_forward_trans_sym_matmul                         &
!!     &         (iflag_matmul, ncomp, nvector, nscalar,                &
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
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_trans_sym_matmul
!
      use m_precision
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
      subroutine leg_backward_trans_sym_matmul                          &
     &         (iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_sml)
!
      use legendre_bwd_sym_matmul
!
      integer(kind = kint), intent(in) :: iflag_matmul
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
      call leg_b_trans_vec_sym_matmul(iflag_matmul, ncomp, nvector,     &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, n_WR, n_WS, WR, WS, WK_l_sml)
      call leg_b_trans_scl_sym_matmul                                   &
     &   (iflag_matmul, ncomp, nvector, nscalar,                        &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    n_WR, n_WS, WR, WS, WK_l_sml)
!
      end subroutine leg_backward_trans_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_sym_matmul                           &
     &         (iflag_matmul, ncomp, nvector, nscalar,                  &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_sml)
!
      use legendre_fwd_sym_matmul
!
      integer(kind = kint), intent(in) :: iflag_matmul
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
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
      call leg_f_trans_vec_sym_matmul(iflag_matmul, ncomp, nvector,     &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%weight_rtm,                &
     &    n_WR, n_WS, WR, WS, WK_l_sml)
      call leg_f_trans_scl_sym_matmul                                   &
     &   (iflag_matmul, ncomp, nvector, nscalar,                        &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,               &
     &    leg%g_sph_rlm, leg%weight_rtm, n_WR, n_WS, WR, WS, WK_l_sml)
!
      end subroutine leg_forward_trans_sym_matmul
!
! -----------------------------------------------------------------------
!
      end module legendre_trans_sym_matmul
