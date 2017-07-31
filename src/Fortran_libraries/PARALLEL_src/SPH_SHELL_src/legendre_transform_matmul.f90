!>@file   legendre_transform_matmul.f90
!!@brief  module legendre_transform_matmul
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
!!      subroutine leg_backward_trans_matmul(ncomp, nvector, nscalar,   &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!      subroutine leg_backward_trans_dgemm(ncomp, nvector, nscalar,    &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!      subroutine leg_backward_trans_matprod(ncomp, nvector, nscalar,  &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
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
!!      subroutine leg_forward_trans_matmul(ncomp, nvector, nscalar,    &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!      subroutine leg_forward_trans_dgemm(ncomp, nvector, nscalar,     &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!!      subroutine leg_forward_trans_matprod(ncomp, nvector, nscalar,   &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS, WK_l_mtl)
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
      module legendre_transform_matmul
!
      use m_precision
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_schmidt_poly_on_rtm
      use t_legendre_work_matmul
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_matmul(ncomp, nvector, nscalar,     &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use legendre_bwd_trans_matmul
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
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
      call leg_b_trans_vector_matmul(ncomp, nvector,                    &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%P_rtm, leg%dPdt_rtm,       &
     &    n_WR, n_WS, WR, WS, WK_l_mtl)
      call leg_b_trans_scalar_matmul(ncomp, nvector, nscalar,           &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    leg%P_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
!
      end subroutine leg_backward_trans_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_matmul(ncomp, nvector, nscalar,      &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use legendre_fwd_trans_matmul
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
      call leg_f_trans_vector_matmul(ncomp, nvector,                    &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%weight_rtm,                &
     &    leg%P_rtm, leg%dPdt_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
      call leg_f_trans_scalar_matmul(ncomp, nvector, nscalar,           &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,               &
     &    leg%g_sph_rlm, leg%weight_rtm, leg%P_rtm,                     &
     &    n_WR, n_WS, WR, WS, WK_l_mtl)
!
      end subroutine leg_forward_trans_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_dgemm(ncomp, nvector, nscalar,      &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use legendre_bwd_trans_matmul
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
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
      call leg_b_trans_vector_dgemm(ncomp, nvector,                     &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%P_rtm, leg%dPdt_rtm,       &
     &    n_WR, n_WS, WR, WS, WK_l_mtl)
      call leg_b_trans_scalar_dgemm(ncomp, nvector, nscalar,            &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    leg%P_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
!
      end subroutine leg_backward_trans_dgemm
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_dgemm(ncomp, nvector, nscalar,       &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use legendre_fwd_trans_matmul
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
      call leg_f_trans_vector_dgemm(ncomp, nvector,                     &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%weight_rtm,                &
     &    leg%P_rtm, leg%dPdt_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
      call leg_f_trans_scalar_dgemm(ncomp, nvector, nscalar,            &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,               &
     &    leg%g_sph_rlm, leg%weight_rtm, leg%P_rtm,                     &
     &    n_WR, n_WS, WR, WS, WK_l_mtl)
!
      end subroutine leg_forward_trans_dgemm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_matprod(ncomp, nvector, nscalar,    &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use legendre_bwd_trans_matmul
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
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
      call leg_b_trans_vector_matprod(ncomp, nvector,                   &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%P_rtm, leg%dPdt_rtm,       &
     &    n_WR, n_WS, WR, WS, WK_l_mtl)
      call leg_b_trans_scalar_matprod(ncomp, nvector, nscalar,          &
     &    sph_rlm, sph_rtm, comm_rlm, comm_rtm, idx_trns,               &
     &    leg%P_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
!
      end subroutine leg_backward_trans_matprod
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_matprod(ncomp, nvector, nscalar,     &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS, WK_l_mtl)
!
      use legendre_fwd_trans_matmul
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
      call leg_f_trans_vector_matprod(ncomp, nvector,                   &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,               &
     &    leg%asin_t_rtm, leg%g_sph_rlm, leg%weight_rtm,                &
     &    leg%P_rtm, leg%dPdt_rtm, n_WR, n_WS, WR, WS, WK_l_mtl)
      call leg_f_trans_scalar_matprod(ncomp, nvector, nscalar,          &
     &    sph_rtm, sph_rlm, comm_rtm, comm_rlm, idx_trns,               &
     &    leg%g_sph_rlm, leg%weight_rtm, leg%P_rtm,                     &
     &    n_WR, n_WS, WR, WS, WK_l_mtl)
!
      end subroutine leg_forward_trans_matprod
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_matmul
