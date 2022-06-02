!> @file  const_sph_radial_grad.f90
!!      module const_sph_radial_grad
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate radial delivatives
!!
!!@verbatim
!!      subroutine const_radial_grad_scalar                             &
!!     &         (sph_rj, r_2nd, sph_bc, bcs_S, fdm2_center, g_sph_rj,  &
!!     &          is_fld, is_grad, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_S
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(phys_data), intent(inout) :: rj_fld
!!        Address for input:    is_fld
!!        Address for solution: is_grad
!!
!!      subroutine const_sph_gradient_no_bc(sph_rj, r_2nd, sph_bc,      &
!!     &          g_sph_rj, is_fld, is_grad, rj_fld)
!!        Address for input:    is_fld
!!        Address for solution: is_grad, it_grad, ids_grad
!!
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!@param is_fld      Spherical hermonics data address for input vector
!!@param is_grad     Spherical hermonics data address for gradient
!!@param rj_fld     Spectr data structure
!
      module const_sph_radial_grad
!
      use m_precision
!
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
      use t_boundary_sph_spectr
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_MHD_boundaries
!
      use cal_sph_exp_1st_diff
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_grad_scalar                               &
     &         (sph_rj, r_2nd, sph_bc, bcs_S, fdm2_center, g_sph_rj,    &
     &          is_fld, is_grad, rj_fld)
!
      use t_coef_fdm2_MHD_boundaries
      use select_exp_scalar_ICB
      use select_exp_scalar_CMB
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      integer(kind = kint), intent(in) :: is_fld, is_grad
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_nod_gradient_2(sph_bc%kr_in, sph_bc%kr_out,          &
     &    is_fld, is_grad, sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,       &
     &    g_sph_rj, r_2nd%fdm(1)%dmat,                                  &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call sel_ICB_radial_grad_scalar(sph_rj, sph_bc,                   &
     &    bcs_S%ICB_Sspec, fdm2_center, g_sph_rj, is_fld, is_grad,      &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call sel_CMB_radial_grad_scalar                                   &
     &   (sph_rj, sph_bc, bcs_S%CMB_Sspec, g_sph_rj, is_fld, is_grad,   &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
!
      call normalize_sph_average_grad(is_grad,                          &
     &    sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj,                    &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_radial_grad_scalar
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_gradient_no_bc(sph_rj, r_2nd, sph_bc,        &
     &          g_sph_rj, is_fld, is_grad, rj_fld)
!
      use cal_sph_exp_nod_none_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      integer(kind = kint), intent(in) :: is_fld, is_grad
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_nod_nobc_in_grad2                                    &
     &   (sph_rj%nidx_rj(2), g_sph_rj, sph_bc%kr_in,                    &
     &    sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB, is_fld, is_grad,       &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call cal_sph_nod_nobc_out_grad2                                   &
     &   (sph_rj%nidx_rj(2), g_sph_rj, sph_bc%kr_out,                   &
     &    sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB, is_fld, is_grad,       &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call cal_sph_nod_gradient_2(sph_bc%kr_in, sph_bc%kr_out,          &
     &    is_fld, is_grad, sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,       &
     &    g_sph_rj, r_2nd%fdm(1)%dmat,                                  &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call normalize_sph_average_grad(is_grad,                          &
     &    sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj,                    &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_sph_gradient_no_bc
!
! -----------------------------------------------------------------------
!
      end module const_sph_radial_grad
