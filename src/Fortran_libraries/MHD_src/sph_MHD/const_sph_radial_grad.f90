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
!!      subroutine const_grad_vp_and_vorticity(sph_rj, r_2nd,           &
!!     &          sph_bc_U, bcs_U, fdm2_free_ICB, fdm2_free_CMB,        &
!!     &          g_sph_rj, is_velo, is_vort, rj_fld)
!!        Address for input:    is_velo, is_velo+2
!!        Address for solution: is_velo+1, 
!!                              is_vort, is_vort+2, is_vort+1
!!
!!      subroutine const_grad_bp_and_current                            &
!!     &         (sph_rj, r_2nd, sph_bc_B, bcs_B, g_sph_rj,             &
!!     &          is_magne, is_current, rj_fld)
!!        Address for input:    is_magne, is_magne+2
!!        Address for solution: is_magne+1,
!!                              is_current, is_current+2, is_current+1
!!
!!      subroutine const_grad_poloidal_moment                           &
!!     &         (sph_rj, r_2nd, sph_bc_U, bcs_U,                       &
!!     &          fdm2_free_ICB, fdm2_free_CMB, is_fld, rj_fld)
!!        Address for input:    is_fld, is_fld+2
!!        Address for solution: is_fld+1
!!
!!      subroutine const_grad_poloidal_magne(sph_rj, r_2nd,             &
!!     &          sph_bc_B, bcs_B, g_sph_rj, is_magne, rj_fld)
!!        Address for input:    is_magne, is_magne+2
!!        Address for solution: is_magne+1
!!
!!      subroutine const_pressure_gradient(sph_rj, r_2nd, sph_bc_U,     &
!!     &          g_sph_rj, coef_press, is_press, is_grad, rj_fld)
!!        Address for input:    is_press
!!        Address for solution: is_grad
!!
!!      subroutine const_sph_gradient_no_bc(sph_rj, r_2nd, sph_bc,      &
!!     &          g_sph_rj, is_fld, is_grad, rj_fld)
!!        Address for input:    is_fld
!!        Address for solution: is_grad, it_grad, ids_grad
!!
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_vector_boundary_data), intent(inout) :: bcs_U
!!        type(sph_vector_boundary_data), intent(inout) :: bcs_B
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!@param sph_bc_B  Structure for basic boundary condition parameters
!!                 for magnetic field
!!@param sph_bc_U  Structure for basic boundary condition parameters
!!                 for velocity
!!
!!@param is_fld      Spherical hermonics data address for input vector
!!@param is_grad     Spherical hermonics data address for gradient
!!@param is_velo     Spherical hermonics data address
!!                   for poloidal velocity
!!@param is_vort     Spherical hermonics data address
!!                   for poloidal vorticity
!!@param is_magne    Spherical hermonics data address
!!                   for poloidal magnetic field
!!@param is_current  Spherical hermonics data address
!!                   for poloidal current density
!!@param is_press    Spherical hermonics data address
!!                   for pressure
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
      call sel_ICB_radial_grad_scalar                                   &
     &   (sph_rj, sph_bc, bcs_S%ICB_Sspec, fdm2_center, g_sph_rj,       &
     &    is_fld, is_grad, rj_fld)
      call sel_CMB_radial_grad_scalar                                   &
     &   (sph_rj, sph_bc, bcs_S%CMB_Sspec, g_sph_rj,                    &
     &    is_fld, is_grad, rj_fld)
!
!
      call normalize_sph_average_grad(is_grad,                          &
     &    sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj,                    &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_radial_grad_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_grad_vp_and_vorticity(sph_rj, r_2nd,             &
     &          sph_bc_U, bcs_U, fdm2_free_ICB, fdm2_free_CMB,          &
     &          g_sph_rj, is_velo, is_vort, rj_fld)
!
      use cal_sph_exp_rotation
      use select_exp_velocity_ICB
      use select_exp_velocity_CMB
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_boundary_data), intent(in) :: bcs_U
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB, fdm2_free_CMB
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      integer(kind = kint), intent(in) :: is_velo, is_vort
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_ICB_grad_vp_and_vorticity                                &
     &   (sph_rj, r_2nd, sph_bc_U, bcs_U%ICB_Vspec, fdm2_free_ICB,      &
     &    g_sph_rj, is_velo, is_vort, rj_fld)
      call sel_CMB_grad_vp_and_vorticity                                &
     &   (sph_rj, sph_bc_U, bcs_U%CMB_Vspec, fdm2_free_CMB,             &
     &    g_sph_rj, is_velo, is_vort, rj_fld)
!
      call cal_sph_diff_pol_and_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                    &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, is_velo, is_vort,       &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_grad_vp_and_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_bp_and_current                              &
     &         (sph_rj, r_2nd, sph_bc_B, bcs_B, g_sph_rj,               &
     &          is_magne, is_current, rj_fld)
!
      use extend_potential_field
      use cal_sph_exp_rotation
      use select_exp_magne_ICB
      use select_exp_magne_CMB
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_vector_boundary_data), intent(in) :: bcs_B
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      integer(kind = kint), intent(in) :: is_magne, is_current
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_ICB_grad_bp_and_current                                  &
     &   (sph_rj, r_2nd, sph_bc_B, bcs_B%ICB_Vspec, g_sph_rj,           &
     &    is_magne, is_current, rj_fld)
      call sel_CMB_grad_bp_and_current                                  &
     &   (sph_rj, sph_bc_B, bcs_B%CMB_Vspec, g_sph_rj,                  &
     &    is_magne, is_current, rj_fld)
!
      call cal_sph_diff_pol_and_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out,   &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                    &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, is_magne, is_current,   &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
!      Extend potential field
      call ext_outside_potential_with_j                                 &
     &   (sph_bc_B%kr_out, is_magne, is_current,                        &
     &    sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                        &
     &    sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                    &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call ext_inside_potential_with_j                                &
     &     (sph_bc_B%kr_in, is_magne, is_current,                       &
     &      sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine const_grad_bp_and_current
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_moment                             &
     &         (sph_rj, r_2nd, sph_bc_U, bcs_U,                         &
     &          fdm2_free_ICB, fdm2_free_CMB, is_fld, rj_fld)
!
      use cal_sph_exp_rotation
      use select_exp_velocity_ICB
      use select_exp_velocity_CMB
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_boundary_data), intent(in) :: bcs_U
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB, fdm2_free_CMB
      integer(kind = kint), intent(in) :: is_fld
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_ICB_grad_poloidal_moment                                 &
     &   (sph_rj, r_2nd, sph_bc_U, bcs_U%ICB_Vspec, fdm2_free_ICB,      &
     &    is_fld, rj_fld)
      call sel_CMB_grad_poloidal_moment                                 &
     &   (sph_rj, sph_bc_U, bcs_U%CMB_Vspec, fdm2_free_CMB,             &
     &    is_fld, rj_fld)
!
      call cal_sph_diff_poloidal2(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &    sph_rj%nidx_rj, r_2nd%fdm(1)%dmat, is_fld,                    &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_grad_poloidal_moment
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_magne(sph_rj, r_2nd,               &
     &          sph_bc_B, bcs_B, g_sph_rj, is_magne, rj_fld)
!
      use extend_potential_field
      use cal_sph_exp_rotation
      use select_exp_magne_ICB
      use select_exp_magne_CMB
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_vector_boundary_data), intent(in) :: bcs_B
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      integer(kind = kint), intent(in) :: is_magne
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_ICB_grad_poloidal_magne                                  &
     &   (sph_rj, r_2nd, sph_bc_B, bcs_B%ICB_Vspec, g_sph_rj,           &
     &    is_magne, rj_fld)
      call sel_CMB_grad_poloidal_magne                                  &
     &    (sph_rj, sph_bc_B, bcs_B%CMB_Vspec, g_sph_rj,                 &
     &     is_magne, rj_fld)
!
!
      call cal_sph_diff_poloidal2(sph_bc_B%kr_in, sph_bc_B%kr_out,      &
     &    sph_rj%nidx_rj, r_2nd%fdm(1)%dmat, is_magne,                  &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
!      Extend potential field
      call ext_outside_potential(sph_bc_B%kr_out, is_magne,             &
     &      sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call ext_inside_potential(sph_bc_B%kr_in, is_magne,             &
     &      sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine const_grad_poloidal_magne
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_pressure_gradient(sph_rj, r_2nd, sph_bc_U,       &
     &          g_sph_rj, coef_press, is_press, is_grad, rj_fld)
!
      use cal_sph_exp_nod_none_bc
      use const_wz_coriolis_rtp
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_press
      integer(kind = kint), intent(in) :: is_press, is_grad
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_nod_gradient_2(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &    is_press, is_grad, sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,     &
     &    g_sph_rj, r_2nd%fdm(1)%dmat,                                  &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call normalize_sph_average_grad(is_grad,                          &
     &    sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj,                    &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call delete_bc_rj_vector(sph_rj%nidx_rj(2), sph_bc_U%kr_in,       &
     &    is_grad, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call delete_bc_rj_vector(sph_rj%nidx_rj(2), sph_bc_U%kr_out,      &
     &    is_grad, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
!$omp parallel
      call ovwrt_rj_coef_prod_vect_smp(sph_rj, (-coef_press), is_grad,  &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!$omp end parallel
!
      end subroutine const_pressure_gradient
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
