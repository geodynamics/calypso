!> @file  sph_radial_grad_4_velocity.f90
!!      module sph_radial_grad_4_velocity
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate radial delivatives for velocity
!!
!!@verbatim
!!      subroutine const_grad_vp_and_vorticity(sph_rj, r_2nd,           &
!!     &          sph_bc_U, bc_fdms_U, bcs_U, g_sph_rj,                 &
!!     &          is_velo, is_vort, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_vector_boundary_data), intent(in) :: bcs_U
!!        type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
!!        real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        integer(kind = kint), intent(in) :: is_velo, is_vort
!!        type(phys_data), intent(inout) :: rj_fld
!!          Address for input:    is_velo, is_velo+2
!!          Address for solution: is_velo+1,
!!                              is_vort, is_vort+2, is_vort+1
!!
!!      subroutine const_grad_poloidal_moment(sph_rj, r_2nd,            &
!!     &          sph_bc_U, bc_fdms_U, bcs_U, is_fld, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
!!        type(sph_vector_boundary_data), intent(in) :: bcs_U
!!        integer(kind = kint), intent(in) :: is_fld
!!        type(phys_data), intent(inout) :: rj_fld
!!          Address for input:    is_fld, is_fld+2
!!          Address for solution: is_fld+1
!!
!!      subroutine const_pressure_gradient(sph_rj, r_2nd, sph_bc_U,     &
!!     &          g_sph_rj, coef_press, is_press, is_grad, rj_fld)
!!        Address for input:    is_press
!!        Address for solution: is_grad
!!
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_vector_boundary_data), intent(inout) :: bcs_U
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!@param sph_bc_U  Structure for basic boundary condition parameters
!!                 for velocity
!!
!!@param is_fld      Spherical hermonics data address for input vector
!!@param is_grad     Spherical hermonics data address for gradient
!!@param is_velo     Spherical hermonics data address
!!                   for poloidal velocity
!!@param is_vort     Spherical hermonics data address
!!                   for poloidal vorticity
!!@param is_press    Spherical hermonics data address
!!                   for pressure
!!@param rj_fld     Spectr data structure
!
      module sph_radial_grad_4_velocity
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
      use t_boundary_sph_spectr
      use t_boundary_params_sph_MHD
      use t_coef_sph_velocity_BCs
!
      use cal_sph_exp_1st_diff
!
      implicit none
!
      private :: delete_bc_rj_vector
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_vp_and_vorticity(sph_rj, r_2nd,             &
     &          sph_bc_U, bc_fdms_U, bcs_U, g_sph_rj,                   &
     &          is_velo, is_vort, rj_fld)
!
      use cal_sph_exp_rotation
      use select_exp_velocity_ICB
      use select_exp_velocity_CMB
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_boundary_data), intent(in) :: bcs_U
      type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      integer(kind = kint), intent(in) :: is_velo, is_vort
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_ICB_grad_vp_and_vorticity(sph_rj, r_2nd,                 &
     &    sph_bc_U, bcs_U%ICB_Vspec, bc_fdms_U%fdm2_free_ICB,           &
     &    g_sph_rj, is_velo, is_vort,                                   &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call sel_CMB_grad_vp_and_vorticity                                &
     &   (sph_rj, sph_bc_U, bcs_U%CMB_Vspec, bc_fdms_U%fdm2_free_CMB,   &
     &    g_sph_rj, is_velo, is_vort,                                   &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call cal_sph_diff_pol_and_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                    &
     &    r_2nd%dmat(-1,1,1), r_2nd%dmat(-1,1,2), is_velo, is_vort,     &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_grad_vp_and_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_moment(sph_rj, r_2nd,              &
     &          sph_bc_U, bc_fdms_U, bcs_U, is_fld, rj_fld)
!
      use cal_sph_exp_rotation
      use select_exp_velocity_ICB
      use select_exp_velocity_CMB
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
      type(sph_vector_boundary_data), intent(in) :: bcs_U
      integer(kind = kint), intent(in) :: is_fld
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_ICB_grad_poloidal_moment(sph_rj, r_2nd,                  &
     &    sph_bc_U, bcs_U%ICB_Vspec, bc_fdms_U%fdm2_free_ICB,           &
     &    is_fld, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call sel_CMB_grad_poloidal_moment(sph_rj,                         &
     &    sph_bc_U, bcs_U%CMB_Vspec, bc_fdms_U%fdm2_free_CMB,           &
     &    is_fld, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call cal_sph_diff_poloidal2(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &    sph_rj%nidx_rj, r_2nd%dmat(-1,1,1), is_fld,                   &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_grad_poloidal_moment
!
! -----------------------------------------------------------------------
!
      subroutine const_pressure_gradient(sph_rj, r_2nd, sph_bc_U,       &
     &          g_sph_rj, coef_press, is_press, is_grad, rj_fld)
!
      use t_const_wz_coriolis_rtp
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
     &    g_sph_rj, r_2nd%dmat(-1,1,1),                                 &
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
      call ovwrt_rj_coef_prod_vect_smp(sph_rj, (-coef_press), is_grad,  &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_pressure_gradient
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine delete_bc_rj_vector(jmax, kr_bc, is_fld,               &
     &                               n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_bc
      integer(kind = kint), intent(in) :: is_fld
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_bc-1) * jmax
!
        d_rj(inod,is_fld  ) = 0.0d0
        d_rj(inod,is_fld+1) = 0.0d0
        d_rj(inod,is_fld+2) = 0.0d0
      end do
!$omp end parallel do
!
      end subroutine delete_bc_rj_vector
!
! -----------------------------------------------------------------------
!
      end module sph_radial_grad_4_velocity
