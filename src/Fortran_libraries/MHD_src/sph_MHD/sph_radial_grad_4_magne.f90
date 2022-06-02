!> @file  sph_radial_grad_4_magne.f90
!!      module sph_radial_grad_4_magne
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate radial delivatives for velocity
!!
!!@verbatim
!!      subroutine const_grad_bp_and_current                            &
!!     &         (sph_rj, r_2nd, sph_bc_B, bcs_B, g_sph_rj,             &
!!     &          is_magne, is_current, rj_fld)
!!        Address for input:    is_magne, is_magne+2
!!        Address for solution: is_magne+1,
!!                              is_current, is_current+2, is_current+1
!!
!!      subroutine const_grad_poloidal_magne(sph_rj, r_2nd,             &
!!     &          sph_bc_B, bcs_B, g_sph_rj, is_magne, rj_fld)
!!        Address for input:    is_magne, is_magne+2
!!        Address for solution: is_magne+1
!!
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        type(sph_vector_boundary_data), intent(inout) :: bcs_B
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine extend_by_potential_with_j                           &
!!     &         (sph_rj, sph_bc_B, is_magne, is_current, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        integer(kind = kint), intent(in) :: is_magne, is_current
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!@param sph_bc_B  Structure for basic boundary condition parameters
!!                 for magnetic field
!!
!!@param is_fld      Spherical hermonics data address for input vector
!!@param is_grad     Spherical hermonics data address for gradient
!!@param is_magne    Spherical hermonics data address
!!                   for poloidal magnetic field
!!@param is_current  Spherical hermonics data address
!!                   for poloidal current density
!!@param rj_fld     Spectr data structure
!
      module sph_radial_grad_4_magne
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
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
      subroutine const_grad_bp_and_current                              &
     &         (sph_rj, r_2nd, sph_bc_B, bcs_B, g_sph_rj,               &
     &          is_magne, is_current, rj_fld)
!
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
      call sel_ICB_grad_bp_and_current(sph_rj, r_2nd, sph_bc_B,         &
     &    bcs_B%ICB_Vspec, g_sph_rj, is_magne, is_current,              &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call sel_CMB_grad_bp_and_current(sph_rj, sph_bc_B,                &
     &    bcs_B%CMB_Vspec, g_sph_rj, is_magne, is_current,              &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call cal_sph_diff_pol_and_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out,   &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                    &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, is_magne, is_current,   &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
!      Extend potential field
      call extend_by_potential_with_j                                   &
     &   (sph_rj, sph_bc_B, is_magne, is_current, rj_fld)
!
      end subroutine const_grad_bp_and_current
!
! -----------------------------------------------------------------------
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
     &    is_magne, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call sel_CMB_grad_poloidal_magne                                  &
     &    (sph_rj, sph_bc_B, bcs_B%CMB_Vspec, g_sph_rj,                 &
     &     is_magne, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
!
      call cal_sph_diff_poloidal2(sph_bc_B%kr_in, sph_bc_B%kr_out,      &
     &    sph_rj%nidx_rj, r_2nd%fdm(1)%dmat, is_magne,                  &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
!      Extend potential field
      call ext_outside_potential(sph_bc_B%kr_out,                       &
     &    sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                        &
     &    sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                    &
     &    rj_fld%n_point, rj_fld%d_fld(1,is_magne))
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call ext_inside_potential(sph_bc_B%kr_in,                       &
     &      sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                  &
     &      rj_fld%n_point, rj_fld%d_fld(1,is_magne))
      end if
!
      end subroutine const_grad_poloidal_magne
!
! -----------------------------------------------------------------------
!
      subroutine extend_by_potential_with_j                             &
     &         (sph_rj, sph_bc_B, is_magne, is_current, rj_fld)
!
      use extend_potential_field
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_current
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if((is_magne*is_current) .eq. 0) return
!      Extend potential field
      call ext_outside_potential_with_j                                 &
     &   (sph_bc_B%kr_out, sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,       &
     &    sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r, rj_fld%n_point,    &
     &    rj_fld%d_fld(1,is_magne), rj_fld%d_fld(1,is_current))
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call ext_inside_potential_with_j                                &
     &     (sph_bc_B%kr_in, sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r, rj_fld%n_point,  &
     &      rj_fld%d_fld(1,is_magne), rj_fld%d_fld(1,is_current))
      end if
!
      end subroutine extend_by_potential_with_j
!
! -----------------------------------------------------------------------
!
      end module sph_radial_grad_4_magne
