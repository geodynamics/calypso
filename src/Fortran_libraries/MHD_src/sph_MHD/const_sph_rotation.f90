!> @file  const_sph_rotation.f90
!!      module const_sph_rotation
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate curl of fields
!!
!!@verbatim
!!      subroutine const_sph_vorticity(sph_rj, r_2nd, sph_bc_U,         &
!!     &          fdm2_free_ICB, fdm2_free_CMB, g_sph_rj,               &
!!     &          is_velo, is_vort, rj_fld)
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: ipol%i_vort, itor%i_vort, idpdr%i_vort
!!
!!      subroutine const_sph_current(sph_rj, r_2nd, sph_bc_B, g_sph_rj, &
!!     &          is_magne, is_current, rj_fld)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: ipol%i_current, itor%i_current, idpdr%i_current
!!
!!      subroutine const_sph_rotation_uxb(sph_rj, r_2nd, sph_bc_B,      &
!!     &          g_sph_rj, is_fld, is_rot, rj_fld)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!
!!      subroutine const_sph_rotation_no_bc(sph_rj, r_2nd, sph_bc,      &
!!     &          g_sph_rj, is_fld, is_rot, rj_fld)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!
!!      subroutine const_sph_force_rot2(sph_rj, r_2nd,                  &
!!     &          sph_bc_U, fdm2_free_ICB, fdm2_free_CMB, g_sph_rj,     &
!!     &          is_fld, is_rot, rj_fld)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!
!!      subroutine const_sph_viscous_by_vort2                           &
!!     &         (sph_rj, r_2nd, sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,&
!!     &          g_sph_rj, coef_diffuse, is_velo, is_vort, is_viscous, &
!!     &          rj_fld)
!!        Input:    ipol%i_vort, itor%i_vort
!!        Solution: ipol%i_v_diffuse, itor%i_v_diffuse, idpdr%i_v_diffuse
!!
!!      subroutine const_sph_mag_diffuse_by_j                           &
!!     &         (sph_rj, r_2nd, sph_bc_B, g_sph_rj, coef_diffuse,      &
!!     &          is_magne, is_current, is_ohmic, rj_fld)
!!        Input:    ipol%i_current, itor%i_current
!!        Solution: ipol%i_b_diffuse, itor%i_b_diffuse, idpdr%i_b_diffuse
!!
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        type(fdm2_free_slip), intent(in) :: fdm2_free_ICB
!!        type(fdm2_free_slip), intent(in) :: fdm2_free_CMB
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic boundary condition parameters
!!                 for velocity
!!@param sph_bc_B  Structure for basic boundary condition parameters
!!                 for magnetic field
!!@param sph_bc  Structure for basic boundary condition parameters
!!
!!@param kr_inside     Radial ID for inner boundary
!!@param kr_outside    RaAdial ID for outer boundary
!!@param coef_fdm_fix_in_2(0:2,3)
!!             Finite difference matrix for inner boundary
!!@param coef_fdm_fix_out_2(0:2,3)
!!             Finite difference matrix for outer boundary
!!
!!@param coef_diffuse   Diffusion coefficient
!!
!!@param is_fld      Input spectr field address
!!@param is_rot      Address of curl of field
!!
!!@param is_velo     Spherical hermonics data address
!!                   for poloidal velocity field
!!@param is_vort     Spherical hermonics data address
!!                   for poloidal voeticity
!!@param is_viscous  Spherical hermonics data address
!!                   for poloidal visous diffusion
!!@param is_magne    Spherical hermonics data address
!!                   for poloidal magnetic field
!!@param is_current  Spherical hermonics data address
!!                   for poloidal current density
!!@param is_ohmic    Spherical hermonics data address
!!                   for poloidal ohmic dissipation
!
      module const_sph_rotation
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_MHD_boundaries
      use t_fdm_coefs
!
      use cal_sph_exp_rotation
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_vorticity(sph_rj, r_2nd, sph_bc_U,           &
     &          fdm2_free_ICB, fdm2_free_CMB, g_sph_rj,                 &
     &          is_velo, is_vort, rj_fld)
!
      use select_exp_velocity_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB, fdm2_free_CMB
!
      integer(kind = kint), intent(in) :: is_velo, is_vort
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_bc_sph_vorticity                                         &
     &   (sph_rj, r_2nd, sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,        &
     &    g_sph_rj, is_velo, is_vort, rj_fld)
!
      call cal_sph_nod_vect_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                    &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, is_velo, is_vort,       &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_sph_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_current(sph_rj, r_2nd, sph_bc_B, g_sph_rj,   &
     &          is_magne, is_current, rj_fld)
!
      use select_exp_magne_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_current
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_bc_sph_current(sph_rj, r_2nd, sph_bc_B,                  &
     &    g_sph_rj, is_magne, is_current, rj_fld)
!
      call cal_sph_nod_vect_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out,       &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                    &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, is_magne, is_current,   &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_sph_current
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_rotation_uxb(sph_rj, r_2nd, sph_bc_B,        &
     &          g_sph_rj, is_fld, is_rot, rj_fld)
!
      use select_exp_magne_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_bc_sph_rotation_uxb                                      &
     &   (sph_rj, r_2nd, sph_bc_B, g_sph_rj, is_fld, is_rot, rj_fld)
!
      call cal_sph_nod_vect_w_div_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out, &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj, r_2nd%fdm(1)%dmat, &
     &    is_fld, is_rot, rj_fld%n_point, rj_fld%ntot_phys,             &
     &    rj_fld%d_fld)
!
      end subroutine const_sph_rotation_uxb
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_rotation_no_bc(sph_rj, r_2nd, sph_bc,        &
     &          g_sph_rj, is_fld, is_rot, rj_fld)
!
      use cal_sph_exp_nod_none_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_nod_nobc_in_rot2(sph_rj%nidx_rj(2), g_sph_rj,        &
     &    sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,          &
     &    is_fld, is_rot, rj_fld%n_point, rj_fld%ntot_phys,             &
     &    rj_fld%d_fld)
      call cal_sph_nod_nobc_out_rot2(sph_rj%nidx_rj(2), g_sph_rj,       &
     &    sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,         &
     &    is_fld, is_rot, rj_fld%n_point, rj_fld%ntot_phys,             &
     &    rj_fld%d_fld)
!
      call cal_sph_nod_vect_rot2(sph_bc%kr_in, sph_bc%kr_out,           &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                    &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, is_fld, is_rot,         &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_sph_rotation_no_bc
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_force_rot2(sph_rj, r_2nd,                    &
     &          sph_bc_U, fdm2_free_ICB, fdm2_free_CMB, g_sph_rj,       &
     &          is_fld, is_rot, rj_fld)
!
      use select_exp_velocity_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB, fdm2_free_CMB
!
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_bc_sph_vorticity                                         &
     &   (sph_rj, r_2nd, sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,        &
     &    g_sph_rj, is_fld, is_rot, rj_fld)
!
      call cal_sph_nod_vect_w_div_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out, &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj, r_2nd%fdm(1)%dmat, &
     &    is_fld, is_rot, rj_fld%n_point, rj_fld%ntot_phys,             &
     &    rj_fld%d_fld)
!
      end subroutine const_sph_force_rot2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_viscous_by_vort2                             &
     &         (sph_rj, r_2nd, sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,  &
     &          g_sph_rj, coef_diffuse, is_velo, is_vort, is_viscous,   &
     &          rj_fld)
!
      use cal_sph_exp_rotation
      use select_exp_velocity_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB, fdm2_free_CMB
!
      integer(kind = kint), intent(in) :: is_velo, is_vort, is_viscous
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_diffuse
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_viscous + 1
!
      call cal_sph_nod_diffuse_by_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out, &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                    &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, coef_diffuse,           &
     &    is_vort, is_viscous, rj_fld%n_point, rj_fld%ntot_phys,        &
     &    rj_fld%d_fld)
!
      call sel_bc_sph_viscous_diffusion(sph_rj, r_2nd, sph_bc_U,        &
     &    fdm2_free_ICB, fdm2_free_CMB, g_sph_rj, coef_diffuse,         &
     &    is_velo, is_vort, is_viscous, idp_diffuse, rj_fld)
!
      end subroutine const_sph_viscous_by_vort2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_mag_diffuse_by_j                             &
     &         (sph_rj, r_2nd, sph_bc_B, g_sph_rj, coef_diffuse,        &
     &          is_magne, is_current, is_ohmic, rj_fld)
!
      use select_exp_magne_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_current
      integer(kind = kint), intent(in) :: is_ohmic
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_diffuse
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_ohmic + 1
!
      call cal_sph_nod_diffuse_by_rot2                                  &
     &   (sph_bc_B%kr_in, sph_bc_B%kr_out,                              &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                    &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, coef_diffuse,           &
     &    is_current, is_ohmic, rj_fld%n_point, rj_fld%ntot_phys,       &
     &    rj_fld%d_fld)
!
      call sel_bc_sph_magnetic_diffusion(sph_rj, r_2nd, sph_bc_B,       &
     &    g_sph_rj, coef_diffuse, is_magne, is_ohmic, idp_diffuse,      &
     &    rj_fld)
!
      end subroutine const_sph_mag_diffuse_by_j
!
! -----------------------------------------------------------------------
!
      end module const_sph_rotation
