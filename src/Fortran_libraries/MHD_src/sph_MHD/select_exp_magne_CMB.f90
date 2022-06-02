!> @file  select_exp_magne_CMB.f90
!!      module select_exp_magne_CMB
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Select boundary condition routines for magnetic field
!!
!!@verbatim
!!      subroutine sel_CMB_grad_bp_and_current                          &
!!     &         (sph_rj, sph_bc_B, CMB_Bspec, g_sph_rj,                &
!!     &          is_magne, is_current, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_magne, is_magne+2
!!        Address for solution: is_magne+1,
!!                              is_current, is_current+2, is_current+1
!!      subroutine sel_CMB_grad_poloidal_magne                          &
!!     &         (sph_rj, sph_bc_B, CMB_Bspec, g_sph_rj,                &
!!     &          is_magne, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_magne, is_magne+2
!!        Address for solution: is_magne+1
!!
!!      subroutine sel_CMB_sph_current(sph_rj, sph_bc_B, g_sph_rj,      &
!!     &          is_magne, is_current, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_magne, is_magne+2
!!        Address for solution: is_current, is_current+2, is_current+1
!!      subroutine sel_CMB_sph_rotation_uxb(sph_rj, sph_bc_B, g_sph_rj, &
!!     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_fld, it_fld
!!        Address for solution: is_rot, it_rot, ids_rot
!!      subroutine sel_CMB_sph_magnetic_diffusion                       &
!!     &         (sph_rj, sph_bc_B, g_sph_rj, coef_diffuse,             &
!!     &          is_magne, is_ohmic, n_point, ntot_phys_rj, d_rj)
!!        Address for input:    is_magne, is_magne+2
!!        Address for solution: is_ohmic, is_ohmic+2, is_ohmic+2
!!          type(sph_boundary_type), intent(in) :: sph_bc_B
!!          type(sph_rj_grid), intent(in) :: sph_rj
!!          type(fdm_matrices), intent(in) :: r_2nd
!!@endverbatim
!!
!!@param sph_bc_B  Structure for basic boundary condition parameters
!!                 for magnetic field
!!
!!@param coef_diffuse   Diffusion coefficient
!!
!!@param is_magne    Spherical hermonics data address
!!                   for poloidal magnetic field
!!@param is_current    Spherical hermonics data address
!!                     for current density
!!@param is_ohmic    Spherical hermonics data address
!!                   for poloidal ohmic dissipation
!!@param ids_ohmic    Spherical hermonics data address
!!                   for radial derivative of poloidal ohmic dissipation
!!@param is_fld      Spherical hermonics data address for input vector
!!@param is_rot      Spherical hermonics data address for curl of field
!
      module select_exp_magne_CMB
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
!
      use set_sph_exp_fix_vector_CMB
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_cmb_qvac
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_grad_bp_and_current                            &
     &         (sph_rj, sph_bc_B, CMB_Bspec, g_sph_rj,                  &
     &          is_magne, is_current, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_vector_BC_coef), intent(in) :: CMB_Bspec
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_magne, is_current
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_b_and_j(sph_rj%nidx_rj(2), g_sph_rj,   &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB,                            &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_B%iflag_cmb .eq. iflag_evolve_field) then
        call cal_sph_nod_cmb_rigid_vect                                 &
     &     (sph_rj%nidx_rj(2), sph_bc_B%kr_out,                         &
     &      CMB_Bspec%Vp_BC, CMB_Bspec%Dp_BC, CMB_Bspec%Vt_BC,          &
     &      is_magne, n_point, ntot_phys_rj, d_rj)
        call cal_sph_nod_cmb_fixed_rot2(sph_rj%nidx_rj(2), g_sph_rj,    &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB,                            &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current, n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_B%iflag_cmb .eq. iflag_sph_insulator) then
      else
        call cal_sph_nod_cmb_ins_b_and_j(sph_rj%nidx_rj(2), g_sph_rj,   &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB,                            &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current, n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_CMB_grad_bp_and_current
!
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_grad_poloidal_magne                            &
     &         (sph_rj, sph_bc_B, CMB_Bspec, g_sph_rj,                  &
     &          is_magne, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_vector_BC_coef), intent(in) :: CMB_Bspec
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_magne
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_mag2                                   &
     &     (sph_rj%nidx_rj(2), sph_bc_B%kr_out, is_magne,               &
     &      n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_B%iflag_cmb .eq. iflag_evolve_field) then
        call cal_sph_nod_cmb_rigid_vect                                 &
     &     (sph_rj%nidx_rj(2), sph_bc_B%kr_out,                         &
     &      CMB_Bspec%Vp_BC, CMB_Bspec%Dp_BC, CMB_Bspec%Vt_BC,          &
     &      is_magne, n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_B%iflag_cmb .eq. iflag_sph_insulator) then
      else
        call cal_sph_nod_cmb_ins_mag2(sph_rj%nidx_rj(2), g_sph_rj,      &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB, is_magne,                  &
     &      n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_CMB_grad_poloidal_magne
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_sph_current(sph_rj, sph_bc_B, g_sph_rj,        &
     &          is_magne, is_current, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_magne, is_current
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_rot2(sph_rj%nidx_rj(2), g_sph_rj,      &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB,                            &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current, n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_B%iflag_cmb .eq. iflag_evolve_field) then
        call cal_sph_nod_cmb_fixed_rot2(sph_rj%nidx_rj(2), g_sph_rj,    &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB,                            &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current, n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_B%iflag_cmb .eq. iflag_sph_insulator) then
      else
        call cal_sph_nod_cmb_ins_rot2(sph_rj%nidx_rj(2), g_sph_rj,      &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB,                            &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current, n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_CMB_sph_current
!
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_sph_rotation_uxb(sph_rj, sph_bc_B, g_sph_rj,   &
     &          is_fld, is_rot, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_vp_rot2                                &
     &     (sph_rj%nidx_rj(2), sph_bc_B%kr_out, is_fld, is_rot,         &
     &      n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_B%iflag_cmb .eq. iflag_sph_insulator) then
      else
        call cal_sph_nod_cmb_ins_vp_rot2                                &
     &     (sph_rj%nidx_rj(2), g_sph_rj, sph_bc_B%kr_out,               &
     &      sph_bc_B%r_CMB, is_fld, is_rot,                             &
     &      n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_CMB_sph_rotation_uxb
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_sph_magnetic_diffusion                         &
     &         (sph_rj, sph_bc_B, g_sph_rj, coef_diffuse,               &
     &          is_magne, is_ohmic, n_point, ntot_phys_rj, d_rj)
!
      use cal_sph_exp_fixed_scalar
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_B
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_magne
      integer(kind = kint), intent(in) :: is_ohmic
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_diffuse
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: ids_ohmic
!
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_diffuse2                               &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB,                            &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      coef_diffuse, is_magne, is_ohmic,                           &
     &      n_point, ntot_phys_rj, d_rj)
      else if(sph_bc_B%iflag_cmb .eq. iflag_evolve_field) then
        call cal_sph_nod_cmb_fixed_diffuse2(sph_rj%nidx_rj(2),          &
     &      g_sph_rj, sph_bc_B%kr_out, sph_bc_B%r_CMB,                  &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      coef_diffuse, is_magne, is_ohmic,                           &
     &      n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc_B%iflag_cmb .eq. iflag_sph_insulator) then
      else
        call cal_sph_nod_cmb_ins_diffuse2                               &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB,                            &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      coef_diffuse, is_magne, is_ohmic,                           &
     &      n_point, ntot_phys_rj, d_rj)
      end if
!
      ids_ohmic = is_ohmic + 1
      call cal_dsdr_sph_no_bc_out_2(sph_rj%nidx_rj(2), sph_bc_B%kr_out, &
     &    sph_bc_B%fdm2_fix_fld_CMB, is_ohmic, ids_ohmic,               &
     &    n_point, ntot_phys_rj, d_rj)
!
      end subroutine sel_CMB_sph_magnetic_diffusion
!
! -----------------------------------------------------------------------
!
      end module select_exp_magne_CMB
