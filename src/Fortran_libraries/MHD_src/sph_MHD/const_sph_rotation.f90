!> @file  const_sph_rotation.f90
!!      module const_sph_rotation
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate curl of fields
!!
!!@verbatim
!!      subroutine const_sph_vorticity(sph_bc_U, is_velo, is_vort)
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: ipol%i_vort, itor%i_vort, idpdr%i_vort
!!
!!      subroutine const_sph_current(sph_bc_B, is_magne, is_current)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: ipol%i_current, itor%i_current, idpdr%i_current
!!
!!      subroutine const_sph_rotation_uxb(isph_bc_B, s_fld, is_rot)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!
!!      subroutine const_sph_rotation_no_bc(sph_bc, is_fld, is_rot)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!
!!      subroutine const_sph_force_rot2(sph_bc_U, is_fld, is_rot)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!
!!      subroutine const_sph_viscous_by_vort2(sph_bc_U, coef_diffuse,   &
!!     &          is_velo, is_vort, is_viscous)
!!        Input:    ipol%i_vort, itor%i_vort
!!        Solution: ipol%i_v_diffuse, itor%i_v_diffuse, idpdr%i_v_diffuse
!!
!!      subroutine const_sph_mag_diffuse_by_j(sph_bc_B, coef_diffuse,   &
!!     &          is_magne, is_current, is_ohmic)
!!        Input:    ipol%i_current, itor%i_current
!!        Solution: ipol%i_b_diffuse, itor%i_b_diffuse, idpdr%i_b_diffuse
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
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
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
      subroutine const_sph_vorticity(sph_bc_U, is_velo, is_vort)
!
      use t_boundary_params_sph_MHD
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_velo, is_vort
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_rot2                                  &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      fdm2_free_vp_ICB, fdm2_free_vt_ICB, is_velo, is_vort)
      else
        call cal_sph_nod_icb_rigid_rot2                                 &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      is_velo, is_vort)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_rot2                                  &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      fdm2_free_vp_CMB, fdm2_free_vt_CMB, is_velo, is_vort)
      else
        call cal_sph_nod_cmb_rigid_rot2                                 &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      is_velo, is_vort)
      end if
!
      call cal_sph_nod_vect_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    is_velo, is_vort)
!
      end subroutine const_sph_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_current(sph_bc_B, is_magne, is_current)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_icb_qvac
      use cal_sph_exp_nod_cmb_qvac
      use set_sph_exp_nod_center
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_current
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_rot2(is_magne, is_current)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_rot2                                   &
     &     (nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,                 &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      is_magne, is_current)
      else
        call cal_sph_nod_icb_ins_rot2                                   &
     &     (nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,                 &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      is_magne, is_current)
      end if
!
      call cal_sph_nod_vect_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out,       &
     &    is_magne, is_current)
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_rot2                                   &
     &     (nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,                &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current)
      else
        call cal_sph_nod_cmb_ins_rot2                                   &
     &     (nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,                &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current)
      end if
!
      end subroutine const_sph_current
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_rotation_uxb(sph_bc_B, is_fld, is_rot)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_icb_qvac
      use cal_sph_exp_nod_cmb_qvac
      use set_sph_exp_nod_center
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_fld, is_rot
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_rot2(is_fld, is_rot)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_vp_rot2(nidx_rj(2), sph_bc_B%kr_in,    &
     &      is_fld, is_rot)
      else
        call cal_sph_nod_icb_ins_vp_rot2(nidx_rj(2), sph_bc_B%kr_in,    &
     &      sph_bc_B%r_ICB, is_fld, is_rot)
      end if
!
      call cal_sph_nod_vect_w_div_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out, &
     &    is_fld, is_rot)
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_vp_rot2(nidx_rj(2), sph_bc_B%kr_out,   &
     &      is_fld, is_rot)
      else
        call cal_sph_nod_cmb_ins_vp_rot2(nidx_rj(2), sph_bc_B%kr_out,   &
     &      sph_bc_B%r_CMB, is_fld, is_rot)
      end if
!
      end subroutine const_sph_rotation_uxb
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_rotation_no_bc(sph_bc, is_fld, is_rot)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_nod_none_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc
!
      integer(kind = kint), intent(in) :: is_fld, is_rot
!
!
      call cal_sph_nod_nobc_in_rot2(nidx_rj(2),                         &
     &    sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,          &
     &    is_fld, is_rot)
      call cal_sph_nod_nobc_out_rot2(nidx_rj(2),                        &
     &    sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,         &
     &    is_fld, is_rot)
!
      call cal_sph_nod_vect_rot2(sph_bc%kr_in, sph_bc%kr_out,           &
     &     is_fld, is_rot)
!
      end subroutine const_sph_rotation_no_bc
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_force_rot2(sph_bc_U, is_fld, is_rot)
!
      use t_boundary_params_sph_MHD
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_fld, is_rot
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_rot2                                  &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      fdm2_free_vp_ICB, fdm2_free_vt_ICB, is_fld, is_rot)
      else
        call cal_sph_nod_icb_rigid_rot2                                 &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      is_fld, is_rot)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_rot2                                  &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      fdm2_free_vp_CMB, fdm2_free_vt_CMB, is_fld, is_rot)
      else
        call cal_sph_nod_cmb_rigid_rot2                                 &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      is_fld, is_rot)
      end if
!
      call cal_sph_nod_vect_w_div_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out, &
     &    is_fld, is_rot)
!
      end subroutine const_sph_force_rot2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_viscous_by_vort2(sph_bc_U, coef_diffuse,     &
     &          is_velo, is_vort, is_viscous)
!
      use t_boundary_params_sph_MHD
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
      use cal_sph_exp_fixed_scalar
      use cal_inner_core_rotation
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_velo, is_vort, is_viscous
      real(kind = kreal), intent(in) :: coef_diffuse
!
      integer(kind = kint) :: idp_diffuse, it_diffuse
!
!
      idp_diffuse = is_viscous + 1
      it_diffuse =  is_viscous + 2
!
      call cal_sph_nod_diffuse_by_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out, &
     &    coef_diffuse, is_vort, is_viscous)
!
      if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_diffuse2                              &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      fdm2_free_vp_ICB, fdm2_free_vt_ICB,                         &
     &      coef_diffuse, is_velo, is_viscous)
      else
        call cal_sph_nod_icb_rigid_diffuse2                             &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      coef_diffuse, is_velo, is_viscous)
      end if
!
      call cal_dsdr_sph_no_bc_in_2(nidx_rj(2),                          &
     &    sph_bc_U%kr_in, sph_bc_U%fdm2_fix_fld_ICB,                    &
     &    is_viscous, idp_diffuse)
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_icore_viscous_drag_explicit(sph_bc_U%kr_in,            &
     &      coef_diffuse, is_vort, it_diffuse)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_diffuse2                              &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      fdm2_free_vp_CMB, fdm2_free_vt_CMB, coef_diffuse,           &
     &      is_velo, is_viscous)
      else
        call cal_sph_nod_cmb_rigid_diffuse2                             &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      coef_diffuse, is_velo, is_viscous)
      end if
      call cal_dsdr_sph_no_bc_out_2(nidx_rj(2),                         &
     &    sph_bc_U%kr_out, sph_bc_U%fdm2_fix_fld_CMB,                   &
     &    is_viscous, idp_diffuse)
!
      end subroutine const_sph_viscous_by_vort2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_mag_diffuse_by_j(sph_bc_B, coef_diffuse,     &
     &          is_magne, is_current, is_ohmic)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_icb_qvac
      use cal_sph_exp_nod_cmb_qvac
      use set_sph_exp_nod_center
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_current
      integer(kind = kint), intent(in) :: is_ohmic
      real(kind = kreal), intent(in) :: coef_diffuse
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_ohmic + 1
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_diffuse2(coef_diffuse,                  &
     &      is_magne, is_ohmic)
        call cal_dsdr_sph_center_2(is_ohmic)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_diffuse2                               &
     &     (nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,                 &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      coef_diffuse, is_magne, is_ohmic)
        call cal_dsdr_sph_no_bc_in_2(nidx_rj(2),                        &
     &      sph_bc_B%kr_in, sph_bc_B%fdm2_fix_fld_ICB,                  &
     &      is_ohmic, idp_diffuse)
      else
        call cal_sph_nod_icb_ins_diffuse2                               &
     &     (nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,                 &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      coef_diffuse, is_magne, is_ohmic)
        call cal_dsdr_sph_no_bc_in_2(nidx_rj(2),                        &
     &      sph_bc_B%kr_in, sph_bc_B%fdm2_fix_fld_ICB,                  &
     &      is_ohmic, idp_diffuse)
      end if
!
      call cal_sph_nod_diffuse_by_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out, &
     &    coef_diffuse, is_current, is_ohmic)
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_diffuse2                               &
     &     (nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,                &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      coef_diffuse, is_magne, is_ohmic)
      else
        call cal_sph_nod_cmb_ins_diffuse2                               &
     &     (nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,                &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      coef_diffuse, is_magne, is_ohmic)
      end if
      call cal_dsdr_sph_no_bc_out_2(nidx_rj(2),                         &
     &    sph_bc_B%kr_out, sph_bc_B%fdm2_fix_fld_CMB,                   &
     &    is_ohmic, idp_diffuse)
!
      end subroutine const_sph_mag_diffuse_by_j
!
! -----------------------------------------------------------------------
!
      end module const_sph_rotation
