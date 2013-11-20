!> @file  const_sph_diffusion.f90
!!      module const_sph_diffusion
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate diffusion terms explicitly
!!
!!@verbatim
!!      subroutine const_sph_viscous_diffusion(sph_bc_U, coef_diffuse,  &
!!     &          is_velo, is_viscous)
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: ipol%i_v_diffuse, itor%i_v_diffuse, idpdr%i_v_diffuse
!!      subroutine const_sph_vorticirty_diffusion(sph_bc_U,             &
!!     &          coef_diffuse, is_vort, is_w_diffuse)
!!        Input:    ipol%i_vort, itor%i_vort
!!        Solution: ipol%i_w_diffuse, itor%i_w_diffuse, idpdr%i_w_diffuse
!!
!!      subroutine const_sph_magnetic_diffusion(sph_bc_B, coef_diffuse, &
!!     &          is_magne, is_ohmic)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: ipol%i_b_diffuse, itor%i_b_diffuse, idpdr%i_b_diffuse
!!
!!      subroutine const_sph_scalar_diffusion(sph_bc, coef_diffuse,     &
!!     &          is_fld, is_diffuse)
!!        Input:    ipol%i_temp
!!        Solution: ipol%i_t_diffuse
!!        Input:    ipol%i_light
!!        Solution: ipol%i_c_diffuse
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!@param sph_bc_U  Structure for basic boundary condition parameters
!!                 for velocity
!!@param sph_bc_B  Structure for basic boundary condition parameters
!!                 for magnetic field
!!
!!@param coef_diffuse   Diffusion coefficient
!!
!!@param is_velo     Spherical hermonics data address
!!                   for poloidal velocity field
!!@param is_vort     Spherical hermonics data address
!!                   for poloidal voeticity
!!@param is_viscous  Spherical hermonics data address
!!                   for poloidal visous diffusion
!!@param is_w_diffuse  Spherical hermonics data address
!!                   for poloidal diffusion term for vorticity
!!@param is_magne    Spherical hermonics data address
!!                   for poloidal magnetic field
!!@param is_ohmic    Spherical hermonics data address
!!                   for poloidal ohmic dissipation
!!
!!@param is_fld       Input spectr field address
!!@param is_diffuse   Input spectr diffusiton term address
!
      module const_sph_diffusion
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use cal_sph_exp_diffusion
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
      subroutine const_sph_viscous_diffusion(sph_bc_U, coef_diffuse,    &
     &          is_velo, is_viscous)
!
      use t_boundary_params_sph_MHD
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
      use cal_sph_exp_fixed_scalar
      use set_sph_exp_nod_center
      use cal_inner_core_rotation
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_velo, is_viscous
      real(kind = kreal), intent(in) :: coef_diffuse
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_viscous + 1
!
      call cal_sph_nod_vect_diffuse2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    coef_diffuse, is_velo, is_viscous)
      call cal_sph_nod_vect_dr_2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    d_rj(1,is_viscous), d_rj(1,idp_diffuse) )
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_diffuse2(coef_diffuse,                  &
     &      is_velo, is_viscous)
        call cal_dsdr_sph_center_2(is_viscous)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
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
      call cal_dsdr_sph_no_bc_in_2(nidx_rj(2),                          &
     &    sph_bc_U%kr_in, sph_bc_U%fdm2_fix_fld_ICB,                    &
     &    is_viscous, idp_diffuse)
!
!   Ovewrite rotatable inner core 
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_icore_viscous_drag_explicit(sph_bc_U%kr_in,            &
     &      coef_diffuse, itor%i_velo, itor%i_v_diffuse)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_diffuse2                              &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      fdm2_free_vp_CMB, fdm2_free_vt_CMB,                         &
     &      coef_diffuse, is_velo, is_viscous)
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
      end subroutine const_sph_viscous_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_vorticirty_diffusion(sph_bc_U,               &
     &          coef_diffuse, is_vort, is_w_diffuse)
!
      use t_boundary_params_sph_MHD
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
      use cal_sph_exp_fixed_scalar
      use set_sph_exp_nod_center
      use cal_inner_core_rotation
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_vort, is_w_diffuse
      real(kind = kreal), intent(in) :: coef_diffuse
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_w_diffuse + 1
!
      call cal_sph_nod_vect_diffuse2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    coef_diffuse, is_vort, is_w_diffuse)
      call cal_sph_nod_vect_dr_2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    d_rj(1,is_w_diffuse), d_rj(1,idp_diffuse) )
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_diffuse2(coef_diffuse,                  &
     &      is_vort, is_w_diffuse)
        call cal_dsdr_sph_center_2(is_w_diffuse)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_w_diffuse2                            &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, fdm2_free_vt_ICB,                &
     &      coef_diffuse, is_vort, is_w_diffuse)
      else
        call cal_sph_nod_icb_rgd_w_diffuse2(nidx_rj(2),                 &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_fld_ICB,  &
     &      coef_diffuse, is_vort, is_w_diffuse)
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_icore_viscous_drag_explicit(sph_bc_U%kr_in,            &
     &      coef_diffuse, is_vort, is_w_diffuse)
      end if
!
      call cal_dsdr_sph_no_bc_in_2(nidx_rj(2),                          &
     &    sph_bc_U%kr_in, sph_bc_U%fdm2_fix_fld_ICB,                    &
     &    is_w_diffuse, idp_diffuse)
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_w_diffuse2                            &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      sph_bc_U%fdm2_fix_fld_CMB, fdm2_free_vt_CMB,                &
     &      coef_diffuse, is_vort, is_w_diffuse)
      else
        call cal_sph_nod_cmb_rgd_w_diffuse2(nidx_rj(2),                 &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_fld_CMB, &
     &      coef_diffuse, is_vort, is_w_diffuse)
      end if
!
      call cal_dsdr_sph_no_bc_out_2(nidx_rj(2),                         &
     &    sph_bc_U%kr_out, sph_bc_U%fdm2_fix_fld_CMB,                   &
     &    is_w_diffuse, idp_diffuse)
!
      end subroutine const_sph_vorticirty_diffusion
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_magnetic_diffusion(sph_bc_B, coef_diffuse,   &
     &          is_magne, is_ohmic)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_icb_qvac
      use cal_sph_exp_nod_cmb_qvac
      use set_sph_exp_nod_center
      use cal_sph_exp_fixed_scalar
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_ohmic
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
!
      call cal_sph_nod_vect_diffuse2(sph_bc_B%kr_in, sph_bc_B%kr_out,   &
     &     coef_diffuse, is_magne, is_ohmic)
      call cal_sph_nod_vect_dr_2(sph_bc_B%kr_in, sph_bc_B%kr_out,       &
     &    d_rj(1,is_ohmic), d_rj(1,idp_diffuse) )
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
      end subroutine const_sph_magnetic_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_scalar_diffusion(sph_bc, coef_diffuse,       &
     &          is_fld, is_diffuse)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: coef_diffuse
!
!
      call cal_sph_nod_scalar_diffuse2(sph_bc%kr_in, sph_bc%kr_out,     &
     &    coef_diffuse, is_fld, is_diffuse)
!
      if (sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call cal_sph_in_fix_flux_diffuse2(nidx_rj(2),                   &
     &     sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB,          &
     &     sph_bc%ICB_flux, coef_diffuse, is_fld, is_diffuse)
      else
        call cal_sph_fix_scalar_in_diffuse2(nidx_rj(2),                 &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      sph_bc%ICB_fld, coef_diffuse, is_fld, is_diffuse)
      end if
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux) then
        call cal_sph_out_fix_flux_diffuse2(nidx_rj(2),                  &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      sph_bc%CMB_flux, coef_diffuse, is_fld, is_diffuse)
      else
        call cal_sph_out_fix_scalar_diffuse2(nidx_rj(2),                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      sph_bc%CMB_fld, coef_diffuse, is_fld, is_diffuse)
      end if
!
      end subroutine const_sph_scalar_diffusion
!
! -----------------------------------------------------------------------
!
      end module const_sph_diffusion
