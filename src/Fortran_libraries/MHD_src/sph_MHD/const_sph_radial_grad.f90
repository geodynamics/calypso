!> @file  const_sph_radial_grad.f90
!!      module const_sph_radial_grad
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate radial delivatives
!!
!!@verbatim
!!      subroutine const_radial_grad_temp
!!      subroutine const_radial_grad_composit
!!
!!      subroutine const_grad_vp_and_vorticity
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: idpdr%i_velo, ipol%i_vort, itor%i_vort, idpdr%i_vort
!!
!!      subroutine const_grad_bp_and_current
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: idpdr%i_magne,
!!                  ipol%i_current, itor%i_current, idpdr%i_current
!!
!!      subroutine const_grad_poloidal_moment(i_field)
!!        Input:    i_field, i_field+2
!!        Solution: i_field+1
!!
!!      subroutine const_grad_poloidal_velo
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: idpdr%i_velo
!!      subroutine const_grad_poloidal_magne
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: idpdr%i_magne
!!
!!      subroutine const_pressure_gradient
!!        Input:    ipol%i_press
!!        Solution: ipol%i_press_grad
!!@endverbatim
!
      module const_sph_radial_grad
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
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
      subroutine const_radial_grad_temp
!
      use m_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
!
      call cal_sph_nod_gradient_2(nlayer_ICB, nlayer_CMB,               &
     &    d_rj(1,ipol%i_temp), d_rj(1,ipol%i_grad_t) )
!
      if (sph_bc_T%iflag_icb .eq. iflag_fixed_flux) then
        call cal_dsdr_sph_in_fix_flux_2(idx_rj_degree_zero, nidx_rj(2), &
     &      sph_bc_T%kr_in, sph_bc_T%r_ICB, sph_bc_T%ICB_flux,          &
     &      ipol%i_temp, ipol%i_grad_t)
      else
        call cal_dsdr_sph_fix_scalar_in_2                               &
     &     (idx_rj_degree_zero, nidx_rj(2),                             &
     &      sph_bc_T%kr_in, sph_bc_T%r_ICB, sph_bc_T%fdm2_fix_fld_ICB,  &
     &      sph_bc_T%ICB_fld, ipol%i_temp, ipol%i_grad_t)
      end if
!
      if (sph_bc_T%iflag_cmb .eq. iflag_fixed_flux) then
        call cal_dsdr_sph_out_fix_flux_2(idx_rj_degree_zero,            &
     &      nidx_rj(2), sph_bc_T%kr_out, sph_bc_T%r_CMB,                &
     &      sph_bc_T%CMB_flux, ipol%i_temp, ipol%i_grad_t)
      else
        call cal_dsdr_sph_fix_scalar_out_2                              &
     &     (idx_rj_degree_zero, nidx_rj(2), sph_bc_T%kr_out,            &
     &      sph_bc_T%r_CMB, sph_bc_T%fdm2_fix_fld_CMB,                  &
     &      sph_bc_T%CMB_fld, ipol%i_temp, ipol%i_grad_t)
      end if
!
      end subroutine const_radial_grad_temp
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_grad_composit
!
      use m_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_rotation
!
!
      call cal_sph_nod_gradient_2(nlayer_ICB, nlayer_CMB,               &
     &     d_rj(1,ipol%i_light), d_rj(1,ipol%i_grad_composit) )
!
      if (sph_bc_C%iflag_icb .eq. iflag_fixed_flux) then
        call cal_dsdr_sph_in_fix_flux_2(idx_rj_degree_zero, nidx_rj(2), &
     &      sph_bc_C%kr_in, sph_bc_C%r_ICB, sph_bc_C%ICB_flux,          &
     &      ipol%i_light, ipol%i_grad_composit)
      else
        call cal_dsdr_sph_fix_scalar_in_2                               &
     &     (idx_rj_degree_zero, nidx_rj(2),                             &
     &      sph_bc_C%kr_in, sph_bc_C%r_ICB, sph_bc_C%fdm2_fix_fld_ICB,  &
     &      sph_bc_C%ICB_fld, ipol%i_light, ipol%i_grad_composit)
      end if
!
      if (sph_bc_C%iflag_cmb .eq. iflag_fixed_flux) then
        call cal_dsdr_sph_out_fix_flux_2(idx_rj_degree_zero,            &
     &      nidx_rj(2), sph_bc_C%kr_out,  sph_bc_C%r_CMB,               &
     &      sph_bc_C%CMB_flux, ipol%i_light, ipol%i_grad_composit)
      else
        call cal_dsdr_sph_fix_scalar_out_2                              &
     &     (idx_rj_degree_zero, nidx_rj(2), sph_bc_C%kr_out,            &
     &      sph_bc_C%r_CMB, sph_bc_C%fdm2_fix_fld_CMB,                  &
     &      sph_bc_C%CMB_fld, ipol%i_light, ipol%i_grad_composit)
      end if
!
      end subroutine const_radial_grad_composit
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_grad_vp_and_vorticity
!
      use m_boundary_params_sph_MHD
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
      use cal_sph_exp_rotation
!
!
      if     (sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_v_and_w(nidx_rj(2), sph_bc_U%kr_in,   &
     &      fdm2_free_vp_ICB, fdm2_free_vt_ICB,                         &
     &      ipol%i_velo, ipol%i_vort)
      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2                               &
     &     (idx_rj_degree_zero, idx_rj_degree_one, nidx_rj(2),          &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, ipol%i_velo)
        call cal_sph_nod_icb_rigid_rot2                                 &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      ipol%i_velo, ipol%i_vort)
      else
        call cal_sph_nod_icb_rigid_velo2(nidx_rj(2),                    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, ipol%i_velo)
        call cal_sph_nod_icb_rigid_rot2                                 &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      ipol%i_velo, ipol%i_vort)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_v_and_w(nidx_rj(2), sph_bc_U%kr_out,  &
     &      fdm2_free_vp_CMB, fdm2_free_vt_CMB,                         &
     &      ipol%i_velo, ipol%i_vort)
      else
        call cal_sph_nod_cmb_rigid_v_and_w                              &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      vt_CMB_bc, ipol%i_velo, ipol%i_vort)
      end if
!
      call cal_sph_diff_pol_and_rot2(nlayer_ICB, nlayer_CMB,            &
     &    ipol%i_velo, ipol%i_vort)
!
      end subroutine const_grad_vp_and_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_bp_and_current
!
      use m_boundary_params_sph_MHD
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_icb_qvac
      use cal_sph_exp_nod_cmb_qvac
      use set_sph_exp_nod_center
      use extend_potential_field
      use cal_sph_exp_rotation
!
      integer(kind = kint) :: kr_in
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        kr_in = ione
        call cal_sph_nod_center_b_and_j(ipol%i_magne, ipol%i_current)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        kr_in = nlayer_ICB
        call cal_sph_nod_icb_qvc_b_and_j                                &
     &     (nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,                 &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      ipol%i_magne, ipol%i_current)
      else
        kr_in = nlayer_ICB
        call cal_sph_nod_icb_ins_b_and_j                                &
     &     (nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,                 &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      ipol%i_magne, ipol%i_current)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_b_and_j                                &
     &     (nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,                &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      ipol%i_magne, ipol%i_current)
      else
        call cal_sph_nod_cmb_ins_b_and_j                                &
     &     (nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,                &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      ipol%i_magne, ipol%i_current)
      end if
!
!
      call cal_sph_diff_pol_and_rot2(kr_in, nlayer_CMB, ipol%i_magne,   &
     &    ipol%i_current)
!
!      Extend potential field
      call ext_outside_potential_with_j(ipol%i_magne, ipol%i_current,   &
     &    nlayer_CMB)
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call ext_inside_potential_with_j(ipol%i_magne, ipol%i_current,  &
     &      nlayer_ICB)
      end if
!
      end subroutine const_grad_bp_and_current
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_moment(i_field)
!
      use m_boundary_params_sph_MHD
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
      use cal_sph_exp_rotation
!
      integer(kind = kint), intent(in) :: i_field
!
!
      if     (sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_vpol2(nidx_rj(2), sph_bc_U%kr_in,     &
     &      fdm2_free_vp_ICB, i_field)
      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2                               &
     &     (idx_rj_degree_zero, idx_rj_degree_one, nidx_rj(2),          &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, i_field)
      else
        call cal_sph_nod_icb_rigid_velo2(nidx_rj(2),                    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, i_field)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_vpol2(nidx_rj(2), sph_bc_U%kr_out,    &
     &      fdm2_free_vp_CMB, i_field)
      else
        call cal_sph_nod_cmb_rigid_velo2(nidx_rj(2),                    &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, vt_CMB_bc, i_field)
      end if
!
      call cal_sph_diff_poloidal2(nlayer_ICB, nlayer_CMB, i_field)
!
      end subroutine const_grad_poloidal_moment
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_velo
!
!
      call const_grad_poloidal_moment(ipol%i_velo)
!
      end subroutine const_grad_poloidal_velo
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_magne
!
      use m_boundary_params_sph_MHD
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_cmb_qvac
      use cal_sph_exp_nod_icb_qvac
      use set_sph_exp_nod_center
      use extend_potential_field
      use cal_sph_exp_rotation
!
      integer(kind = kint) :: kr_in
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        kr_in = ione
        call cal_dsdr_sph_center_2(ipol%i_magne)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        kr_in = nlayer_ICB
        call cal_sph_nod_icb_qvc_mag2(nidx_rj(2), sph_bc_B%kr_in,       &
     &      ipol%i_magne)
      else
        kr_in = nlayer_ICB
        call cal_sph_nod_icb_ins_mag2(nidx_rj(2), sph_bc_B%kr_in,       &
     &      sph_bc_B%r_ICB, ipol%i_magne)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_mag2(nidx_rj(2), sph_bc_B%kr_out,      &
     &      ipol%i_magne)
      else
        call cal_sph_nod_cmb_ins_mag2(nidx_rj(2), sph_bc_B%kr_out,      &
     &      sph_bc_B%r_CMB, ipol%i_magne)
      end if
!
!
      call cal_sph_diff_poloidal2(kr_in, nlayer_CMB, ipol%i_magne)
!
!      Extend potential field
      call ext_outside_potential(ipol%i_magne, nlayer_CMB)
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call ext_inside_potential(ipol%i_magne, nlayer_ICB)
      end if
!
      end subroutine const_grad_poloidal_magne
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_pressure_gradient
!
      use m_physical_property
      use cal_sph_exp_1st_diff
      use cal_sph_exp_nod_none_bc
      use const_wz_coriolis_rtp
!
!
      call cal_sph_nod_gradient_2(nlayer_ICB, nlayer_CMB,               &
     &    d_rj(1,ipol%i_press), d_rj(1,ipol%i_press_grad) )
!
      call delete_bc_rj_vector(nidx_rj(2), nlayer_ICB,                  &
     &    ipol%i_press_grad)
      call delete_bc_rj_vector(nidx_rj(2), nlayer_CMB,                  &
     &    ipol%i_press_grad)
!
!$omp parallel
      call ovwrt_rj_coef_prod_vect_smp( (-coef_press),                  &
     &    ipol%i_press_grad)
!$omp end parallel
!
      end subroutine const_pressure_gradient
!
! -----------------------------------------------------------------------
!
      end module const_sph_radial_grad
