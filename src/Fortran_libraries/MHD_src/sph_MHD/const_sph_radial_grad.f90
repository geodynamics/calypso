!> @file  const_sph_radial_grad.f90
!!      module const_sph_radial_grad
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate radial delivatives
!!
!!@verbatim
!!      subroutine const_radial_grad_scalar(sph_bc, is_fld, is_grad)
!!        Input:    is_fld
!!        Solution: is_grad
!!
!!      subroutine const_grad_vp_and_vorticity(is_velo, is_vort)
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: idpdr%i_velo, ipol%i_vort, itor%i_vort, idpdr%i_vort
!!
!!      subroutine const_grad_bp_and_current(sph_bc_B,                  &
!!     &           is_magne, is_current)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: idpdr%i_magne,
!!                  ipol%i_current, itor%i_current, idpdr%i_current
!!
!!      subroutine const_grad_poloidal_moment(is_fld)
!!        Input:    is_fld, is_fld+2
!!        Solution: is_fld+1
!!
!!      subroutine const_grad_poloidal_magne(sph_bc_B, is_magne)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: idpdr%i_magne
!!
!!      subroutine const_pressure_gradient(sph_bc_U, is_press, is_grad)
!!        Input:    ipol%i_press
!!        Solution: ipol%i_press_grad
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
!!@param is_velo     Spherical hermonics data address
!!                   for poloidal magnetic field
!!@param is_vort     Spherical hermonics data address
!!                   for poloidal current density
!!@param is_press    Spherical hermonics data address
!!                   for pressure
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
      subroutine const_radial_grad_scalar(sph_bc, is_fld, is_grad)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_rotation
!
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: is_fld, is_grad
!
!
      call cal_sph_nod_gradient_2(sph_bc%kr_in, sph_bc%kr_out,          &
     &     d_rj(1,is_fld), d_rj(1,is_grad) )
!
      if (sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call cal_dsdr_sph_in_fix_flux_2(idx_rj_degree_zero, nidx_rj(2), &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%ICB_flux,                &
     &      is_fld, is_grad)
      else
        call cal_dsdr_sph_fix_scalar_in_2                               &
     &     (idx_rj_degree_zero, nidx_rj(2),                             &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      sph_bc%ICB_fld, is_fld, is_grad)
      end if
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux) then
        call cal_dsdr_sph_out_fix_flux_2(idx_rj_degree_zero,            &
     &      nidx_rj(2), sph_bc%kr_out, sph_bc%r_CMB,                    &
     &      sph_bc%CMB_flux, is_fld, is_grad)
      else
        call cal_dsdr_sph_fix_scalar_out_2                              &
     &     (idx_rj_degree_zero, nidx_rj(2), sph_bc%kr_out,              &
     &      sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,                      &
     &      sph_bc%CMB_fld, is_fld, is_grad)
      end if
!
      end subroutine const_radial_grad_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_grad_vp_and_vorticity(is_velo, is_vort)
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
      integer(kind = kint), intent(in) :: is_velo, is_vort
!
!
      if     (sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_v_and_w(nidx_rj(2), sph_bc_U%kr_in,   &
     &      fdm2_free_vp_ICB, fdm2_free_vt_ICB, is_velo, is_vort)
      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2                               &
     &     (idx_rj_degree_zero, idx_rj_degree_one, nidx_rj(2),          &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, is_velo)
        call cal_sph_nod_icb_rigid_rot2                                 &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      is_velo, is_vort)
      else
        call cal_sph_nod_icb_rigid_velo2(nidx_rj(2),                    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, is_velo)
        call cal_sph_nod_icb_rigid_rot2                                 &
     &     (nidx_rj(2), sph_bc_U%kr_in, sph_bc_U%r_ICB,                 &
     &      sph_bc_U%fdm2_fix_fld_ICB, sph_bc_U%fdm2_fix_dr_ICB,        &
     &      is_velo, is_vort)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_v_and_w(nidx_rj(2), sph_bc_U%kr_out,  &
     &      fdm2_free_vp_CMB, fdm2_free_vt_CMB, is_velo, is_vort)
      else
        call cal_sph_nod_cmb_rigid_v_and_w                              &
     &     (nidx_rj(2), sph_bc_U%kr_out, sph_bc_U%r_CMB,                &
     &      sph_bc_U%fdm2_fix_fld_CMB, sph_bc_U%fdm2_fix_dr_CMB,        &
     &      vt_CMB_bc, is_velo, is_vort)
      end if
!
      call cal_sph_diff_pol_and_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    is_velo, is_vort)
!
      end subroutine const_grad_vp_and_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_bp_and_current(sph_bc_B,                    &
     &           is_magne, is_current)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_icb_qvac
      use cal_sph_exp_nod_cmb_qvac
      use set_sph_exp_nod_center
      use extend_potential_field
      use cal_sph_exp_rotation
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_current
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_b_and_j(is_magne, is_current)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_b_and_j                                &
     &     (nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,                 &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      is_magne, is_current)
      else
        call cal_sph_nod_icb_ins_b_and_j                                &
     &     (nidx_rj(2), sph_bc_B%kr_in, sph_bc_B%r_ICB,                 &
     &      sph_bc_B%fdm2_fix_fld_ICB, sph_bc_B%fdm2_fix_dr_ICB,        &
     &      is_magne, is_current)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_b_and_j                                &
     &     (nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,                &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current)
      else
        call cal_sph_nod_cmb_ins_b_and_j                                &
     &     (nidx_rj(2), sph_bc_B%kr_out, sph_bc_B%r_CMB,                &
     &      sph_bc_B%fdm2_fix_fld_CMB, sph_bc_B%fdm2_fix_dr_CMB,        &
     &      is_magne, is_current)
      end if
!
!
      call cal_sph_diff_pol_and_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out,   &
     &    is_magne, is_current)
!
!      Extend potential field
      call ext_outside_potential_with_j(is_magne, is_current,           &
     &    sph_bc_B%kr_out)
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call ext_inside_potential_with_j(is_magne, is_current,          &
     &      sph_bc_B%kr_in)
      end if
!
      end subroutine const_grad_bp_and_current
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_moment(is_fld)
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
      integer(kind = kint), intent(in) :: is_fld
!
!
      if     (sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_vpol2(nidx_rj(2), sph_bc_U%kr_in,     &
     &      fdm2_free_vp_ICB, is_fld)
      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2                               &
     &     (idx_rj_degree_zero, idx_rj_degree_one, nidx_rj(2),          &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, is_fld)
      else
        call cal_sph_nod_icb_rigid_velo2(nidx_rj(2),                    &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, vt_ICB_bc, is_fld)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_vpol2(nidx_rj(2), sph_bc_U%kr_out,    &
     &      fdm2_free_vp_CMB, is_fld)
      else
        call cal_sph_nod_cmb_rigid_velo2(nidx_rj(2),                    &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, vt_CMB_bc, is_fld)
      end if
!
      call cal_sph_diff_poloidal2(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &    is_fld)
!
      end subroutine const_grad_poloidal_moment
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_magne(sph_bc_B, is_magne)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_cmb_qvac
      use cal_sph_exp_nod_icb_qvac
      use set_sph_exp_nod_center
      use extend_potential_field
      use cal_sph_exp_rotation
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_dsdr_sph_center_2(is_magne)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_mag2(nidx_rj(2), sph_bc_B%kr_in,       &
     &      is_magne)
      else
        call cal_sph_nod_icb_ins_mag2(nidx_rj(2), sph_bc_B%kr_in,       &
     &      sph_bc_B%r_ICB, is_magne)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_mag2(nidx_rj(2), sph_bc_B%kr_out,      &
     &      is_magne)
      else
        call cal_sph_nod_cmb_ins_mag2(nidx_rj(2), sph_bc_B%kr_out,      &
     &      sph_bc_B%r_CMB, is_magne)
      end if
!
!
      call cal_sph_diff_poloidal2(sph_bc_B%kr_in, sph_bc_B%kr_out,      &
     &    is_magne)
!
!      Extend potential field
      call ext_outside_potential(is_magne, sph_bc_B%kr_out)
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call ext_inside_potential(is_magne, sph_bc_B%kr_in)
      end if
!
      end subroutine const_grad_poloidal_magne
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_pressure_gradient(sph_bc_U, is_press, is_grad)
!
      use m_physical_property
      use t_boundary_params_sph_MHD
      use cal_sph_exp_1st_diff
      use cal_sph_exp_nod_none_bc
      use const_wz_coriolis_rtp
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_press, is_grad
!
!
      call cal_sph_nod_gradient_2(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &    d_rj(1,is_press), d_rj(1,is_grad) )
!
      call delete_bc_rj_vector(nidx_rj(2), sph_bc_U%kr_in,  is_grad)
      call delete_bc_rj_vector(nidx_rj(2), sph_bc_U%kr_out, is_grad)
!
!$omp parallel
      call ovwrt_rj_coef_prod_vect_smp( (-coef_press), is_grad)
!$omp end parallel
!
      end subroutine const_pressure_gradient
!
! -----------------------------------------------------------------------
!
      end module const_sph_radial_grad
