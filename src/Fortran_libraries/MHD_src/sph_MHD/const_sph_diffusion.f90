!> @file  const_sph_diffusion.f90
!!      module const_sph_diffusion
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate diffusion terms explicitly
!!
!!@verbatim
!!      subroutine const_sph_viscous_diffusion
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: ipol%i_v_diffuse, itor%i_v_diffuse, idpdr%i_v_diffuse
!!      subroutine const_sph_vorticirty_diffusion
!!        Input:    ipol%i_vort, itor%i_vort
!!        Solution: ipol%i_w_diffuse, itor%i_w_diffuse, idpdr%i_w_diffuse
!!      subroutine const_sph_magnetic_diffusion
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: ipol%i_b_diffuse, itor%i_b_diffuse, idpdr%i_b_diffuse
!!      subroutine const_sph_thermal_diffusion
!!        Input:    ipol%i_temp
!!        Solution: ipol%i_t_diffuse
!!      subroutine const_sph_composit_diffusion
!!        Input:    ipol%i_light
!!        Solution: ipol%i_c_diffuse
!!@endverbatim
!
      module const_sph_diffusion
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_physical_property
      use m_control_params_sph_MHD
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
      subroutine const_sph_viscous_diffusion
!
      use m_boundary_params_sph_MHD
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
      use cal_sph_exp_fixed_scalar
      use set_sph_exp_nod_center
      use cal_inner_core_rotation
!
!
      call cal_sph_nod_vect_diffuse2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    coef_d_velo, ipol%i_velo, ipol%i_v_diffuse)
      call cal_sph_nod_vect_dr_2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    d_rj(1,ipol%i_v_diffuse), d_rj(1,idpdr%i_v_diffuse) )
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_diffuse2(coef_d_velo,                   &
     &      ipol%i_velo, ipol%i_v_diffuse)
        call cal_dsdr_sph_center_2(ipol%i_v_diffuse)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_diffuse2(coef_d_velo,                 &
     &      ipol%i_velo, ipol%i_v_diffuse)
      else
        call cal_sph_nod_icb_rigid_diffuse2(coef_d_velo,                &
     &      ipol%i_velo, ipol%i_v_diffuse)
      end if
      call cal_dsdr_sph_icb_nobc_2(ipol%i_v_diffuse, idpdr%i_v_diffuse)
!
!   Ovewrite rotatable inner core 
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_icore_viscous_drag_explicit(coef_d_velo,               &
     &      itor%i_velo, itor%i_v_diffuse)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_diffuse2(coef_d_velo,                 &
     &      ipol%i_velo, ipol%i_v_diffuse)
      else
        call cal_sph_nod_cmb_rigid_diffuse2(coef_d_velo,                &
     &      ipol%i_velo, ipol%i_v_diffuse)
      end if
      call cal_dsdr_sph_cmb_nobc_2(ipol%i_v_diffuse, idpdr%i_v_diffuse)
!
      end subroutine const_sph_viscous_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_vorticirty_diffusion
!
      use m_boundary_params_sph_MHD
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
      use cal_sph_exp_fixed_scalar
      use set_sph_exp_nod_center
      use cal_inner_core_rotation
!
!
      call cal_sph_nod_vect_diffuse2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    coef_d_velo, ipol%i_vort, ipol%i_w_diffuse)
      call cal_sph_nod_vect_dr_2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    d_rj(1,ipol%i_w_diffuse), d_rj(1,idpdr%i_w_diffuse) )
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_diffuse2(coef_d_velo,                   &
     &      ipol%i_vort, ipol%i_w_diffuse)
        call cal_dsdr_sph_center_2(ipol%i_w_diffuse)
      else if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_w_diffuse2(coef_d_velo,               &
     &      ipol%i_vort, ipol%i_w_diffuse)
      else
        call cal_sph_nod_icb_rgd_w_diffuse2(coef_d_velo,                &
     &      ipol%i_vort, ipol%i_w_diffuse)
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_icore_viscous_drag_explicit(coef_d_velo,               &
     &      ipol%i_vort, ipol%i_w_diffuse)
      end if
!
      call cal_dsdr_sph_icb_nobc_2(ipol%i_w_diffuse, idpdr%i_w_diffuse)
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_w_diffuse2(coef_d_velo,               &
     &      ipol%i_vort, ipol%i_w_diffuse)
      else
        call cal_sph_nod_cmb_rgd_w_diffuse2(coef_d_velo,                &
     &      ipol%i_vort, ipol%i_w_diffuse)
      end if
!
      call cal_dsdr_sph_cmb_nobc_2(ipol%i_w_diffuse, idpdr%i_w_diffuse)
!
      end subroutine const_sph_vorticirty_diffusion
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_magnetic_diffusion
!
      use m_boundary_params_sph_MHD
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_icb_qvac
      use cal_sph_exp_nod_cmb_qvac
      use set_sph_exp_nod_center
      use cal_sph_exp_fixed_scalar
!
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_nod_center_diffuse2(coef_d_magne,                  &
     &      ipol%i_magne, ipol%i_b_diffuse)
        call cal_dsdr_sph_center_2(ipol%i_b_diffuse)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_diffuse2(coef_d_magne,                 &
     &      ipol%i_magne, ipol%i_b_diffuse)
        call cal_dsdr_sph_icb_nobc_2(ipol%i_b_diffuse,                  &
     &      idpdr%i_b_diffuse)
      else
        call cal_sph_nod_icb_ins_diffuse2(coef_d_magne,                 &
     &      ipol%i_magne, ipol%i_b_diffuse)
        call cal_dsdr_sph_icb_nobc_2(ipol%i_b_diffuse,                  &
     &      idpdr%i_b_diffuse)
      end if
!
!
      call cal_sph_nod_vect_diffuse2(sph_bc_B%kr_in, sph_bc_B%kr_out,   &
     &     coef_d_magne, ipol%i_magne, ipol%i_b_diffuse)
      call cal_sph_nod_vect_dr_2(sph_bc_B%kr_in, sph_bc_B%kr_out,       &
     &    d_rj(1,ipol%i_b_diffuse), d_rj(1,idpdr%i_b_diffuse) )
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_diffuse2(coef_d_magne,                 &
     &      ipol%i_magne, ipol%i_b_diffuse)
      else
        call cal_sph_nod_cmb_ins_diffuse2(coef_d_magne,                 &
     &      ipol%i_magne, ipol%i_b_diffuse)
      end if
      call cal_dsdr_sph_cmb_nobc_2(ipol%i_b_diffuse, idpdr%i_b_diffuse)
!
      end subroutine const_sph_magnetic_diffusion
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_thermal_diffusion
!
      use m_machine_parameter
      use m_control_params_sph_MHD
      use m_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
!
      call cal_sph_nod_scalar_diffuse2(sph_bc_T%kr_in, sph_bc_T%kr_out, &
     &    coef_d_temp, ipol%i_temp, ipol%i_t_diffuse)
!
      if (sph_bc_T%iflag_icb .eq. iflag_fixed_flux) then
        call cal_sph_icb_fix_flux_diffuse2(nidx_rj(2), h_flux_ICB_bc,   &
     &      coef_d_temp, ipol%i_temp, ipol%i_t_diffuse)
      else
        call cal_sph_icb_fix_scalar_diffuse2(nidx_rj(2), temp_ICB_bc,   &
     &      coef_d_temp, ipol%i_temp, ipol%i_t_diffuse)
      end if
!
      if (sph_bc_T%iflag_cmb .eq. iflag_fixed_flux) then
        call cal_sph_cmb_fix_flux_diffuse2(nidx_rj(2), h_flux_CMB_bc,   &
     &      coef_d_temp, ipol%i_temp, ipol%i_t_diffuse)
      else
        call cal_sph_cmb_fix_scalar_diffuse2(nidx_rj(2), temp_CMB_bc,   &
     &      coef_d_temp, ipol%i_temp, ipol%i_t_diffuse)
      end if
!
      end subroutine const_sph_thermal_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_composit_diffusion
!
      use m_control_params_sph_MHD
      use m_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
!
      call cal_sph_nod_scalar_diffuse2(sph_bc_C%kr_in, sph_bc_C%kr_out, &
     &    coef_d_light, ipol%i_light, ipol%i_c_diffuse)
!
      if (sph_bc_C%iflag_icb .eq. iflag_fixed_flux) then
        call cal_sph_icb_fix_flux_diffuse2(nidx_rj(2), c_flux_ICB_bc,   &
     &      coef_d_light, ipol%i_light, ipol%i_c_diffuse)
      else
        call cal_sph_icb_fix_scalar_diffuse2(nidx_rj(2),                &
     &      composition_ICB_bc, coef_d_light,                           &
     &      ipol%i_light, ipol%i_c_diffuse)
      end if
!
      if (sph_bc_C%iflag_cmb .eq. iflag_fixed_flux) then
        call cal_sph_cmb_fix_flux_diffuse2(nidx_rj(2), c_flux_CMB_bc,   &
     &      coef_d_light, ipol%i_light, ipol%i_c_diffuse)
      else
        call cal_sph_cmb_fix_scalar_diffuse2(nidx_rj(2),                &
     &      composition_CMB_bc, coef_d_light,                           &
     &      ipol%i_light, ipol%i_c_diffuse)
      end if
!
      end subroutine const_sph_composit_diffusion
!
! -----------------------------------------------------------------------
!
      end module const_sph_diffusion
