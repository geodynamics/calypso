!>@file   m_sph_phys_address.f90
!!@brief  module m_sph_phys_address
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2007
!
!>@brief  start addresses for spetr fields
!!
!!@verbatim
!!      subroutine set_sph_sprctr_data_address
!!@endverbatim
!
      module m_sph_phys_address
!
      use m_precision
      use m_constants
!
      use t_phys_address
!
      implicit  none
!
!>   address for spectr data (poloidal component for vector)
      type(phys_address), save :: ipol
!
!>   address for radial gradient for poloidal component
      type(phys_address), save :: idpdr
!
!>   address for toroidal component
      type(phys_address), save :: itor
!
      private :: set_sph_vect_spec_address, set_vect_sph_address
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_sph_sprctr_data_address
!
      use m_sph_spectr_data
      use set_field_address
!
!   set address of spectr fields
!
      call set_field_addresses(ione, num_phys_rj,                       &
     &    phys_name_rj, num_phys_comp_rj, ipol)
!
      call set_sph_vect_spec_address
!
      end subroutine set_sph_sprctr_data_address
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine set_sph_vect_spec_address
!
!
      call set_vect_sph_address(ipol%i_velo, itor%i_velo, idpdr%i_velo)
      call set_vect_sph_address(ipol%i_vort, itor%i_vort, idpdr%i_vort)
!
      call set_vect_sph_address(ipol%i_magne, itor%i_magne,             &
     &    idpdr%i_magne)
      call set_vect_sph_address(ipol%i_vecp, itor%i_vecp,idpdr%i_vecp)
      call set_vect_sph_address(ipol%i_current, itor%i_current,         &
     &    idpdr%i_current)
!
      call set_vect_sph_address(ipol%i_filter_velo, itor%i_filter_velo, &
     &    idpdr%i_filter_velo)
      call set_vect_sph_address(ipol%i_filter_vecp, itor%i_filter_vecp, &
     &    idpdr%i_filter_vecp)
      call set_vect_sph_address(ipol%i_filter_magne,                    &
     &    itor%i_filter_magne, idpdr%i_filter_magne)
!
      call set_vect_sph_address(ipol%i_v_diffuse, itor%i_v_diffuse,     &
     &    idpdr%i_v_diffuse)
      call set_vect_sph_address(ipol%i_w_diffuse, itor%i_w_diffuse,     &
     &    idpdr%i_w_diffuse)
      call set_vect_sph_address(ipol%i_vp_diffuse, itor%i_vp_diffuse,   &
     &    idpdr%i_vp_diffuse)
      call set_vect_sph_address(ipol%i_b_diffuse, itor%i_b_diffuse,     &
     &    idpdr%i_b_diffuse)
!
      call set_vect_sph_address(ipol%i_h_flux, itor%i_h_flux,           &
     &    idpdr%i_h_flux)
      call set_vect_sph_address(ipol%i_ph_flux, itor%i_ph_flux,         &
     &    idpdr%i_ph_flux)
      call set_vect_sph_address(ipol%i_c_flux, itor%i_c_flux,           &
     &    idpdr%i_c_flux)
!
      call set_vect_sph_address(ipol%i_m_advect, itor%i_m_advect,       &
     &    idpdr%i_m_advect)
      call set_vect_sph_address(ipol%i_m_flux_div, itor%i_m_flux_div,   &
     &    idpdr%i_m_flux_div)
      call set_vect_sph_address(ipol%i_maxwell_div, itor%i_maxwell_div, &
     &    idpdr%i_maxwell_div)
      call set_vect_sph_address(ipol%i_induct_div, itor%i_induct_div,   &
     &    idpdr%i_induct_div)
      call set_vect_sph_address(ipol%i_induction, itor%i_induction,     &
     &    idpdr%i_induction)
      call set_vect_sph_address(ipol%i_vp_induct, itor%i_vp_induct,     &
     &    idpdr%i_vp_induct)
      call set_vect_sph_address(ipol%i_mag_stretch, itor%i_mag_stretch, &
     &    idpdr%i_mag_stretch)
      call set_vect_sph_address(ipol%i_m_tension, itor%i_m_tension,     &
     &    idpdr%i_m_tension)
      call set_vect_sph_address(ipol%i_lorentz, itor%i_lorentz,         &
     &    idpdr%i_lorentz)
      call set_vect_sph_address(ipol%i_coriolis, itor%i_coriolis,       &
     &    idpdr%i_coriolis)
      call set_vect_sph_address(ipol%i_buoyancy, itor%i_buoyancy,       &
     &    idpdr%i_buoyancy)
      call set_vect_sph_address(ipol%i_comp_buo, itor%i_comp_buo,       &
     &    idpdr%i_comp_buo)
      call set_vect_sph_address(ipol%i_filter_buo, itor%i_filter_buo,   &
     &    idpdr%i_filter_buo)
!
      call set_vect_sph_address(ipol%i_rot_inertia,                     &
     &    itor%i_rot_inertia, idpdr%i_rot_inertia)
      call set_vect_sph_address(ipol%i_rot_Lorentz,                     &
     &    itor%i_rot_Lorentz, idpdr%i_rot_Lorentz)
      call set_vect_sph_address(ipol%i_rot_Coriolis,                    &
     &    itor%i_rot_Coriolis, idpdr%i_rot_Coriolis)
      call set_vect_sph_address(ipol%i_rot_buoyancy,                    &
     &    itor%i_rot_buoyancy, idpdr%i_rot_buoyancy)
      call set_vect_sph_address(ipol%i_rot_comp_buo,                    &
     &    itor%i_rot_comp_buo, idpdr%i_rot_comp_buo)
      call set_vect_sph_address(ipol%i_rot_filter_buo,                  &
     &    itor%i_rot_filter_buo, idpdr%i_rot_filter_buo)
!
      call set_vect_sph_address(ipol%i_grad_t, itor%i_grad_t,           &
     &    idpdr%i_grad_t)
      call set_vect_sph_address(ipol%i_grad_part_t,                     &
     &    itor%i_grad_part_t, idpdr%i_grad_part_t)
      call set_vect_sph_address(ipol%i_grad_composit,                   &
     &    itor%i_grad_composit, idpdr%i_grad_composit)
      call set_vect_sph_address(ipol%i_grad_filter_temp,                &
     &    itor%i_grad_filter_temp, idpdr%i_grad_filter_temp)
!
!
      call set_vect_sph_address(ipol%i_grad_vx, itor%i_grad_vx,         &
     &    idpdr%i_grad_vx)
      call set_vect_sph_address(ipol%i_grad_vy, itor%i_grad_vy,         &
     &    idpdr%i_grad_vy)
      call set_vect_sph_address(ipol%i_grad_vz, itor%i_grad_vz,         &
     &    idpdr%i_grad_vz)
      call set_vect_sph_address(ipol%i_grad_wx, itor%i_grad_wx,         &
     &    idpdr%i_grad_wx)
      call set_vect_sph_address(ipol%i_grad_wy, itor%i_grad_wy,         &
     &    idpdr%i_grad_wy)
      call set_vect_sph_address(ipol%i_grad_wz, itor%i_grad_wz,         &
     &    idpdr%i_grad_wz)
!
      call set_vect_sph_address(ipol%i_grad_ax, itor%i_grad_ax,         &
     &    idpdr%i_grad_ax)
      call set_vect_sph_address(ipol%i_grad_ay, itor%i_grad_ay,         &
     &    idpdr%i_grad_ay)
      call set_vect_sph_address(ipol%i_grad_az, itor%i_grad_az,         &
     &    idpdr%i_grad_az)
      call set_vect_sph_address(ipol%i_grad_bx, itor%i_grad_bx,         &
     &    idpdr%i_grad_bx)
      call set_vect_sph_address(ipol%i_grad_by, itor%i_grad_by,         &
     &    idpdr%i_grad_by)
      call set_vect_sph_address(ipol%i_grad_bz, itor%i_grad_bz,         &
     &    idpdr%i_grad_bz)
      call set_vect_sph_address(ipol%i_grad_jx, itor%i_grad_jx,         &
     &    idpdr%i_grad_jx)
      call set_vect_sph_address(ipol%i_grad_jy, itor%i_grad_jy,         &
     &    idpdr%i_grad_jy)
      call set_vect_sph_address(ipol%i_grad_jz, itor%i_grad_jz,         &
     &    idpdr%i_grad_jz)
!
      call set_vect_sph_address(ipol%i_SGS_h_flux,                      &
     &    itor%i_SGS_h_flux, idpdr%i_SGS_h_flux)
      call set_vect_sph_address(ipol%i_SGS_c_flux,                      &
     &    itor%i_SGS_c_flux, idpdr%i_SGS_c_flux)
!
      call set_vect_sph_address(ipol%i_SGS_div_m_flux,                  &
     &    itor%i_SGS_div_m_flux, idpdr%i_SGS_div_m_flux)
      call set_vect_sph_address(ipol%i_SGS_Lorentz,                     &
     &    itor%i_SGS_Lorentz, idpdr%i_SGS_Lorentz)
      call set_vect_sph_address(ipol%i_SGS_induction,                   &
     &    itor%i_SGS_induction, idpdr%i_SGS_induction)
      call set_vect_sph_address(ipol%i_SGS_vp_induct,                   &
     &    itor%i_SGS_vp_induct, idpdr%i_SGS_vp_induct)
      call set_vect_sph_address(ipol%i_SGS_buoyancy,                    &
     &    itor%i_SGS_buoyancy, idpdr%i_SGS_buoyancy)
      call set_vect_sph_address(ipol%i_SGS_comp_buo,                    &
     &    itor%i_SGS_comp_buo, idpdr%i_SGS_comp_buo)
!
      call set_vect_sph_address(ipol%i_forces, itor%i_forces,           &
     &    idpdr%i_forces)
      call set_vect_sph_address(ipol%i_rot_forces, itor%i_rot_forces,   &
     &    idpdr%i_rot_forces)
!
      call set_vect_sph_address(ipol%i_pre_mom, itor%i_pre_mom,         &
     &    idpdr%i_pre_mom)
      call set_vect_sph_address(ipol%i_pre_uxb, itor%i_pre_uxb,         &
     &    idpdr%i_pre_uxb)
!
      call set_vect_sph_address(ipol%i_chk_mom, itor%i_chk_mom,         &
     &    idpdr%i_chk_mom)
      call set_vect_sph_address(ipol%i_chk_uxb, itor%i_chk_uxb,         &
     &    idpdr%i_chk_uxb)
!
      call set_vect_sph_address(ipol%i_chk_mom_2, itor%i_chk_mom_2,     &
     &    idpdr%i_chk_mom_2)
      call set_vect_sph_address(ipol%i_chk_uxb_2, itor%i_chk_uxb_2,     &
     &    idpdr%i_chk_uxb_2)
!
      end subroutine set_sph_vect_spec_address
!
!  --------------------------------------------------------------------
!
      subroutine set_vect_sph_address(i_pol, i_tor, i_dpol)
!
      integer(kind = kint), intent(in) :: i_pol
      integer(kind = kint), intent(inout) :: i_tor, i_dpol
!
!
      if(i_pol .le. 0) return
      i_tor =  i_pol + 2
      i_dpol = i_pol + 1
!
      end subroutine set_vect_sph_address
!
!  --------------------------------------------------------------------
!
      end module m_sph_phys_address
