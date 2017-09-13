!>@file   set_field_address.f90
!!        module set_field_address
!!
!! @author H. Matsui
!! @date   Programmed on July, 2006
!! @n      Modified  on Jan., 2012
!!
!
!> @brief Set start address for field data
!!
!!@verbatim
!!      subroutine init_field_address(numnod, fld, iphys)
!!        type(phys_data), intent(inout) :: fld
!!        type(phys_address), intent(inout) :: iphys
!!      subroutine set_field_addresses(istart_adress, num_field,        &
!!     &          field_name, num_component, iphys)
!!      integer(kind = kint), intent(in) :: istart_adress
!!      integer(kind = kint), intent(in) :: num_field
!!      integer(kind = kint), intent(in) :: num_component(num_field)
!!      character(len = kchara), intent(in) :: field_name(num_field)
!!      type(phys_address), intent(inout) :: iphys
!!@endverbatim
!!
!!@n @param istart_adress             start address for field data
!!@n @param num_field                 number of field
!!@n @param num_component(num_field)  number of components of field
!!@n @param field_name(num_field)     list of field names
!!@n @param iphys                     structure of field addresses
!
!
      module set_field_address
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
!
      implicit none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine init_field_address(numnod, fld, iphys)
!
      integer(kind = kint), intent(in) :: numnod
      type(phys_data), intent(inout) :: fld
      type(phys_address), intent(inout) :: iphys
!
!
      call alloc_phys_data_type(numnod, fld)
      call set_field_addresses(ione, fld%num_phys,                      &
     &    fld%phys_name, fld%num_component, iphys)
!
      end subroutine init_field_address
!
!  --------------------------------------------------------------------
!
      subroutine set_field_addresses(istart_adress, num_field,          &
     &          field_name, num_component, iphys)
!
      use m_phys_labels
!
      integer(kind = kint), intent(in) :: istart_adress
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: num_component(num_field)
      character(len = kchara), intent(in) :: field_name(num_field)
!
      type(phys_address), intent(inout) :: iphys
!
      integer(kind = kint) :: i, i0
!
!
      i0 = istart_adress
      do i = 1, num_field
!
        if      ( field_name(i) .eq. fhd_velo ) then
          iphys%i_velo = i0
        else if ( field_name(i) .eq. fhd_press ) then
          iphys%i_press = i0
        else if ( field_name(i) .eq. fhd_temp ) then
          iphys%i_temp = i0
        else if ( field_name(i) .eq. fhd_part_temp ) then
          iphys%i_par_temp = i0
        else if ( field_name(i) .eq. fhd_vort ) then
          iphys%i_vort = i0
        else if ( field_name(i) .eq. fhd_light ) then
          iphys%i_light = i0
        else if ( field_name(i) .eq. fhd_part_light ) then
          iphys%i_par_light = i0
        else if ( field_name(i) .eq. fhd_magne ) then
          iphys%i_magne = i0
        else if ( field_name(i) .eq. fhd_vecp ) then
          iphys%i_vecp = i0
        else if ( field_name(i) .eq. fhd_mag_potential ) then
          iphys%i_mag_p = i0
        else if ( field_name(i) .eq. fhd_scalar_potential ) then
          iphys%i_scalar_p = i0
        else if ( field_name(i) .eq. fhd_current ) then
          iphys%i_current = i0
        else if ( field_name(i) .eq. fhd_e_field ) then
          iphys%i_electric = i0
        else if ( field_name(i) .eq. fhd_poynting ) then
          iphys%i_poynting = i0
        else if ( field_name(i) .eq. fhd_entropy ) then
          iphys%i_entropy = i0
        else if ( field_name(i) .eq. fhd_per_entropy ) then
          iphys%i_par_entropy = i0
        else if ( field_name(i) .eq. fhd_ref_entropy ) then
          iphys%i_ref_entropy = i0
        else if ( field_name(i) .eq. fhd_density ) then
          iphys%i_density = i0
        else if ( field_name(i) .eq. fhd_per_density ) then
          iphys%i_par_density = i0
        else if ( field_name(i) .eq. fhd_ref_density ) then
          iphys%i_ref_density = i0
!
        else if ( field_name(i) .eq. fhd_heat_source ) then
          iphys%i_heat_source = i0
        else if ( field_name(i) .eq. fhd_light_source ) then
          iphys%i_light_source = i0
        else if ( field_name(i) .eq. fhd_entropy_source ) then
          iphys%i_entropy_source = i0
!
        else if ( field_name(i) .eq. fhd_div_v ) then
          iphys%i_div_v =    i0
        else if ( field_name(i) .eq. fhd_div_b ) then
          iphys%i_div_b =    i0
        else if ( field_name(i) .eq. fhd_div_a ) then
          iphys%i_div_a =    i0
        end if
!
        if ( field_name(i) .eq. fhd_filter_velo ) then
          iphys%i_filter_velo =    i0
        else if ( field_name(i) .eq. fhd_filter_vort ) then
          iphys%i_filter_vort =    i0
        else if ( field_name(i) .eq. fhd_filter_temp ) then
          iphys%i_filter_temp =    i0
        else if ( field_name(i) .eq. fhd_filter_vecp ) then
          iphys%i_filter_vecp =    i0
        else if ( field_name(i) .eq. fhd_filter_magne ) then
          iphys%i_filter_magne =   i0
        else if ( field_name(i) .eq. fhd_filter_current ) then
          iphys%i_filter_current = i0
        else if ( field_name(i) .eq. fhd_filter_part_temp ) then
          iphys%i_filter_par_t =   i0
        else if ( field_name(i) .eq. fhd_filter_comp ) then
          iphys%i_filter_comp =    i0
!
        else if ( field_name(i) .eq. fhd_div_filter_v ) then
          iphys%i_div_filter_v =    i0
        else if ( field_name(i) .eq. fhd_div_filter_b ) then
          iphys%i_div_filter_b =    i0
        else if ( field_name(i) .eq. fhd_div_filter_a ) then
          iphys%i_div_filter_a =    i0
!
        else if ( field_name(i) .eq. fhd_kinetic_helicity ) then
          iphys%i_k_heli = i0
        else if ( field_name(i) .eq. fhd_magnetic_helicity ) then
          iphys%i_m_heli = i0
        else if ( field_name(i) .eq. fhd_current_helicity ) then
          iphys%i_c_heli = i0
        else if ( field_name(i) .eq. fhd_cross_helicity ) then
          iphys%i_x_heli = i0
!
        else if (field_name(i) .eq. fhd_mag_ene_gen) then
          iphys%i_me_gen =   i0
        else if (field_name(i) .eq. fhd_Lorentz_work) then
          iphys%i_ujb =      i0
        else if (field_name(i) .eq. fhd_work_agst_Lorentz) then
          iphys%i_nega_ujb = i0
        else if (field_name(i) .eq. fhd_mag_tension_work) then
          iphys%i_m_tension_wk = i0
        else if (field_name(i) .eq. fhd_buoyancy_flux) then
          iphys%i_buo_gen =   i0
        else if (field_name(i) .eq. fhd_comp_buo_flux) then
          iphys%i_c_buo_gen = i0
        else if (field_name(i) .eq. fhd_filter_buo_flux) then
          iphys%i_f_buo_gen = i0
        else if (field_name(i) .eq. fhd_vis_ene_diffuse) then
          iphys%i_vis_e_diffuse = i0
        else if (field_name(i) .eq. fhd_mag_ene_diffuse) then
          iphys%i_mag_e_diffuse = i0
        else if (field_name(i) .eq. fhd_temp_generation) then
          iphys%i_temp_gen = i0
        else if (field_name(i) .eq. fhd_part_temp_gen) then
          iphys%i_par_t_gen = i0
        else if (field_name(i) .eq. fhd_part_comp_gen) then
          iphys%i_par_c_gen = i0
!
        else if (field_name(i) .eq. fhd_thermal_diffusion) then
          iphys%i_t_diffuse =  i0
        else if (field_name(i) .eq. fhd_viscous) then
          iphys%i_v_diffuse =  i0
        else if (field_name(i) .eq. fhd_w_viscous) then
          iphys%i_w_diffuse =  i0
        else if (field_name(i) .eq. fhd_vecp_diffuse) then
          iphys%i_vp_diffuse = i0
        else if (field_name(i) .eq. fhd_mag_diffuse) then
          iphys%i_b_diffuse = i0
        else if (field_name(i) .eq. fhd_c_diffuse) then
          iphys%i_c_diffuse =  i0
!
        else if (field_name(i) .eq. fhd_h_flux ) then
          iphys%i_h_flux =   i0
        else if (field_name(i) .eq. fhd_ph_flux ) then
          iphys%i_ph_flux =  i0
        else if (field_name(i) .eq. fhd_c_flux ) then
          iphys%i_c_flux =   i0
        else if (field_name(i) .eq. fhd_pc_flux ) then
          iphys%i_pc_flux =   i0
        else if (field_name(i) .eq. fhd_mom_flux ) then
          iphys%i_m_flux =   i0
        else if (field_name(i) .eq. fhd_maxwell_t ) then
          iphys%i_maxwell =  i0
        else if (field_name(i) .eq. fhd_induct_t ) then
          iphys%i_induct_t = i0
!
        else if (field_name(i) .eq. fhd_heat_advect ) then
          iphys%i_h_advect =    i0
        else if (field_name(i) .eq. fhd_part_h_advect ) then
          iphys%i_ph_advect =   i0
        else if (field_name(i) .eq. fhd_inertia) then
          iphys%i_m_advect =    i0
        else if (field_name(i) .eq. fhd_div_h_flux ) then
          iphys%i_h_flux_div =  i0
        else if (field_name(i) .eq. fhd_div_ph_flux ) then
          iphys%i_ph_flux_div = i0
        else if (field_name(i) .eq. fhd_div_c_flux ) then
          iphys%i_c_flux_div =  i0
        else if (field_name(i) .eq. fhd_div_pc_flux ) then
          iphys%i_pc_flux_div = i0
        else if (field_name(i) .eq. fhd_div_m_flux) then
          iphys%i_m_flux_div =  i0
        else if (field_name(i) .eq. fhd_div_maxwell_t) then
          iphys%i_maxwell_div = i0
        else if (field_name(i) .eq. fhd_div_induct_t) then
          iphys%i_induct_div =  i0
        else if (field_name(i) .eq. fhd_mag_induct) then
          iphys%i_induction =   i0
        else if (field_name(i) .eq. fhd_vp_induct) then
          iphys%i_vp_induct =   i0
        else if (field_name(i) .eq. fhd_mag_stretch) then
          iphys%i_mag_stretch = i0
        else if (field_name(i) .eq. fhd_press_grad) then
          iphys%i_press_grad =  i0
        else if (field_name(i) .eq. fhd_mag_tension) then
          iphys%i_m_tension =   i0
        else if (field_name(i) .eq. fhd_Lorentz) then
          iphys%i_lorentz =     i0
        else if (field_name(i) .eq. fhd_Coriolis) then
          iphys%i_coriolis =    i0
        else if (field_name(i) .eq. fhd_buoyancy) then
          iphys%i_buoyancy =    i0
        else if (field_name(i) .eq. fhd_comp_buo) then
          iphys%i_comp_buo =    i0
        else if (field_name(i) .eq. fhd_filter_buo) then
          iphys%i_filter_buo =  i0
        else if (field_name(i) .eq. fhd_composit_advect ) then
          iphys%i_c_advect =    i0
        else if (field_name(i) .eq. fhd_part_c_advect ) then
          iphys%i_pc_advect =   i0
        end if
!
        if ( field_name(i) .eq. fhd_SGS_h_flux ) then
          iphys%i_SGS_h_flux =   i0
        else if ( field_name(i) .eq. fhd_SGS_c_flux ) then
          iphys%i_SGS_c_flux =   i0
        else if ( field_name(i) .eq. fhd_SGS_m_flux ) then
          iphys%i_SGS_m_flux =   i0
        else if ( field_name(i) .eq. fhd_SGS_maxwell_t ) then
          iphys%i_SGS_maxwell =  i0
        else if ( field_name(i) .eq. fhd_SGS_induct_t ) then
          iphys%i_SGS_induct_t = i0
        else if ( field_name(i) .eq. fhd_SGS_inertia ) then
          iphys%i_SGS_inertia = i0
        else if ( field_name(i) .eq. fhd_wide_SGS_h_flux ) then
          iphys%i_wide_SGS_h_flux = i0
        else if ( field_name(i) .eq. fhd_wide_SGS_c_flux ) then
          iphys%i_wide_SGS_c_flux = i0
        else if ( field_name(i) .eq. fhd_wide_SGS_inertia ) then
          iphys%i_wide_SGS_inertia = i0
        else if ( field_name(i) .eq. fhd_wide_SGS_Lorentz ) then
          iphys%i_wide_SGS_Lorentz = i0
        else if (field_name(i) .eq. fhd_wide_SGS_vp_induct) then
          iphys%i_wide_SGS_vp_induct =  i0
        end if
!
        if ( field_name(i) .eq. fhd_Csim_SGS_h_flux ) then
          iphys%i_Csim_SGS_h_flux =   i0
        else if ( field_name(i) .eq. fhd_Csim_SGS_c_flux ) then
          iphys%i_Csim_SGS_c_flux =   i0
        else if ( field_name(i) .eq. fhd_Csim_SGS_m_flux ) then
          iphys%i_Csim_SGS_m_flux =   i0
        else if ( field_name(i) .eq. fhd_Csim_SGS_Lorentz ) then
          iphys%i_Csim_SGS_Lorentz =  i0
        else if ( field_name(i) .eq. fhd_Csim_SGS_induction ) then
          iphys%i_Csim_SGS_induction = i0
        else if ( field_name(i) .eq. fhd_Csim_SGS_buoyancy ) then
          iphys%i_Csim_SGS_buoyancy = i0
        else if ( field_name(i) .eq. fhd_Csim_SGS_comp_buo ) then
          iphys%i_Csim_SGS_comp_buo = i0
        end if
!
        if ( field_name(i) .eq. fhd_div_SGS_h_flux ) then
          iphys%i_SGS_div_h_flux = i0
        else if ( field_name(i) .eq. fhd_div_SGS_c_flux ) then
          iphys%i_SGS_div_c_flux = i0
        else if (field_name(i) .eq. fhd_div_SGS_m_flux) then
          iphys%i_SGS_div_m_flux = i0
        else if (field_name(i) .eq. fhd_SGS_Lorentz) then
          iphys%i_SGS_Lorentz =    i0
        else if (field_name(i) .eq. fhd_SGS_induction) then
          iphys%i_SGS_induction =  i0
        else if (field_name(i) .eq. fhd_SGS_vp_induct) then
          iphys%i_SGS_vp_induct =  i0
        else if ( field_name(i) .eq. fhd_SGS_buoyancy ) then
          iphys%i_SGS_buoyancy =   i0
        else if ( field_name(i) .eq. fhd_SGS_comp_buo ) then
          iphys%i_SGS_comp_buo =   i0
!
        else if ( field_name(i) .eq. fhd_SGS_rot_inertia ) then
          iphys%i_SGS_rot_inertia =   i0
        else if ( field_name(i) .eq. fhd_SGS_div_inertia ) then
          iphys%i_SGS_div_inertia =   i0
        else if ( field_name(i) .eq. fhd_SGS_rot_Lorentz ) then
          iphys%i_SGS_rot_Lorentz =   i0
        else if ( field_name(i) .eq. fhd_SGS_div_Lorentz ) then
          iphys%i_SGS_div_Lorentz =   i0
!
        else if ( field_name(i) .eq. fhd_SGS_temp_gen ) then
          iphys%i_SGS_temp_gen =     i0
        else if ( field_name(i) .eq. fhd_SGS_m_ene_gen ) then
          iphys%i_SGS_me_gen =       i0
        else if ( field_name(i) .eq. fhd_SGS_Lorentz_work ) then
          iphys%i_SGS_Lor_wk =       i0
        else if ( field_name(i) .eq. fhd_Reynolds_work ) then
          iphys%i_reynolds_wk =      i0
        else if ( field_name(i) .eq. fhd_SGS_buo_flux ) then
          iphys%i_SGS_buo_wk =       i0
        else if ( field_name(i) .eq. fhd_SGS_comp_buo_flux ) then
          iphys%i_SGS_comp_buo_wk =  i0
!
        else if ( field_name(i) .eq. fhd_geostrophic ) then
          iphys%i_geostrophic =  i0
!
        else if ( field_name(i) .eq. fhd_h_flux_w_sgs ) then
          iphys%i_h_flux_w_sgs =  i0
        else if ( field_name(i) .eq. fhd_c_flux_w_sgs ) then
          iphys%i_c_flux_w_sgs =  i0
        else if ( field_name(i) .eq. fhd_inertia_w_sgs ) then
          iphys%i_inertia_w_sgs =  i0
        else if ( field_name(i) .eq. fhd_Lorentz_w_sgs ) then
          iphys%i_Lorentz_w_sgs =  i0
        else if ( field_name(i) .eq. fhd_vp_induct_w_sgs ) then
          iphys%i_vp_induct_w_sgs =  i0
        else if ( field_name(i) .eq. fhd_mag_induct_w_sgs ) then
          iphys%i_mag_induct_w_sgs =  i0
        else if ( field_name(i) .eq. fhd_mom_flux_w_sgs ) then
          iphys%i_mom_flux_w_sgs =  i0
        else if ( field_name(i) .eq. fhd_maxwell_t_w_sgs ) then
          iphys%i_maxwell_t_w_sgs =  i0
!
        else if ( field_name(i) .eq. fhd_SGS_div_h_flux_true ) then
          iphys%i_SGS_div_hf_true = i0
        else if ( field_name(i) .eq. fhd_SGS_div_c_flux_true ) then
          iphys%i_SGS_div_cf_true = i0
        else if ( field_name(i) .eq. fhd_SGS_div_m_flux_true ) then
          iphys%i_SGS_div_mf_true = i0
        else if ( field_name(i) .eq. fhd_SGS_Lorentz_true ) then
          iphys%i_SGS_Lor_true =    i0
        else if ( field_name(i) .eq. fhd_SGS_mag_induct_true ) then
          iphys%i_SGS_idct_true =   i0
!
        else if ( field_name(i) .eq. fhd_SGS_Lorentz_wk_true ) then
          iphys%i_SGS_Lor_wk_tr =  i0
        else if ( field_name(i) .eq. fhd_Reynolds_work_true ) then
          iphys%i_reynolds_wk_tr = i0
        else if ( field_name(i) .eq. fhd_SGS_temp_gen_true ) then
          iphys%i_SGS_t_gen_tr =   i0
        else if ( field_name(i) .eq. fhd_SGS_comp_gen_true ) then
          iphys%i_SGS_c_gen_tr =   i0
        else if ( field_name(i) .eq. fhd_SGS_m_ene_gen_true ) then
          iphys%i_SGS_me_gen_tr =  i0
        end if
!
        if ( field_name(i) .eq. fhd_press_work ) then
          iphys%i_p_phi = i0
        else if ( field_name(i) .eq. fhd_m_potential_work ) then
          iphys%i_m_phi = i0
        else if ( field_name(i) .eq. fhd_ref_temp ) then
          iphys%i_ref_t = i0
        else if ( field_name(i) .eq. fhd_ref_light ) then
          iphys%i_ref_c = i0
!
        else if ( field_name(i) .eq. fhd_grad_v_1 ) then
          iphys%i_grad_vx =      i0
        else if ( field_name(i) .eq. fhd_grad_v_2 ) then
          iphys%i_grad_vy =      i0
        else if ( field_name(i) .eq. fhd_grad_v_3 ) then
          iphys%i_grad_vz =      i0
        else if ( field_name(i) .eq. fhd_grad_w_1 ) then
          iphys%i_grad_wx =      i0
        else if ( field_name(i) .eq. fhd_grad_w_2 ) then
          iphys%i_grad_wy =      i0
        else if ( field_name(i) .eq. fhd_grad_w_3 ) then
          iphys%i_grad_wz =      i0
        else if ( field_name(i) .eq. fhd_grad_a_1 ) then
          iphys%i_grad_ax =      i0
        else if ( field_name(i) .eq. fhd_grad_a_2 ) then
          iphys%i_grad_ay =      i0
        else if ( field_name(i) .eq. fhd_grad_a_3 ) then
          iphys%i_grad_az =      i0
        else if ( field_name(i) .eq. fhd_grad_b_1 ) then
          iphys%i_grad_bx =      i0
        else if ( field_name(i) .eq. fhd_grad_b_2 ) then
          iphys%i_grad_by =      i0
        else if ( field_name(i) .eq. fhd_grad_b_3 ) then
          iphys%i_grad_bz =      i0
        else if ( field_name(i) .eq. fhd_grad_j_1 ) then
          iphys%i_grad_jx =      i0
        else if ( field_name(i) .eq. fhd_grad_j_2 ) then
          iphys%i_grad_jy =      i0
        else if ( field_name(i) .eq. fhd_grad_j_3 ) then
          iphys%i_grad_jz =      i0
!
        else if ( field_name(i) .eq. fhd_grad_temp ) then
          iphys%i_grad_t =           i0
        else if ( field_name(i) .eq. fhd_grad_par_temp ) then
          iphys%i_grad_part_t =      i0
        else if ( field_name(i) .eq. fhd_grad_ref_temp ) then
          iphys%i_gref_t =           i0
        else if ( field_name(i) .eq. fhd_grad_filter_temp ) then
          iphys%i_grad_filter_temp = i0
        else if ( field_name(i) .eq. fhd_grad_composit ) then
          iphys%i_grad_composit =    i0
        else if ( field_name(i) .eq. fhd_grad_par_light ) then
          iphys%i_grad_part_c =      i0
        else if ( field_name(i) .eq. fhd_grad_ref_light) then
          iphys%i_gref_c =           i0
        end if
!
        if ( field_name(i) .eq. fhd_SGS_simi ) then
          iphys%i_sgs_simi =     i0
        else if ( field_name(i) .eq. fhd_SGS_grad ) then
          iphys%i_sgs_grad =     i0
        else if ( field_name(i) .eq. fhd_SGS_grad_f ) then
          iphys%i_sgs_grad_f =   i0
        else if ( field_name(i) .eq. fhd_SGS_diffuse) then
          iphys%i_sgs_diffuse =  i0
        else if ( field_name(i) .eq. fhd_SGS_temp) then
          iphys%i_sgs_temp =     i0
        else if ( field_name(i) .eq. fhd_SGS_comp) then
          iphys%i_sgs_composit = i0
!
        else if ( field_name(i) .eq. fhd_w_filter_velo ) then
          iphys%i_wide_fil_velo =    i0
        else if ( field_name(i) .eq. fhd_w_filter_vort ) then
          iphys%i_wide_fil_vort =    i0
        else if ( field_name(i) .eq. fhd_w_filter_temp ) then
          iphys%i_wide_fil_temp =    i0
        else if ( field_name(i) .eq. fhd_w_filter_comp ) then
          iphys%i_wide_fil_comp =    i0
        else if ( field_name(i) .eq. fhd_w_filter_vecp ) then
          iphys%i_wide_fil_vecp =    i0
        else if ( field_name(i) .eq. fhd_w_filter_magne ) then
          iphys%i_wide_fil_magne =   i0
        else if ( field_name(i) .eq. fhd_w_filter_current ) then
          iphys%i_wide_fil_current = i0
        end if
!
        if ( field_name(i) .eq. fhd_div_inertia ) then
          iphys%i_div_inertia =    i0
        else if ( field_name(i) .eq. fhd_div_Lorentz ) then
          iphys%i_div_Lorentz =    i0
        else if ( field_name(i) .eq. fhd_div_Coriolis ) then
          iphys%i_div_Coriolis =   i0
        else if ( field_name(i) .eq. fhd_div_buoyancy ) then
          iphys%i_div_buoyancy =   i0
        else if ( field_name(i) .eq. fhd_div_comp_buo ) then
          iphys%i_div_comp_buo =   i0
        else if ( field_name(i) .eq. fhd_div_filter_buo ) then
          iphys%i_div_filter_buo = i0
        else if ( field_name(i) .eq. fhd_div_viscous ) then
          iphys%i_div_viscous =    i0
!
        else if ( field_name(i) .eq. fhd_rot_inertia ) then
          iphys%i_rot_inertia =    i0
        else if ( field_name(i) .eq. fhd_rot_Lorentz ) then
          iphys%i_rot_Lorentz =    i0
        else if ( field_name(i) .eq. fhd_rot_Coriolis ) then
          iphys%i_rot_Coriolis =   i0
        else if ( field_name(i) .eq. fhd_rot_buoyancy ) then
          iphys%i_rot_buoyancy =   i0
        else if ( field_name(i) .eq. fhd_rot_comp_buo ) then
          iphys%i_rot_comp_buo =   i0
        else if ( field_name(i) .eq. fhd_rot_filter_buo ) then
          iphys%i_rot_filter_buo = i0
!
        else if ( field_name(i) .eq. fhd_forces ) then
          iphys%i_forces =     i0
        else if ( field_name(i) .eq. fhd_rot_forces ) then
          iphys%i_rot_forces = i0
        else if ( field_name(i) .eq. fhd_div_forces ) then
          iphys%i_div_forces = i0
        end if
!
        if ( field_name(i) .eq. fhd_pre_mom ) then
          iphys%i_pre_mom =      i0
        else if ( field_name(i) .eq. fhd_pre_uxb ) then
          iphys%i_pre_uxb =      i0
        else if ( field_name(i) .eq. fhd_pre_heat ) then
          iphys%i_pre_heat =     i0
        else if ( field_name(i) .eq. fhd_pre_composit ) then
          iphys%i_pre_composit = i0
        else if ( field_name(i) .eq. fhd_pre_press ) then
          iphys%i_pre_press =    i0
!
        else if ( field_name(i) .eq. fhd_chk_mom ) then
          iphys%i_chk_mom =       i0
        else if ( field_name(i) .eq. fhd_chk_uxb ) then
          iphys%i_chk_uxb =       i0
        else if ( field_name(i) .eq. fhd_chk_heat ) then
          iphys%i_chk_heat =      i0
        else if ( field_name(i) .eq. fhd_chk_composit ) then
          iphys%i_chk_composit =  i0
        else if ( field_name(i) .eq. fhd_chk_press ) then
          iphys%i_chk_press =     i0
        else if ( field_name(i) .eq. fhd_chk_potential ) then
          iphys%i_chk_potential = i0
!
        else if ( field_name(i) .eq. fhd_chk_mom_2 ) then
          iphys%i_chk_mom_2 =       i0
        else if ( field_name(i) .eq. fhd_chk_uxb_2 ) then
          iphys%i_chk_uxb_2 =       i0
        else if ( field_name(i) .eq. fhd_chk_heat_2 ) then
          iphys%i_chk_heat_2 =      i0
        else if ( field_name(i) .eq. fhd_chk_composit_2 ) then
          iphys%i_chk_composit_2 =  i0
        else if ( field_name(i) .eq. fhd_chk_press_2 ) then
          iphys%i_chk_press_2 =     i0
        else if ( field_name(i) .eq. fhd_chk_potential_2 ) then
          iphys%i_chk_potential_2 = i0
        end if
!
        if ( field_name(i) .eq. fhd_velocity_scale) then
          iphys%i_velo_scale =  i0
        else if ( field_name(i) .eq. fhd_magnetic_scale) then
          iphys%i_magne_scale = i0
        else if ( field_name(i) .eq. fhd_temp_scale) then
          iphys%i_temp_scale =  i0
        else if ( field_name(i) .eq. fhd_composition_scale) then
          iphys%i_comp_scale =  i0
        end if
!
!   Old field label... Should be deleted later!!
        if (field_name(i) .eq. fhd_buoyancy_work) then
          iphys%i_buo_gen =   i0
        end if
!
        i0 = i0 + num_component(i)
      end do
!
      end subroutine set_field_addresses
!
!  --------------------------------------------------------------------
!
      end module set_field_address
