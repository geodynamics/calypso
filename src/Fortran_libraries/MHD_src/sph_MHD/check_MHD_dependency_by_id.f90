!>@file   check_MHD_dependency_by_id.f90
!!@brief  module check_MHD_dependency_by_id
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Check dependecy of field list fro MHD dynamo
!!
!!@verbatim
!!      subroutine check_dependencies_by_id(cd_prop, iphys, fld)
!!      subroutine check_dependence_FEM_MHD_by_id(iphys, fld)
!!      subroutine check_dependence_SPH_MHD_by_id(iphys, fld)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: fld
!!@endverbatim
!
      module check_MHD_dependency_by_id
!
      use m_precision
      use m_error_IDs
      use m_phys_labels
!
      use calypso_mpi
!
      use t_physical_property
      use t_phys_address
      use t_phys_data
!
      implicit none
!
      private :: check_missing_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_dependencies_by_id(cd_prop, iphys, fld)
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint) :: i, i_start
!
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(     i_start .eq. iphys%i_filter_velo                        &
     &     .or. i_start .eq. iphys%i_wide_fil_velo                      &
     &     .or. i_start .eq. iphys%i_vort                               &
     &     .or. i_start .eq. iphys%i_press                              &
     &     .or. i_start .eq. iphys%i_magne                              &
     &     .or. i_start .eq. iphys%i_temp                               &
     &     .or. i_start .eq. iphys%i_light                              &
     &     .or. i_start .eq. iphys%i_v_diffuse                          &
     &     .or. i_start .eq. iphys%i_m_advect                           &
     &     .or. i_start .eq. iphys%i_m_flux                             &
     &     .or. i_start .eq. iphys%i_coriolis                           &
     &     .or. i_start .eq. iphys%i_SGS_m_flux                         &
     &     .or. i_start .eq. iphys%i_square_v                           &
     &     .or. i_start .eq. iphys%i_grad_vx                            &
     &     .or. i_start .eq. iphys%i_grad_vy                            &
     &     .or. i_start .eq. iphys%i_grad_vz) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
        else if(i_start .eq. iphys%i_filter_vort                        &
     &     .or. i_start .eq. iphys%i_wide_fil_vort                      &
     &     .or. i_start .eq. iphys%i_SGS_inertia                        &
     &     .or. i_start .eq. iphys%i_wide_SGS_inertia                   &
     &     .or. i_start .eq. iphys%i_velo_scale                         &
     &     .or. i_start .eq. iphys%i_k_heli                             &
     &     .or. i_start .eq. iphys%i_square_w                           &
     &     .or. i_start .eq. iphys%i_grad_wx                            &
     &     .or. i_start .eq. iphys%i_grad_wy                            &
     &     .or. i_start .eq. iphys%i_grad_wz ) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_vort, fhd_vort)
        else if(i_start .eq. iphys%i_press_grad) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_press, fhd_press)
!
        else if(i_start .eq. iphys%i_filter_magne                       &
     &     .or. i_start .eq. iphys%i_wide_fil_magne                     &
     &     .or. i_start .eq. iphys%i_current                            &
     &     .or. i_start .eq. iphys%i_b_diffuse                          &
     &     .or. i_start .eq. iphys%i_mag_p                              &
     &     .or. i_start .eq. iphys%i_m_tension                          &
     &     .or. i_start .eq. iphys%i_lorentz                            &
     &     .or. i_start .eq. iphys%i_maxwell                            &
     &     .or. i_start .eq. iphys%i_SGS_maxwell                        &
     &     .or. i_start .eq. iphys%i_square_b                           &
     &     .or. i_start .eq. iphys%i_grad_bx                            &
     &     .or. i_start .eq. iphys%i_grad_by                            &
     &     .or. i_start .eq. iphys%i_grad_bz                            &
     &     .or. i_start .eq. iphys%i_truncated_B) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
        else if(i_start .eq. iphys%i_filter_vecp                        &
     &     .or. i_start .eq. iphys%i_scalar_p                           &
     &     .or. i_start .eq. iphys%i_m_heli                             &
     &     .or. i_start .eq. iphys%i_vp_diffuse                         &
     &     .or. i_start .eq. iphys%i_square_a                           &
     &     .or. i_start .eq. iphys%i_grad_ax                            &
     &     .or. i_start .eq. iphys%i_grad_ay                            &
     &     .or. i_start .eq. iphys%i_grad_az) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_vecp, fhd_vecp)
        else if(i_start .eq. iphys%i_filter_current                     &
     &     .or. i_start .eq. iphys%i_wide_fil_current                   &
     &     .or. i_start .eq. iphys%i_c_heli                             &
     &     .or. i_start .eq. iphys%i_magne_scale                        &
     &     .or. i_start .eq. iphys%i_square_j                           &
     &     .or. i_start .eq. iphys%i_grad_jx                            &
     &     .or. i_start .eq. iphys%i_grad_jy                            &
     &     .or. i_start .eq. iphys%i_grad_jz) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_current, fhd_current)
        else if(i_start .eq. iphys%i_vecp) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_mag_p, fhd_mag_potential)
!
        else if(i_start .eq. iphys%i_t_diffuse                          &
     &     .or. i_start .eq. iphys%i_par_temp                           &
     &     .or. i_start .eq. iphys%i_filter_temp                        &
     &     .or. i_start .eq. iphys%i_wide_fil_temp                      &
     &     .or. i_start .eq. iphys%i_buoyancy                           &
     &     .or. i_start .eq. iphys%i_heat_source                        &
     &     .or. i_start .eq. iphys%i_square_t                           &
     &     .or. i_start .eq. iphys%i_grad_t) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
        else if(i_start .eq. iphys%i_grad_composit                      &
     &     .or. i_start .eq. iphys%i_filter_comp                        &
     &     .or. i_start .eq. iphys%i_wide_fil_comp                      &
     &     .or. i_start .eq. iphys%i_comp_buo                           &
     &     .or. i_start .eq. iphys%i_c_diffuse                          &
     &     .or. i_start .eq. iphys%i_square_c                           &
     &     .or. i_start .eq. iphys%i_light_source) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_light, fhd_light)
        else if(i_start .eq. iphys%i_entropy_source) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_entropy, fhd_entropy)
        end if
      end do
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(     i_start .eq. iphys%i_grad_filter_vx                     &
     &     .or. i_start .eq. iphys%i_grad_filter_vy                     &
     &     .or. i_start .eq. iphys%i_grad_filter_vz) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_velo, fhd_filter_velo)
        else if(i_start .eq. iphys%i_grad_filter_wx                     &
     &     .or. i_start .eq. iphys%i_grad_filter_wy                     &
     &     .or. i_start .eq. iphys%i_grad_filter_wz) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_vort, fhd_filter_vort)
        else if(i_start .eq. iphys%i_grad_filter_ax                     &
     &     .or. i_start .eq. iphys%i_grad_filter_ay                     &
     &     .or. i_start .eq. iphys%i_grad_filter_az) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_vecp, fhd_filter_vecp)
        else if(i_start .eq. iphys%i_grad_filter_bx                     &
     &     .or. i_start .eq. iphys%i_grad_filter_by                     &
     &     .or. i_start .eq. iphys%i_grad_filter_bz) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_magne, fhd_filter_magne)
        else if(i_start .eq. iphys%i_grad_filter_jx                     &
     &     .or. i_start .eq. iphys%i_grad_filter_jy                     &
     &     .or. i_start .eq. iphys%i_grad_filter_jz) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_current, fhd_filter_current)
        else if(i_start .eq. iphys%i_grad_filter_temp) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_temp, fhd_filter_temp)
        else if(i_start .eq. iphys%i_grad_filter_comp) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_comp, fhd_filter_comp)
        end if
      end do
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(     i_start .eq. iphys%i_filter_buo) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_temp, fhd_filter_temp)
        else if(i_start .eq. iphys%i_temp_scale) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_t_diffuse, fhd_thermal_diffusion)
        else if(i_start .eq. iphys%i_comp_scale) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_c_diffuse, fhd_c_diffuse)
        else if(i_start .eq. iphys%i_mag_stretch) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_grad_vx, fhd_grad_v_1)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_grad_vy, fhd_grad_v_2)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_grad_vz,fhd_grad_v_3)
!
        else if(i_start .eq. iphys%i_c_advect                           &
     &     .or. i_start .eq. iphys%i_pc_advect                          &
     &     .or. i_start .eq. iphys%i_c_flux                             &
     &     .or. i_start .eq. iphys%i_pc_flux                            &
     &     .or. i_start .eq. iphys%i_c_buo_gen                          &
     &     .or. i_start .eq. iphys%i_SGS_c_flux) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_light, fhd_light)
        else if(i_start .eq. iphys%i_f_buo_gen) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_temp, fhd_filter_temp)
        else if(i_start .eq. iphys%i_temp_gen) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_h_advect, fhd_heat_advect)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
        else if(i_start .eq. iphys%i_par_t_gen) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_ph_advect, fhd_part_h_advect)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
        else if(i_start .eq. iphys%i_par_c_gen) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_pc_advect, fhd_part_c_advect)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_light, fhd_light)
        else if(i_start .eq. iphys%i_par_entropy) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_entropy, fhd_per_entropy)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_ref_entropy, fhd_ref_entropy)
!
        else if(i_start .eq. iphys%i_x_heli                             &
     &     .or. i_start .eq. iphys%i_vp_induct                          &
     &     .or. i_start .eq. iphys%i_induct_t                           &
     &     .or. i_start .eq. iphys%i_wide_SGS_vp_induct                 &
     &     .or. i_start .eq. iphys%i_SGS_vp_induct                      &
     &     .or. i_start .eq. iphys%i_SGS_induct_t) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
        else if(i_start .eq. iphys%i_buo_gen                            &
     &     .or. i_start .eq. iphys%i_h_flux                             &
     &     .or. i_start .eq. iphys%i_ph_flux                            &
     &     .or. i_start .eq. iphys%i_SGS_h_flux                         &
     &     .or. i_start .eq. iphys%i_wide_SGS_h_flux                    &
     &     .or. i_start .eq. iphys%i_entropy                            &
     &     .or. i_start .eq. iphys%i_SGS_temp_gen                       &
     &     .or. i_start .eq. iphys%i_ph_advect                          &
     &     .or. i_start .eq. iphys%i_h_advect) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
        else if(i_start .eq. iphys%i_induction) then 
          if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
            call check_missing_field                                    &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
            call check_missing_field                                    &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          else
            call check_missing_field                                    &
     &       (fld, i_start, iphys%i_vp_induct, fhd_vp_induct)
          end if
        else if(i_start .eq. iphys%i_SGS_induction) then 
          if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
            call check_missing_field                                    &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
            call check_missing_field                                    &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          else
            call check_missing_field                                    &
     &         (fld, i_start, iphys%i_SGS_vp_induct, fhd_SGS_vp_induct)
          end if
        end if
      end do
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(     i_start .eq. iphys%i_me_gen) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_induction, fhd_mag_induct)
        else if(i_start .eq. iphys%i_m_tension_wk) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_m_tension, fhd_mag_tension)
        else if(i_start .eq. iphys%i_vis_e_diffuse) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_v_diffuse, fhd_viscous)
        else if(i_start .eq. iphys%i_mag_e_diffuse) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_b_diffuse, fhd_mag_diffuse)
        else if(i_start .eq. iphys%i_ujb                                &
     &     .or. i_start .eq. iphys%i_nega_ujb                           &
     &     .or. i_start .eq. iphys%i_SGS_Lor_wk) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_current, fhd_current)
        else if(i_start .eq. iphys%i_density) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_light, fhd_light)
        else if(i_start .eq. iphys%i_par_density) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_density, fhd_density)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_ref_density, fhd_ref_density)
        else if(i_start .eq. iphys%i_electric                           &
     &     .or. i_start .eq. iphys%i_poynting) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_vp_induct, fhd_vp_induct)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_current, fhd_current)
!
        else if(i_start .eq. iphys%i_div_Lorentz                        &
     &     .or. i_start .eq. iphys%i_rot_Lorentz) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_lorentz, fhd_Lorentz)
        else if(i_start .eq. iphys%i_geostrophic) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_coriolis, fhd_Coriolis)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_press_grad, fhd_press_grad)
!
        else if(i_start .eq. iphys%i_m_flux_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_m_flux, fhd_mom_flux)
        else if(i_start .eq. iphys%i_maxwell_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_maxwell, fhd_maxwell_t)
        else if(i_start .eq. iphys%i_induct_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_induct_t, fhd_induct_t)
        else if(i_start .eq. iphys%i_h_flux_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_h_flux, fhd_h_flux)
        else if(i_start .eq. iphys%i_ph_flux_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_ph_flux, fhd_ph_flux)
        else if(i_start .eq. iphys%i_c_flux_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_c_flux, fhd_c_flux)
        else if(i_start .eq. iphys%i_pc_flux_div) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_pc_flux, fhd_pc_flux)
!
        else if(i_start .eq. iphys%i_SGS_div_hf_true) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_velo, fhd_filter_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_temp, fhd_filter_temp)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_h_flux_div, fhd_div_h_flux)
        else if(i_start .eq. iphys%i_SGS_div_cf_true) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_velo, fhd_filter_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_comp, fhd_filter_comp)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_c_flux_div, fhd_div_c_flux)
        else if(i_start .eq. iphys%i_SGS_div_mf_true) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_velo, fhd_filter_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_m_flux_div, fhd_div_m_flux)
        else if(i_start .eq. iphys%i_SGS_Lor_true) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_magne, fhd_filter_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_maxwell_div, fhd_div_maxwell_t)
        else if(i_start .eq. iphys%i_SGS_idct_true) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_velo, fhd_filter_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_filter_magne, fhd_filter_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_induct_div, fhd_div_induct_t)
        else if(i_start .eq. iphys%i_SGS_Lor_wk_tr) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_Lor_true, fhd_SGS_Lorentz_true)
        else if(i_start .eq. iphys%i_reynolds_wk_tr) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_div_mf_true,                    &
     &        fhd_SGS_div_m_flux_true)
        else if(i_start .eq. iphys%i_SGS_t_gen_tr) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_div_hf_true,                    &
     &        fhd_SGS_div_h_flux_true)
        else if(i_start .eq. iphys%i_SGS_c_gen_tr) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_light, fhd_light)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_div_cf_true,                    &
     &        fhd_SGS_div_c_flux_true)
        else if(i_start .eq. iphys%i_SGS_idct_true) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
        end if
      end do
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(     i_start .eq. iphys%i_SGS_rot_inertia                    &
     &     .or. i_start .eq. iphys%i_SGS_div_inertia                    &
     &     .or. i_start .eq. iphys%i_dbl_SGS_inertia) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_inertia, fhd_SGS_inertia)
        else if(i_start .eq. iphys%i_SGS_div_c_flux                     &
     &     .or. i_start .eq. iphys%i_wide_SGS_c_flux) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_c_flux, fhd_SGS_c_flux)
        else if(i_start .eq. iphys%i_SGS_me_gen) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_induction, fhd_SGS_induction)
        else if(i_start .eq. iphys%i_SGS_div_m_flux) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_m_flux, fhd_SGS_m_flux)
        else if(i_start .eq. iphys%i_SGS_Lorentz                        &
     &     .or. i_start .eq. iphys%i_wide_SGS_Lorentz) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
        else if(i_start .eq. iphys%i_SGS_rot_Lorentz                    &
     &     .or. i_start .eq. iphys%i_SGS_div_Lorentz                    &
     &     .or. i_start .eq. iphys%i_dbl_SGS_Lorentz) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_Lorentz, fhd_SGS_Lorentz)
        else if(i_start .eq. iphys%i_dbl_SGS_vp_induct) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_vp_induct, fhd_SGS_vp_induct)
!
        else if(i_start .eq. iphys%i_dbl_SGS_h_flux) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_h_flux, fhd_SGS_h_flux)
        else if(i_start .eq. iphys%i_dbl_SGS_c_flux) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_c_flux, fhd_SGS_c_flux)
!
        else if(i_start .eq. iphys%i_SGS_buoyancy                       &
     &     .or. i_start .eq. iphys%i_SGS_buo_wk) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_h_flux, fhd_SGS_h_flux)
        else if(i_start .eq. iphys%i_SGS_comp_buo                       &
     &     .or. i_start .eq. iphys%i_SGS_comp_buo_wk) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_c_flux, fhd_SGS_c_flux)
!
        else if(i_start .eq. iphys%i_Csim_SGS_h_flux) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_h_flux, fhd_SGS_h_flux)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_wide_SGS_h_flux,                    &
     &        fhd_wide_SGS_h_flux)
        else if(i_start .eq. iphys%i_Csim_SGS_c_flux) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_c_flux, fhd_SGS_c_flux)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_wide_SGS_c_flux,                    &
     &        fhd_wide_SGS_c_flux)
        else if(i_start .eq. iphys%i_Csim_SGS_m_flux) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_inertia, fhd_SGS_inertia)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_wide_SGS_inertia,                   &
     &        fhd_wide_SGS_inertia)
        else if(i_start .eq. iphys%i_Csim_SGS_Lorentz) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_Lorentz, fhd_SGS_Lorentz)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_wide_SGS_Lorentz,                   &
     &        fhd_wide_SGS_Lorentz)
        else if(i_start .eq. iphys%i_Csim_SGS_induction) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_vp_induct, fhd_SGS_vp_induct)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_wide_SGS_vp_induct,                 &
     &       fhd_wide_SGS_vp_induct)
        else if(i_start .eq. iphys%i_Csim_SGS_buoyancy) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_h_flux, fhd_SGS_h_flux)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
        else if(i_start .eq. iphys%i_Csim_SGS_comp_buo) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_c_flux, fhd_SGS_c_flux)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
!
        else if(i_start .eq. iphys%i_h_flux_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_h_flux, fhd_h_flux)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_h_flux, fhd_SGS_h_flux)
        else if(i_start .eq. iphys%i_c_flux_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_c_flux, fhd_c_flux)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_c_flux, fhd_SGS_c_flux)
        else if(i_start .eq. iphys%i_inertia_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_m_advect, fhd_inertia)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_inertia, fhd_SGS_inertia)
        else if(i_start .eq. iphys%i_Lorentz_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_lorentz, fhd_Lorentz)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_Lorentz, fhd_SGS_Lorentz)
        else if(i_start .eq. iphys%i_Lorentz_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_lorentz, fhd_Lorentz)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_Lorentz, fhd_SGS_Lorentz)
        else if(i_start .eq. iphys%i_vp_induct_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_vp_induct, fhd_vp_induct)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_vp_induct, fhd_SGS_vp_induct)
        else if(i_start .eq. iphys%i_mag_induct_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_induction, fhd_mag_induct)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_induction, fhd_SGS_induction)
        else if(i_start .eq. iphys%i_mom_flux_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_m_flux, fhd_mom_flux)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_m_flux, fhd_SGS_m_flux)
        else if(i_start .eq. iphys%i_maxwell_t_w_sgs) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_maxwell, fhd_maxwell_t)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_maxwell, fhd_SGS_maxwell_t)
        end if
      end do
!
      end subroutine check_dependencies_by_id
!
! -----------------------------------------------------------------------
!
      subroutine check_dependence_FEM_MHD_by_id(iphys, fld)
!
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint) :: i, i_start
!
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(i_start .eq. iphys%i_reynolds_wk) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_div_m_flux, fhd_div_SGS_m_flux)
        end if
      end do
!
      end subroutine check_dependence_FEM_MHD_by_id
!
! -----------------------------------------------------------------------
!
      subroutine check_dependence_SPH_MHD_by_id(iphys, fld)
!
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint) :: i, i_start
!
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(i_start .eq. iphys%i_reynolds_wk) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_SGS_inertia, fhd_SGS_inertia)
        end if
      end do
!
      end subroutine check_dependence_SPH_MHD_by_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_missing_field(fld, iphys_tgt, iphys_ref, name)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: iphys_tgt, iphys_ref
      character(len = kchara), intent(in) :: name
!
      if(iphys_ref .gt. 0) return
      write(*,*) 'Following fields are required for ',                  &
     &     trim(field_name_by_address(fld, iphys_tgt)),                 &
     &     ': ', trim(name)
      call calypso_MPI_abort(ierr_fld,'Stop program.')
!
      end subroutine check_missing_field
!
! -----------------------------------------------------------------------
!
     end module check_MHD_dependency_by_id
