!set_nodal_field_name.f90
!      module set_nodal_field_name
!
!        programmed by H.Matsui on Jan., 2008
!        modified by H.Matsui on Oct.,  2009
!        modified by H.Matsui on June., 2012
!
!      subroutine set_vector_field_name(phys_nod_name_ctl, icou,        &
!     &          phys_nod_name, num_nod_component, iflag)
!      subroutine set_scalar_field_name(phys_nod_name_ctl, icou,        &
!     &          phys_nod_name, num_nod_component, iflag)
!      subroutine set_tensor_field_name(phys_nod_name_ctl, icou,        &
!     &          phys_nod_name, num_nod_component, iflag)
!
!      subroutine check_vis_control_flag(visualize_ctl, iflag_viz)
!      subroutine check_monitor_control_flag(iflag, monitor_ctl,        &
!     &          iflag_fld_monitor)
!
      module set_nodal_field_name
!
      use m_precision
      use m_phys_labels
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_vector_field_name(phys_nod_name_ctl, icou,         &
     &          phys_nod_name, num_nod_component, iflag)
!
      character(len = kchara), intent(in) :: phys_nod_name_ctl
      integer (kind = kint), intent(inout) :: icou
      integer (kind = kint), intent(inout) :: num_nod_component
      character(len = kchara), intent(inout) :: phys_nod_name
      integer (kind = kint), intent(inout) :: iflag
!
!  set number of components ( vector and scalar )
!
      if (iflag .gt. 0) return
!
      if (   (phys_nod_name_ctl .eq. fhd_velo               )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_vort               )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_magne              )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_vecp               )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_current            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_e_field            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_poynting           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_temp          )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_par_temp      )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_ref_temp      )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_composit      )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_filter_temp   )           &
     &      )   iflag = 1
!
      if (   (phys_nod_name_ctl .eq. fhd_filter_v           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_filter_a           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_filter_b           )           &
     &      )   iflag = 1
!
      if (   (phys_nod_name_ctl .eq. fhd_grad_v_1           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_v_2           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_v_3           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_w_1           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_w_2           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_w_3           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_a_1           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_a_2           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_a_3           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_b_1           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_b_2           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_b_3           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_j_1           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_j_2           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_grad_j_3           )           &
     &      )   iflag = 1
!
      if (   (phys_nod_name_ctl .eq. fhd_viscous            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_w_viscous          )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_vecp_diffuse       )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_mag_diffuse        )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_mag_tension        )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_h_flux             )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_ph_flux            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_c_flux             )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_inertia            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_div_m_flux         )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_div_maxwell_t      )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_div_induct_t       )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_mag_induct         )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_vp_induct          )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_mag_stretch        )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_press_grad         )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_Lorentz            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_Coriolis           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_buoyancy           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_comp_buo           )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_filter_buo         )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_rot_inertia        )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_rot_Lorentz        )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_rot_Coriolis       )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_rot_buoyancy       )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_rot_comp_buo       )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_rot_filter_buo     )           &
     &      )   iflag = 1
!
      if (   (phys_nod_name_ctl .eq. fhd_div_SGS_m_flux     )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_h_flux         )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_c_flux         )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_Lorentz        )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_induction      )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_vp_induct      )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_buoyancy       )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_comp_buo       )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_div_m_flux_true)           &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_Lorentz_true   )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_mag_induct_true)           &
     &      )   iflag = 1
!
      if (   (phys_nod_name_ctl .eq. fhd_w_filter_velo     )            &
     &  .or. (phys_nod_name_ctl .eq. fhd_w_filter_vecp     )            &
     &  .or. (phys_nod_name_ctl .eq. fhd_w_filter_magne    )            &
     &       )  iflag = 1
!
      if (   (phys_nod_name_ctl .eq. fhd_pre_mom            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_pre_uxb            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_chk_mom            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_chk_uxb            )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_chk_mom_2          )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_chk_uxb_2          )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_forces             )           &
     &  .or. (phys_nod_name_ctl .eq. fhd_rot_forces         )           &
     &      )   iflag = 1
!
      if (iflag .eq. 1) then
        icou = icou + 1
        phys_nod_name = phys_nod_name_ctl
        num_nod_component = 3
      end if
!
      end subroutine set_vector_field_name
!
! -----------------------------------------------------------------------
!
      subroutine set_scalar_field_name(phys_nod_name_ctl, icou,         &
     &          phys_nod_name, num_nod_component, iflag)
!
      character(len = kchara), intent(in) :: phys_nod_name_ctl
      integer (kind = kint), intent(inout) :: icou
      integer (kind = kint), intent(inout) :: num_nod_component
      character(len = kchara), intent(inout) :: phys_nod_name
      integer (kind = kint), intent(inout) :: iflag
!
!  set number of components ( vector and scalar )
!
      if (iflag .gt. 0) return
!
      if (   (phys_nod_name_ctl .eq. fhd_press                )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_temp                 )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_light                )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_mag_potential        )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_scalar_potential     )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_entropy              )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_heat_source          )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_light_source         )         &
     &  .or. (phys_nod_name_ctl .eq. fhd_entropy_source       )         &
     &      )   iflag = 1
!
      if (    (phys_nod_name_ctl .eq. fhd_ref_temp            )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_press_work          )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_m_potential_work    )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_density             )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_part_light          )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_per_density         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_ref_density         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_per_entropy         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_ref_entropy         )         &
     &      )   iflag = 1
!
      if (    (phys_nod_name_ctl .eq. fhd_filter_temp         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_filter_part_temp    )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_filter_comp         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_temp            )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_comp            )         &
     &      )   iflag = 1
!
      if (    (phys_nod_name_ctl .eq. fhd_kinetic_helicity    )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_magnetic_helicity   )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_current_helicity    )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_cross_helicity      )         &
     &      )   iflag = 1
!
      if (    (phys_nod_name_ctl .eq. fhd_thermal_diffusion   )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_c_diffuse           )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_heat_advect         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_part_h_advect       )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_div_h_flux          )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_div_ph_flux         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_composit_advect     )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_mag_ene_gen         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_work_agst_Lorentz   )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_Lorentz_work        )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_mag_tension_work    )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_buoyancy_flux       )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_comp_buo_flux       )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_filter_buo_flux     )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_div_SGS_h_flux      )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_m_ene_gen       )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_temp_gen        )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_Lorentz_work    )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_Reynolds_work       )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_buo_flux        )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_comp_buo_flux   )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_temp_generation     )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_part_temp_gen       )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_part_temp           )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_vis_ene_diffuse     )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_mag_ene_diffuse     )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_div_h_flux_true )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_Lorentz_wk_true )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_Reynolds_work_true  )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_temp_gen_true   )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_m_ene_gen_true  )         &
     &      )   iflag = 1
!
      if (    (phys_nod_name_ctl .eq. fhd_div_inertia         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_div_Lorentz         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_div_Coriolis        )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_div_buoyancy        )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_div_comp_buo        )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_div_filter_buo      )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_div_viscous         )         &
     &      )   iflag = 1
!
      if (    (phys_nod_name_ctl .eq. fhd_w_filter_temp      )          &
     &       ) iflag = 1
!
      if (    (phys_nod_name_ctl .eq. fhd_pre_heat            )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_pre_composit        )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_pre_press           )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_heat            )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_composit        )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_press           )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_potential       )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_heat_2          )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_composit_2      )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_press_2         )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_chk_potential_2     )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_div_forces          )         &
     &      )   iflag = 1
!
      if (    (phys_nod_name_ctl .eq. fhd_velocity_scale      )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_magnetic_scale      )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_temp_scale          )         &
     &   .or. (phys_nod_name_ctl .eq. fhd_composition_scale   )         &
     &       ) iflag = 1
!
      if (iflag .eq. 1) then
        icou = icou + 1
        phys_nod_name = phys_nod_name_ctl
        num_nod_component = 1
      end if
!
!   Old field label... Should be deleted later!!
      if (     phys_nod_name_ctl .eq. fhd_buoyancy_work       ) then
        iflag = 1
        icou = icou + 1
        phys_nod_name = fhd_buoyancy_flux
        num_nod_component = 1
      end if
!
      end subroutine set_scalar_field_name
!
! -----------------------------------------------------------------------
!
      subroutine set_tensor_field_name(phys_nod_name_ctl, icou,         &
     &          phys_nod_name, num_nod_component, iflag)
!
      character(len = kchara), intent(in) :: phys_nod_name_ctl
      integer (kind = kint), intent(inout) :: icou
      integer (kind = kint), intent(inout) :: num_nod_component
      character(len = kchara), intent(inout) :: phys_nod_name
      integer (kind = kint), intent(inout) :: iflag
!
!
!  set number of components ( vector and scalar )
!
      if (iflag .gt. 0) return
!
       if (   (phys_nod_name_ctl .eq. fhd_mom_flux     )                &
     &   .or. (phys_nod_name_ctl .eq. fhd_maxwell_t    )                &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_m_flux   )                &
     &   .or. (phys_nod_name_ctl .eq. fhd_SGS_maxwell_t)                &
     &       ) then
!
        iflag = 1
        icou = icou + 1
        phys_nod_name = phys_nod_name_ctl
        num_nod_component = 6
      end if
!
       if (  (phys_nod_name_ctl .eq. fhd_induct_t      )                &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_induct_t  )                &
     &       ) then
!
        iflag = 1
        icou = icou + 1
        phys_nod_name = phys_nod_name_ctl
        num_nod_component = 3
      end if
!
      if (   (phys_nod_name_ctl .eq. fhd_SGS_simi   )                   &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_grad   )                   &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_grad_f )                   &
     &  .or. (phys_nod_name_ctl .eq. fhd_SGS_diffuse)                   &
     &      ) then
!
        iflag = 1
        icou = icou + 1
        phys_nod_name = phys_nod_name_ctl
!        if (iflag_SGS_difference .eq. 2) then
!          num_nod_component = 7
!        else
          num_nod_component = 6
!        end if
      end if
!
      end subroutine set_tensor_field_name
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_vis_control_flag(visualize_ctl, iflag_viz)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: visualize_ctl
      integer (kind = kint), intent(inout) :: iflag_viz
!
      if (cmp_no_case(visualize_ctl, 'Viz_On') .gt. 0) then
        iflag_viz = 1
      else
        iflag_viz = 0
      end if
!
      end subroutine check_vis_control_flag
!
! -----------------------------------------------------------------------
!
      subroutine check_monitor_control_flag(iflag, monitor_ctl,         &
     &          iflag_fld_monitor)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: monitor_ctl
      integer (kind = kint), intent(in) :: iflag
      integer (kind = kint), intent(inout) :: iflag_fld_monitor
!
      if (iflag .eq. 0) return
      if (cmp_no_case(monitor_ctl, 'Monitor_On') .gt. 0) then
        iflag_fld_monitor = 1
      else
        iflag_fld_monitor = 0
      end if
!
      end subroutine check_monitor_control_flag
!
! -----------------------------------------------------------------------
!
      end module set_nodal_field_name
