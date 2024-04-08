!>@file   set_control_SPH_MHD_noviz.f90
!!@brief  module set_control_SPH_MHD_noviz
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Set control data for spherical transform MHD dynamo simulation
!!
!!@verbatim
!!      subroutine s_set_control_SPH_MHD_noviz(model_ctl, smonitor_ctl, &
!!     &          MHD_prop, MHD_BC, rj_fld, monitor)
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!        type(node_monitor_IO), intent(inout) :: nod_mntr
!!
!!      subroutine set_control_SPH_MHD_monitors(smonitor_ctl,           &
!!     &                                        MHD_BC, rj_fld, monitor)
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!@endverbatim
!
      module set_control_SPH_MHD_noviz
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_field_data_IO
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_ctl_data_crust_filter
      use t_bc_data_list
      use t_flex_delta_t_data
      use t_SPH_mesh_field_data
      use t_radial_reference_field
      use t_field_on_circle
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_control_SPH_MHD_noviz(model_ctl, smonitor_ctl,   &
     &          MHD_prop, MHD_BC, rj_fld, monitor)
!
      use t_phys_data
      use t_sph_mhd_monitor_data_IO
!
      use set_control_sph_data_MHD
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(mhd_model_control), intent(inout) :: model_ctl
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
!
!       set spectr field list
      if (iflag_debug.gt.0) write(*,*) 'set_control_sph_mhd_fields'
      call set_control_sph_mhd_fields                                   &
     &   (MHD_prop, model_ctl%fld_ctl%field_ctl, rj_fld)
!
!   set_pickup modes
      call set_control_SPH_MHD_monitors                                 &
     &   (smonitor_ctl, MHD_BC, rj_fld, monitor)
!
      end subroutine s_set_control_SPH_MHD_noviz
!
! ----------------------------------------------------------------------
!
      subroutine set_control_SPH_MHD_monitors(smonitor_ctl,             &
     &                                        MHD_BC, rj_fld, monitor)
!
      use t_phys_data
      use t_sph_mhd_monitor_data_IO
      use t_no_heat_Nusselt
      use t_CMB_dipolarity
      use t_sph_typical_scales
      use t_field_on_circle
      use t_field_4_dynamobench
      use t_multi_flag_labels
      use m_file_format_labels
      use m_base_field_labels
!
      use set_control_4_pickup_sph
      use set_control_sph_spectr
      use set_ctl_sph_spectr_w_dbench
      use cal_CMB_dipolarity
      use cal_typical_scale
!
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
!
      if(allocated(gzip_flags%flags) .eqv. .FALSE.) then
        call init_multi_flags_by_labels(itwo, gzip_names, gzip_flags)
      end if
!
!   Set spectr monitor
      call set_ctl_params_layered_spectr                                &
     &   (smonitor_ctl%lp_ctl, monitor%pwr)
      call s_set_ctl_sph_spectr_w_dbench(smonitor_ctl, MHD_BC,          &
     &    monitor%pwr, monitor%circ_mid_eq%circle, monitor%bench)
!   Set parameters for dynamo benchmark output
      if(monitor%bench%iflag_dynamobench .gt. 0) then
        call set_ctl_circle_for_dbench(smonitor_ctl%dbench_ctl,         &
     &      monitor%circ_mid_eq%circle)
      end if
!
!   set_pickup modes
      call set_ctl_params_pick_sph                                      &
     &   (smonitor_ctl%pspec_ctl, monitor%pick_list, monitor%pick_coef)
!
      call set_ctl_params_pick_gauss                                    &
     &   (smonitor_ctl%g_pwr, monitor%gauss_list, monitor%gauss_coef)
!
      call set_ctl_params_no_heat_Nu(heat_source%name,                  &
     &    smonitor_ctl%heat_nusselt_file_prefix,                        &
     &    smonitor_ctl%heat_nusselt_file_format,                        &
     &    rj_fld, monitor%heat_Nusselt)
      call set_ctl_params_no_heat_Nu(composition_source%name,           &
     &    smonitor_ctl%comp_nusselt_file_prefix,                        &
     &    smonitor_ctl%comp_nusselt_file_format,                        &
     &    rj_fld, monitor%comp_Nusselt)
!
      call set_ctl_dipolarity_params                                    &
     &   (smonitor_ctl%fdip_ctl%fdip_file_prefix_ctl,                   &
     &    smonitor_ctl%fdip_ctl%fdip_file_format_ctl,                   &
     &    smonitor_ctl%fdip_ctl%fdip_truncation_ctl,                    &
     &    rj_fld, monitor%dip)
      call set_ctl_typical_scale_params                                 &
     &   (smonitor_ctl%typ_scale_file_prefix_ctl,                       &
     &    smonitor_ctl%typ_scale_file_format_ctl, rj_fld, monitor%tsl)
!
      call set_control_circles_def(smonitor_ctl, monitor%mul_circle)
!
      end subroutine set_control_SPH_MHD_monitors
!
! ----------------------------------------------------------------------
!
      end module set_control_SPH_MHD_noviz
