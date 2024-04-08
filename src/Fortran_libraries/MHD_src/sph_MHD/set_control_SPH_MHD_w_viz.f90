!>@file   set_control_SPH_MHD_w_viz.f90
!!@brief  module set_control_SPH_MHD_w_viz
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Set control data for spherical transform MHD dynamo simulation
!!
!!@verbatim
!!      subroutine s_set_control_SPH_MHD_w_viz                          &
!!     &         (model_ctl, psph_ctl, smonitor_ctl, crust_filter_ctl,  &
!!     &          nmtr_ctl, MHD_prop, MHD_BC, sph, rj_fld, nod_fld,     &
!!     &          monitor, nod_mntr)
!!        type(mhd_model_control), intent(in) :: model_ctl
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!!        type(clust_filtering_ctl), intent(in) :: crust_filter_ctl
!!        type(node_monitor_control), intent(in) :: nmtr_ctl
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(sph_grids), intent(inout) :: sph
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!        type(node_monitor_IO), intent(inout) :: nod_mntr
!!
!!      subroutine set_crustal_filtering_control                        &
!!     &         (crust_truncation_c, monitor)
!!        type(phys_data), intent(in) :: crust_truncation_c
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!@endverbatim
!
      module set_control_SPH_MHD_w_viz
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
      subroutine s_set_control_SPH_MHD_w_viz                            &
     &         (model_ctl, psph_ctl, smonitor_ctl, crust_filter_ctl,    &
     &          nmtr_ctl, MHD_prop, MHD_BC, sph, rj_fld, nod_fld,       &
     &          monitor, nod_mntr)
!
      use t_phys_data
      use t_sph_mhd_monitor_data_IO
      use t_node_monitor_IO
!
      use set_control_sph_data_MHD
      use set_control_field_data
      use set_controls_4_sph_shell
      use set_nodal_field_name
      use set_control_SPH_MHD_noviz
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(mhd_model_control), intent(inout) :: model_ctl
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
      type(clust_filtering_ctl), intent(in) :: crust_filter_ctl
      type(node_monitor_control), intent(in) :: nmtr_ctl
      type(MHD_BC_lists), intent(in) :: MHD_BC
!
      type(sph_grids), intent(inout) :: sph
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: nod_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
      type(node_monitor_IO), intent(inout) :: nod_mntr
!
      integer(kind = kint) :: ierr
!
!
!       set nodal field list
      if (iflag_debug.gt.0) write(*,*) 's_set_control_field_data'
      call s_set_control_field_data                                     &
     &   (model_ctl%fld_ctl%field_ctl, nod_fld, ierr)
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
      call set_crustal_filtering_control(crust_filter_ctl, monitor)
!
      call set_FEM_mesh_mode_4_SPH(psph_ctl%spctl, sph%sph_params)
!
      call set_control_node_grp_monitor(nmtr_ctl, nod_mntr)
      call count_field_4_monitor(rj_fld, nod_mntr)
!
      end subroutine s_set_control_SPH_MHD_w_viz
!
! ----------------------------------------------------------------------
!
      subroutine set_crustal_filtering_control(crust_c, monitor)
!
      use t_sph_mhd_monitor_data_IO
!
      use set_control_4_pickup_sph
!
      type(clust_filtering_ctl), intent(in) :: crust_c
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
!
      monitor%ltr_crust = 1
      if(crust_c%crust_truncation_ctl%iflag .gt. 0) then
        monitor%ltr_crust = crust_c%crust_truncation_ctl%intvalue
      end if
!
      end subroutine set_crustal_filtering_control
!
! ----------------------------------------------------------------------
!
      end module set_control_SPH_MHD_w_viz
