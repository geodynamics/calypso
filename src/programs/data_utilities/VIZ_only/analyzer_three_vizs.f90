!>@file   analyzer_three_vizs.f90
!!@brief  module analyzer_three_vizs
!!
!!@author H. Matsui
!!@date Programmed in May, 2023
!!
!> @brief Top module for three data visualizations
!!
!!@verbatim
!!      subroutine initialize_three_vizs
!!      subroutine analyze_three_vizs
!!@endverbatim
      module analyzer_three_vizs
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_control_data_three_vizs
      use t_three_visualizers
      use t_VIZ_only_step_parameter
      use t_FEM_mesh_field_4_viz
      use t_VIZ_mesh_field
      use t_mesh_SR
      use FEM_analyzer_four_vizs
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: fname_viz_ctl = "control_viz"
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ3
!
!>      Structure of control data for visualization
      type(control_data_three_vizs), save :: vizs_ctl3
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz3
!>      Structure of work area for mesh communications
      type(mesh_SR) :: m_SR13
!>      Structure of data for visualization
      type(VIZ_mesh_field), save :: VIZ_DAT3
!>      Structure of viualization modules
      type(three_visualize_modules), save :: vizs_m3
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialize_three_vizs
!
      use m_elapsed_labels_4_VIZ
      use m_elapsed_labels_SEND_RECV
      use input_control_three_vizs
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv

      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
!  Load controls
      if (iflag_debug.gt.0) write(*,*) 's_input_control_three_vizs'
      call s_input_control_three_vizs(fname_viz_ctl, vizs_ctl3,         &
     &                               FEM_viz3, t_VIZ3)
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_four_vizs'
      call FEM_initialize_four_vizs(t_VIZ3%init_d, t_VIZ3%ucd_step,     &
     &    t_VIZ3%viz_step, FEM_viz3, VIZ_DAT3, m_SR13)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_three_visualize'
      call init_three_visualize(t_VIZ3%viz_step,                        &
     &    FEM_viz3%geofem, FEM_viz3%field, VIZ_DAT3,                    &
     &    vizs_ctl3%viz3_ctl, vizs_m3, m_SR13)
!
      end subroutine initialize_three_vizs
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_three_vizs
!
      integer(kind=kint ) :: i_step
!
!
      do i_step = t_VIZ3%init_d%i_time_step, t_VIZ3%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ3%ucd_step) .eqv. .FALSE.) cycle
        if(iflag_vizs_w_fix_step(i_step, t_VIZ3%viz_step)               &
     &        .eqv. .FALSE.) cycle
!
!  Load field data
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                       'FEM_analyze_four_vizs', i_step
        call FEM_analyze_four_vizs                                      &
     &     (i_step, t_VIZ3%ucd_step, t_VIZ3%time_d, FEM_viz3, m_SR13)
!
!  Rendering
        if(iflag_debug .gt. 0)  write(*,*) 'visualize_three', i_step
        call istep_viz_w_fix_dt(i_step, t_VIZ3%viz_step)
        call visualize_three(t_VIZ3%viz_step, t_VIZ3%time_d,            &
     &      FEM_viz3%geofem, FEM_viz3%field, VIZ_DAT3, vizs_m3, m_SR13)
      end do
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_three_vizs
!
!  ---------------------------------------------------------------------
!
      end module analyzer_three_vizs
