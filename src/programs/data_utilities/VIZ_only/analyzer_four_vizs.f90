!analyzer_four_vizs.f90
!      module analyzer_four_vizs
!
!     Written by H. Matsui on July, 2006
!
!      subroutine initialize_four_vizs
!      subroutine analyze_four_vizs
!
      module analyzer_four_vizs
!
      use m_precision
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use t_control_data_four_vizs
      use t_four_visualizers
      use t_VIZ_only_step_parameter
      use t_FEM_mesh_field_4_viz
      use t_VIZ_mesh_field
      use t_mesh_SR
      use FEM_analyzer_four_vizs
      use m_elapsed_labels_4_VIZ
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: fname_viz_ctl = "control_viz"
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ4
!
!>      Structure of control data for visualization
      type(control_data_four_vizs), save :: vizs_ctl4
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz4
!>      Structure of work area for mesh communications
      type(mesh_SR) :: m_SR14
!>      Structure of data for visualization
      type(VIZ_mesh_field), save :: VIZ_DAT4
!>      Structure of viualization modules
      type(four_visualize_modules), save :: vizs_m4
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialize_four_vizs
!
      use m_elapsed_labels_SEND_RECV
      use input_control_four_vizs
!
      call init_elapse_time_by_TOTAL
      call set_elpsed_label_4_VIZ(flag_detailed1, elps_VIZ1, elps1)
      call elpsed_label_field_send_recv

      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
!  Load controls
      if (iflag_debug.gt.0) write(*,*) 's_input_control_four_vizs'
      call s_input_control_four_vizs(fname_viz_ctl, vizs_ctl4,          &
     &                               FEM_viz4, t_VIZ4)
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_four_vizs'
      call FEM_initialize_four_vizs                                     &
     &   (elps_VIZ1, t_VIZ4%init_d, t_VIZ4%ucd_step,                    &
     &    t_VIZ4%viz_step, FEM_viz4, VIZ_DAT4, m_SR14)
!
!  VIZ Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'init_four_visualize'
      call init_four_visualize(elps_VIZ1, t_VIZ4%viz_step,              &
     &    FEM_viz4%geofem, FEM_viz4%field, VIZ_DAT4,                    &
     &    vizs_ctl4%viz4_ctl, vizs_m4, m_SR14)
      call dealloc_viz4_controls(vizs_ctl4%viz4_ctl)
!
      end subroutine initialize_four_vizs
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_four_vizs
!
      integer(kind=kint ) :: i_step
!
!
      do i_step = t_VIZ4%init_d%i_time_step, t_VIZ4%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ4%ucd_step) .eqv. .FALSE.) cycle
        if(iflag_vizs_w_fix_step(i_step, t_VIZ4%viz_step)               &
     &        .eqv. .FALSE.) cycle
!
!  Load field data
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                       'FEM_analyze_four_vizs', i_step
        call FEM_analyze_four_vizs                                      &
     &     (i_step, t_VIZ4%ucd_step, t_VIZ4%time_d, FEM_viz4, m_SR14)
!
!  Rendering
        if(iflag_debug .gt. 0)  write(*,*) 'visualize_four', i_step
        call istep_viz_w_fix_dt(i_step, t_VIZ4%viz_step)
        call visualize_four(elps_VIZ1, t_VIZ4%viz_step, t_VIZ4%time_d,  &
     &      FEM_viz4%geofem, FEM_viz4%field, VIZ_DAT4, vizs_m4, m_SR14)
      end do
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_four_vizs
!
!  ---------------------------------------------------------------------
!
      end module analyzer_four_vizs
