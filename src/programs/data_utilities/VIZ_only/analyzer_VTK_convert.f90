!
!      module analyzer_VTK_convert
!
!     Written by H. Matsui on July, 2006
!
!      subroutine init_analyzer_VTK_convert
!      subroutine analyze_VTK_convert
!
      module analyzer_VTK_convert
!
      use m_precision
      use m_work_time
!
      use t_viz_VTK_convert
      use t_VIZ_only_step_parameter
      use t_control_data_section_only
      use t_FEM_mesh_field_4_viz
      use t_file_IO_parameter
      use t_vector_for_solver
      use t_elapsed_labels_4_SECTIONS
      use FEM_analyzer_viz_surf
      use t_mesh_SR
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: fname_viz_ctl = "control_viz"
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ5
!>      Structure of control data for sectioning only
      type(control_data_section_only), save :: sec_viz_ctl5
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz5
!>      Structure of work area for mesh communications
      type(mesh_SR) :: m_SR15
!
!>        Structure for VTK file output paramters
      type(field_IO_params) :: vtk_file_IO5
!>          FEM field data to VTK
      type(ucd_data), save :: vtk_out5
!
!>          Elapsed time labels
      logical, parameter :: flag_detailed1 = .TRUE.
      type(elapsed_labels_4_SECTIONS), save :: elps_SECT1
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_analyzer_VTK_convert
!
      use calypso_mpi
      use m_elapsed_labels_SEND_RECV
      use input_control_section_only
!
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_SECT(flag_detailed1, elps_SECT1, elps1)
      call elpsed_label_field_send_recv
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_sections'
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      call s_input_control_section_only(fname_viz_ctl, sec_viz_ctl5,    &
     &                                  FEM_viz5, t_VIZ5)
!
!  FEM Initialization
      call FEM_initialize_VTK_convert(t_VIZ5%ucd_step, t_VIZ5%init_d,   &
     &                                FEM_viz5, m_SR15)
!
!  VIZ Initialization
      call init_visualize_convert_vtk                                   &
     &   (FEM_viz5%geofem, FEM_viz5%field, t_VIZ5%ucd_step,             &
     &    sec_viz_ctl5%surfacing_ctls%output_ucd_fmt_s_ctl,             &
     &    FEM_viz5%ucd_file_IO, vtk_file_IO5, vtk_out5,                 &
     &    m_SR15%SR_sig, m_SR15%SR_i)
!
      end subroutine init_analyzer_VTK_convert
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_VTK_convert
!
      integer(kind = kint) :: i_step, istep_ucd
!
!
      do i_step = t_VIZ5%init_d%i_time_step, t_VIZ5%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ5%ucd_step) .eqv. .FALSE.) cycle
!
!  Load field data
        call FEM_analyze_surface                                        &
     &     (i_step, t_VIZ5%ucd_step, t_VIZ5%time_d, FEM_viz5, m_SR15)
!
!  Generate field lines
        istep_ucd = istep_file_w_fix_dt(i_step, t_VIZ5%ucd_step)
        call visualize_convert_vtk(i_step, istep_ucd, elps_SECT1,       &
     &      t_VIZ5%time_d, vtk_file_IO5, vtk_out5)
      end do
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_VTK_convert
!
!  ---------------------------------------------------------------------
!
      end module analyzer_VTK_convert
