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
      use t_surfacing
      use t_viz_VTK_convert
      use t_VIZ_only_step_parameter
      use t_control_data_section_only
      use FEM_analyzer_VTK_convert
!
      implicit none
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ5
!>      Structure of control data for sectioning only
      type(control_data_section_only), save :: sec_viz_ctl5
!>      Structure of mesh and field for sectioning only
      type(FEM_mesh_field_4_surfacing), save :: sfcing5
!>          FEM field data to VTK
      type(ucd_data), save :: vtk_out5
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
      use m_elapsed_labels_4_VIZ
      use m_elapsed_labels_SEND_RECV
      use load_mesh_and_field_4_viz
!
      integer(kind = kint) :: ierr
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      call read_control_data_section_only(sec_viz_ctl5)
      call set_control_params_4_viz                                     &
     &   (sec_viz_ctl5%t_sect_ctl, sec_viz_ctl5%sect_plt,               &
     &    sfcing5%mesh_file_IO, sfcing5%ucd_file_IO, t_VIZ5, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_VTK_convert(t_VIZ5%init_d, sfcing5)
!
!  VIZ Initialization
      call init_visualize_convert_vtk                                   &
     &   (sfcing5%geofem, sfcing5%nod_fld, t_VIZ5%ucd_step,             &
     &    sec_viz_ctl5%surfacing_ctls%output_ucd_fmt_s_ctl,             &
     &    sfcing5%ucd_file_IO, sfcing5%vtk_file_IO, vtk_out5)
!
      end subroutine init_analyzer_VTK_convert
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_VTK_convert
!
      integer(kind=kint ) :: i_step
!
!
      do i_step = t_VIZ5%init_d%i_time_step, t_VIZ5%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ5%ucd_step) .ne. izero) cycle
        call set_IO_step_flag(i_step,t_VIZ5%ucd_step)
!
!  Load field data
        call FEM_analyze_VTK_convert                                    &
     &     (i_step, t_VIZ5%time_d, t_VIZ5%ucd_step, sfcing5)
!
!  Generate field lines
        call visualize_convert_vtk(t_VIZ5%ucd_step, t_VIZ5%time_d,      &
     &      sfcing5%vtk_file_IO, vtk_out5)
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
