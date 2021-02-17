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
      use FEM_analyzer_viz_surf
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
!
      integer(kind = kint) :: ierr
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_sections'
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      call read_control_file_section_only(sec_viz_ctl5)
      call set_control_params_4_sections(sec_viz_ctl5,                  &
     &                                   sfcing5, t_VIZ5, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_VTK_convert                                   &
     &   (t_VIZ5%ucd_step, t_VIZ5%init_d, sfcing5)
      call dealloc_field_lists_for_vizs(sfcing5%viz_fld_list)
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
      integer(kind = kint) :: i_step, istep_ucd
!
!
      do i_step = t_VIZ5%init_d%i_time_step, t_VIZ5%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ5%ucd_step) .eqv. .FALSE.) cycle
!
!  Load field data
        call FEM_analyze_surface                                        &
     &     (i_step, t_VIZ5%ucd_step, t_VIZ5%time_d, sfcing5)
!
!  Generate field lines
        istep_ucd = istep_file_w_fix_dt(i_step, t_VIZ5%ucd_step)
        call visualize_convert_vtk(i_step, istep_ucd, t_VIZ5%time_d,    &
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
