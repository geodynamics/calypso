!
!      module analyzer_psf
!
!     Written by H. Matsui on July, 2006
!
!      subroutine init_analyzer_psf
!      subroutine analyze_psf
!
      module analyzer_psf
!
      use m_precision
      use m_work_time
!
      use t_surfacing
      use t_viz_sections
      use t_VIZ_only_step_parameter
      use t_control_data_section_only
      use FEM_analyzer_viz_surf
!
      implicit none
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ2
!>      Structure of control data for sectioning only
      type(control_data_section_only), save :: sec_viz_ctl2
!>      Structure of mesh and field for sectioning only
      type(FEM_mesh_field_4_surfacing), save :: sfcing2
!>      Structure of sectioning and isosurfaceing modules
      type(surfacing_modules), save :: viz_psfs2
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_analyzer_psf
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
      call read_control_data_section_only(sec_viz_ctl2)
      call set_control_params_4_viz                                     &
     &   (sec_viz_ctl2%t_sect_ctl, sec_viz_ctl2%sect_plt,               &
     &    sfcing2%mesh_file_IO, sfcing2%ucd_file_IO, t_VIZ2, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_surface(t_VIZ2%init_d, sfcing2)
!
!  VIZ Initialization
      call init_visualize_surface(sfcing2%geofem, sfcing2%nod_fld,      &
     &    sec_viz_ctl2%surfacing_ctls, viz_psfs2)
!
      end subroutine init_analyzer_psf
!
!  ---------------------------------------------------------------------
!
      subroutine analyze_psf
!
      integer(kind=kint ) :: i_step
!
!
      do i_step = t_VIZ2%init_d%i_time_step, t_VIZ2%finish_d%i_end_step
        if(output_IO_flag(i_step,t_VIZ2%ucd_step) .ne. izero) cycle
        call set_IO_step_flag(i_step,t_VIZ2%ucd_step)
!
!  Load field data
        call FEM_analyze_surface                                        &
     &     (i_step, t_VIZ2%time_d, t_VIZ2%viz_step, sfcing2)
!
!  Generate field lines
        call visualize_surface(t_VIZ2%viz_step, t_VIZ2%time_d,          &
     &      sfcing2%geofem, sfcing2%nod_fld, viz_psfs2)
      end do
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze_psf
!
!  ---------------------------------------------------------------------
!
      end module analyzer_psf
