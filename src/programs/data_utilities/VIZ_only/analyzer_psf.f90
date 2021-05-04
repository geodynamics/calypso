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
      use t_viz_sections
      use t_VIZ_only_step_parameter
      use t_control_data_section_only
      use t_FEM_mesh_field_4_viz
      use FEM_analyzer_viz_surf
!
      implicit none
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ2
!>      Structure of control data for sectioning only
      type(control_data_section_only), save :: sec_viz_ctl2
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz2
!>      Structure of sectioning and isosurfaceing modules
      type(surfacing_modules), save :: viz_psfs2
!>      Edge communication table
      type(communication_table), save :: edge_comm_PSF
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
      call read_control_file_section_only(sec_viz_ctl2)
      call set_control_params_4_sections(sec_viz_ctl2,                  &
     &                                   FEM_viz2, t_VIZ2, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_surface                                       &
     &   (t_VIZ2%ucd_step, t_VIZ2%init_d, FEM_viz2, edge_comm_PSF)
!
!  VIZ Initialization
      call init_visualize_surface                                       &
     &   (FEM_viz2%geofem, edge_comm_PSF, FEM_viz2%field,               &
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
        if(output_IO_flag(i_step,t_VIZ2%ucd_step) .eqv. .FALSE.) cycle
        if(iflag_vizs_w_fix_step(i_step,t_VIZ2%viz_step)                &
     &       .eqv. .FALSE.) cycle
!
!  Load field data
        call FEM_analyze_surface                                        &
     &     (i_step, t_VIZ2%ucd_step, t_VIZ2%time_d, FEM_viz2)
!
!  Generate field lines
        t_VIZ2%viz_step%istep_psf                                       &
     &     = istep_file_w_fix_dt(i_step, t_VIZ2%viz_step%PSF_t)
        t_VIZ2%viz_step%istep_iso                                       &
     &     = istep_file_w_fix_dt(i_step, t_VIZ2%viz_step%ISO_t)
!
        call visualize_surface(t_VIZ2%viz_step, t_VIZ2%time_d,          &
     &      FEM_viz2%geofem, edge_comm_PSF, FEM_viz2%field,             &
     &      viz_psfs2)
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
