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
      use t_mesh_SR
      use t_elapsed_labels_4_SECTIONS
      use FEM_analyzer_viz_surf
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: fname_viz_ctl = "control_viz"
!
!>         Structure for time stepping parameters
!!          with field and visualization
      type(time_step_param_w_viz), save :: t_VIZ2
!>      Structure of control data for sectioning only
      type(control_data_section_only), save :: sec_viz_ctl2
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_for_viz), save :: FEM_viz2
!>      Structure of work area for mesh communications
      type(mesh_SR) :: m_SR12
!>      Structure of sectioning and isosurfaceing modules
      type(surfacing_modules), save :: viz_psfs2
!>      Edge communication table
      type(communication_table), save :: edge_comm_PSF
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
      subroutine init_analyzer_psf
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
      call s_input_control_section_only(fname_viz_ctl, sec_viz_ctl2,    &
     &                                  FEM_viz2, t_VIZ2)
!
!  FEM Initialization
      call FEM_initialize_surface(t_VIZ2%ucd_step, t_VIZ2%init_d,       &
     &                            FEM_viz2, edge_comm_PSF, m_SR12)
!
!  VIZ Initialization
      call init_visualize_surface(elps_SECT1, t_VIZ2%viz_step,          &
     &    FEM_viz2%geofem, edge_comm_PSF, FEM_viz2%field,               &
     &    sec_viz_ctl2%surfacing_ctls, viz_psfs2, m_SR12)
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
     &     (i_step, t_VIZ2%ucd_step, t_VIZ2%time_d, FEM_viz2, m_SR12)
!
!  Generate SEctions
        t_VIZ2%viz_step%istep_psf                                       &
     &     = istep_file_w_fix_dt(i_step, t_VIZ2%viz_step%PSF_t)
        t_VIZ2%viz_step%istep_iso                                       &
     &     = istep_file_w_fix_dt(i_step, t_VIZ2%viz_step%ISO_t)
!
        call visualize_surface(elps_SECT1,                              &
     &                         t_VIZ2%viz_step, t_VIZ2%time_d,          &
     &                         FEM_viz2%geofem, edge_comm_PSF,          &
     &                         FEM_viz2%field, viz_psfs2, m_SR12)
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
