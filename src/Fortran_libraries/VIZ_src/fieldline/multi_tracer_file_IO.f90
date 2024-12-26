!>@file   multi_tracer_file_IO.f90
!!@brief  module multi_tracer_file_IO
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine output_tracer_restarts(time_d, finish_d, rst_step,   &
!!     &                                  num_fline, fln_prm, fline_lc)
!!        type(time_data), intent(in) :: time_d
!!        type(finish_data), intent(in) :: finish_d
!!        type(IO_step_param), intent(in) :: rst_step
!!        integer(kind = kint), intent(in) :: num_fline
!!        type(fieldline_paramter), intent(in) :: fln_prm(num_fline)
!!        type(local_fieldline), intent(inout) :: fline_lc(num_fline)
!!      subroutine input_tracer_restarts(init_d, rst_step, num_fline,   &
!!     &                                 fln_prm, fline_lc)
!!        type(time_data), intent(in) :: init_d
!!        type(IO_step_param), intent(in) :: rst_step
!!        integer(kind = kint), intent(in) :: num_fline
!!        type(fieldline_paramter), intent(in) :: fln_prm(num_fline)
!!        type(local_fieldline), intent(inout) :: fline_lc(num_fline)
!!      subroutine sel_input_tracer_restarts(init_d, rst_step,          &
!!     &          num_fline, fln_prm, fln_tce, fline_lc)
!!        type(time_data), intent(in) :: init_d
!!        type(IO_step_param), intent(in) :: rst_step
!!        integer(kind = kint), intent(in) :: num_fline
!!        type(fieldline_paramter), intent(in) :: fln_prm(num_fline)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
!!        type(local_fieldline), intent(inout) :: fline_lc(num_fline)
!!
!!      subroutine output_tracer_viz_files(istep_file, time_d,          &
!!     &                                   num_fline,fln_prm, fline_lc)
!!        integer(kind = kint), intent(in) :: istep_file
!!        type(time_data), intent(in) :: time_d
!!        integer(kind = kint), intent(in) :: num_fline
!!        type(fieldline_paramter), intent(in) :: fln_prm(num_fline)
!!        type(local_fieldline), intent(in) ::    fline_lc(num_fline)
!!      subroutine output_field_lines(istep_file, time_d,               &
!!     &                              num_fline, fln_prm, fline_lc)
!!        integer(kind = kint), intent(in) :: istep_file
!!        type(time_data), intent(in) :: time_d
!!        integer(kind = kint), intent(in) :: num_fline
!!        type(fieldline_paramter), intent(inout) :: fln_prm(num_fline)
!!        type(local_fieldline), intent(inout) :: fline_lc(num_fline)
!!@endverbatim
!
      module multi_tracer_file_IO
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_paralell_surface_indices
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_trace_data_send_recv
      use t_broadcast_trace_data
      use t_tracing_data
      use t_local_fline
      use t_IO_step_parameter
      use t_ucd_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_tracer_restarts(time_d, finish_d, rst_step,     &
     &                                  num_fline, fln_prm, fline_lc)
!
      use tracer_restart_file_IO
!
      type(time_data), intent(in) :: time_d
      type(finish_data), intent(in) :: finish_d
      type(IO_step_param), intent(in) :: rst_step
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(in) :: fln_prm(num_fline)
      type(local_fieldline), intent(inout) :: fline_lc(num_fline)
!
      integer(kind = kint) :: i_fln, istep_rst
!
      if(output_IO_flag(time_d%i_time_step, rst_step)) then
        istep_rst = set_IO_step(time_d%i_time_step, rst_step)
        do i_fln = 1, num_fline
          call output_tracer_restart(fln_prm(i_fln)%fline_rst_IO,       &
     &        istep_rst, time_d, fln_prm(i_fln)%fline_fields,           &
     &        fline_lc(i_fln))
        end do
      end if
!
      if(finish_d%flag_terminate_by_elapsed) then
        do i_fln = 1, num_fline
          call output_tracer_restart(fln_prm(i_fln)%fline_rst_IO,       &
     &        -1, time_d, fln_prm(i_fln)%fline_fields,                  &
     &        fline_lc(i_fln))
        end do
      end if
!
      end subroutine output_tracer_restarts
!
!  ---------------------------------------------------------------------
!
      subroutine input_tracer_restarts(init_d, rst_step, num_fline,     &
     &                                 fln_prm, fline_lc)
!
      use trace_particle
      use tracer_restart_file_IO
!
      type(time_data), intent(in) :: init_d
      type(IO_step_param), intent(in) :: rst_step
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(in) :: fln_prm(num_fline)
      type(local_fieldline), intent(inout) :: fline_lc(num_fline)
!
      integer(kind = kint) :: i_fln, istep_rst
!
!
      istep_rst = set_IO_step(init_d%i_time_step, rst_step)
      do i_fln = 1, num_fline
        call input_tracer_restart(fln_prm(i_fln)%fline_rst_IO,          &
     &      istep_rst, init_d, fln_prm(i_fln)%fline_fields,             &
     &      fline_lc(i_fln))
      end do
!
      end subroutine input_tracer_restarts
!
!  ---------------------------------------------------------------------
!
      subroutine sel_input_tracer_restarts(init_d, rst_step,            &
     &          num_fline, fln_prm, fln_tce, fline_lc)
!
      use trace_particle
      use tracer_restart_file_IO
!
      type(time_data), intent(in) :: init_d
      type(IO_step_param), intent(in) :: rst_step
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(in) :: fln_prm(num_fline)
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
      type(local_fieldline), intent(inout) :: fline_lc(num_fline)
!
      integer(kind = kint) :: i_fln, istep_rst, ntot_comp
!
!
      istep_rst = set_IO_step(init_d%i_time_step, rst_step)
      write(*,*) 'istep_rst', istep_rst, init_d%i_time_step
      do i_fln = 1, num_fline
        ntot_comp = fln_prm(i_fln)%fline_fields%ntot_color_comp
!
        if(fln_prm(i_fln)%id_fline_seed_type                            &
     &                       .eq. iflag_read_reastart) then
          call input_tracer_restart(fln_prm(i_fln)%fline_rst_IO,        &
     &        istep_rst, init_d, fln_prm(i_fln)%fline_fields,           &
     &        fline_lc(i_fln))
        else
          call local_tracer_from_seeds(fln_prm(i_fln), fln_tce(i_fln),  &
     &                                 fline_lc(i_fln))
          call output_tracer_restart(fln_prm(i_fln)%fline_rst_IO,       &
     &        istep_rst, init_d, fln_prm(i_fln)%fline_fields,           &
     &        fline_lc(i_fln))
        end if
      end do
!
      end subroutine sel_input_tracer_restarts
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine output_tracer_viz_files(istep_file, time_d,            &
     &                                   num_fline,fln_prm, fline_lc)
!
      use t_mesh_SR
      use collect_fline_data
      use parallel_ucd_IO_select
!
      integer(kind = kint), intent(in) :: istep_file
      type(time_data), intent(in) :: time_d
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(in) :: fln_prm(num_fline)
      type(local_fieldline), intent(in) ::    fline_lc(num_fline)
!
      type(time_data) :: t_IO
      type(ucd_data) :: fline_ucd
      integer(kind = kint) :: i_fln
!
!
      do i_fln = 1, num_fline
        call copy_time_step_size_data(time_d, t_IO)
        call copy_local_particles_to_IO                                 &
     &     (fln_prm(i_fln)%fline_fields, fline_lc(i_fln), fline_ucd)
        call sel_write_parallel_ucd_file                                &
     &     (istep_file, fln_prm(i_fln)%fline_file_IO, t_IO, fline_ucd)
        call deallocate_parallel_ucd_mesh(fline_ucd)
      end do
!
      end subroutine output_tracer_viz_files
!
!  ---------------------------------------------------------------------
!
      subroutine output_field_lines(istep_file, time_d,                 &
     &                              num_fline, fln_prm, fline_lc)
!
      use set_fields_for_fieldline
      use collect_fline_data
      use parallel_ucd_IO_select
      use set_fline_seeds_from_list
      use parallel_ucd_IO_select
!
      integer(kind = kint), intent(in) :: istep_file
      type(time_data), intent(in) :: time_d
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(inout) :: fln_prm(num_fline)
      type(local_fieldline), intent(inout) :: fline_lc(num_fline)
!
      type(time_data) :: t_IO
      type(ucd_data) :: fline_ucd
      integer(kind = kint) :: i_fln
!  
!
      do i_fln = 1, num_fline
        call copy_time_step_size_data(time_d, t_IO)
        call copy_local_fieldline_to_IO(fln_prm(i_fln)%fline_fields,    &
     &                                  fline_lc(i_fln), fline_ucd)
        call sel_write_parallel_ucd_file                                &
     &     (istep_file, fln_prm(i_fln)%fline_file_IO, t_IO, fline_ucd)
        call deallocate_parallel_ucd_mesh(fline_ucd)
        call calypso_mpi_barrier
      end do
!
      end subroutine output_field_lines
!
!  ---------------------------------------------------------------------
!
      end module multi_tracer_file_IO
