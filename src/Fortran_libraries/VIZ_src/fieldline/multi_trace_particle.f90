!>@file   multi_trace_particle.f90
!!@brief  module multi_trace_particle
!!
!!@author H. Matsui
!!@date Programmed in May, 2024
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine set_tracer_controls(init_d, rst_step, mesh, group,   &
!!     &          nod_fld, num_fline, fline_ctls, fln_prm)
!!        type(time_data), intent(in) :: init_d
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(in) ::num_fline
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fieldline_paramter), intent(inout) :: fln_prm(num_fline)
!!
!!      subroutine s_multi_trace_particle(time_d, elps_tracer,          &
!!     &          mesh, para_surf, nod_fld, num_trace, fln_prm,         &
!!     &          fln_tce, particle_lc, fln_SR, fln_bcast, m_SR)
!!        type(time_data), intent(in) :: time_d
!!        type(elapsed_lables), intent(in) :: elps_tracer
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(in) :: num_trace
!!        type(fieldline_paramter), intent(in) ::      fln_prm(num_trace)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce(num_trace)
!!        type(local_fieldline), intent(inout)                          &
!!     &                      :: particle_lc(np_smp,num_trace)
!!        type(trace_data_send_recv), intent(inout) :: fln_SR(num_trace)
!!        type(broadcast_trace_data), intent(inout)                     &
!!     &                              :: fln_bcast(num_trace)
!!        type(mesh_SR), intent(inout) :: m_SR
!!
!!      subroutine alloc_each_TRACER_data(node, num_trace, fln_tce)
!!      subroutine dealloc_each_TRACER_data(num_trace, fln_tce)
!!        type(node_data), intent(in) :: node
!!        integer(kind = kint), intent(in) :: num_trace
!!        type(each_fieldline_trace), intent(inout)                     &
!!     &                            :: fln_tce(num_trace)
!!@endverbatim
!
      module multi_trace_particle
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
!
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_parallel_surface_indices
      use t_control_params_4_fline
      use t_trace_data_send_recv
      use t_broadcast_trace_data
      use t_tracing_data
      use t_local_fline
      use t_ucd_data
      use t_IO_step_parameter
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_tracer_controls(init_d, rst_step, mesh, group,     &
     &          nod_fld, num_fline, fline_ctls, fln_prm)
!
      use t_control_data_flines
      use set_fline_control

      type(time_data), intent(in) :: init_d
      type(IO_step_param), intent(in) :: rst_step
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) ::num_fline
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      type(fieldline_paramter), intent(inout) :: fln_prm(num_fline)
!
      integer(kind = kint) :: i_fln
!
      do i_fln = 1, num_fline
        call s_set_tracer_control                                       &
     &     (init_d, rst_step, mesh, group, nod_fld,                     &
     &      fline_ctls%fline_ctl_struct(i_fln), fln_prm(i_fln))
      end do
!
      end subroutine set_tracer_controls
!
!  ---------------------------------------------------------------------
!
      subroutine s_multi_trace_particle(time_d, elps_tracer,            &
     &          mesh, para_surf, nod_fld, num_trace, fln_prm,           &
     &          fln_tce, particle_lc, fln_SR, fln_bcast, m_SR)
!
      use m_work_time
      use trace_particle
!
      type(time_data), intent(in) :: time_d
      type(elapsed_lables), intent(in) :: elps_tracer
      type(mesh_geometry), intent(in) :: mesh
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: num_trace
      type(fieldline_paramter), intent(in) ::      fln_prm(num_trace)
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_trace)
      type(local_fieldline), intent(inout)                              &
     &                      :: particle_lc(np_smp,num_trace)
      type(trace_data_send_recv), intent(inout) :: fln_SR(num_trace)
      type(broadcast_trace_data), intent(inout) :: fln_bcast(num_trace)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_trc
!  
      do i_trc = 1, num_trace
        call s_trace_particle                                           &
     &     (time_d%dt, elps_tracer, mesh, para_surf, nod_fld,           &
     &      fln_prm(i_trc), fln_tce(i_trc),                             &
     &      particle_lc(1,i_trc), fln_SR(i_trc),                        &
     &      fln_bcast(i_trc), fln_tce(i_trc)%v_prev, m_SR)
      end do
!
      end subroutine s_multi_trace_particle
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_each_TRACER_data(node, num_trace, fln_tce)
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_trace
!
      type(each_fieldline_trace), intent(inout)                         &
     &                            :: fln_tce(num_trace)
!
      integer(kind = kint) :: i_fln
!
      do i_fln = 1, num_trace
        call alloc_velocity_at_previous(node%numnod, fln_tce(i_fln))
      end do
!
      end subroutine alloc_each_TRACER_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_each_TRACER_data(num_trace, fln_tce)
!
      integer(kind = kint), intent(in) :: num_trace
      type(each_fieldline_trace), intent(inout)                         &
     &                            :: fln_tce(num_trace)
!
      integer(kind = kint) :: i_fln
!
      if (num_trace .le. 0) return
      do i_fln = 1, num_trace
        call dealloc_velocity_at_previous(fln_tce(i_fln))
      end do
!
      end subroutine dealloc_each_TRACER_data
!
!  ---------------------------------------------------------------------
!
      end module multi_trace_particle
