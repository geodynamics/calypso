!>@file   multi_tracer_fieldline.f90
!!@brief  module multi_tracer_fieldline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine alloc_each_FLINE_data(num_fline, fln_prm, fln_src,   &
!!     &                                 fln_tce, fline_lc,             &
!!     &                                 fln_SR, fln_bcast)
!!      subroutine dealloc_each_FLINE_data(num_fline, fln_prm, fln_src, &
!!     &                                   fln_tce, fline_lc,           &
!!     &                                   fln_SR, fln_bcast)
!!        integer(kind = kint), intent(in) :: num_fline
!!        type(fieldline_paramter), intent(inout) :: fln_prm(num_fline)
!!        type(each_fieldline_source), intent(inout):: fln_src(num_fline)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
!!        type(local_fieldline), intent(inout) :: fline_lc(num_fline)
!!        type(trace_data_send_recv), intent(inout) :: fln_SR(num_fline)
!!        type(broadcast_trace_data),intent(inout):: fln_bcast(num_fline)
!!
!!      subroutine set_fixed_FLINE_seed_points(mesh, num_fline,         &
!!     &                                      fln_prm, fln_src)
!!        type(mesh_geometry), intent(in) :: mesh
!!        integer(kind = kint), intent(in) :: num_fline
!!        type(fieldline_paramter), intent(inout) :: fln_prm(num_fline)
!!        type(each_fieldline_source),intent(inout) :: fln_src(num_fline)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
!!      subroutine set_FLINE_seed_fields(mesh, group, para_surf,        &
!!     &          nod_fld, num_fline, fln_prm, fln_src, fln_tce)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(in) :: num_fline
!!        type(fieldline_paramter), intent(inout) :: fln_prm(num_fline)
!!        type(each_fieldline_source), intent(inout):: fln_src(num_fline)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
!!@endverbatim
!
      module multi_tracer_fieldline
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
      subroutine alloc_each_FLINE_data(num_fline, fln_prm, fln_src,     &
     &                                 fln_tce, fline_lc,               &
     &                                 fln_SR, fln_bcast)
!
      integer(kind = kint), intent(in) :: num_fline
!
      type(fieldline_paramter), intent(inout) ::    fln_prm(num_fline)
      type(each_fieldline_source), intent(inout) :: fln_src(num_fline)
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
      type(local_fieldline), intent(inout) ::      fline_lc(num_fline)
      type(trace_data_send_recv), intent(inout) :: fln_SR(num_fline)
      type(broadcast_trace_data), intent(inout) :: fln_bcast(num_fline)
!
      integer(kind = kint) :: i_fln
!
!
      do i_fln = 1, num_fline
        call alloc_start_point_fline(nprocs, fln_prm(i_fln),            &
     &                               fln_src(i_fln))
        call alloc_num_gl_start_fline(nprocs,                           &
     &                     fln_prm(i_fln)%fline_fields, fln_tce(i_fln))
        call alloc_broadcast_trace_data                                 &
     &     (fln_prm(i_fln)%num_each_field_line,                         &
     &      fln_prm(i_fln)%fline_fields, fln_bcast(i_fln))
        call alloc_local_fline(fln_prm(i_fln)%fline_fields,             &
     &                         fline_lc(i_fln))
        call alloc_trace_data_SR_num(fln_prm(i_fln)%fline_fields,       &
     &                               fln_SR(i_fln))
      end do
!
      end subroutine alloc_each_FLINE_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_each_FLINE_data(num_fline, fln_prm, fln_src,   &
     &                                   fln_tce, fline_lc,             &
     &                                   fln_SR, fln_bcast)
!
      integer(kind = kint), intent(in) :: num_fline
!
      type(fieldline_paramter), intent(inout) ::    fln_prm(num_fline)
      type(each_fieldline_source), intent(inout) :: fln_src(num_fline)
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
      type(local_fieldline), intent(inout) ::      fline_lc(num_fline)
      type(trace_data_send_recv), intent(inout) :: fln_SR(num_fline)
      type(broadcast_trace_data), intent(inout) :: fln_bcast(num_fline)
!
      integer(kind = kint) :: i_fln
!
!
      if (num_fline .le. 0) return
!
      do i_fln = 1, num_fline
        call dealloc_local_fline(fline_lc(i_fln))
        call dealloc_iflag_fline_used_ele(fln_prm(i_fln))
        call dealloc_fline_starts_ctl(fln_prm(i_fln))
!
        call dealloc_start_point_fline(fln_src(i_fln))
        call dealloc_num_gl_start_fline(fln_tce(i_fln))
        call dealloc_broadcast_trace_data(fln_bcast(i_fln))
        call dealloc_trace_data_SR_num(fln_SR(i_fln))
      end do
!
      end subroutine dealloc_each_FLINE_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_FLINE_seed_points(mesh, num_fline,           &
     &                                      fln_prm, fln_src)
!
      use m_connect_hexa_2_tetra
      use t_find_interpolate_in_ele
      use set_fline_control
      use set_fline_seeds_from_list
!
      type(mesh_geometry), intent(in) :: mesh
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(inout) :: fln_prm(num_fline)
      type(each_fieldline_source), intent(inout) :: fln_src(num_fline)
!
!
      integer(kind = kint) :: i_fln
      type(FLINE_element_size) :: fln_dist
      logical :: flag_fln_dist
!
!
      flag_fln_dist = .FALSE.
      do i_fln = 1, num_fline
        if(fln_prm(i_fln)%id_fline_seed_type                            &
     &      .eq. iflag_position_list) flag_fln_dist = .TRUE.
      end do
      if(flag_fln_dist) then
        call alloc_FLINE_element_size(mesh%ele, fln_dist)
        call cal_FLINE_element_size(mesh%node, mesh%ele, fln_dist)
      end if
      do i_fln = 1, num_fline
        if(fln_prm(i_fln)%id_fline_seed_type                            &
     &                       .eq. iflag_position_list) then
          call alloc_init_tracer_position(fln_prm(i_fln),               &
     &                                    fln_src(i_fln))
          call init_FLINE_seed_from_list(mesh%node, mesh%ele,           &
     &        fln_prm(i_fln), fln_src(i_fln), fln_dist)
        end if
      end do
      if(flag_fln_dist) call dealloc_FLINE_element_size(fln_dist)
!
      end subroutine set_fixed_FLINE_seed_points
!
!  ---------------------------------------------------------------------
!
      subroutine set_FLINE_seed_fields(mesh, group, para_surf, &
     &          nod_fld, num_fline, fln_prm, fln_src, fln_tce)
!
      use set_fields_for_fieldline
      use const_field_lines
      use collect_fline_data
      use parallel_ucd_IO_select
      use set_fline_seeds_from_list
!
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(inout) :: fln_prm(num_fline)
      type(each_fieldline_source), intent(inout) :: fln_src(num_fline)
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
!
      integer(kind = kint) :: i_fln
!  
      do i_fln = 1, num_fline
        if(fln_prm(i_fln)%id_fline_seed_type                            &
     &                       .eq. iflag_position_list) then
        else if(fln_prm(i_fln)%id_fline_seed_type                       &
     &                       .eq. iflag_position_list) then
          call count_FLINE_seed_from_list                               &
     &       (fln_prm(i_fln), fln_src(i_fln), fln_tce(i_fln))
          call set_FLINE_seed_field_from_list                           &
     &       (mesh%node, mesh%ele, nod_fld,                             &
     &        fln_prm(i_fln), fln_src(i_fln), fln_tce(i_fln))
        else
          call s_set_fields_for_fieldline                               &
     &       (mesh, group, para_surf, nod_fld,                          &
     &        fln_prm(i_fln), fln_src(i_fln), fln_tce(i_fln))
        end if
      end do
!
      end subroutine set_FLINE_seed_fields
!
!  ---------------------------------------------------------------------
!
      end module multi_tracer_fieldline
