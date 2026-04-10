!>@file   multi_tracer_fieldline.f90
!!@brief  module multi_tracer_fieldline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine alloc_each_FLINE_data(mesh, num_fline, fln_prm,      &
!!     &          fln_tce, fline_lc, fln_SR, fln_bcast)
!!      subroutine dealloc_each_FLINE_data(num_fline, fln_prm, fln_tce, &
!!     &                                   fline_lc, fln_SR, fln_bcast)
!!        type(mesh_geometry), intent(in) :: mesh
!!        integer(kind = kint), intent(in) :: num_fline
!!        type(fieldline_paramter), intent(inout) :: fln_prm(num_fline)
!!        type(each_fieldline_source), intent(inout):: fln_src(num_fline)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
!!        type(local_fieldline), intent(inout)                          &
!!     &                      :: fline_lc(np_smp,num_fline)
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
!!      subroutine set_TRACER_seed_fields(mesh, group, para_surf,       &
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
      use t_parallel_surface_indices
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_trace_data_send_recv
      use t_broadcast_trace_data
      use t_tracing_data
      use t_local_fline
      use t_IO_step_parameter
      use t_ucd_data
!
      use calypso_mpi
!
      implicit  none
!
      private :: init_FLINE_seed_from_list
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_each_FLINE_data(mesh, num_fline, fln_prm,        &
     &          fln_tce, fline_lc, fln_SR, fln_bcast)
!
      use t_mesh_data
!
      type(mesh_geometry), intent(in) :: mesh
!
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(inout) ::    fln_prm(num_fline)
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
      type(local_fieldline), intent(inout)                              &
     &                      :: fline_lc(np_smp,num_fline)
      type(trace_data_send_recv), intent(inout) :: fln_SR(num_fline)
      type(broadcast_trace_data), intent(inout) :: fln_bcast(num_fline)
!
      integer(kind = kint) :: i_fln, ip
!
!
      do i_fln = 1, num_fline
        call alloc_num_gl_start_fline(nprocs,                           &
     &                     fln_prm(i_fln)%fline_fields, fln_tce(i_fln))
        call alloc_broadcast_trace_data                                 &
     &     (fln_prm(i_fln)%num_each_field_line, fln_bcast(i_fln))
        call alloc_trace_data_SR_num(fln_SR(i_fln))
!
        do ip = 1, np_smp
          call alloc_local_fline(mesh%ele, fline_lc(ip,i_fln))
        end do
      end do
!
      end subroutine alloc_each_FLINE_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_each_FLINE_data(num_fline, fln_prm, fln_tce,   &
     &                                   fline_lc, fln_SR, fln_bcast)
!
      integer(kind = kint), intent(in) :: num_fline
!
      type(fieldline_paramter), intent(inout) ::    fln_prm(num_fline)
      type(each_fieldline_trace), intent(inout) :: fln_tce(num_fline)
      type(local_fieldline), intent(inout)                              &
     &                      :: fline_lc(np_smp,num_fline)
      type(trace_data_send_recv), intent(inout) :: fln_SR(num_fline)
      type(broadcast_trace_data), intent(inout) :: fln_bcast(num_fline)
!
      integer(kind = kint) :: i_fln, ip
!
!
      if (num_fline .le. 0) return
!
      do i_fln = 1, num_fline
        call dealloc_iflag_fline_used_ele(fln_prm(i_fln))
        call dealloc_fline_starts_ctl(fln_prm(i_fln))
!
        call dealloc_num_gl_start_fline(fln_tce(i_fln))
        call dealloc_broadcast_trace_data(fln_bcast(i_fln))
        call dealloc_trace_data_SR_num(fln_SR(i_fln))
!
        do ip = 1, np_smp*num_fline
          call dealloc_local_fline(fline_lc(ip,i_fln))
        end do
      end do
!
      end subroutine dealloc_each_FLINE_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_FLINE_seed_points(mesh, num_fline,           &
     &                                       fln_prm, fln_src)
!
      use calypso_mpi
      use m_connect_hexa_2_tetra
      use set_fline_control
      use set_fline_seeds_from_list
      use field_at_each_seed_point
!
      type(mesh_geometry), intent(in) :: mesh
      integer(kind = kint), intent(in) :: num_fline
      type(fieldline_paramter), intent(inout) :: fln_prm(num_fline)
      type(each_fieldline_source), intent(inout) :: fln_src(num_fline)
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
!
      if(flag_fln_dist) then
        call alloc_FLINE_element_size(mesh%ele, fln_dist)
        call cal_FLINE_element_size(mesh%node, mesh%ele,                &
     &                              fln_dist%ele_size)
      end if
      do i_fln = 1, num_fline
        if(fln_prm(i_fln)%id_fline_seed_type                            &
     &                       .eq. iflag_position_list) then
          call alloc_init_tracer_position(fln_prm(i_fln),               &
     &                                    fln_src(i_fln))
          call init_FLINE_seed_from_list(i_fln, mesh%node, mesh%ele,    &
     &        fln_prm(i_fln), fln_src(i_fln), fln_dist)
        end if
      end do
      if(flag_fln_dist)  call dealloc_FLINE_element_size(fln_dist)
!
      end subroutine set_fixed_FLINE_seed_points
!
!  ---------------------------------------------------------------------
!
      subroutine set_TRACER_seed_fields(mesh, group, para_surf,         &
     &          nod_fld, num_fline, fln_prm, fln_src, fln_tce)
!
      use set_fields_for_fieldline
      use collect_fline_data
      use parallel_ucd_IO_select
      use set_fline_seeds_from_list
      use copy_field_smp
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
      integer(kind = kint) :: i_fln, i_velo
!
      do i_fln = 1, num_fline
        if(fln_prm(i_fln)%id_fline_seed_type                            &
     &                       .eq. iflag_position_list) then
          call const_FLINE_seed_from_list                               &
     &       (fln_prm(i_fln), fln_src(i_fln), fln_tce(i_fln))
        else
          call s_set_fields_for_fieldline                               &
     &       (mesh, group, para_surf, nod_fld,                          &
     &        fln_src(i_fln)%num_line_local,                            &
     &        fln_prm(i_fln), fln_tce(i_fln))
        end if
!
        i_velo = fln_prm(i_fln)%iphys_4_fline
        call copy_nod_vector_smp(mesh%node%numnod,                      &
     &      nod_fld%d_fld(1,i_velo), fln_tce(i_fln)%v_prev(1,1))
      end do
!
      end subroutine set_TRACER_seed_fields
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_FLINE_seed_from_list(i_fln, node, ele,            &
     &                                     fln_prm, fln_src, fln_dist)
!
      use calypso_mpi_int
      use t_control_data_flines
      use t_find_interpolate_in_ele
      use field_at_each_seed_point
      use set_fline_control
      use set_fline_seeds_from_list
      use quicksort
!
      integer(kind = kint), intent(in) :: i_fln
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
      type(FLINE_element_size), intent(inout) :: fln_dist
!
      type(cal_interpolate_coefs_work) :: itp_ele_work_f
      integer(kind = kint) :: ierr_inter
      integer(kind = kint) :: num_search
!
      integer(kind = kint) :: i
!
!
      call alloc_work_4_interpolate(ele%nnod_4_ele, itp_ele_work_f)
!
      do i = 1, fln_prm%num_each_field_line
        call seed_distance_from_ele_center                              &
     &     (ele, fln_prm%xx_surf_start_fline(1,i), fln_dist%ele_size,   &
     &      fln_dist%index, fln_dist%distance, num_search)
!
        if(num_search .gt. 1) then
          call quicksort_real_w_index(ele%numele, fln_dist%distance(1), &
     &        ione, num_search, fln_dist%index(1))
        else
          ierr_inter = 0
        end if
!
        call find_seed_point_in_each_ele                                &
     &     (node, ele, fln_prm%xx_surf_start_fline(1,i),                &
     &      fln_dist%index, num_search, itp_ele_work_f,                 &
     &      fln_src%ip_surf_start_fline(i),                             &
     &      fln_src%iele_surf_start_fline(i),                           &
     &      fln_src%xi_surf_start_fline(1:3,i), ierr_inter)
      end do
      call dealloc_work_4_interpolate(itp_ele_work_f)
!
      fln_src%num_line_local = 0
      do i = 1, fln_prm%num_each_field_line
      if(fln_src%ip_surf_start_fline(i) .eq. my_rank)                   &
        fln_src%num_line_local = fln_src%num_line_local + 1
      end do
!
      end subroutine init_FLINE_seed_from_list
!
!  ---------------------------------------------------------------------
!
      end module multi_tracer_fieldline
