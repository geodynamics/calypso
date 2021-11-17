!>@file   t_fieldline.f90
!!@brief  module t_fieldline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine FLINE_initialize                                     &
!!     &         (increment_fline, fem, nod_fld, fline_ctls, fline)
!!      subroutine FLINE_visualize                                      &
!!     &         (istep_fline, fem, next_tbl, nod_fld, fline)
!!      subroutine FLINE_finalize(fline)
!!        type(mesh_data), intent(in) :: fem
!!        type(next_nod_ele_table), intent(in) :: next_tbl
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fieldline_module), intent(inout) :: fline
!!@endverbatim
!
      module t_fieldline
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use t_mesh_data
      use t_phys_data
      use t_next_node_ele_4_node
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_local_fline
      use t_global_fieldline
!
      implicit  none
!
      type fieldline_module
        integer(kind = kint) :: num_fline
!
        type(fieldline_paramter), allocatable :: fln_prm(:)
!
        type(each_fieldline_source), allocatable :: fln_src(:)
        type(each_fieldline_trace), allocatable :: fln_tce(:)
!
        type(local_fieldline) :: fline_lc
        type(global_fieldline_data) :: fline_gl
      end type fieldline_module
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_initialize                                       &
     &         (increment_fline, fem, nod_fld, fline_ctls, fline)
!
      use calypso_mpi
      use t_control_data_flines
      use set_fline_control
!
      integer(kind = kint), intent(in) :: increment_fline
      type(mesh_data), intent(in) :: fem
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_controls), intent(inout) :: fline_ctls
      type(fieldline_module), intent(inout) :: fline
!
      integer(kind = kint) :: i_fln
!
!
      fline%num_fline = fline_ctls%num_fline_ctl
      if(increment_fline .le. 0) fline%num_fline = 0
      if(fline%num_fline .le. 0) return
!
      allocate(fline%fln_prm(fline%num_fline))
      allocate(fline%fln_src(fline%num_fline))
      allocate(fline%fln_tce(fline%num_fline))
!
      if (iflag_debug.eq.1) write(*,*) 'read_controls_4_fline'
      call read_controls_4_fline(fline%num_fline, fline_ctls)
!
      do i_fln = 1, fline%num_fline
        call s_set_fline_control(fem%mesh, fem%group, nod_fld,          &
     &      fline_ctls%fline_ctl_struct(i_fln), fline%fln_prm(i_fln),   &
     &      fline%fln_src(i_fln))
      end do
!
      call dealloc_fline_fhead_ctl(fline_ctls)
!
      do i_fln = 1, fline%num_fline
        call alloc_local_data_4_fline                                   &
     &     (fem%mesh%node, fline%fln_src(i_fln))
        call alloc_start_point_fline                                    &
     &     (fline%fln_prm(i_fln), fline%fln_src(i_fln))
        call alloc_num_gl_start_fline(nprocs,                           &
     &      fline%fln_prm(i_fln), fline%fln_tce(i_fln))
      end do
!
      call alloc_local_fline(fline%fline_lc)
      call alloc_global_fline_num(fline%fline_gl)
!
      end subroutine FLINE_initialize
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_visualize                                        &
     &         (istep_fline, fem, next_tbl, nod_fld, fline)
!
      use set_fields_for_fieldline
      use const_field_lines
      use collect_fline_data
!
      integer(kind = kint), intent(in) :: istep_fline
      type(mesh_data), intent(in) :: fem
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_module), intent(inout) :: fline
!
      integer(kind = kint) :: i_fln
!
!
      if (fline%num_fline.le.0 .or. istep_fline.le.0) return
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_field_4_fline'
      do i_fln = 1, fline%num_fline
        call set_local_field_4_fline(fem%mesh%node, nod_fld,            &
     &    fline%fln_prm(i_fln), fline%fln_src(i_fln))
!
        if (iflag_debug.eq.1) write(*,*) 's_set_fields_for_fieldline'
        call s_set_fields_for_fieldline(fem%mesh, fem%group,            &
     &      fline%fln_prm(i_fln), fline%fln_src(i_fln),                 &
     &      fline%fln_tce(i_fln))
      end do
!
      do i_fln = 1, fline%num_fline
        if (iflag_debug.eq.1) write(*,*) 's_const_field_lines', i_fln
        call s_const_field_lines(fem%mesh%node, fem%mesh%ele,           &
     &      fem%mesh%surf, next_tbl%neib_ele, fem%mesh%nod_comm,        &
     &      fline%fln_prm(i_fln), fline%fln_src(i_fln),                 &
     &      fline%fln_tce(i_fln), fline%fline_lc)
!
        if (iflag_debug.eq.1) write(*,*) 's_collect_fline_data', i_fln
       call s_collect_fline_data(istep_fline, fline%fln_prm(i_fln),     &
     &     fline%fline_lc, fline%fline_gl)
      end do
!
      end subroutine FLINE_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine FLINE_finalize(fline)
!
      type(fieldline_module), intent(inout) :: fline
!
      integer(kind = kint) :: i
!
!
      if (fline%num_fline .le. 0) return
!
      call dealloc_local_fline(fline%fline_lc)
      call dealloc_global_fline_num(fline%fline_gl)
!
      do i = 1, fline%num_fline
        call dealloc_iflag_fline_used_ele(fline%fln_prm(i))
        call dealloc_fline_starts_ctl(fline%fln_prm(i))
!
        call dealloc_local_start_grp_item(fline%fln_src(i))
        call dealloc_local_data_4_fline(fline%fln_src(i))
        call dealloc_start_point_fline(fline%fln_src(i))
        call dealloc_num_gl_start_fline(fline%fln_tce(i))
      end do
!
      deallocate(fline%fln_src, fline%fln_tce, fline%fln_prm)
!
      end subroutine FLINE_finalize
!
!  ---------------------------------------------------------------------
      end module t_fieldline
