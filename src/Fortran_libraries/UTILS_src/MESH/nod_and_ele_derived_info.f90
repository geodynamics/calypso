!>@file   nod_and_ele_derived_info.f90
!!@brief  module nod_and_ele_derived_info
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Construct node and element derived informations
!!
!!@verbatim
!!      subroutine set_nod_and_ele_infos(node, ele)
!!        type(node_data), intent(inout) :: node
!!        type(element_data), intent(inout) :: ele
!!      subroutine dealloc_nod_and_ele_infos(mesh)
!!      subroutine empty_nod_and_ele_infos(mesh)
!!        type(mesh_geometry), intent(inout) :: mesh
!!
!!      subroutine dup_nod_infos(org_mesh, new_mesh)
!!        type(mesh_geometry), intent(in) :: org_mesh
!!        type(mesh_geometry), intent(inout) :: new_mesh
!!
!!      subroutine easy_internal_element_flag(internal_node, numele, ie,&
!!     &          internal_ele, interior_ele)
!!        integer(kind = kint), intent(in) :: internal_node
!!        integer(kind = kint), intent(in) :: numele
!!        integer(kind = kint), intent(in) :: ie(numele,1)
!!        integer(kind = kint), intent(inout) :: internal_ele
!!        integer(kind = kint), intent(inout) :: interior_ele(numele)
!!@endverbatim
!!
      module nod_and_ele_derived_info
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_nod_and_ele_infos(node, ele)
!
      use set_size_4_smp_types
      use cal_mesh_position
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
!
      logical :: read_element
!
!
      read_element = allocated(ele%x_ele)
!
      if(read_element .eqv. .false.) then
        call alloc_overlapped_ele(ele)

        call alloc_ele_geometry(ele)
        if (iflag_debug.eq.1) write(*,*) 'set_center_of_element'
        call set_center_of_element(node, ele)
      end if
      call easy_internal_element_flag                                   &
     &   (node%internal_node, ele%numele, ele%ie(1,1),                  &
     &    ele%internal_ele, ele%interior_ele)
!
       if (iflag_debug.eq.1) write(*,*) 'count_size_4_smp_mesh'
      call count_size_4_smp_mesh(node, ele)
!
       if (iflag_debug.eq.1) write(*,*) 'set_spherical_position'
      call set_spherical_position(node)
!
      call find_subdomain_position_range(node)
!
       if (iflag_debug.eq.1) write(*,*) 'count_overlap_ele'
      call count_overlap_ele(node, ele)
!
      end subroutine set_nod_and_ele_infos
!
! ----------------------------------------------------------------------
!
      subroutine empty_nod_and_ele_infos(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      mesh%ele%numele = 0
      call alloc_overlapped_ele(mesh%ele)
      call alloc_ele_geometry(mesh%ele)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_node_param_smp'
      call alloc_node_param_smp(mesh%node)
      call alloc_ele_param_smp(mesh%ele)
!
      end subroutine empty_nod_and_ele_infos
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_nod_and_ele_infos(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call dealloc_overlapped_ele(mesh%ele)
      call dealloc_ele_geometry(mesh%ele)
      call dealloc_ele_param_smp(mesh%ele)
      call dealloc_node_param_smp(mesh%node)
!
      end subroutine dealloc_nod_and_ele_infos
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine easy_internal_element_flag(internal_node, numele, ie,  &
     &          internal_ele, interior_ele)
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ie(numele,1)
!
      integer(kind = kint), intent(inout) :: internal_ele
      integer(kind = kint), intent(inout) :: interior_ele(numele)
!
      integer(kind = kint) :: iele, icou
!
!
!$omp parallel workshare
      interior_ele(1:numele) = 0
!$omp end parallel workshare
!
!%omp parallel do private(iele)
      do iele = 1, numele
        if(ie(iele,1) .le. internal_node) interior_ele(iele) = 1
      end do
!%omp end parallel do
!
      icou = 0
!%omp parallel do private(iele) reduction(+:icou)
      do iele = 1, numele
        icou = icou + interior_ele(iele)
      end do
!%omp end parallel do
      internal_ele = icou
!
      end subroutine easy_internal_element_flag
!
! ----------------------------------------------------------------------
!
      end module nod_and_ele_derived_info
