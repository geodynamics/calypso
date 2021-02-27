!>@file   set_size_4_smp_types.f90
!!@brief  module set_size_4_smp_types
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief set numbers for SMP parallelization
!!
!!@verbatim
!!      subroutine count_size_4_smp_mesh(nod, ele)
!!      subroutine finalize_size_4_smp_mesh(nod, ele)
!!        type(node_data),    intent(inout) :: nod
!!        type(element_data), intent(inout) :: ele
!!      subroutine count_size_4_smp_surf_edge(surf, edge)
!!      subroutine finalize_size_4_smp_surf_edge(surf, edge)
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data),    intent(inout) :: edge
!!
!!      subroutine count_node_4_smp_mesh(nod)
!!        type(node_data),    intent(inout) :: nod
!!      subroutine count_surf_size_smp(surf)
!!        type(surface_data), intent(inout) :: surf
!!      subroutine count_edge_size_smp(edge)
!!        type(edge_data),    intent(inout) :: edge
!!
!!      subroutine count_overlap_ele(nod, ele)
!!        type(node_data),    intent(in) :: nod
!!        type(element_data), intent(inout) :: ele
!!@endverbatim
!
      module set_size_4_smp_types
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
      private :: count_ele_4_smp_mesh
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_size_4_smp_mesh(nod, ele)
!
      type(node_data),    intent(inout) :: nod
      type(element_data), intent(inout) :: ele
!
!
      call alloc_node_param_smp(nod)
      call alloc_ele_param_smp(ele)
!
      call count_node_4_smp_mesh(nod)
      call count_ele_4_smp_mesh(ele)
!
      end subroutine count_size_4_smp_mesh
!
!-----------------------------------------------------------------------
!
      subroutine count_size_4_smp_surf_edge(surf, edge)
!
      type(surface_data), intent(inout) :: surf
      type(edge_data),    intent(inout) :: edge
!
!
      call alloc_surf_param_smp(surf)
      call alloc_edge_param_smp(edge)
!
      call count_surf_size_smp(surf)
      call count_edge_size_smp(edge)
!
      end subroutine count_size_4_smp_surf_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_node_4_smp_mesh(nod)
!
      use cal_minmax_and_stacks
!
      type(node_data),    intent(inout) :: nod
!
!
      call count_number_4_smp( np_smp, ione, nod%numnod,                &
     &    nod%istack_nod_smp, nod%max_nod_smp )
!
      call count_number_4_smp( np_smp, ione, nod%internal_node,         &
     &    nod%istack_internal_smp, nod%max_internal_nod_smp )
!
      end subroutine count_node_4_smp_mesh
!
!-----------------------------------------------------------------------
!
      subroutine count_ele_4_smp_mesh(ele)
!
      use cal_minmax_and_stacks
!
      type(element_data), intent(inout) :: ele
!
!
      call count_number_4_smp( np_smp, ione, ele%numele,                &
     &    ele%istack_ele_smp, ele%max_ele_smp )
!
      end subroutine count_ele_4_smp_mesh
!
!-----------------------------------------------------------------------
!
      subroutine count_surf_size_smp(surf)
!
      use cal_minmax_and_stacks
!
      type(surface_data), intent(inout) :: surf
!
!
      call count_number_4_smp(np_smp, ione, surf%numsurf,               &
     &    surf%istack_surf_smp, surf%max_surf_smp)
!
      end subroutine count_surf_size_smp
!
!-----------------------------------------------------------------------
!
      subroutine count_edge_size_smp(edge)
!
      use cal_minmax_and_stacks
!
      type(edge_data),    intent(inout) :: edge
!
      call count_number_4_smp(np_smp, ione, edge%numedge,               &
     &    edge%istack_edge_smp, edge%max_edge_smp)
!
      end subroutine count_edge_size_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine finalize_size_4_smp_mesh(nod, ele)
!
      type(node_data),    intent(inout) :: nod
      type(element_data), intent(inout) :: ele
!
      call dealloc_node_param_smp(nod)
      call dealloc_ele_param_smp(ele)
!
      end subroutine finalize_size_4_smp_mesh
!
!-----------------------------------------------------------------------
!
      subroutine finalize_size_4_smp_surf_edge(surf, edge)
!
      type(surface_data), intent(inout) :: surf
      type(edge_data),    intent(inout) :: edge
!
!
      call dealloc_surf_param_smp(surf)
      call dealloc_edge_param_smp(edge)
!
      end subroutine finalize_size_4_smp_surf_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_overlap_ele(nod, ele)
!
      use count_overlap
!
      type(node_data),    intent(in) :: nod
      type(element_data), intent(inout) :: ele
!
!
      call set_overlap_flag(np_smp, ele%istack_ele_smp,                 &
     &    nod%internal_node, ele%numele, ele%ie(1:ele%numele,1),        &
     &    ele%internal_ele, ele%interior_ele)
!
      end subroutine count_overlap_ele
!
! ----------------------------------------------------------------------
!
      end module set_size_4_smp_types
