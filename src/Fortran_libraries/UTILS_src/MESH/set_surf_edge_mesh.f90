!>@file   set_surf_edge_mesh.f90
!!@brief  module set_surf_edge_mesh
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!!
!> @brief set surface and edge connectivity
!!
!!@verbatim
!!      subroutine dealloc_surface_and_edge(surf, edge)
!!      subroutine empty_surface_and_edge(ele, surf, edge)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data), intent(inout) :: edge
!!      subroutine const_surf_connectivity(node, ele, surf)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(inout) :: surf
!!      subroutine const_edge_connectivity                              &
!!     &         (node, ele, surf, irank_local, inod_local, edge)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(inout) :: edge
!!@endverbatim
!
      module set_surf_edge_mesh
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
! ----------------------------------------------------------------------
!
     contains
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_surface_and_edge(surf, edge)
!
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
!
      call dealloc_surf_connectivity(surf)
      call dealloc_edge_connectivity(edge)
!
      end subroutine dealloc_surface_and_edge
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine empty_surface_and_edge(ele, surf, edge)
!
      use const_surface_data
      use const_edge_data
!
      type(element_data), intent(in) :: ele
!
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
!
      call empty_surface_connect(ele, surf)
      call empty_edge_connect_type(ele, surf, edge)
!
      call alloc_surface_geometory(surf)
      call alloc_edge_geometory(edge)
!
      end subroutine empty_surface_and_edge
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_surf_connectivity(node, ele, surf)
!
      use const_surface_data
      use set_size_4_smp_types
!      use check_geometries
!
!      integer, intent(in) :: id_rank
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(surface_data), intent(inout) :: surf
!
      logical :: read_surface
!
!
      read_surface = allocated(surf%ie_surf)
!
      if(read_surface .eqv. .false.) then
        if (iflag_debug.eq.1) write(*,*) 'construct_surface_data'
        call construct_surface_data(node, ele, surf)
!
!        call check_surface_data(id_rank, ele, surf)
!        call check_external_surface(id_rank, surf)
!        call check_iso_surface(id_rank, surf)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'count_overlap_surf_type'
      call alloc_surf_param_smp(surf)
      call count_surf_size_smp(surf)
!
      end subroutine const_surf_connectivity
!
! ----------------------------------------------------------------------
!
      subroutine const_edge_connectivity                                &
     &         (node, ele, surf, irank_local, inod_local, edge)
!
      use const_edge_data
      use set_size_4_smp_types
!      use check_geometries
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in) :: irank_local(node%numnod)
      integer(kind = kint), intent(in) :: inod_local(node%numnod)
!
      type(edge_data), intent(inout) :: edge
!
      logical :: read_edge
!
!
      read_edge =    allocated(edge%ie_edge)
!
      if(read_edge .eqv. .false.) then
        if (iflag_debug.eq.1) write(*,*) 'construct_edge_data'
        call construct_edge_data                                        &
     &     (node, ele, surf, irank_local, inod_local, edge)
!
!        call check_edge_data(id_rank, surf, edge)
!        call check_edge_hexa_data(id_rank, ele, edge)
      end if
!
      call alloc_edge_param_smp(edge)
      call count_edge_size_smp(edge)
!
      end subroutine const_edge_connectivity
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_surf_connectivity(surf)
!
      use set_size_4_smp_types
!
      type(surface_data), intent(inout) :: surf
!
      call dealloc_surf_param_smp(surf)
!
      end subroutine dealloc_surf_connectivity
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_edge_connectivity(edge)
!
      use set_size_4_smp_types
!
      type(edge_data), intent(inout) :: edge
!
      call dealloc_edge_param_smp(edge)
!
      end subroutine dealloc_edge_connectivity
!
! ----------------------------------------------------------------------
!
      end module set_surf_edge_mesh
