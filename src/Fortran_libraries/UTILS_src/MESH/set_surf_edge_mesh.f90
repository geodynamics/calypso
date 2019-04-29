!set_surf_edge_mesh.f90
!      module set_surf_edge_mesh
!
!     Written by H. Matsui on Dec., 2008
!
!!      subroutine set_surface_and_edge(node, ele, surf, edge)
!!      subroutine empty_surface_and_edge(ele, surf, edge)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data), intent(inout) :: edge
!!
!!      subroutine set_surf_connectivity(node, ele, surf)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(inout) :: surf
!!      subroutine set_edge_connectivity(node, ele, surf, edge)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(inout) :: edge
!!
!!      subroutine set_surf_geometry(node, surf)
!!      subroutine set_edge_geometry(node, edge)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data), intent(inout) :: edge
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
      private :: set_surf_connectivity, set_edge_connectivity
      private :: set_surf_geometry, set_edge_geometry
!
! ----------------------------------------------------------------------
!
     contains
!
! ----------------------------------------------------------------------
!
      subroutine set_surface_and_edge(node, ele, surf, edge)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_surf_connectivity'
      call set_surf_connectivity(node, ele, surf)
      if (iflag_debug.gt.0) write(*,*) 'set_edge_connectivity'
      call set_edge_connectivity(node, ele, surf, edge)
!
      if (iflag_debug.gt.0) write(*,*) 'set_surf_geometry'
      call set_surf_geometry(node, surf)
      if (iflag_debug.gt.0) write(*,*) 'set_edge_geometry'
      call set_edge_geometry(node, edge)
!
      end subroutine set_surface_and_edge
!
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
      call allocate_surface_geom_type(surf)
      call alloc_edge_geometory(edge)
!
      end subroutine empty_surface_and_edge
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_surf_connectivity(node, ele, surf)
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
      read_surface = allocated(surf%isurf_global)
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
      call count_surf_size_smp_type(surf)
      call count_overlap_surf(node, surf)
!
      end subroutine set_surf_connectivity
!
! ----------------------------------------------------------------------
!
      subroutine set_edge_connectivity(node, ele, surf, edge)
!
      use const_edge_data
      use set_size_4_smp_types
!      use check_geometries
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(edge_data), intent(inout) :: edge
!
      logical :: read_edge
!
!
      read_edge =    allocated(edge%iedge_global)
!
      if(read_edge .eqv. .false.) then
        if (iflag_debug.eq.1) write(*,*) 'construct_edge_data'
        call construct_edge_data(node, ele, surf, edge)
!
!        call check_edge_data(id_rank, surf, edge)
!        call check_edge_hexa_data(id_rank, ele, edge)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'count_overlap_edge'
      call count_edge_size_smp_type(edge)
      call count_overlap_edge(node, edge)
!
      end subroutine set_edge_connectivity
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_surf_geometry(node, surf)
!
      use cal_mesh_position
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(inout) :: surf
!
!
      call allocate_surface_geom_type(surf)
      call set_center_of_surface(node, surf)
!
      end subroutine set_surf_geometry
!
! ----------------------------------------------------------------------
!
      subroutine set_edge_geometry(node, edge)
!
      use cal_mesh_position
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(inout) :: edge
!
!
      call alloc_edge_geometory(edge)
      call set_center_of_edge(node, edge)
!
      end subroutine set_edge_geometry
!
! ----------------------------------------------------------------------
!
      end module set_surf_edge_mesh
