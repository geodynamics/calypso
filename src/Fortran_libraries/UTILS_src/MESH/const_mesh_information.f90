!>@file   const_mesh_information.f90
!!@brief  module const_mesh_information
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Construct mesh strucuture informations
!!
!!@verbatim
!!      subroutine empty_mesh_info(id_rank, mesh, group)
!!      subroutine dealloc_empty_mesh_info(mesh)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!
!!      subroutine const_surface_infos(id_rank, node, ele, surf_grp,    &
!!     &                               surf, surf_nod_grp)
!!      subroutine const_nod_ele_infos                                  &
!!     &         (id_rank, node, ele, nod_grp, ele_grp, surf_grp)
!!
!!      subroutine set_nod_and_ele_infosiflag_ele_mesh, (node, ele)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(node_data), intent(inout) :: node
!!        type(element_data), intent(inout) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data),    intent(inout) :: edge
!!        type(group_data), intent(inout) :: nod_grp
!!        type(group_data), intent(inout) :: ele_grp
!!        type(surface_group_data), intent(inout) :: surf_grp
!!@endverbatim
!
      module const_mesh_information
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_group_connects
      use t_surface_group_connect
      use t_surface_group_normals
      use t_surface_data
      use t_edge_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine empty_mesh_info(id_rank, mesh, group)
!
      use t_element_group_table
      use set_smp_4_group_types
      use set_connects_4_surf_group
      use set_surf_edge_mesh
      use nod_and_ele_derived_info
!
      integer, intent(in) :: id_rank
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      if (iflag_debug.eq.1) write(*,*) 'empty_nod_and_ele_infos'
      call empty_nod_and_ele_infos(mesh)
!
!
      call empty_surface_and_edge(mesh%ele, mesh%surf, mesh%edge)
!
      group%surf_grp%num_item = 0
      group%nod_grp%num_grp_smp =  0
      group%ele_grp%num_grp_smp =  0
      group%surf_grp%num_grp_smp = 0
      call count_num_groups_smp                                         &
     &   (id_rank, group%nod_grp, group%ele_grp, group%surf_grp)
!
     if (iflag_debug.eq.1) write(*,*) 'empty_surface_node_grp_type'
      call empty_surface_node_grp_type                                  &
     &   (group%surf_grp, group%surf_nod_grp)
!
      end subroutine empty_mesh_info
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_empty_mesh_info(mesh)
!
      use nod_and_ele_derived_info
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call dealloc_nod_and_ele_infos(mesh)
!
      end subroutine dealloc_empty_mesh_info
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_surface_infos(id_rank, node, ele, surf_grp,      &
     &                               surf, surf_nod_grp)
!
      use m_connect_hexa_2_tetra
      use t_element_group_table
      use cal_mesh_position
      use const_surface_data
      use set_surf_edge_mesh
      use set_connects_4_surf_group
      use set_size_4_smp_types
      use nod_and_ele_derived_info
!      use check_surface_groups
!
      integer, intent(in) :: id_rank
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: surf_grp
!
      type(surface_data), intent(inout) :: surf
      type(surface_node_grp_data), intent(inout) :: surf_nod_grp
!
      integer(kind = kint) :: num_internal_surf
!
!
      if(ele%nnod_4_ele .eq. num_t_linear) then
        call set_1_hexa_2_5_tetra
      else if (ele%nnod_4_ele .eq. num_t_quad) then
        call set_1_hexa_2_21_tetra
      else if (ele%nnod_4_ele .eq. num_t_lag) then
        call set_1_hexa_2_40_tetra
      end if
!
!     set connectivity and geometry for surface and edge
      if(iflag_debug .gt. 0) write(*,*) 'const_surf_connectivity'
      call const_surf_connectivity(node, ele, surf)
!     set connection relation of element and surface
      if (iflag_debug.gt.0) write(*,*) 'const_ele_list_4_surface'
      call const_ele_list_4_surface(ele, surf)
!
!     connectivity for surface group data and node info for each of them
!     also the smp info for surface group data
      if (iflag_debug.gt.0) write(*,*) 'set_node_4_surf_group'
      call set_node_4_surf_group(node, ele,                             &
     &    surf, surf_grp, surf_nod_grp)
!       call check_surface_node_id(id_rank, surf_nod_grp)
!
!      if (iflag_debug.gt.0) then
!        call check_surf_nod_4_sheard_para                              &
!     &     (id_rank, surf_grp%num_grp, surf_nod_grp)
!      end if
!
      if (iflag_debug.gt.0) write(*,*) 'set_center_of_surface'
      call alloc_surface_geometory(surf)
      call set_center_of_surface(node, surf)
!
      call alloc_interior_surf(surf)
      call easy_internal_element_flag                                   &
     &   (node%internal_node, surf%numsurf, surf%ie_surf,               &
     &    num_internal_surf, surf%interior_surf)
!
      end subroutine const_surface_infos
!
! ----------------------------------------------------------------------
!
!>     initial const info in node and element data structure
!>     like center of element
!>     and initial smp processor address depends on multiprocessing
      subroutine const_nod_ele_infos(id_rank, mesh, group)
!
      use nod_and_ele_derived_info
      use set_smp_4_group_types
!
      integer, intent(in) :: id_rank
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos(mesh%node, mesh%ele)
!      if (iflag_debug.gt.0) then
!        call check_nod_size_smp_type(mesh%node, id_rank)
!      end if
!
      if (iflag_debug.gt.0) write(*,*) 'count_num_groups_smp'
      call count_num_groups_smp                                         &
     &   (id_rank, group%nod_grp, group%ele_grp, group%surf_grp)
!
      end subroutine const_nod_ele_infos
!
! ----------------------------------------------------------------------
!
      end module const_mesh_information
