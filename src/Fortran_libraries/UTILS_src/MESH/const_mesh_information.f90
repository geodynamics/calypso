!>@file   const_mesh_information.f90
!!@brief  module const_mesh_information
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Construct mesh strucuture informations
!!
!!@verbatim
!!      subroutine empty_mesh_info(mesh, group)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!
!!      subroutine const_mesh_infos(id_rank, mesh, group)
!!      subroutine const_nod_ele_infos                                  &
!!     &         (id_rank, node, ele, nod_grp, ele_grp, surf_grp)
!!
!!      subroutine set_local_element_info(surf, edge)
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
!!        type(element_group_table), intent(inout) :: tbls_ele_grp
!!        type(surface_group_table), intent(inout) :: tbls_sf_grp
!!
!!      subroutine empty_nod_and_ele_type_infos(geom)
!!        type(mesh_geometry), intent(inout) :: geom
!!
!!      subroutine const_group_type_info                                &
!!     &         (node, ele, surf, edge, ele_grp, surf_grp,             &
!!     &          tbls_ele_grp, tbls_surf_grp)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data),    intent(inout) :: edge
!!        type(group_data), intent(in) ::         ele_grp
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type (element_group_table), intent(inout) :: tbls_ele_grp
!!        type (surface_group_table), intent(inout) :: tbls_surf_grp
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
      use t_surface_group_geometry
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
      subroutine empty_mesh_info(mesh, group)
!
      use set_smp_4_group_types
      use set_connects_4_ele_group
      use set_connects_4_surf_group
      use set_surf_edge_mesh
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      if (iflag_debug.eq.1) write(*,*) 'empty_nod_and_ele_type_infos'
      call empty_nod_and_ele_type_infos(mesh)
!
!
      call empty_surface_and_edge(mesh%ele, mesh%surf, mesh%edge)
!
      group%nod_grp%num_grp_smp =  0
      group%ele_grp%num_grp_smp =  0
      group%surf_grp%num_grp_smp = 0
      call count_num_groups_smp                                         &
     &   (group%nod_grp, group%ele_grp, group%surf_grp)
!
     if (iflag_debug.eq.1) write(*,*) 'empty_surface_node_grp_type'
      call empty_surface_node_grp_type                                  &
     &   (group%surf_grp, group%surf_nod_grp)
!
      if (iflag_debug.eq.1) write(*,*) 'empty_sf_ed_nod_ele_grp_type'
      call empty_sf_ed_nod_ele_grp_type                                 &
     &   (group%ele_grp, group%tbls_ele_grp)
!
      group%surf_grp%num_item = 0
      if (iflag_debug.eq.1) write(*,*) 'empty_sf_ed_nod_surf_grp_type'
      call empty_sf_ed_nod_surf_grp_type                                &
     &   (group%surf_grp, group%tbls_surf_grp)
!
      end subroutine empty_mesh_info
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_mesh_infos(id_rank, mesh, group)
!
      use const_surface_data
      use set_surf_edge_mesh
      use set_connects_4_surf_group
!      use check_surface_groups
!
      integer, intent(in) :: id_rank
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!     initial const info in node and element data structure
!     like center of element
!     also initial smp processor address depends on multiprocessing
       if (iflag_debug.gt.0) write(*,*) 'const_nod_ele_infos'
      call const_nod_ele_infos(id_rank, mesh%node, mesh%ele,            &
     &    group%nod_grp, group%ele_grp, group%surf_grp)
!
!     allocate and set node info for surface and edge
      if (iflag_debug.gt.0) write(*,*) 'set_local_element_info'
      call set_local_element_info(mesh%surf, mesh%edge)
!
!     set connectivity and geometry for surface and edge
      if (iflag_debug.gt.0) write(*,*) 'set_surface_and_edge'
      call set_surface_and_edge                                         &
     &   (mesh%node, mesh%ele, mesh%surf, mesh%edge)
!
!     set connection relation of element and surface
      if (iflag_debug.gt.0) write(*,*) 'const_ele_list_4_surface'
      call const_ele_list_4_surface(mesh%ele, mesh%surf)
!
!     connectivity for surface group data and node info for each of them
!     also the smp info for surface group data
      if (iflag_debug.gt.0) write(*,*) 'set_node_4_surf_group'
      call set_node_4_surf_group(mesh%node, mesh%ele,                   &
     &    mesh%surf, group%surf_grp, group%surf_nod_grp)
!       call check_surface_node_id(id_rank, group%surf_nod_grp)
!
!      if (iflag_debug.gt.0) then
!        call check_surf_nod_4_sheard_para                              &
!     &     (id_rank, group%surf_grp%num_grp, group%surf_nod_grp)
!      end if
!
!     set surface and element group conectivity
       if (iflag_debug.eq.1) write(*,*) 'const_group_connectiviy_1st'
      call const_group_type_info                                        &
     &   (mesh%node, mesh%ele, mesh%surf, mesh%edge,                    &
     &    group%ele_grp, group%surf_grp,                                &
     &    group%tbls_ele_grp, group%tbls_surf_grp)
!
      end subroutine const_mesh_infos
!
! ----------------------------------------------------------------------
!
      subroutine const_nod_ele_infos                                    &
     &         (id_rank, node, ele, nod_grp, ele_grp, surf_grp)
!
      use set_smp_4_group_types
!
      integer, intent(in) :: id_rank
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
!
      type(group_data), intent(inout) ::         nod_grp
      type(group_data), intent(inout) ::         ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_nod_and_ele_infos'
      call set_nod_and_ele_infos(node, ele)
!      if (iflag_debug.gt.0) then
!        call check_nod_size_smp_type(node, id_rank)
!      end if
!
       if (iflag_debug.gt.0) write(*,*) 'count_num_groups_smp'
      call count_num_groups_smp(nod_grp, ele_grp, surf_grp)
!
!       if (iflag_debug.gt.0) then
!         call check_grp_4_sheard_para(id_rank, nod_grp)
!         call check_grp_4_sheard_para(id_rank, ele_grp)
!         call check_surf_grp_4_sheard_para(id_rank, surf_grp)
!       end if
!
      end subroutine const_nod_ele_infos
!
! ----------------------------------------------------------------------
!
      subroutine set_local_element_info(surf, edge)
!
      use set_local_id_table_4_1ele
!
      type(surface_data), intent(inout) :: surf
      type(edge_data),    intent(inout) :: edge
!
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_inod_in_surf'
      call allocate_inod_in_surf(surf)
      call set_inod_in_surf(surf%nnod_4_surf,                           &
     &    surf%node_on_sf, surf%node_on_sf_n)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_inod_in_edge'
      call alloc_inod_in_edge(edge)
      call copy_inod_in_edge(edge%nnod_4_edge,                          &
     &    edge%node_on_edge, edge%node_on_edge_sf)
!
      end subroutine set_local_element_info
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
        call alloc_ele_geometry(ele)
        if (iflag_debug.eq.1) write(*,*) 'set_center_of_element'
        call set_center_of_element(node, ele)
      end if
!
       if (iflag_debug.eq.1) write(*,*) 'count_size_4_smp_mesh_type'
      call count_size_4_smp_mesh_type(node, ele)
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
! ----------------------------------------------------------------------
!
      subroutine empty_nod_and_ele_type_infos(geom)
!
      type(mesh_geometry), intent(inout) :: geom
!
!
      geom%ele%numele = 0
      call alloc_ele_geometry(geom%ele)
!
      if (iflag_debug.eq.1) write(*,*) 'allocate_node_param_smp_type'
      call allocate_node_param_smp_type(geom%node)
      call allocate_ele_param_smp_type(geom%ele)
!
      end subroutine empty_nod_and_ele_type_infos
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_group_type_info                                  &
     &         (node, ele, surf, edge, ele_grp, surf_grp,               &
     &          tbls_ele_grp, tbls_surf_grp)
!
      use set_connects_4_ele_group
      use set_connects_4_surf_group
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data),    intent(in) :: edge
      type(group_data), intent(in) ::         ele_grp
      type(surface_group_data), intent(in) :: surf_grp
!
      type (element_group_table), intent(inout) :: tbls_ele_grp
      type (surface_group_table), intent(inout) :: tbls_surf_grp
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_4_ele_group'
      call set_surf_4_ele_group(ele, surf, ele_grp, tbls_ele_grp)
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_ele_group'
      call set_edge_4_ele_group(ele, edge, ele_grp, tbls_ele_grp)
!
       if (iflag_debug.eq.1) write(*,*) 'set_node_4_ele_group'
      call set_node_4_ele_group(ele, node, ele_grp, tbls_ele_grp)
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_id_4_surf_group'
      call set_surf_id_4_surf_group(ele, surf, surf_grp, tbls_surf_grp)
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_surf_group'
      call set_edge_4_surf_group(surf, edge, surf_grp, tbls_surf_grp)
!
      end subroutine const_group_type_info
!
! ----------------------------------------------------------------------
!
      end module const_mesh_information
