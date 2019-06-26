!>@file   set_nodes_for_psf.f90
!!@brief  module set_nodes_for_psf
!!
!!@date  Programmed by H.Matsui in June, 2006
!
!>@brief Check node positions to generate sections
!!
!!@verbatim
!!      subroutine count_nodes_4_psf                                    &
!!     &         (num_psf, node, edge, sf_grp, sf_grp_nod,              &
!!     &          psf_def, psf_search, psf_list, psf_grp_list, psf_mesh)
!!        type(node_data), intent(in) :: node
!!        type(edge_data), intent(in) :: edge
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(section_define), intent(in) :: psf_def(num_psf)
!!        type(psf_search_lists), intent(in) :: psf_search(num_psf)
!!        type(sectioning_list), intent(inout) :: psf_list(num_psf)
!!        type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!      subroutine count_nodes_4_iso                                    &
!!     &         (num_iso, edge, iso_search, iso_list, iso_mesh)
!!        type(edge_data), intent(in) :: edge
!!        type(psf_search_lists), intent(inout) :: iso_search(num_iso)
!!        type(sectioning_list), intent(inout) :: iso_list(num_iso)
!!        type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!!
!!      subroutine set_nodes_4_psf(num_psf, node, edge, nod_comm,       &
!!      &         sf_grp, sf_grp_nod, psf_def, psf_search,              &
!!      &         psf_list, psf_grp_list, psf_mesh)
!!        type(node_data), intent(in) :: node
!!        type(edge_data), intent(in) :: edge
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(communication_table), intent(in) :: nod_comm
!!        type(section_define), intent(in) :: psf_def(num_psf)
!!        type(psf_search_lists), intent(in) :: psf_search(num_psf)
!!        type(sectioning_list), intent(inout) :: psf_list(num_psf)
!!        type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
!!        type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!!      subroutine set_nodes_4_iso(num_iso, node, edge, nod_comm,       &
!!     &          iso_search, iso_list, iso_mesh)
!!        type(node_data), intent(in) :: node
!!        type(edge_data), intent(in) :: edge
!!        type(communication_table), intent(in) :: nod_comm
!!        type(psf_search_lists), intent(in) :: iso_search(num_iso)
!!        type(sectioning_list), intent(inout) :: iso_list(num_iso)
!!        type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!!@endverbatim
!
      module set_nodes_for_psf
!
      use m_precision
!
      use m_machine_parameter
      use set_node_for_sections
      use set_nodal_field_for_psf
      use set_psf_nodes_4_by_surf_grp
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_nodes_4_psf                                      &
     &         (num_psf, node, edge, sf_grp, sf_grp_nod,                &
     &          psf_def, psf_search, psf_list, psf_grp_list, psf_mesh)
!
      use t_control_params_4_psf
      use t_geometry_data
      use t_edge_data
      use t_group_data
      use t_surface_group_connect
      use t_psf_geometry_list
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_psf
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
      type(section_define), intent(in) :: psf_def(num_psf)
      type(psf_search_lists), intent(in) :: psf_search(num_psf)
!
      type(sectioning_list), intent(inout) :: psf_list(num_psf)
      type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_psf
        if(psf_def(i)%id_section_method .gt. 0) then
          call count_node_on_edge_4_psf                                 &
     &       (edge%numedge, edge%nnod_4_edge, edge%ie_edge,             &
     &        edge%interior_edge, psf_search(i)%edge_list, psf_list(i))
          call count_position_4_psf(psf_list(i),  psf_mesh(i)%node)
!
          psf_grp_list(i)%internod_on_nod = 0
          psf_grp_list(i)%externod_on_nod = 0
!
        else if(psf_def(i)%id_section_method .eq. 0) then
          call count_node_at_node_on_grp                                &
     &       (psf_def(i)%id_psf_group, node%internal_node,              &
     &        sf_grp%num_grp, sf_grp_nod%ntot_node_sf_grp,              &
     &        sf_grp_nod%inod_stack_sf_grp, sf_grp_nod%inod_surf_grp,   &
     &        psf_grp_list(i))
!
          call count_position_4_psf_grp                                 &
     &      (psf_grp_list(i), psf_mesh(i)%node)
        end if
!
      end do
!
      end subroutine count_nodes_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine count_nodes_4_iso                                      &
     &         (num_iso, edge, iso_search, iso_list, iso_mesh)
!
      use t_edge_data
      use t_psf_geometry_list
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_iso
      type(edge_data), intent(in) :: edge
!
      type(psf_search_lists), intent(inout) :: iso_search(num_iso)
      type(sectioning_list), intent(inout) :: iso_list(num_iso)
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_iso
        call alloc_nnod_psf(np_smp, edge, iso_list(i))
        call count_node_on_edge_4_psf(edge%numedge, edge%nnod_4_edge,   &
     &      edge%ie_edge, edge%interior_edge,                           &
     &      iso_search(i)%edge_list, iso_list(i))
!
        call count_position_4_psf(iso_list(i),  iso_mesh(i)%node)
      end do
!
      end subroutine count_nodes_4_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_nodes_4_psf(num_psf, node, edge, nod_comm,         &
      &         sf_grp, sf_grp_nod, psf_def, psf_search,                &
      &         psf_list, psf_grp_list, psf_mesh)
!
      use calypso_mpi
      use t_control_params_4_psf
      use t_comm_table
      use t_geometry_data
      use t_edge_data
      use t_group_data
      use t_surface_group_connect
      use t_psf_geometry_list
      use t_psf_patch_data
      use set_node_on_edge_quad_psf
      use set_psf_nodes_4_by_surf_grp
      use cal_mesh_position
      use psf_global_nod_id
!
      integer(kind = kint), intent(in) :: num_psf
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
      type(communication_table), intent(in) :: nod_comm
!
      type(section_define), intent(in) :: psf_def(num_psf)
      type(psf_search_lists), intent(in) :: psf_search(num_psf)
      type(sectioning_list), intent(inout) :: psf_list(num_psf)
      type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i, ist, num, igrp, inod
!
!
      do i = 1, num_psf
        if(psf_def(i)%id_section_method .gt. 0) then
          call set_node_on_edge_4_psf                                   &
     &       (edge%numedge, edge%nnod_4_edge, edge%ie_edge,             &
     &        edge%interior_edge, psf_search(i)%edge_list, psf_list(i))
!
          call set_node_on_edge_int_quad_psf                            &
     &       (node%numnod, edge%numedge, edge%nnod_4_edge,              &
     &        edge%ie_edge, node%xx, psf_def(i)%const_psf, psf_list(i))
!
          call alloc_iedge_global_psf(psf_list(i))
          call const_edge_comm_table_4_psf                              &
     &       (node, nod_comm, edge, psf_list(i))
!
          call psf_global_nod_id_on_edge(edge%numedge,                  &
     &        psf_mesh(i)%node%istack_internod, psf_list(i))
          call dealloc_iedge_global_psf(psf_list(i))
!
          call set_position_4_psf(node%numnod,                          &
     &        edge%numedge, edge%nnod_4_edge, edge%ie_edge,             &
     &        node%xx, psf_mesh(i)%node%istack_internod(my_rank),       &
     &        psf_mesh(i)%node%numnod, psf_mesh(i)%node%inod_global,    &
     &        psf_mesh(i)%node%xx, psf_list(i))
!
        else if(psf_def(i)%id_section_method .eq. 0) then
          igrp = psf_def(i)%id_psf_group
          ist = sf_grp_nod%inod_stack_sf_grp(igrp-1)
          num = sf_grp_nod%inod_stack_sf_grp(igrp  ) - ist
          call set_node_at_node_on_grp(igrp, node%internal_node,        &
     &        sf_grp%num_grp, sf_grp_nod%ntot_node_sf_grp,              &
     &        sf_grp_nod%inod_stack_sf_grp, sf_grp_nod%inod_surf_grp,   &
     &        psf_grp_list(i))
!
          call psf_global_nod_id_on_node(nod_comm, node%numnod,         &
     &       psf_mesh(i)%node%istack_internod,                          &
     &       psf_grp_list(i)%id_n_on_n)
!
          call set_position_psf_grp(node%numnod, node%xx,               &
     &        psf_mesh(i)%node%istack_internod(my_rank),                &
     &        psf_mesh(i)%node%numnod, psf_mesh(i)%node%inod_global,    &
     &        psf_mesh(i)%node%xx, psf_grp_list(i))
        end if
!
        call set_spherical_position(psf_mesh(i)%node)
!
        do inod  = 1, psf_mesh(i)%node%numnod
          if(psf_mesh(i)%node%rr(inod) .gt. 1.0e4) then
            write(*,*) my_rank, inod, psf_mesh(i)%node%xx(inod,1:3)
          end if
        end do
      end do
!
      end subroutine set_nodes_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_nodes_4_iso(num_iso, node, edge, nod_comm,         &
     &          iso_search, iso_list, iso_mesh)
!
      use calypso_mpi
      use t_geometry_data
      use t_edge_data
      use t_comm_table
      use t_psf_geometry_list
      use t_psf_patch_data
      use set_node_on_edge_quad_psf
      use cal_mesh_position
      use psf_global_nod_id
      use const_element_comm_tables
!
      integer(kind = kint), intent(in) :: num_iso
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: nod_comm
!
      type(psf_search_lists), intent(in) :: iso_search(num_iso)
      type(sectioning_list), intent(inout) :: iso_list(num_iso)
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_iso
        call alloc_inod_psf(iso_list(i))
        call alloc_node_geometry_w_sph(iso_mesh(i)%node)
        call const_global_numnod_list(iso_mesh(i)%node)
!
        call set_node_on_edge_4_psf                                     &
     &     (edge%numedge, edge%nnod_4_edge, edge%ie_edge,               &
     &      edge%interior_edge, iso_search(i)%edge_list, iso_list(i))
!
        call set_node_on_edge_int_linear_psf(node%numnod, edge%numedge, &
     &      edge%nnod_4_edge, edge%ie_edge, iso_list(i))
!
        call alloc_iedge_global_psf(iso_list(i))
        call const_edge_comm_table_4_psf                                &
     &       (node, nod_comm, edge, iso_list(i))
!
        call psf_global_nod_id_on_edge(edge%numedge,                    &
     &      iso_mesh(i)%node%istack_internod, iso_list(i))
        call dealloc_iedge_global_psf(iso_list(i))
!
!
        call set_position_4_psf                                         &
     &     (node%numnod, edge%numedge, edge%nnod_4_edge, edge%ie_edge,  &
     &      node%xx, iso_mesh(i)%node%istack_internod(my_rank),         &
     &      iso_mesh(i)%node%numnod, iso_mesh(i)%node%inod_global,      &
     &      iso_mesh(i)%node%xx, iso_list(i))
!
        call set_spherical_position(iso_mesh(i)%node)
      end do
!
      end subroutine set_nodes_4_iso
!
!  ---------------------------------------------------------------------
!
      end module set_nodes_for_psf
