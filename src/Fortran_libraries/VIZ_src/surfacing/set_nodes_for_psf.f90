!>@file   set_nodes_for_psf.f90
!!@brief  module set_nodes_for_psf
!!
!!@date  Programmed by H.Matsui in June, 2006
!
!>@brief Check node positions to generate sections
!!
!!@verbatim
!!      subroutine count_nodes_4_psf(num_psf, internal_node,            &
!!     &         numedge, nnod_4_edge, ie_edge, interior_edge,          &
!!     &         num_surf, ntot_node_sf_grp, inod_stack_sf_grp,         &
!!     &         inod_surf_grp, psf_search, psf_list, psf_grp_list,     &
!!     &         psf_mesh)
!!      subroutine count_nodes_4_iso                                    &
!!     &         (num_iso, numedge, nnod_4_edge, ie_edge,               &
!!     &          interior_edge, iso_search, iso_list, iso_mesh)
!!
!!      subroutine set_nodes_4_psf                                      &
!!      &        (num_psf, numnod, internal_node, numedge, nnod_4_edge, &
!!      &         xx, ie_edge, interior_edge, nod_comm, edge_comm,      &
!!      &         num_surf, ntot_node_sf_grp, inod_stack_sf_grp,        &
!!      &         inod_surf_grp, psf_search, psf_list, psf_mesh)
!!      subroutine set_nodes_4_iso(num_iso, numnod, numedge,            &
!!     &          nnod_4_edge, xx, ie_edge, interior_edge,              &
!!     &          iso_search, iso_list, iso_mesh)
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
      subroutine count_nodes_4_psf(num_psf, internal_node,              &
     &         numedge, nnod_4_edge, ie_edge, interior_edge,            &
     &         num_surf, ntot_node_sf_grp, inod_stack_sf_grp,           &
     &         inod_surf_grp, psf_search, psf_list, psf_grp_list,       &
     &         psf_mesh)
!
      use m_control_params_4_psf
      use t_psf_geometry_list
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: interior_edge(numedge)
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: ntot_node_sf_grp
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_surf_grp(ntot_node_sf_grp)
!
!
      type(psf_search_lists), intent(inout) :: psf_search(num_psf)
      type(sectioning_list), intent(inout) :: psf_list(num_psf)
      type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_psf
        if( id_section_method(i) .gt. 0) then
          call count_node_on_edge_4_psf                                 &
     &       (numedge, nnod_4_edge, ie_edge, interior_edge,             &
     &        psf_search(i)%edge_list, psf_list(i))
          call count_position_4_psf(psf_list(i),  psf_mesh(i)%node)
!
        else if ( id_section_method(i) .eq. 0) then
          call count_node_at_node_on_grp                                &
     &       (id_psf_group(i), internal_node, num_surf,                 &
     &        ntot_node_sf_grp, inod_stack_sf_grp, inod_surf_grp,       &
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
     &         (num_iso, numedge, nnod_4_edge, ie_edge,                 &
     &          interior_edge, iso_search, iso_list, iso_mesh)
!
      use m_control_params_4_iso
      use t_psf_geometry_list
      use t_psf_patch_data
!
      integer(kind = kint), intent(in) :: num_iso
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: interior_edge(numedge)
!
      type(psf_search_lists), intent(inout) :: iso_search(num_iso)
      type(sectioning_list), intent(inout) :: iso_list(num_iso)
      type(psf_local_data), intent(inout) :: iso_mesh(num_iso)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_iso
        call alloc_nnod_psf(np_smp, numedge, iso_list(i))
        call count_node_on_edge_4_psf                                   &
     &     (numedge, nnod_4_edge, ie_edge, interior_edge,               &
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
      subroutine set_nodes_4_psf                                        &
      &        (num_psf, numnod, internal_node, numedge, nnod_4_edge,   &
      &         xx, ie_edge, interior_edge, nod_comm, edge_comm,        &
      &         num_surf, ntot_node_sf_grp, inod_stack_sf_grp,          &
      &         inod_surf_grp, psf_search, psf_list, psf_grp_list,      &
      &         psf_mesh)
!
      use m_control_params_4_psf
      use calypso_mpi
      use t_comm_table
      use t_psf_geometry_list
      use t_psf_patch_data
      use set_node_on_edge_quad_psf
      use set_psf_nodes_4_by_surf_grp
      use cal_mesh_position
      use psf_global_nod_id
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: internal_node, numnod
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: interior_edge(numedge)
!
      integer(kind = kint), intent(in) :: num_surf, ntot_node_sf_grp
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_surf_grp(ntot_node_sf_grp)
!
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: edge_comm
!
      type(psf_search_lists), intent(in) :: psf_search(num_psf)
      type(sectioning_list), intent(inout) :: psf_list(num_psf)
      type(grp_section_list), intent(inout) :: psf_grp_list(num_psf)
      type(psf_local_data), intent(inout) :: psf_mesh(num_psf)
!
      integer(kind = kint) :: i, ist, num, igrp, inod
!
      do i = 1, num_psf
        if( id_section_method(i) .gt. 0) then
          call set_node_on_edge_4_psf                                   &
     &       (numedge, nnod_4_edge, ie_edge, interior_edge,             &
     &        psf_search(i)%edge_list, psf_list(i))
!
          call set_node_on_edge_int_quad_psf(numnod, numedge,           &
     &        nnod_4_edge, ie_edge, xx, const_psf(1,i), psf_list(i))
!
          call psf_global_nod_id_on_edge(edge_comm, numedge,            &
     &       psf_mesh(i)%node%istack_internod, psf_list(i)%id_n_on_e)
!
          call set_position_4_psf(numnod, numedge, nnod_4_edge,         &
     &       ie_edge, xx, psf_mesh(i)%node%istack_internod(my_rank),    &
     &       psf_mesh(i)%node%numnod, psf_mesh(i)%node%inod_global,     &
     &       psf_mesh(i)%node%xx, psf_list(i))
!
        else if( id_section_method(i) .eq. 0) then
          igrp = id_psf_group(i)
          ist = inod_stack_sf_grp(igrp-1)
          num = inod_stack_sf_grp(igrp  ) - ist
          call set_node_at_node_on_grp(igrp, internal_node,             &
     &          num_surf, ntot_node_sf_grp, inod_stack_sf_grp,          &
     &          inod_surf_grp, psf_grp_list(i))
!
          call psf_global_nod_id_on_node(nod_comm, numnod,              &
     &       psf_mesh(i)%node%istack_internod,                          &
     &       psf_grp_list(i)%id_n_on_n)
!
          call set_position_psf_grp                                     &
     &      (numnod, xx, psf_mesh(i)%node%istack_internod(my_rank),     &
     &       psf_mesh(i)%node%numnod, psf_mesh(i)%node%inod_global,     &
     &       psf_mesh(i)%node%xx, psf_grp_list(i))
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
      subroutine set_nodes_4_iso(num_iso, numnod, numedge,              &
     &          nnod_4_edge, xx, ie_edge, interior_edge,                &
     &          edge_comm, iso_search, iso_list, iso_mesh)
!
      use calypso_mpi
      use m_control_params_4_iso
      use t_comm_table
      use t_psf_geometry_list
      use t_psf_patch_data
      use set_node_on_edge_quad_psf
      use cal_mesh_position
      use psf_global_nod_id
      use const_element_comm_tables
!
      integer(kind = kint), intent(in) :: num_iso
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: interior_edge(numedge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      type(communication_table), intent(in) :: edge_comm
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
        call allocate_node_geometry_type(iso_mesh(i)%node)
        call const_global_numnod_list(iso_mesh(i)%node)
!
        call set_node_on_edge_4_psf                                     &
     &     (numedge,  nnod_4_edge, ie_edge, interior_edge,              &
     &      iso_search(i)%edge_list, iso_list(i))
!
        call set_node_on_edge_int_linear_psf(numnod, numedge,           &
     &      nnod_4_edge, ie_edge, iso_list(i))
!
        call psf_global_nod_id_on_edge(edge_comm, numedge,              &
     &      iso_mesh(i)%node%istack_internod, iso_list(i)%id_n_on_e)
!
!
        call set_position_4_psf(numnod, numedge, nnod_4_edge,           &
     &     ie_edge, xx, iso_mesh(i)%node%istack_internod(my_rank),      &
     &     iso_mesh(i)%node%numnod, iso_mesh(i)%node%inod_global,       &
     &     iso_mesh(i)%node%xx, iso_list(i))
!
        call set_spherical_position(iso_mesh(i)%node)
      end do
!
      end subroutine set_nodes_4_iso
!
!  ---------------------------------------------------------------------
!
      end module set_nodes_for_psf
