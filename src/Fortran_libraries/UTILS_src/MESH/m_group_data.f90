!>@file   m_group_data.f90
!!@brief  module m_group_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2011
!
!>@brief group data from structure to 1st mesh modules
!!
!!@verbatim
!!      subroutine group_data_from_type(group)
!!      subroutine compare_group_type_vs_1st(my_rank, group)
!!        type(mesh_groups), intent(inout) :: group
!!
!!      subroutine const_group_connectivity_1st
!!      subroutine dealloc_grp_connectivity_1st
!!      subroutine deallocate_surf_grp_geometry
!!@endverbatim
!
      module m_group_data
!
      use m_precision
      use t_group_data
      use t_group_connects
      use t_surface_group_connect
      use t_surface_group_geometry
!
      implicit  none
!
!>  Structure for node and node group
      type(group_data), save :: nod_grp1
!>  Structure for element group
      type(group_data), save :: ele_grp1
!>  Structure for surfacet group
      type(surface_group_data), save :: sf_grp1
!
!
!>   Structure of connectivities for element group
      type(element_group_table), save :: ele_grp_tbl1
!
!>   Structure of connectivities for surface group
      type(surface_group_table), save :: sf_grp_tbl1
!> Structure of connectivity data for surface group items
      type(surface_node_grp_data), save :: sf_grp_nod1
!
!
!>   Structure of geometry data for surface group
      type(surface_group_geometry), save :: sf_grp_v1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine group_data_from_type(group)
!
      use t_mesh_data
      use t_group_data
      use copy_mesh_structures
!
      type(mesh_groups), intent(inout) :: group
!
!
      call copy_group_data(group%nod_grp, nod_grp1)
      call copy_group_data(group%ele_grp, ele_grp1)
      call copy_surface_group(group%surf_grp, sf_grp1)
!
      call dealloc_groups_data(group)
!
      end subroutine group_data_from_type
!
!-----------------------------------------------------------------------
!
      subroutine compare_group_type_vs_1st(my_rank, group_ref)
!
      use t_mesh_data
      use t_group_data
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_groups), intent(in) :: group_ref
!
!
      call compare_nod_grp_type_vs_1st                                  &
     &   (my_rank, group_ref%nod_grp, nod_grp1)
      call compare_nod_grp_type_vs_1st                                  &
     &   (my_rank, group_ref%ele_grp, ele_grp1)
      call compare_surf_grp_type_vs_1st                                 &
     &   (my_rank, group_ref%surf_grp, sf_grp1)
!
      end subroutine compare_group_type_vs_1st
!
!-----------------------------------------------------------------------
!
      subroutine const_group_connectivity_1st
!
      use m_machine_parameter
      use m_geometry_data
!
      use set_connects_4_ele_group
      use set_connects_4_surf_group
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_4_ele_group'
      call set_surf_4_ele_group(ele1, surf1, ele_grp1, ele_grp_tbl1)
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_ele_group'
      call set_edge_4_ele_group(ele1, edge1, ele_grp1, ele_grp_tbl1)
!
       if (iflag_debug.eq.1) write(*,*) 'set_node_4_ele_group'
      call set_node_4_ele_group(ele1, node1, ele_grp1, ele_grp_tbl1)
!
!
!
       if (iflag_debug.eq.1) write(*,*) 'set_surf_id_4_surf_group'
      call set_surf_id_4_surf_group(ele1, surf1, sf_grp1, sf_grp_tbl1)
!
       if (iflag_debug.eq.1) write(*,*) 'set_edge_4_surf_group'
      call set_edge_4_surf_group(surf1, edge1, sf_grp1, sf_grp_tbl1)
!
      end subroutine const_group_connectivity_1st
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_grp_connectivity_1st
!
!
      call dealloc_grp_connect(sf_grp_tbl1%edge)
      call dealloc_surf_item_sf_grp_type(sf_grp_tbl1)
      call dealloc_num_surf_grp_nod_smp(sf_grp_nod1)
      call dealloc_surf_grp_nod(sf_grp_nod1)
!
      call dealloc_grp_connect(ele_grp_tbl1%surf)
      call dealloc_grp_connect(ele_grp_tbl1%edge)
      call dealloc_grp_connect(ele_grp_tbl1%node)
!
      end subroutine dealloc_grp_connectivity_1st
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_surf_grp_geometry
!
      call dealloc_surf_grp_type_geom(sf_grp_v1)
!
      end subroutine deallocate_surf_grp_geometry
!
!-----------------------------------------------------------------------
!
      end module m_group_data
