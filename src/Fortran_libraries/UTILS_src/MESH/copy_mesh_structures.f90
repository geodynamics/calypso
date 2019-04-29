!>@file   copy_mesh_structures.f90
!!@brief  module copy_mesh_structures
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!
!>@brief Copy FEM mesh structures
!!
!!@verbatim
!!      subroutine set_mesh_data_from_type                              &
!!     &         (mesh, group, tgt_mesh, tgt_e_mesh, tgt_grp)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!        type(mesh_geometry), intent(inout) :: tgt_mesh
!!        type(element_geometry), intent(inout) :: tgt_e_mesh
!!        type(mesh_groups), intent(inout) :: tgt_grp
!!
!!      subroutine copy_node_geometry_types(org_node, new_node)
!!      subroutine copy_node_sph_to_xx(org_node, new_node)
!!      subroutine copy_node_cyl_to_xx(org_node, new_node)
!!        type(node_data), intent(in) :: org_node
!!        type(node_data), intent(in) :: new_node
!!      subroutine copy_element_connect_types(org_ele, new_ele)
!!        type(element_data), intent(in) :: org_ele
!!        type(element_data), intent(in) :: new_ele
!!
!!      subroutine copy_comm_tbl_types(org_comm, comm_tbls)
!!        type(communication_table), intent(in) :: org_comm
!!        type(communication_table), intent(inout) :: new_comm
!!
!!      subroutine copy_group_data(org_grp, new_grp)
!!        type(group_data), intent(in) :: org_grp
!!        type(group_data), intent(inout) :: new_grp
!!      subroutine copy_surface_group(org_sf_grp, new_sf_grp)
!!        type(surface_group_data), intent(in) :: org_sf_grp
!!        type(surface_group_data), intent(inout) :: new_sf_grp
!!@endverbatim
!
      module copy_mesh_structures
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_data_from_type                                &
     &         (mesh, group, tgt_mesh, tgt_e_mesh, tgt_grp)
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use set_nnod_4_ele_by_type
!
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
      type(mesh_geometry), intent(inout) :: tgt_mesh
      type(element_geometry), intent(inout) :: tgt_e_mesh
      type(mesh_groups), intent(inout) :: tgt_grp
!
!
      call copy_comm_tbl_types(mesh%nod_comm, tgt_mesh%nod_comm)
!
      call copy_node_geometry_types(mesh%node, tgt_mesh%node)
      call copy_element_connect_types(mesh%ele, tgt_mesh%ele)
!
!
      call copy_group_data(group%nod_grp, tgt_grp%nod_grp)
      call copy_group_data(group%ele_grp, tgt_grp%ele_grp)
      call copy_surface_group(group%surf_grp, tgt_grp%surf_grp)
!
      call alloc_sph_node_geometry(tgt_mesh%node)
!      call alloc_ele_geometry(tgt_mesh%ele)
      call set_3D_nnod_4_sfed_by_ele(tgt_mesh%ele%nnod_4_ele,           &
     &    tgt_e_mesh%surf%nnod_4_surf, tgt_e_mesh%edge%nnod_4_edge)
!
      call dealloc_groups_data(group)
      call dealloc_mesh_geometry_base(mesh)
!
      end subroutine set_mesh_data_from_type
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_node_geometry_types(org_node, new_node)
!
      use t_geometry_data
!
      type(node_data), intent(in) ::    org_node
      type(node_data), intent(inout) :: new_node
!
      integer(kind = kint) :: inod
!
!
      new_node%numnod =        org_node%numnod
      new_node%internal_node = org_node%internal_node
!
      call alloc_node_geometry_base(new_node)
!
!$omp parallel do
      do inod = 1, new_node%numnod
        new_node%inod_global(inod) = org_node%inod_global(inod)
        new_node%xx(inod,1) = org_node%xx(inod,1)
        new_node%xx(inod,2) = org_node%xx(inod,2)
        new_node%xx(inod,3) = org_node%xx(inod,3)
      end do
!$omp end parallel do
!
      end subroutine copy_node_geometry_types
!
!------------------------------------------------------------------
!
      subroutine copy_node_sph_to_xx(org_node, new_node)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
      integer(kind = kint) :: inod
!
!
      new_node%numnod =        org_node%numnod
      new_node%internal_node = org_node%internal_node
!
      call alloc_node_geometry_base(new_node)
!
!$omp parallel do
      do inod = 1, org_node%numnod
        new_node%inod_global(inod) = org_node%inod_global(inod)
        new_node%xx(inod,1) = org_node%rr(inod)
        new_node%xx(inod,2) = org_node%theta(inod)
        new_node%xx(inod,3) = org_node%phi(inod)
      end do
!$omp end parallel do
!
!
      end subroutine copy_node_sph_to_xx
!
!------------------------------------------------------------------
!
      subroutine copy_node_cyl_to_xx(org_node, new_node)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
      integer(kind = kint) :: inod
!
!
      new_node%numnod =        org_node%numnod
      new_node%internal_node = org_node%internal_node
!
      call alloc_node_geometry_base(new_node)
!
!$omp parallel do
      do inod = 1, org_node%numnod
        new_node%inod_global(inod) = org_node%inod_global(inod)
        new_node%xx(inod,1) = org_node%ss(inod)
        new_node%xx(inod,2) = org_node%phi(inod)
        new_node%xx(inod,3) = org_node%xx(inod,3)
      end do
!$omp end parallel do
!
!
      end subroutine copy_node_cyl_to_xx
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_element_connect_types(org_ele, new_ele)
!
      use m_geometry_constants
      use t_geometry_data
      use set_nnod_4_ele_by_type
!
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(inout) :: new_ele
!
!
      integer(kind = kint) :: iele, k1
!
!
      new_ele%numele =         org_ele%numele
      new_ele%first_ele_type = org_ele%first_ele_type
!
      new_ele%nnod_4_ele                                                &
     &      = set_nnod_4_ele_by_eletype(new_ele%first_ele_type)
!
      call allocate_ele_connect_type(new_ele)
!
!$omp parallel private(k1)
      do k1 = 1, new_ele%nnod_4_ele
!$omp do private(iele)
        do iele = 1, new_ele%numele
          new_ele%ie(iele,k1) = org_ele%ie(iele,k1)
        end do
!$omp end do nowait
      end do
!
!$omp do
      do iele = 1, new_ele%numele
        new_ele%iele_global(iele) = org_ele%iele_global(iele)
        new_ele%elmtyp(iele) =      org_ele%elmtyp(iele)
        new_ele%nodelm(iele) =      org_ele%nodelm(iele)
      end do
!$omp end do
!$omp end parallel
!
      end subroutine copy_element_connect_types
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_comm_tbl_types(org_comm, new_comm)
!
      use t_comm_table
!
      type(communication_table), intent(in) :: org_comm
      type(communication_table), intent(inout) :: new_comm
!
!
      new_comm%num_neib =    org_comm%num_neib
      call alloc_comm_table_num(new_comm)
!
      if(new_comm%num_neib .gt. 0) then
        new_comm%id_neib(1:new_comm%num_neib)                           &
     &      = org_comm%id_neib(1:new_comm%num_neib)
        new_comm%num_import(1:new_comm%num_neib)                        &
     &      =    org_comm%num_import(1:new_comm%num_neib)
        new_comm%num_export(1:new_comm%num_neib)                        &
     &      = org_comm%num_export(1:new_comm%num_neib)
      end if
      new_comm%istack_import(0:new_comm%num_neib)                       &
     &      = org_comm%istack_import(0:new_comm%num_neib)
      new_comm%istack_export(0:new_comm%num_neib)                       &
     &      = org_comm%istack_export(0:new_comm%num_neib)
      new_comm%ntot_import = org_comm%ntot_import
      new_comm%ntot_export = org_comm%ntot_export
!
      call alloc_comm_table_item(new_comm)
!
      if(new_comm%ntot_import .gt. 0) then
        new_comm%item_import(1:new_comm%ntot_import)                    &
     &      = org_comm%item_import(1:new_comm%ntot_import)
      end if
      if(new_comm%ntot_export .gt. 0) then
        new_comm%item_export(1:new_comm%ntot_export)                    &
     &      = org_comm%item_export(1:new_comm%ntot_export)
      end if
!
      end subroutine copy_comm_tbl_types
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_group_data(org_grp, new_grp)
!
      use t_group_data
!
      type(group_data), intent(in) :: org_grp
      type(group_data), intent(inout) :: new_grp
!
!
      new_grp%num_grp =     org_grp%num_grp
      new_grp%num_item = org_grp%num_item
!
      call alloc_group_num(new_grp)
!
      if (new_grp%num_grp .gt. 0) then
        new_grp%grp_name(1:new_grp%num_grp)                             &
     &          =    org_grp%grp_name(1:new_grp%num_grp)
        new_grp%istack_grp(0:new_grp%num_grp)                           &
     &          =  org_grp%istack_grp(0:new_grp%num_grp)
      end if
!
      call alloc_group_item(new_grp)
!
      if (new_grp%num_item .gt. 0) then
        new_grp%item_grp(1:new_grp%num_item)                            &
     &          = org_grp%item_grp(1:new_grp%num_item)
      end if
!
      end subroutine copy_group_data
!
!  ---------------------------------------------------------------------
!
      subroutine copy_surface_group(org_sf_grp, new_sf_grp)
!
      use t_group_data
!
      type(surface_group_data), intent(in) :: org_sf_grp
      type(surface_group_data), intent(inout) :: new_sf_grp
!
!
      new_sf_grp%num_grp = org_sf_grp%num_grp
      new_sf_grp%num_item = org_sf_grp%num_item
!
      call alloc_sf_group_num(new_sf_grp)
!
      if(new_sf_grp%num_grp .gt. 0) then
        new_sf_grp%grp_name(1:new_sf_grp%num_grp)                       &
     &     = org_sf_grp%grp_name(1:new_sf_grp%num_grp)
        new_sf_grp%istack_grp(0:new_sf_grp%num_grp)                     &
     &     =  org_sf_grp%istack_grp(0:new_sf_grp%num_grp)
      end if
!
      call alloc_sf_group_item(new_sf_grp)
!
      if(new_sf_grp%num_item .gt. 0) then
        new_sf_grp%item_sf_grp(1,1:new_sf_grp%num_item)                 &
     &     = org_sf_grp%item_sf_grp(1,1:new_sf_grp%num_item)
        new_sf_grp%item_sf_grp(2,1:new_sf_grp%num_item)                 &
     &     = org_sf_grp%item_sf_grp(2,1:new_sf_grp%num_item)
      end if
!
      end subroutine copy_surface_group
!
!  ---------------------------------------------------------------------
!
      end module copy_mesh_structures
