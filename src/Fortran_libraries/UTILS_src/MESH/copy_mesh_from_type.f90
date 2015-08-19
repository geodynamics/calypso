!
!     module copy_mesh_from_type
!
!      written by H. Matsui on June, 2007
!
!>@file   copy_mesh_from_type.f90
!!@brief  module copy_mesh_from_type
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!
!>@brief Copy FEM mesh data from structure to 1st mesh module
!!
!!@verbatim
!!      subroutine set_mesh_from_type(mesh, group)
!!      subroutine compare_mesh_type_vs_1st(my_rank, mesh, group)
!!      subroutine set_geometry_data_from_type(mesh)
!!      subroutine compare_geometry_type_vs_1st(my_rank, mesh)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!@endverbatim
!
      module copy_mesh_from_type
!
      use m_precision
!
      implicit  none
!
!
      private :: set_geometry_data_from_type
      private :: copy_node_geometry_from_type
      private :: copy_element_connect_from_type
      private :: compare_node_type_vs_1st
      private :: compare_element_type_vs_1st
      private :: compare_node_comm_type_vs_1st
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_mesh_from_type(mesh, group)
!
      use t_mesh_data
      use m_group_data
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
!
      call set_geometry_data_from_type(mesh)
      call group_data_from_type(group)
!
      end subroutine set_mesh_from_type
!
!  ---------------------------------------------------------------------
!
      subroutine compare_mesh_type_vs_1st(my_rank, mesh, group)
!
      use t_mesh_data
      use m_group_data
!
      integer(kind = kint), intent(in)  :: my_rank
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
!
      call compare_geometry_type_vs_1st(my_rank, mesh)
      call compare_group_type_vs_1st(my_rank, group)
!
      end subroutine compare_mesh_type_vs_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_geometry_data_from_type(mesh)
!
      use m_nod_comm_table
      use m_geometry_data
      use t_mesh_data
!
      type(mesh_geometry),    intent(inout) :: mesh
!
!
      call copy_node_comm_tbl_from_type(mesh%nod_comm)
      call copy_node_geometry_from_type(mesh%node)
      call copy_element_connect_from_type(mesh%ele)
!
      call allocate_sph_node_geometry(mesh%node)
      call allocate_ele_geometry_type(ele1)
!
      call deallocate_ele_connect_type(mesh%ele)
      call deallocate_node_geometry_type(mesh%node)
!
      end subroutine set_geometry_data_from_type
!
!-----------------------------------------------------------------------
!
      subroutine compare_geometry_type_vs_1st(my_rank, mesh)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in)  :: my_rank
      type(mesh_geometry),    intent(inout) :: mesh
!
!
      call compare_node_type_vs_1st(my_rank, mesh%node)
      call compare_element_type_vs_1st(my_rank, mesh%ele)
      call compare_node_comm_type_vs_1st(my_rank, mesh%nod_comm)
!
      end subroutine compare_geometry_type_vs_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_node_geometry_from_type(node_org)
!
      use m_geometry_data
      use t_geometry_data
!
      type(node_data), intent(inout) :: node_org
!
      integer(kind = kint) :: inod
!
!
      node1%numnod =        node_org%numnod
      node1%internal_node = node_org%internal_node
!
      call allocate_node_geometry_type(node1)
!
!$omp parallel do
      do inod = 1, node1%numnod
        node1%inod_global(inod) = node_org%inod_global(inod)
        node1%xx(inod,1) = node_org%xx(inod,1)
        node1%xx(inod,2) = node_org%xx(inod,2)
        node1%xx(inod,3) = node_org%xx(inod,3)
      end do
!$omp end parallel do
!
      end subroutine copy_node_geometry_from_type
!
!------------------------------------------------------------------
!
      subroutine copy_element_connect_from_type(ele_org)
!
      use m_geometry_constants
      use m_geometry_data
      use t_geometry_data
      use set_nnod_4_ele_by_type
!
      type(element_data), intent(inout) :: ele_org
!
!
      integer(kind = kint) :: iele, k1
!
!
      ele1%numele =         ele_org%numele
      ele1%first_ele_type = ele_org%first_ele_type
!
      call set_3D_nnod_4_ele_by_type(ele1%first_ele_type,               &
     &    ele1%nnod_4_ele, surf1%nnod_4_surf, edge1%nnod_4_edge)
!
      call allocate_ele_connect_type(ele1)
!
!$omp parallel private(k1)
      do k1 = 1, ele1%nnod_4_ele
!$omp do private(iele)
        do iele = 1, ele1%numele
          ele1%ie(iele,k1) = ele_org%ie(iele,k1)
        end do
!$omp end do nowait
      end do
!
!$omp do
      do iele = 1, ele1%numele
        ele1%iele_global(iele) = ele_org%iele_global(iele)
        ele1%elmtyp(iele) =      ele_org%elmtyp(iele)
        ele1%nodelm(iele) =      ele_org%nodelm(iele)
      end do
!$omp end do
!$omp end parallel
!
      end subroutine copy_element_connect_from_type
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine compare_node_type_vs_1st(my_rank, node_org)
!
      use m_geometry_data
      use t_geometry_data
!
      integer(kind = kint), intent(in)  :: my_rank
      type(node_data), intent(in) :: node_org
!
      integer(kind = kint) :: i
      real(kind = kreal) :: err
!
!
      if(node_org%numnod .ne. node1%numnod) write(*,*) 'numnod',        &
     &      my_rank, node_org%numnod, node1%numnod
      if(node_org%internal_node .ne. node1%internal_node) write(*,*)    &
     &      'numnod', my_rank, node_org%internal_node,                  &
     &      node1%internal_node
      do i = 1, node1%numnod
        err = sqrt((node_org%xx(i,1) - node1%xx(i,1))**2                &
     &           + (node_org%xx(i,2) - node1%xx(i,2))**2                &
     &           + (node_org%xx(i,3) - node1%xx(i,3))**2)
        if(node_org%inod_global(i) .ne. node1%inod_global(i))           &
     &       write(*,*) 'inod_global(i)', my_rank, i,                   &
     &       node_org%inod_global(i), node1%inod_global(i)
        if(err .gt. 1d-11) write(*,*) 'xx', my_rank, err, i,            &
     &       node_org%xx(i,1:3), node1%xx(i,1:3)
      end do
!
      end subroutine compare_node_type_vs_1st
!
!------------------------------------------------------------------
!
      subroutine compare_element_type_vs_1st(my_rank, ele_org)
!
      use m_geometry_data
      use t_geometry_data
!
      integer(kind = kint), intent(in)  :: my_rank
      type(element_data), intent(inout) :: ele_org
!
!
      integer(kind = kint) :: i, k1, iflag
!
!
      if(ele_org%numele .ne. ele1%numele) write(*,*) 'numele',          &
     &      my_rank, ele_org%numele, ele1%numele
      if(ele_org%first_ele_type .ne. ele1%first_ele_type) write(*,*)    &
     &       'first_ele_type', my_rank, ele_org%first_ele_type,         &
     &       ele1%first_ele_type
      if(ele_org%nnod_4_ele .ne. ele1%nnod_4_ele) write(*,*)            &
     &      'nnod_4_ele', my_rank, ele_org%nnod_4_ele, ele1%nnod_4_ele
!
      do i = 1, ele1%numele
        iflag = 0
        do k1 = 1, ele1%nnod_4_ele
          if(ele_org%ie(i,k1) .ne. ele1%ie(i,k1)) iflag = 1
        end do
        if(ele_org%iele_global(i) .ne. ele1%iele_global(i))             &
     &       write(*,*) 'iele_global(i)', my_rank, i,                   &
     &       ele_org%iele_global(i), ele1%iele_global(i)
        if(ele_org%elmtyp(i) .ne. ele1%elmtyp(i)) write(*,*)            &
     &       'elmtyp(i)', my_rank, i, ele_org%elmtyp(i), ele1%elmtyp(i)
        if(ele_org%nodelm(i) .ne. ele1%nodelm(i)) write(*,*)            &
     &       'nodelm(i)', my_rank, i, ele_org%nodelm(i), ele1%nodelm(i)
        if(iflag .gt. 0) then
          do k1 = 1, ele1%nnod_4_ele
            if(ele_org%nodelm(i) .ne. ele1%nodelm(i)) write(*,*)        &
     &         'ie(i,k)', my_rank, i, k1,                               &
     &         ele_org%ie(i,k1), ele1%ie(i,k1)
          end do
        end if
      end do
!
      end subroutine compare_element_type_vs_1st
!
!------------------------------------------------------------------
!
      subroutine compare_node_comm_type_vs_1st(my_rank, new_comm)
!
      use m_nod_comm_table
      use copy_communication_table
      use t_comm_table
!
      integer(kind = kint), intent(in)  :: my_rank
      type(communication_table), intent(in) :: new_comm
!
      integer(kind = kint) :: i
!
!
      if(new_comm%num_neib .ne. nod_comm%num_neib)                      &
     &    write(*,*) 'num_neib', my_rank,                               &
     &            new_comm%num_neib, nod_comm%num_neib
      if(new_comm%ntot_export .ne. nod_comm%ntot_export)                &
     &      write(*,*) 'ntot_export',                                   &
     &      my_rank, new_comm%ntot_export, nod_comm%ntot_export
      if(new_comm%ntot_import .ne. nod_comm%ntot_import)                &
     &      write(*,*) 'ntot_import',                                   &
     &      my_rank, new_comm%ntot_import, nod_comm%ntot_import
      do i = 1, nod_comm%num_neib
        if(new_comm%id_neib(i) .ne. nod_comm%id_neib(i))                &
     &       write(*,*) 'id_neib(i)', my_rank, i,                       &
     &       new_comm%id_neib(i), nod_comm%id_neib(i)
        if(new_comm%istack_import(i) .ne. nod_comm%istack_import(i))    &
     &       write(*,*) 'istack_import(i)', my_rank, i,                 &
     &       new_comm%istack_import(i), nod_comm%istack_import(i)
        if(new_comm%istack_export(i) .ne. nod_comm%istack_export(i))    &
     &       write(*,*) 'istack_export(i)', my_rank, i,                 &
     &       new_comm%istack_export(i), nod_comm%istack_export(i)
      end do
      do i = 1, nod_comm%ntot_export
        if(new_comm%item_export(i) .ne. nod_comm%item_export(i))        &
     &       write(*,*) 'item_export(i)', my_rank, i,                   &
     &       new_comm%item_export(i), nod_comm%item_export(i)
      end do
      do i = 1, nod_comm%ntot_import
        if(new_comm%item_import(i) .ne. nod_comm%item_import(i))        &
     &       write(*,*) 'item_import(i)', my_rank, i,                   &
     &       new_comm%item_import(i), nod_comm%item_import(i)
      end do
!
      end subroutine compare_node_comm_type_vs_1st
!
!-----------------------------------------------------------------------
!
      end module copy_mesh_from_type
