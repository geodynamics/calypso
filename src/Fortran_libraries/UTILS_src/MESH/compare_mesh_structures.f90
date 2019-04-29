!>@file   compare_mesh_structures.f90
!!@brief  module compare_mesh_structures
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!
!>@brief Compare FEM mesh structures
!!
!!@verbatim
!!      subroutine compare_mesh_type(id_rank, nod_comm, node, ele, mesh)
!!        type(mesh_geometry),    intent(inout) :: mesh
!!        type(communication_table), intent(inout) :: nod_comm
!!        type(node_data), intent(inout) ::           node
!!        type(element_data), intent(inout) ::        ele
!!      subroutine compare_node_types(id_rank, org_node, new_node)
!!        type(node_data), intent(in) :: org_node
!!        type(node_data), intent(in) :: new_node
!!      subroutine compare_element_types(id_rank, org_ele, new_ele)
!!        type(element_data), intent(in) :: org_ele
!!        type(element_data), intent(in) :: new_ele
!!
!!      subroutine compare_node_comm_types(id_rank, org_comm, new_comm)
!!        type(communication_table), intent(in) :: org_comm
!!        type(communication_table), intent(in) :: new_comm
!!@endverbatim
!
      module compare_mesh_structures
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine compare_mesh_type(id_rank, nod_comm, node, ele, mesh)
!
      use t_comm_table
      use t_geometry_data
      use t_mesh_data
!
      integer, intent(in) :: id_rank
      type(mesh_geometry),    intent(inout) :: mesh
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) ::           node
      type(element_data), intent(inout) ::        ele
!
!
      call compare_node_types(id_rank, node, mesh%node)
      call compare_element_types(id_rank, ele, mesh%ele)
      call compare_node_comm_types(id_rank, nod_comm, mesh%nod_comm)
!
      end subroutine compare_mesh_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine compare_node_types(id_rank, org_node, new_node)
!
      use t_geometry_data
!
      integer, intent(in) :: id_rank
      type(node_data), intent(in) :: org_node
      type(node_data), intent(in) :: new_node
!
      integer(kind = kint) :: i
      real(kind = kreal) :: err
!
!
      if(new_node%numnod .ne. org_node%numnod) write(*,*) 'numnod',     &
     &      id_rank, new_node%numnod, org_node%numnod
      if(new_node%internal_node .ne. org_node%internal_node) write(*,*) &
     &      'numnod', id_rank, new_node%internal_node,                  &
     &      org_node%internal_node
      do i = 1, org_node%numnod
        err = sqrt((new_node%xx(i,1) - org_node%xx(i,1))**2             &
     &           + (new_node%xx(i,2) - org_node%xx(i,2))**2             &
     &           + (new_node%xx(i,3) - org_node%xx(i,3))**2)
        if(new_node%inod_global(i) .ne. org_node%inod_global(i))        &
     &       write(*,*) 'inod_global(i)', id_rank, i,                   &
     &       new_node%inod_global(i), org_node%inod_global(i)
        if(err .gt. 1d-11) write(*,*) 'xx', id_rank, err, i,            &
     &       new_node%xx(i,1:3), org_node%xx(i,1:3)
      end do
!
      end subroutine compare_node_types
!
!------------------------------------------------------------------
!
      subroutine compare_element_types(id_rank, org_ele, new_ele)
!
      use t_geometry_data
!
      integer, intent(in) :: id_rank
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(in) :: new_ele
!
!
      integer(kind = kint) :: i, k1, iflag
!
!
      if(new_ele%numele .ne. org_ele%numele) write(*,*) 'numele',       &
     &      id_rank, new_ele%numele, org_ele%numele
      if(new_ele%first_ele_type .ne. org_ele%first_ele_type) write(*,*) &
     &       'first_ele_type', id_rank, new_ele%first_ele_type,         &
     &       org_ele%first_ele_type
      if(new_ele%nnod_4_ele .ne. org_ele%nnod_4_ele) write(*,*)         &
     &   'nnod_4_ele', id_rank, new_ele%nnod_4_ele, org_ele%nnod_4_ele
!
      do i = 1, org_ele%numele
        iflag = 0
        do k1 = 1, org_ele%nnod_4_ele
          if(new_ele%ie(i,k1) .ne. org_ele%ie(i,k1)) iflag = 1
        end do
        if(new_ele%iele_global(i) .ne. org_ele%iele_global(i))          &
     &       write(*,*) 'iele_global(i)', id_rank, i,                   &
     &       new_ele%iele_global(i), org_ele%iele_global(i)
        if(new_ele%elmtyp(i) .ne. org_ele%elmtyp(i)) write(*,*)         &
     &       'elmtyp(i)', id_rank, i,                                   &
     &        new_ele%elmtyp(i), org_ele%elmtyp(i)
        if(new_ele%nodelm(i) .ne. org_ele%nodelm(i)) write(*,*)         &
     &       'nodelm(i)', id_rank, i,                                   &
     &       new_ele%nodelm(i), org_ele%nodelm(i)
        if(iflag .gt. 0) then
          do k1 = 1, org_ele%nnod_4_ele
            if(new_ele%nodelm(i) .ne. org_ele%nodelm(i)) write(*,*)     &
     &         'ie(i,k)', id_rank, i, k1,                               &
     &         new_ele%ie(i,k1), org_ele%ie(i,k1)
          end do
        end if
      end do
!
      end subroutine compare_element_types
!
!------------------------------------------------------------------
!
      subroutine compare_node_comm_types(id_rank, org_comm, new_comm)
!
      use copy_communication_table
      use t_comm_table
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: org_comm
      type(communication_table), intent(in) :: new_comm
!
      integer(kind = kint) :: i
!
!
      if(new_comm%num_neib .ne. org_comm%num_neib)                      &
     &    write(*,*) 'num_neib', id_rank,                               &
     &            new_comm%num_neib, org_comm%num_neib
      if(new_comm%ntot_export .ne. org_comm%ntot_export)                &
     &      write(*,*) 'ntot_export',                                   &
     &      id_rank, new_comm%ntot_export, org_comm%ntot_export
      if(new_comm%ntot_import .ne. org_comm%ntot_import)                &
     &      write(*,*) 'ntot_import',                                   &
     &      id_rank, new_comm%ntot_import, org_comm%ntot_import
      do i = 1, org_comm%num_neib
        if(new_comm%id_neib(i) .ne. org_comm%id_neib(i))                &
     &       write(*,*) 'id_neib(i)', id_rank, i,                       &
     &       new_comm%id_neib(i), org_comm%id_neib(i)
        if(new_comm%istack_import(i) .ne. org_comm%istack_import(i))    &
     &       write(*,*) 'istack_import(i)', id_rank, i,                 &
     &       new_comm%istack_import(i), org_comm%istack_import(i)
        if(new_comm%istack_export(i) .ne. org_comm%istack_export(i))    &
     &       write(*,*) 'istack_export(i)', id_rank, i,                 &
     &       new_comm%istack_export(i), org_comm%istack_export(i)
      end do
      do i = 1, org_comm%ntot_export
        if(new_comm%item_export(i) .ne. org_comm%item_export(i))        &
     &       write(*,*) 'item_export(i)', id_rank, i,                   &
     &       new_comm%item_export(i), org_comm%item_export(i)
      end do
      do i = 1, org_comm%ntot_import
        if(new_comm%item_import(i) .ne. org_comm%item_import(i))        &
     &       write(*,*) 'item_import(i)', id_rank, i,                   &
     &       new_comm%item_import(i), org_comm%item_import(i)
      end do
!
      end subroutine compare_node_comm_types
!
!-----------------------------------------------------------------------
!
      end module compare_mesh_structures
