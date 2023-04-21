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
!!      subroutine compare_node_position                                &
!!     &         (id_rank, org_node, new_node, icount_error)
!!        type(node_data), intent(in) :: org_node
!!        type(node_data), intent(in) :: new_node
!!        integer(kind = kint), intent(inout) :: icount_error
!!      subroutine compare_ele_connect                                  &
!!     &         (id_rank, org_ele, new_ele, icount_error)
!!        type(element_data), intent(in) :: org_ele
!!        type(element_data), intent(in) :: new_ele
!!        integer(kind = kint), intent(inout) :: icount_error
!!
!!      subroutine compare_node_comm_types(id_rank, org_comm, new_comm)
!!        type(communication_table), intent(in) :: org_comm
!!        type(communication_table), intent(in) :: new_comm
!!@endverbatim
!
      module compare_mesh_structures
!
      use m_precision
      use m_machine_parameter
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
      integer(kind = kint) :: ierror_count, icou_error
!
!
      call compare_node_position(id_rank, node, mesh%node,              &
     &                           ierror_count)
      call compare_ele_connect(id_rank, ele, mesh%ele, icou_error)
      ierror_count = ierror_count + icou_error
      call compare_node_comm_types(id_rank, nod_comm, mesh%nod_comm)
!
      end subroutine compare_mesh_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine compare_node_position                                  &
     &         (id_rank, org_node, new_node, icount_error)
!
      use t_geometry_data
      use m_phys_constants
      use compare_indices
!
      integer, intent(in) :: id_rank
      type(node_data), intent(in) :: org_node
      type(node_data), intent(in) :: new_node
      integer(kind = kint), intent(inout) :: icount_error
!
      character(len=kchara), parameter :: field_name = 'position'
      integer(kind = kint) :: icou_error
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) id_rank, 'numnod', org_node%numnod, new_node%numnod
        write(*,*) id_rank, 'internal_node',                            &
     &            org_node%internal_node, new_node%internal_node
      end if
!
      icount_error = 0
      if(org_node%numnod .ne. new_node%numnod) then
        write(*,*) 'Number of node is different in domain ', id_rank,   &
     &              ':  ', org_node%numnod, new_node%numnod
        icount_error = icount_error + 1
      end if
      if(org_node%internal_node .ne. new_node%internal_node) then
        write(*,*)                                                      &
     &      'Number of internal node is different in domain ', id_rank, &
     &      ':  ', org_node%internal_node, new_node%internal_node
        icount_error = icount_error + 1
      end if
      call compare_field_vector(org_node%numnod, n_vector, field_name,  &
     &    org_node%xx(1,1), new_node%xx(1,1), icou_error)
      icount_error = icount_error + icou_error
!
      end subroutine compare_node_position
!
!------------------------------------------------------------------
!
      subroutine compare_ele_connect                                    &
     &         (id_rank, org_ele, new_ele, icount_error)
!
      use t_geometry_data
!
      integer, intent(in) :: id_rank
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(in) :: new_ele
      integer(kind = kint), intent(inout) :: icount_error
!
      integer(kind = kint) :: iele, k1
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) id_rank, 'numele', org_ele%numele, new_ele%numele
        write(*,*) id_rank, 'nnod_4_ele', org_ele%nnod_4_ele,           &
     &                                     new_ele%nnod_4_ele
        write(*,*) id_rank, 'first_ele_type', org_ele%first_ele_type,   &
     &                               new_ele%first_ele_type
      end if
!
      icount_error = 0
      if(org_ele%numele .ne. new_ele%numele) then
        write(*,*) 'Number of element is differenct: ',                 &
     &             org_ele%numele, new_ele%numele
        icount_error = icount_error + 1
      end if
      if(org_ele%nnod_4_ele .ne. new_ele%nnod_4_ele) then
        write(*,*) 'Element type is differennt: ',                      &
     &             org_ele%nnod_4_ele, new_ele%nnod_4_ele
        icount_error = icount_error + 1
      end if
!
      do iele = 1, org_ele%numele
        if(org_ele%elmtyp(iele) .ne. new_ele%elmtyp(iele)) then
          write(*,*) 'element type at ', iele, ' is differ',            &
     &        org_ele%elmtyp(iele), new_ele%elmtyp(iele)
          icount_error = icount_error + 1
        end if
      end do
      do iele = 1, org_ele%numele
        if(org_ele%nodelm(iele) .ne. new_ele%nodelm(iele)) then
          write(*,*) 'number of node for ', iele, ' is differ',         &
     &        org_ele%nodelm(iele), new_ele%nodelm(iele)
          icount_error = icount_error + 1
        end if
      end do
      do k1 = 1, org_ele%nnod_4_ele
        do iele = 1, org_ele%numele
          if(org_ele%ie(iele,k1) .ne. new_ele%ie(iele,k1)) then
            write(*,*) 'connectivity at ', iele, k1, ' is differ',      &
     &          org_ele%ie(iele,k1), new_ele%ie(iele,k1)
            icount_error = icount_error + 1
          end if
        end do
      end do
!
      end subroutine compare_ele_connect
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
