!>@file   const_edge_comm_table.f90
!!@brief  module const_edge_comm_table
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine s_const_edge_comm_table                              &
!!     &         (node, nod_comm, edge_comm, edge, edge_gl)
!!      subroutine dealloc_edge_comm_table(edge_comm, edge_gl)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(edge_data), intent(in) :: edge
!!        type(communication_table), intent(inout) :: edge_comm
!!        type(global_edge_data), intent(inout) :: edge_gl
!!
!!      subroutine dup_global_edge_id                                   &
!!     &         (org_edge_comm, org_edge_gl, new_edge,                 &
!!     &          new_edge_comm, new_edge_gl)
!!        type(communication_table), intent(in) :: org_edge_comm
!!        type(global_edge_data), intent(in) :: org_edge_gl
!!        type(edge_data), intent(in) :: new_edge
!!        type(communication_table), intent(inout) :: new_edge_comm
!!        type(global_edge_data), intent(inout) :: new_edge_gl
!!      subroutine copy_global_edge_id                                  &
!!     &         (org_edge_gl, numedge, iedge_global, interior_edge)
!!        type(global_edge_data), intent(in) :: org_edge_gl
!!        integer(kind=kint), intent(in) ::  numedge
!!        integer(kind=kint_gl), intent(inout) :: iedge_global(numedge)
!!        integer(kind=kint), intent(inout) ::    interior_edge(numedge)
!!
!!      subroutine edge_send_recv_test                                  &
!!     &         (edge, edge_gl, edge_comm, edge_check)
!!        type(edge_data), intent(in) :: edge
!!        type(global_edge_data), intent(in) :: edge_gl
!!        type(communication_table), intent(in) :: edge_comm
!!        type(work_for_comm_check), intent(inout) :: edge_check
!!@endverbatim
!
      module const_edge_comm_table
!
      use m_precision
      use calypso_mpi
      use t_next_node_ele_4_node
      use t_mesh_data
      use t_geometry_data
      use t_edge_data
      use t_comm_table
      use t_failed_export_list
!
      use m_machine_parameter
      use m_geometry_constants
!
      implicit none
!
      character(len=kchara), parameter :: txt_edge = 'edge'
      private :: txt_edge
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_const_edge_comm_table                                &
     &         (node, nod_comm, edge, edge_comm, edge_gl)
!
      use m_geometry_constants
      use t_para_double_numbering
      use t_element_double_number
      use t_const_comm_table
      use set_ele_id_4_node_type
      use const_global_element_ids
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(edge_data), intent(in) :: edge
!
      type(communication_table), intent(inout) :: edge_comm
      type(global_edge_data), intent(inout) :: edge_gl
!
      type(node_ele_double_number) :: inod_dbl
      type(element_double_number) :: iedge_dbl
      type(element_around_node) :: neib_edge
      type(failed_table) :: fail_tbl_d
!
      integer(kind = kint) :: internal_num = 0
      integer(kind = kint_gl), allocatable :: istack_ineredge(:)
      integer(kind = kint) :: i, i1, i2
!
!
      call alloc_interior_edge(edge, edge_gl)
!
      call alloc_double_numbering(node%numnod, inod_dbl)
      call set_node_double_numbering(node, nod_comm, inod_dbl)
!
      call alloc_ele_double_number(edge%numedge, iedge_dbl)
      call find_belonged_pe_4_edge(my_rank, inod_dbl,                   &
     &    edge%numedge, edge%nnod_4_edge, edge%ie_edge,                 &
     &    internal_num, edge_gl%interior_edge, iedge_dbl)
!
      do i = 1, edge%numedge
        i1 = edge%ie_edge(i,1)
        i2 = edge%ie_edge(i,2)
        if(node%inod_global(i1).eq.9687462  &
     &      .and. node%inod_global(i2).eq.9687687) write(*,*)           &
     &     'edge with global_node(9687462, 9687687): ', my_rank, i,     &
     &     'iedge_dbl: ', iedge_dbl%irank(i), iedge_dbl%k_ref(i),       &
     &     'local node 1: ', i1, node%xx(i1,1:3),                       &
     &     'local node 2: ', i2, node%xx(i2,1:3),                       &
     &     'inod_dbl%irank: ', inod_dbl%irank(i1), inod_dbl%irank(i2),  &
     &     'inod_dbl%index: ', inod_dbl%index(i1), inod_dbl%index(i2)
        if(node%inod_global(i1).eq.9687687  &
     &      .and. node%inod_global(i2).eq.9687462) write(*,*)           &
     &     'edge with global_node(9687687, 9687462): ', my_rank, i,     &
     &     'iedge_dbl: ', iedge_dbl%irank(i), iedge_dbl%irank(i),       &
     &     'local node 1: ', i1, node%xx(i1,1:3),                       &
     &     'local node 2: ', i2, node%xx(i2,1:3),                       &
     &     'inod_dbl%irank: ', inod_dbl%irank(i1), inod_dbl%irank(i2),  &
     &     'inod_dbl%index: ', inod_dbl%index(i1), inod_dbl%index(i2)
        if(node%inod_global(i2).eq.16298885) write(*,*)          &
     &     'edge with global_node(12452273, 16298885): ', my_rank, i,   &
     &     'iedge_dbl: ', iedge_dbl%irank(i), iedge_dbl%irank(i),       &
     &     'local node 1: ', i1, node%xx(i1,1:3),                       &
     &     'local node 2: ', i2, node%xx(i2,1:3),                       &
     &     'inod_dbl%irank: ', inod_dbl%irank(i1), inod_dbl%irank(i2),  &
     &     'inod_dbl%index: ', inod_dbl%index(i1), inod_dbl%index(i2)
        if(node%inod_global(i1).eq.16298885) write(*,*)          &
     &     'edge with global_node(16298885, 12452273): ', my_rank, i,   &
     &     'iedge_dbl: ', iedge_dbl%irank(i), iedge_dbl%irank(i),       &
     &     'local node 1: ', i1, node%xx(i1,1:3),                       &
     &     'local node 2: ', i2, node%xx(i2,1:3),                       &
     &     'inod_dbl%irank: ', inod_dbl%irank(i1), inod_dbl%irank(i2),  &
     &     'inod_dbl%index: ', inod_dbl%index(i1), inod_dbl%index(i2)
      end do

!
      if(iflag_debug.gt.0) write(*,*) ' set_edge_id_4_node in edge'
      call set_edge_id_4_node(node, edge, neib_edge)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' const_comm_table_by_connenct in edge'
      call alloc_failed_export(0, fail_tbl_d)
      call const_comm_table_by_connenct                                 &
     &   (txt_edge, edge%numedge, edge%nnod_4_edge, edge%ie_edge,       &
     &    edge%x_edge, node, nod_comm, inod_dbl, iedge_dbl,             &
     &    neib_edge, edge_comm, fail_tbl_d)
      call dealloc_iele_belonged(neib_edge)
      call dealloc_failed_export(fail_tbl_d)
!
!
      allocate(istack_ineredge(0:nprocs))
      istack_ineredge(0:nprocs) = 0
!
      call count_number_of_node_stack(internal_num, istack_ineredge)
      call set_global_ele_id                                            &
     &   (txt_edge, edge%numedge, istack_ineredge,                      &
     &    edge_gl%interior_edge, edge_comm, edge_gl%iedge_global)
      deallocate(istack_ineredge)
!
!
!      write(*,*) 'check_element_position', my_rank
!      if(iflag_ecomm_time) call start_elapsed_time(ist_elapsed+6)
      call check_element_position                                       &
     &   (txt_edge, node%numnod, node%inod_global, edge%numedge,        &
     &    edge%nnod_4_edge, edge%ie_edge, edge_gl%iedge_global,         &
     &    edge%x_edge, inod_dbl, iedge_dbl, edge_comm)
!      if(iflag_ecomm_time) call end_elapsed_time(ist_elapsed+6)
      call dealloc_double_numbering(inod_dbl)
      call dealloc_ele_double_number(iedge_dbl)
!
      end subroutine s_const_edge_comm_table
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_edge_comm_table(edge_comm, edge_gl)
!
      type(communication_table), intent(inout) :: edge_comm
      type(global_edge_data), intent(inout) :: edge_gl
!
      call dealloc_comm_table(edge_comm)
      call dealloc_interior_edge(edge_gl)
!
      end subroutine dealloc_edge_comm_table
!
!-----------------------------------------------------------------------
!
      subroutine dup_global_edge_id                                     &
     &         (org_edge_comm, org_edge_gl, new_edge,                   &
     &          new_edge_comm, new_edge_gl)
!
      type(communication_table), intent(in) :: org_edge_comm
      type(global_edge_data), intent(in) :: org_edge_gl
      type(edge_data), intent(in) :: new_edge
!
      type(communication_table), intent(inout) :: new_edge_comm
      type(global_edge_data), intent(inout) :: new_edge_gl
!
!
      call copy_comm_tbl_type(org_edge_comm, new_edge_comm)
      call alloc_interior_edge(new_edge, new_edge_gl)
      call copy_global_edge_id(org_edge_gl, new_edge%numedge,           &
     &    new_edge_gl%iedge_global, new_edge_gl%interior_edge)
!
      end subroutine dup_global_edge_id
!
!-----------------------------------------------------------------------
!
      subroutine copy_global_edge_id                                    &
     &         (org_edge_gl, numedge, iedge_global, interior_edge)
!
      type(global_edge_data), intent(in) :: org_edge_gl
      integer(kind=kint), intent(in) ::  numedge
      integer(kind=kint_gl), intent(inout) :: iedge_global(numedge)
      integer(kind=kint), intent(inout) ::    interior_edge(numedge)
!
!
      if(numedge .le. 0) return
!$omp parallel workshare
      iedge_global(1:numedge) =  org_edge_gl%iedge_global(1:numedge)
      interior_edge(1:numedge) = org_edge_gl%interior_edge(1:numedge)
!$omp end parallel workshare
!
      end subroutine copy_global_edge_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine edge_send_recv_test                                    &
     &         (edge, edge_gl, edge_comm, edge_check)
!
      use t_work_for_comm_check
      use m_solver_SR
      use diff_geometory_comm_test
      use solver_SR_type
      use mesh_send_recv_check
      use const_element_comm_tables
!
      type(edge_data), intent(in) :: edge
      type(global_edge_data), intent(in) :: edge_gl
      type(communication_table), intent(in) :: edge_comm
      type(work_for_comm_check), intent(inout) :: edge_check
!
!
      call alloc_geom_4_comm_test(edge%numedge, edge_check)
      call set_element_4_comm_test(edge%numedge ,edge_gl%interior_edge, &
     &                             edge%x_edge, edge_check%xx_test)
      call SOLVER_SEND_RECV_3_type(edge%numedge, edge_comm,             &
     &                             SR_sig1, SR_r1, edge_check%xx_test)
!
      call ele_send_recv_check                                          &
     &   (edge%numedge, edge_gl%iedge_global, edge%x_edge, edge_check)
!
      if(i_debug .gt. 0)  write(*,*) my_rank,                           &
     &     'Failed communication for edge', edge_check%num_diff
      call collect_failed_comm(edge_check)
      if(my_rank .eq. 0) write(*,*) my_rank,                            &
     &   'Total Failed communication for edge',                         &
     &    edge_check%istack_diff_pe(nprocs)
!
      end subroutine edge_send_recv_test
!
! ----------------------------------------------------------------------
!
      end module const_edge_comm_table
