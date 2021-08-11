!>@file   mesh_send_recv_check.f90
!!@brief  module mesh_send_recv_check
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief test routine for mesh communication
!!
!!@verbatim
!!      subroutine node_send_recv_test(node, nod_comm, nod_check,       &
!!     &                               SR_sig, SR_r, SR_il)
!!      subroutine ele_send_recv_test(ele, ele_comm, ele_check,         &
!!     &                              SR_sig, SR_r)
!!      subroutine edge_send_recv_test(edge, edge_comm, edge_check,     &
!!     &                               SR_sig, SR_r)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(work_for_comm_check), intent(inout) :: nod_check
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: ele_comm
!!        type(work_for_comm_check), intent(inout) :: ele_check
!!        type(edge_data), intent(in) :: edge
!!        type(communication_table), intent(in) :: edge_comm
!!        type(work_for_comm_check), intent(inout) :: edge_check
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine node_transfer_test(node, new_node, new_comm,         &
!!     &          trans_tbl, nod_check, SR_sig, SR_r, SR_il)
!!        type(node_data), intent(in) :: node, new_node
!!        type(communication_table), intent(in) :: new_comm
!!        type(calypso_comm_table), intent(in) :: trans_tbl
!!        type(work_for_comm_check), intent(inout) :: nod_check
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!
!!      subroutine ele_send_recv_check(numele, iele_gl, x_ele, wk_check)
!!      subroutine collect_failed_comm(wk_check)
!!        type(work_for_comm_check), intent(inout) :: wk_check
!!@endverbatim
!
      module mesh_send_recv_check
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_solver_SR
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_work_for_comm_check
      use t_calypso_comm_table
      use t_solver_SR
      use t_solver_SR_int8
!
      implicit  none
!
      type(send_recv_status), save, private :: SR_sig_c
!
      private :: nod_send_recv_check
!
! ----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine node_send_recv_test(node, nod_comm, nod_check,         &
     &                               SR_sig, SR_r, SR_il)
!
      use diff_geometory_comm_test
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      type(work_for_comm_check), intent(inout) :: nod_check
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
      call alloc_geom_4_comm_test(node%numnod, nod_check)
      call set_node_4_comm_test                                         &
     &   (node%numnod, node%internal_node, node%inod_global, node%xx,   &
     &    nod_check%i_gl_test, nod_check%xx_test)
      call SOLVER_SEND_RECV_int8_type                                   &
     &   (node%numnod, nod_comm, SR_sig, SR_il, nod_check%i_gl_test)
      call SOLVER_SEND_RECV_3_type(node%numnod, nod_comm,               &
     &                             SR_sig, SR_r, nod_check%xx_test)
!
      call nod_send_recv_check(node, nod_check)
!
      if(i_debug .gt. 0)  write(*,*) my_rank,                           &
     &     'Failed communication for node', nod_check%num_diff
      call collect_failed_comm(nod_check)
      if(my_rank .eq. 0) write(*,*) my_rank,                            &
     &   'Total Failed communication for node',                         &
     &    nod_check%istack_diff_pe(nprocs)
!
      end subroutine node_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine ele_send_recv_test(ele, ele_comm, ele_check,           &
     &                              SR_sig, SR_r)
!
      use diff_geometory_comm_test
      use nod_phys_send_recv
      use solver_SR_type
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: ele_comm
!
      type(work_for_comm_check), intent(inout) :: ele_check
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call alloc_geom_4_comm_test(ele%numele, ele_check)
      call set_element_4_comm_test(ele%numele, ele%interior_ele,        &
     &                             ele%x_ele, ele_check%xx_test)
      call SOLVER_SEND_RECV_3_type(ele%numele, ele_comm,                &
     &                             SR_sig, SR_r, ele_check%xx_test)
!
      call ele_send_recv_check                                          &
     &   (ele%numele, ele%iele_global, ele%x_ele, ele_check)
!
      if(i_debug .gt. 0)  write(*,*) my_rank,                           &
     &     'Failed communication for element', ele_check%num_diff
      call collect_failed_comm(ele_check)
      if(my_rank .eq. 0) write(*,*) my_rank,                            &
     &   'Total Failed communication for element',                      &
     &    ele_check%istack_diff_pe(nprocs)
!
      end subroutine ele_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine edge_send_recv_test(edge, edge_comm, edge_check,       &
     &                               SR_sig, SR_r)
!
      use diff_geometory_comm_test
      use solver_SR_type
!
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: edge_comm
!
      type(work_for_comm_check), intent(inout) :: edge_check
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call alloc_geom_4_comm_test(edge%numedge, edge_check)
      call set_element_4_comm_test(edge%numedge ,edge%interior_edge,    &
     &                             edge%x_edge, edge_check%xx_test)
      call SOLVER_SEND_RECV_3_type(edge%numedge, edge_comm,             &
     &                             SR_sig, SR_r, edge_check%xx_test)
!
      call ele_send_recv_check                                          &
     &   (edge%numedge, edge%iedge_global, edge%x_edge, edge_check)
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
! ----------------------------------------------------------------------
!
      subroutine node_transfer_test(node, new_node, new_comm,           &
     &          trans_tbl, nod_check, SR_sig, SR_r, SR_il)
!
      use diff_geometory_comm_test
      use solver_SR_type
      use calypso_SR_type
      use select_copy_from_recv
!
      type(node_data), intent(in) :: node, new_node
      type(communication_table), intent(in) :: new_comm
      type(calypso_comm_table), intent(in) :: trans_tbl
!
      type(work_for_comm_check), intent(inout) :: nod_check
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      type(work_for_comm_check) :: org_check
!
!
      call alloc_geom_4_comm_test(node%numnod, org_check)
      call set_node_4_comm_test                                         &
     &   (node%numnod, node%internal_node, node%inod_global, node%xx,   &
     &    org_check%i_gl_test, org_check%xx_test)
!
      call alloc_geom_4_comm_test(new_node%numnod, nod_check)
      call calypso_SR_type_int8(iflag_import_item, trans_tbl,           &
     &    node%numnod, new_node%numnod,                                 &
     &    org_check%i_gl_test, nod_check%i_gl_test, SR_sig, SR_il)
      call calypso_SR_type_3(iflag_import_item, trans_tbl,              &
     &    node%numnod, new_node%numnod,                                 &
     &    org_check%xx_test, nod_check%xx_test, SR_sig, SR_r)
      call dealloc_ele_4_comm_test(org_check)
!
      call SOLVER_SEND_RECV_int8_type(new_node%numnod, new_comm,        &
     &    SR_sig, SR_il, nod_check%i_gl_test)
      call SOLVER_SEND_RECV_3_type(new_node%numnod, new_comm,           &
     &                             SR_sig, SR_r, nod_check%xx_test)
!
      call nod_send_recv_check(new_node, nod_check)
!
      if(i_debug .gt. 0)  write(*,*) my_rank,                           &
     &     'Failed communication for node', nod_check%num_diff
      call collect_failed_comm(nod_check)
      if(my_rank .eq. 0) write(*,*) my_rank,                            &
     &   'Total Failed communication for node',                         &
     &    nod_check%istack_diff_pe(nprocs)
!
      end subroutine node_transfer_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine nod_send_recv_check(node, nod_check)
!
      use diff_geometory_comm_test
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(work_for_comm_check), intent(inout) :: nod_check
!
!
      nod_check%num_diff                                                &
     &   = count_node_comm_test(node%numnod, node%inod_global, node%xx, &
     &                          nod_check%i_gl_test, nod_check%xx_test)
!
      call alloc_diff_ele_comm_test(nod_check)
      call compare_nod_comm_test                                        &
     &   (node%numnod, node%inod_global, node%xx,                       &
     &    nod_check%i_gl_test, nod_check%xx_test, nod_check%num_diff,   &
     &    nod_check%i_diff, nod_check%x_diff)
!
      end subroutine nod_send_recv_check
!
! ----------------------------------------------------------------------
!
      subroutine ele_send_recv_check(numele, iele_gl, x_ele, wk_check)
!
      use diff_geometory_comm_test
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint_gl), intent(in) :: iele_gl(numele)
      real(kind = kreal), intent(in) :: x_ele(numele,3)
!
      type(work_for_comm_check), intent(inout) :: wk_check
!
!
      wk_check%num_diff =  count_ele_comm_test                          &
     &               (numele, x_ele, wk_check%xx_test)
      call alloc_diff_ele_comm_test(wk_check)
      call compare_ele_comm_test(numele, x_ele,                         &
     &    wk_check%xx_test, wk_check%num_diff,                          &
     &    wk_check%i_diff, wk_check%x_diff)
      call dealloc_ele_4_comm_test(wk_check)
!
      end subroutine ele_send_recv_check
!
! ----------------------------------------------------------------------
!
      subroutine collect_failed_comm(wk_check)
!
      use diff_geometory_comm_test
      use solver_SR_type
      use collect_SR_int
      use collect_SR_N
!
      type(work_for_comm_check), intent(inout) :: wk_check
!
!
      call alloc_comm_stack_ctest_IO(wk_check)
!
      call resize_SR_flag(1, nprocs, SR_sig_c)
      call count_collect_SR_num                                         &
     &   (wk_check%num_diff,  wk_check%istack_diff_pe, SR_sig_c)
!
      call alloc_ele_comm_test_IO(wk_check)
      call collect_send_recv_int                                        &
     &   (0, wk_check%num_diff, wk_check%i_diff,                        &
     &    wk_check%istack_diff_pe, wk_check%i_diff_IO, SR_sig_c)
      call collect_send_recv_N                                          &
     &   (0, isix, wk_check%num_diff, wk_check%x_diff,                  &
     &    wk_check%istack_diff_pe, wk_check%x_diff_IO, SR_sig_c)
      call dealloc_diff_ele_comm_test(wk_check)
      call dealloc_SR_flag(SR_sig_c)
!
      end subroutine collect_failed_comm
!
! ----------------------------------------------------------------------
!
      end module mesh_send_recv_check
