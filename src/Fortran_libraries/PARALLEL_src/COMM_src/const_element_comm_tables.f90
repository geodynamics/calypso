!>@file   const_element_comm_tables.f90
!!@brief  module const_element_comm_tables
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine const_global_mesh_infos(mesh)
!!
!!      subroutine const_ele_comm_table(node, nod_comm, ele,            &
!!     &                                ele_comm, m_SR)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(inout) :: ele_comm
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine const_edge_comm_table(node, nod_comm,                &
!!     &                                 edge_comm, edge, m_SR)
!!      subroutine dealloc_edge_comm_table(edge_comm, edge)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(inout) :: edge_comm
!!        type(edge_data), intent(inout) :: edge
!!        type(mesh_SR), intent(inout) :: m_SR
!!
!!      subroutine const_global_numnod_list(node)
!!      subroutine const_global_numele_list(ele)
!!      subroutine set_node_ele_double_address                          &
!!     &         (node, ele, nod_comm, ele_comm, inod_dbl, iele_dbl,    &
!!     &          SR_sig, SR_i)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: ele_comm
!!        type(node_ele_double_number), intent(inout) :: inod_dbl
!!        type(node_ele_double_number), intent(inout) :: iele_dbl
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!@endverbatim
!
      module const_element_comm_tables
!
      use m_precision
      use calypso_mpi
      use t_next_node_ele_4_node
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_comm_table
      use t_failed_export_list
      use t_mesh_SR
!
      use m_machine_parameter
!
      implicit none
!
      character(len=kchara), parameter :: txt_ele =  'element'
      character(len=kchara), parameter :: txt_edge = 'edge'
!
      private :: txt_ele, txt_edge
      private :: find_position_range
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_global_mesh_infos(mesh)
!
      use set_element_id_4_node
      use const_global_element_ids
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      if(iflag_debug.gt.0) write(*,*)' const_global_numnod_list'
      call const_global_numnod_list(mesh%node)
!
      if(iflag_debug.gt.0) write(*,*) ' find_position_range'
      call find_position_range(mesh%node)
!
      end subroutine const_global_mesh_infos
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_ele_comm_tbls_gl_nele(mesh)
!
      type(mesh_geometry), intent(inout) :: mesh
!
!
      call dealloc_numnod_stack(mesh%node)
      call dealloc_numele_stack(mesh%ele)
!
      end subroutine dealloc_ele_comm_tbls_gl_nele
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_global_numnod_list(node)
!
      use const_global_element_ids
!
      type(node_data), intent(inout) :: node
!
!
      call alloc_numnod_stack(nprocs, node)
!
      call count_number_of_node_stack(node%numnod, node%istack_numnod)
      call count_number_of_node_stack                                   &
     &   (node%internal_node, node%istack_internod)
!
      end subroutine const_global_numnod_list
!
!  ---------------------------------------------------------------------
!
      subroutine const_global_numele_list(ele)
!
      use const_global_element_ids
!
      type(element_data), intent(inout) :: ele
!
!
      call alloc_numele_stack(nprocs, ele)
!
      call count_number_of_node_stack(ele%numele, ele%istack_numele)
      call count_number_of_node_stack                                   &
     &   (ele%internal_ele, ele%istack_interele)
!
      end subroutine const_global_numele_list
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_ele_comm_table(node, nod_comm, ele,              &
     &                                ele_comm, m_SR)
!
      use m_geometry_constants
      use t_solver_SR_int
      use t_solver_SR_int8
      use t_para_double_numbering
      use t_element_double_number
      use t_const_comm_table
      use t_sum_local_node_id_list
      use const_global_element_ids
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(element_data), intent(in) :: ele
      type(communication_table), intent(inout) :: ele_comm
      type(mesh_SR), intent(inout) :: m_SR
!
      type(node_ele_double_number) :: inod_dbl
      type(element_double_number) :: iele_dbl
      type(element_around_node) :: neib_ele
      type(failed_table) :: fail_tbl_e
      type(sum_of_local_id_list) :: sum_list_e
!
!
      call alloc_double_numbering(node%numnod, inod_dbl)
      call set_node_double_numbering(node, nod_comm, inod_dbl,          &
     &                               m_SR%SR_sig, m_SR%SR_i)
!
      call alloc_ele_double_number(ele%numele, iele_dbl)
      call find_belonged_pe_4_ele                                       &
     &   (inod_dbl, ele%numele, ele%ie(1,1), iele_dbl)
!
      call set_ele_id_4_node_sum_order(node, ele, inod_dbl,             &
     &                                 neib_ele, sum_list_e)
!
      call alloc_failed_export(0, fail_tbl_e)
      call const_comm_table_by_connenct                                 &
     &   (txt_ele, ele%numele, ele%nnod_4_ele, ele%ie,                  &
     &    ele%x_ele, node, nod_comm, inod_dbl, iele_dbl, neib_ele,      &
     &    sum_list_e, ele_comm, fail_tbl_e, m_SR%SR_sig)
      call dealloc_iele_belonged(neib_ele)
      call dealloc_failed_export(fail_tbl_e)
!
      call check_global_ele_id(txt_ele, ele%numele,                     &
     &    ele%interior_ele, ele_comm, ele%iele_global,                  &
     &    m_SR%SR_sig, m_SR%SR_il)
!
      call check_element_position                                       &
     &   (txt_ele, node%inod_global, ele%numele,                        &
     &    ele%nnod_4_ele, ele%ie, ele%iele_global, ele%x_ele,           &
     &    inod_dbl, ele_comm, m_SR%SR_sig, m_SR%SR_r)
      call dealloc_sum_of_local_id_list(sum_list_e)
      call dealloc_double_numbering(inod_dbl)
      call dealloc_ele_double_number(iele_dbl)
!
      end subroutine const_ele_comm_table
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_edge_comm_table(node, nod_comm,                  &
     &                                 edge_comm, edge, m_SR)
!
      use m_geometry_constants
      use t_para_double_numbering
      use t_element_double_number
      use t_const_comm_table
      use t_sum_local_node_id_list
      use const_global_element_ids
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      type(communication_table), intent(inout) :: edge_comm
      type(edge_data), intent(inout) :: edge
      type(mesh_SR), intent(inout) :: m_SR
!
      type(node_ele_double_number) :: inod_dbl
      type(element_double_number) :: iedge_dbl
      type(element_around_node) :: neib_edge
      type(failed_table) :: fail_tbl_d
      type(sum_of_local_id_list) :: sum_list_d
!
      integer(kind = kint) :: internal_num = 0
      integer(kind = kint_gl), allocatable :: istack_ineredge(:)
!      integer(kind = kint) :: i, i1, i2
!
!
      call alloc_double_numbering(node%numnod, inod_dbl)
      call set_node_double_numbering(node, nod_comm, inod_dbl,          &
     &                               m_SR%SR_sig, m_SR%SR_i)
!
      call alloc_ele_double_number(edge%numedge, iedge_dbl)
      call alloc_interior_edge(edge)
      call find_belonged_pe_4_edge(my_rank, inod_dbl,                   &
     &    edge%numedge, edge%nnod_4_edge, edge%ie_edge,                 &
     &    internal_num, edge%interior_edge, iedge_dbl)
!
!
      if(iflag_debug.gt.0) write(*,*) 'set_edge_id_4_node_sum_order'
      call set_edge_id_4_node_sum_order(node, edge, inod_dbl,           &
     &                                  neib_edge, sum_list_d)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &          ' const_comm_table_by_connenct in edge'
      call alloc_failed_export(0, fail_tbl_d)
      call const_comm_table_by_connenct                                 &
     &   (txt_edge, edge%numedge, edge%nnod_4_edge, edge%ie_edge,       &
     &    edge%x_edge, node, nod_comm, inod_dbl, iedge_dbl, neib_edge,  &
     &    sum_list_d, edge_comm, fail_tbl_d, m_SR%SR_sig)
      call dealloc_iele_belonged(neib_edge)
      call dealloc_failed_export(fail_tbl_d)
!
!
      allocate(istack_ineredge(0:nprocs))
      istack_ineredge(0:nprocs) = 0
!
      call count_number_of_node_stack(internal_num, istack_ineredge)
      call set_global_ele_id(txt_edge, edge%numedge, istack_ineredge,   &
     &    edge%interior_edge, edge_comm, edge%iedge_global,             &
     &    m_SR%SR_sig, m_SR%SR_il)
      deallocate(istack_ineredge)
!
      call calypso_mpi_barrier
      call check_element_position                                       &
     &   (txt_edge, node%inod_global, edge%numedge,                     &
     &    edge%nnod_4_edge, edge%ie_edge, edge%iedge_global,            &
     &    edge%x_edge, inod_dbl, edge_comm, m_SR%SR_sig, m_SR%SR_r)
      call dealloc_sum_of_local_id_list(sum_list_d)
      call dealloc_double_numbering(inod_dbl)
      call dealloc_ele_double_number(iedge_dbl)
!
      end subroutine const_edge_comm_table
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_edge_comm_table(edge_comm, edge)
!
      type(communication_table), intent(inout) :: edge_comm
      type(edge_data), intent(inout) :: edge
!
      call dealloc_comm_table(edge_comm)
      call dealloc_interior_edge(edge)
!
      end subroutine dealloc_edge_comm_table
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine find_position_range(node)
!
      use t_geometry_data
      use calypso_mpi_real
      use transfer_to_long_integers
!
      type(node_data), intent(inout) :: node
!
!
!  Evaluate range in local domain
      call calypso_mpi_allreduce_real(node%xyz_max_lc, node%xyz_max_gl, &
     &                                cast_long(3), MPI_MAX)
      call calypso_mpi_allreduce_real(node%xyz_min_lc, node%xyz_min_gl, &
     &                                cast_long(3), MPI_MIN)
!
      if(iflag_debug .gt. 0) then
        write(*,*)  'x range: ', node%xyz_min_gl(1), node%xyz_max_gl(1)
        write(*,*)  'y range: ', node%xyz_min_gl(2), node%xyz_max_gl(2)
        write(*,*)  'z range: ', node%xyz_min_gl(3), node%xyz_max_gl(3)
      end if
!
      end subroutine find_position_range
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_node_ele_double_address                            &
     &         (node, ele, nod_comm, ele_comm, inod_dbl, iele_dbl,      &
     &          SR_sig, SR_i)
!
      use t_solver_SR_int
      use t_para_double_numbering
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
!
      type(node_ele_double_number), intent(inout) :: inod_dbl
      type(node_ele_double_number), intent(inout) :: iele_dbl
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      call set_node_double_numbering(node, nod_comm, inod_dbl,          &
     &                               SR_sig, SR_i)
      call set_ele_double_numbering(ele%numele, ele%ie(1,1), ele_comm,  &
     &                              inod_dbl, iele_dbl, SR_sig, SR_i)
!
      end subroutine set_node_ele_double_address
!
! -----------------------------------------------------------------------
!
      end module const_element_comm_tables
