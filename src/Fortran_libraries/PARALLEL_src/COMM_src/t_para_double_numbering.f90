!>@file   t_para_double_numbering.f90
!!       module t_para_double_numbering
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2013
!
!> @brief Data for merged UCD file output
!!
!!@verbatim
!!      subroutine alloc_double_numbering(numnod, dbl_id)
!!      subroutine dealloc_double_numbering(dbl_id)
!!        type(node_ele_double_number), intent(inout) :: dbl_id
!!
!!      subroutine set_node_double_numbering(node, nod_comm, inod_dbl,  &
!!     &                                     SR_sig, SR_i)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_ele_double_number), intent(inout) :: inod_dbl
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!      subroutine set_ele_double_numbering(numele, ie, ele_comm,       &
!!     &          inod_dbl, iele_dbl, SR_sig, SR_i)
!!        integer(kind = kint), intent(in) :: numele
!!        integer(kind = kint), intent(in) :: ie(numele,1)
!!        type(communication_table), intent(in) :: ele_comm
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(node_ele_double_number), intent(inout) :: iele_dbl
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!
!!      subroutine find_belonged_pe_4_node                              &
!!     &         (my_rank, node, nod_comm, ip_node)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        integer(kind = kint), intent(inout) :: ip_node(node%numnod)
!!@endverbatim
!
      module t_para_double_numbering
!
      use m_precision
      use m_constants
!
      use calypso_mpi
!
      implicit none
!
!>      Structure of double numbering
      type node_ele_double_number
!>        number of node for each subdomain
        integer(kind = kint) :: num_dbl
!>        local node ID
        integer(kind = kint), allocatable :: index(:)
!>        belonged subdomains ID for each node
        integer(kind = kint), allocatable :: irank(:)
      end type node_ele_double_number
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_double_numbering(numnod, dbl_id)
!
      integer(kind = kint), intent(in) :: numnod
      type(node_ele_double_number), intent(inout) :: dbl_id
!
!
      dbl_id%num_dbl = numnod
      allocate(dbl_id%index(dbl_id%num_dbl))
      allocate(dbl_id%irank(dbl_id%num_dbl))
      if(dbl_id%num_dbl .gt. 0) then
!$omp parallel workshare
        dbl_id%index(1:dbl_id%num_dbl) =  0
        dbl_id%irank(1:dbl_id%num_dbl) = -1
!$omp end parallel workshare
      end if
!
      end subroutine alloc_double_numbering
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_double_numbering(dbl_id)
!
      type(node_ele_double_number), intent(inout) :: dbl_id
!
!
      deallocate(dbl_id%index, dbl_id%irank)
!
      end subroutine dealloc_double_numbering
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_node_double_numbering(node, nod_comm, inod_dbl,    &
     &                                     SR_sig, SR_i)
!
      use t_solver_SR
      use t_solver_SR_int
      use t_geometry_data
      use t_comm_table
      use solver_SR_type
      use find_belonged_process
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(node_ele_double_number), intent(inout) :: inod_dbl
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint) :: inod
!
!
      call find_belonged_pe_4_node                                      &
     &   (my_rank, node, nod_comm, inod_dbl%irank)
!
!$omp parallel do
      do inod = 1, node%numnod
        inod_dbl%index(inod) = inod
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type(node%numnod, nod_comm,             &
     &                               SR_sig, SR_i, inod_dbl%index(1))
!
      end subroutine set_node_double_numbering
!
! -----------------------------------------------------------------------
!
      subroutine set_ele_double_numbering(numele, ie, ele_comm,         &
     &          inod_dbl, iele_dbl, SR_sig, SR_i)

!
      use t_solver_SR
      use t_solver_SR_int
      use t_geometry_data
      use t_comm_table
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ie(numele,1)
      type(communication_table), intent(in) :: ele_comm
      type(node_ele_double_number), intent(in) :: inod_dbl
!
      type(node_ele_double_number), intent(inout) :: iele_dbl
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      integer(kind = kint) :: iele
!
!$omp parallel do
      do iele = 1, numele
        iele_dbl%index(iele) = iele
        iele_dbl%irank(iele) = my_rank
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type(numele, ele_comm,                  &
     &                               SR_sig, SR_i, iele_dbl%irank(1))
      call SOLVER_SEND_RECV_int_type(numele, ele_comm,                  &
     &                               SR_sig, SR_i, iele_dbl%index(1))
!
      end subroutine set_ele_double_numbering
!
! -----------------------------------------------------------------------
!
      end module t_para_double_numbering
