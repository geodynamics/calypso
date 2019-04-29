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
!!        type(parallel_double_numbering), intent(inout) :: dbl_id
!!
!!      subroutine set_para_double_numbering                            &
!!     &         (internal_node, nod_comm, dbl_id)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(parallel_double_numbering), intent(inout) :: dbl_id
!!      subroutine set_para_ele_double_numbering                        &
!!     &         (internal_node, ele_comm, ele, dbl_id)
!!        type(communication_table), intent(in) :: ele_comm
!!        type(element_data), intent(in) :: ele
!!        type(parallel_double_numbering), intent(inout) :: dbl_id
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
      type parallel_double_numbering
!>        number of node for each subdomain
        integer(kind = kint) :: nnod_local
!>        local node ID
        integer(kind = kint), allocatable :: inod_local(:)
!>        belonged subdomains ID for each node
        integer(kind = kint), allocatable :: irank_home(:)
      end type parallel_double_numbering
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
      type(parallel_double_numbering), intent(inout) :: dbl_id
!
!
      dbl_id%nnod_local = numnod
      allocate(dbl_id%inod_local(dbl_id%nnod_local))
      allocate(dbl_id%irank_home(dbl_id%nnod_local))
      if(dbl_id%nnod_local .gt. 0) then
        dbl_id%inod_local = 0
        dbl_id%irank_home = 0
      end if
!
      end subroutine alloc_double_numbering
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_double_numbering(dbl_id)
!
      type(parallel_double_numbering), intent(inout) :: dbl_id
!
!
      deallocate(dbl_id%inod_local, dbl_id%irank_home)
!
      end subroutine dealloc_double_numbering
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_para_double_numbering                              &
     &         (internal_node, nod_comm, dbl_id)
!
      use t_ucd_data
      use t_comm_table
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: internal_node
      type(communication_table), intent(in) :: nod_comm
      type(parallel_double_numbering), intent(inout) :: dbl_id
!
      integer(kind = kint) :: inod
!
!$omp parallel do
      do inod = 1, internal_node
        dbl_id%inod_local(inod) = inod
        dbl_id%irank_home(inod) = my_rank
      end do
!$omp end parallel do
!$omp parallel do
      do inod = internal_node+1, dbl_id%nnod_local
        dbl_id%inod_local(inod) =  0
        dbl_id%irank_home(inod) = -1
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (dbl_id%nnod_local, nod_comm, dbl_id%inod_local)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (dbl_id%nnod_local, nod_comm, dbl_id%irank_home)
!
      end subroutine set_para_double_numbering
!
! -----------------------------------------------------------------------
!
      subroutine set_para_ele_double_numbering                          &
     &         (internal_node, ele_comm, ele, dbl_id)
!
      use t_ucd_data
      use t_comm_table
      use t_geometry_data
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: internal_node
      type(communication_table), intent(in) :: ele_comm
      type(element_data), intent(in) :: ele
      type(parallel_double_numbering), intent(inout) :: dbl_id
!
      integer(kind = kint) :: iele
!
!$omp parallel do
      do iele = 1, dbl_id%nnod_local
        if(ele%ie(iele,1) .le. internal_node) then
          dbl_id%inod_local(iele) = iele
          dbl_id%irank_home(iele) = my_rank
        else
          dbl_id%inod_local(iele) = 0
          dbl_id%irank_home(iele) = -1
        end if
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int_type                                    &
     &   (ele%numele, ele_comm, dbl_id%inod_local)
      call SOLVER_SEND_RECV_int_type                                    &
     &   (ele%numele, ele_comm, dbl_id%irank_home)
!
      end subroutine set_para_ele_double_numbering
!
! -----------------------------------------------------------------------
!
      end module t_para_double_numbering
