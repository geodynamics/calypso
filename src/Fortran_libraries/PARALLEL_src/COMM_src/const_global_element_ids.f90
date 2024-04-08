!>@file   const_global_element_ids.f90
!!@brief  module const_global_element_ids
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Construct global element IDs by number of internal elements
!!
!!@verbatim
!!      subroutine count_number_of_node_stack(nnod, istack_nod_list)
!!      subroutine count_number_of_node_stack4(nnod, istack_nod_list)
!!      subroutine set_global_ele_id(txt, nele, istack_internal_e,      &
!!     &          internal_flag, e_comm, iele_global, SR_sig, SR_il)
!!        character(len=kchara), intent(in) :: txt
!!        integer(kind = kint), intent(in) :: nele
!!        integer(kind = kint), intent(in) :: internal_flag(nele)
!!        integer(kind = kint_gl), intent(in)                           &
!!     &        :: istack_internal_e(0:nprocs)
!!        type(communication_table), intent(in) :: e_comm
!!        integer(kind = kint_gl), intent(inout)  :: iele_global(nele)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!      subroutine check_global_ele_id(txt, nele, internal_flag,        &
!!     &          e_comm, iele_global, SR_sig, SR_il)
!!        character(len=kchara), intent(in) :: txt
!!        integer(kind = kint), intent(in) :: nele
!!        integer(kind = kint), intent(in) :: internal_flag(nele)
!!        type(communication_table), intent(in) :: e_comm
!!        integer(kind = kint_gl), intent(in)  :: iele_global(nele)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!      subroutine check_element_position(txt, inod_global,             &
!!     &          nele, nnod_4_ele, ie, iele_global, x_ele,             &
!!     &          inod_dbl, e_comm, SR_sig, SR_r)
!!        character(len=kchara), intent(in) :: txt
!!        integer(kind = kint_gl), intent(in) :: inod_global(nele)
!!        integer(kind = kint), intent(in) :: nele, nnod_4_ele
!!        integer(kind = kint), intent(in) :: ie(nele,nnod_4_ele)
!!        integer(kind = kint_gl), intent(in) :: iele_global(nele)
!!        real(kind = kreal), intent(in)  :: x_ele(nele,3)
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(communication_table), intent(in) :: e_comm
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!!
      module const_global_element_ids
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_solver_SR
      use t_comm_table
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_number_of_node_stack(nnod, istack_nod_list)
!
      use calypso_mpi_int
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint_gl), intent(inout)                            &
     &            :: istack_nod_list(0:nprocs)
!
      integer(kind = kint), allocatable :: nnod_list_gl(:)
!
      integer(kind = kint) :: ip
!
!
      allocate(nnod_list_gl(nprocs))
      nnod_list_gl = 0
!
      call calypso_mpi_allgather_one_int(nnod, nnod_list_gl)
!
      istack_nod_list(0) = 0
      do ip = 1, nprocs
        istack_nod_list(ip) = istack_nod_list(ip-1) + nnod_list_gl(ip)
      end do
!
      deallocate(nnod_list_gl)
!
      end subroutine count_number_of_node_stack
!
!-----------------------------------------------------------------------
!
      subroutine count_number_of_node_stack4(nnod, istack_nod_list)
!
      use calypso_mpi_int
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(inout) :: istack_nod_list(0:nprocs)
!
      integer(kind = kint), allocatable :: nnod_list_gl(:)
!
      integer(kind = kint) :: ip
!
!
      allocate(nnod_list_gl(nprocs))
      nnod_list_gl = 0
!
      call calypso_mpi_allgather_one_int(nnod, nnod_list_gl)
!
      istack_nod_list(0) = 0
      do ip = 1, nprocs
        istack_nod_list(ip) = istack_nod_list(ip-1) + nnod_list_gl(ip)
      end do
!
      deallocate(nnod_list_gl)
!
      end subroutine count_number_of_node_stack4
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_global_ele_id(txt, nele, istack_internal_e,        &
     &          internal_flag, e_comm, iele_global, SR_sig, SR_il)
!
      use t_solver_SR_int8
      use solver_SR_type
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: nele
      integer(kind = kint), intent(in) :: internal_flag(nele)
      integer(kind = kint_gl), intent(in)                               &
     &        :: istack_internal_e(0:nprocs)
!
      type(communication_table), intent(in) :: e_comm
!
      integer(kind = kint_gl), intent(inout)  :: iele_global(nele)
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer(kind = kint) :: iele, icou
!
!
      icou = 0
      do iele = 1, nele
        if(internal_flag(iele) .gt. 0) then
          icou = icou + 1
          iele_global(iele) = icou + istack_internal_e(my_rank)
        else
          iele_global(iele) = 0
        end if
      end do
!
      call SOLVER_SEND_RECV_int8_type(nele, e_comm,                     &
     &                                SR_sig, SR_il, iele_global)
!
      do iele = 1, nele
        if(iele_global(iele) .eq. 0)  write(*,*)                        &
     &        'Missing communication for ', trim(txt), ': ', iele
      end do
!
      end subroutine set_global_ele_id
!
!-----------------------------------------------------------------------
!
      subroutine check_global_ele_id(txt, nele, internal_flag,          &
     &          e_comm, iele_global, SR_sig, SR_il)
!
      use t_solver_SR_int8
      use solver_SR_type
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: nele
      integer(kind = kint), intent(in) :: internal_flag(nele)
!
      type(communication_table), intent(in) :: e_comm
!
      integer(kind = kint_gl), intent(in)  :: iele_global(nele)
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer(kind = kint_gl), allocatable :: iele_comm(:)
      integer(kind = kint) :: iele
!
!
      allocate(iele_comm(nele))
!
!$omp parallel do private(iele)
      do iele = 1, nele
        if(internal_flag(iele) .gt. 0) then
          iele_comm(iele) = iele_global(iele)
        else
          iele_comm(iele) = 0
        end if
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int8_type(nele, e_comm,                     &
     &                                SR_sig, SR_il, iele_comm)
!
      do iele = 1, nele
        if(iele_comm(iele) .ne. iele_global(iele))  write(*,*)          &
     &        'Failed communication for ', trim(txt), ': ', iele
      end do
      deallocate(iele_comm)
!
      end subroutine check_global_ele_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_element_position(txt, inod_global,               &
     &          nele, nnod_4_ele, ie, iele_global, x_ele,               &
     &          inod_dbl, e_comm, SR_sig, SR_r)
!
      use t_para_double_numbering
      use t_element_double_number
      use calypso_mpi_int
      use solver_SR_type
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: nele, nnod_4_ele
      integer(kind = kint_gl), intent(in) :: inod_global(nele)
      integer(kind = kint), intent(in) :: ie(nele,nnod_4_ele)
      integer(kind = kint_gl), intent(in) :: iele_global(nele)
      real(kind = kreal), intent(in)  :: x_ele(nele,3)
!
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(communication_table), intent(in) :: e_comm
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      real(kind = kreal) :: dx, dy, dz
      real(kind = kreal), allocatable :: x_test(:)
!      integer(kind = kint), allocatable :: id_test(:,:)
!      integer(kind = kint), allocatable :: ir_test(:,:)
      integer(kind = kint_gl), allocatable :: l_test(:)
      integer(kind = kint) :: inod_e(nnod_4_ele)
      integer(kind = kint) :: iele, inum, iflag, iflag_gl
!      integer(kind = kint) :: k1
!
!
      if(i_debug .gt. 0) write(*,*) 'Number of  ', trim(txt),           &
     &           ' for ', my_rank, ': ',   nele, size(x_ele,1)
      allocate(x_test(3*nele))
!      allocate(id_test(nele,nnod_4_ele))
!      allocate(ir_test(nele,nnod_4_ele))
      allocate(l_test(nele))
!
!$omp parallel do
      do iele = 1, nele
        x_test(3*iele-2) = x_ele(iele,1)
        x_test(3*iele-1) = x_ele(iele,2)
        x_test(3*iele  ) = x_ele(iele,3)
      end do
!$omp end parallel do
!
!$omp parallel workshare
      l_test(1:nele) = iele_global(1:nele)
!$omp end parallel workshare
!
!      do k1 = 1, nnod_4_ele
!        id_test(1:nele,k1) = inod_dbl%index(ie(iele,k1))
!        ir_test(1:nele,k1) = inod_dbl%irank(ie(iele,k1))
!      end do
!
!
!$omp parallel do private(inum,iele)
      do inum = 1, e_comm%ntot_import
        iele = e_comm%item_import(inum)
        x_test(3*iele-2) = 1.e30
        x_test(3*iele-1) = 1.e30
        x_test(3*iele  ) = 1.e30
        l_test(iele) = 0
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_3_type(nele, e_comm,                        &
     &                             SR_sig, SR_r, x_test(1))
!
      iflag = 0
      do iele = 1, nele
        dx = x_test(3*iele-2) - x_ele(iele,1)
        dy = x_test(3*iele-1) - x_ele(iele,2)
        dz = x_test(3*iele  ) - x_ele(iele,3)
        if(     (abs(dx) .ge. TINY)  .or. (abs(dy) .ge. TINY)           &
     &     .or. (abs(dz) .ge. TINY)) then
          iflag = iflag + 1
          inod_e(1:nnod_4_ele) = ie(iele,1:nnod_4_ele)
          write(*,*) 'wrong ', trim(txt), ' position at: ',             &
     &      my_rank, iele, x_ele(iele,1:3), dx, dy, dz,                 &
     &      'local connectivity: ', inod_e(1:nnod_4_ele),               &
     &      'origin  rank: ', inod_dbl%irank(inod_e(1:nnod_4_ele)),     &
     &      'origin node id: ', inod_dbl%index(inod_e(1:nnod_4_ele)),   &
     &      'origin global node id: ',                                  &
     &      inod_global(inod_e(1:nnod_4_ele))

        end if
      end do
!
      call calypso_mpi_allreduce_one_int(iflag, iflag_gl, MPI_SUM)
      if(iflag_gl .eq. 0 .and. my_rank .eq. 0) write(*,*)               &
     &     trim(txt), ' position is successfully syncronizad'
!
!      deallocate(id_test, ir_test)
      deallocate(x_test, l_test)
!
      end subroutine check_element_position
!
!-----------------------------------------------------------------------
!
      end module const_global_element_ids
