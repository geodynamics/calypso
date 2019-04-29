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
!!     &         internal_flag, e_comm, iele_global)
!!      subroutine check_element_position(txt, nele, x_ele, e_comm)
!!@endverbatim
!!
      module const_global_element_ids
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
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
      integer(kind = kint) :: nnod
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
      call MPI_Allgather(nnod, 1, CALYPSO_INTEGER,                      &
     &    nnod_list_gl, 1, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
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
      integer(kind = kint) :: nnod
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
      call MPI_Allgather(nnod, 1, CALYPSO_INTEGER,                      &
     &    nnod_list_gl, 1, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
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
     &          internal_flag, e_comm, iele_global)
!
      use t_comm_table
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
      call SOLVER_SEND_RECV_int8_type(nele, e_comm, iele_global)
!
      do iele = 1, nele
        if(iele_global(iele) .eq. 0)  write(*,*)                        &
     &        'Missing communication for ', trim(txt), ': ', iele
      end do
!
      end subroutine set_global_ele_id
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_element_position(txt, nele, x_ele, e_comm)
!
      use t_comm_table
      use solver_SR_type
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: nele
      real(kind = kreal), intent(in)  :: x_ele(nele,3)
!
      type(communication_table), intent(in) :: e_comm
!
!
      real(kind = kreal) :: dx, dy, dz
      real(kind = kreal), allocatable :: x_test(:)
      integer(kind = kint) :: iele, inum, iflag, iflag_gl
!
!
      if(i_debug .gt. 0) write(*,*) 'Number of  ', trim(txt),           &
     &           ' for ', my_rank, ': ',   nele, size(x_ele,1)
      allocate(x_test(3*nele))
!
!$omp parallel do
      do iele = 1, nele
        x_test(3*iele-2) = x_ele(iele,1)
        x_test(3*iele-1) = x_ele(iele,2)
        x_test(3*iele  ) = x_ele(iele,3)
      end do
!$omp end parallel do
!
!$omp parallel do private(inum,iele)
      do inum = 1, e_comm%ntot_import
        iele = e_comm%item_import(inum)
        x_test(3*iele-2) = 1.e30
        x_test(3*iele-1) = 1.e30
        x_test(3*iele  ) = 1.e30
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_3_type(nele, e_comm, x_test(1))
!
      iflag = 0
      do iele = 1, nele
        dx = x_test(3*iele-2) - x_ele(iele,1)
        dy = x_test(3*iele-1) - x_ele(iele,2)
        dz = x_test(3*iele  ) - x_ele(iele,3)
        if(     (abs(dx) .ge. TINY)  .or. (abs(dy) .ge. TINY)           &
     &     .or. (abs(dz) .ge. TINY)) then
          write(*,*) 'wrong ', trim(txt), ' position at: ',             &
     &         my_rank, iele, x_ele(iele,1:3), dx, dy, dz
        end if
      end do
!
      call mpi_Allreduce(iflag, iflag_gl, 1, CALYPSO_INTEGER, MPI_SUM,  &
     &     CALYPSO_COMM, ierr_MPI)
      if(iflag_gl .eq. 0 .and. my_rank .eq. 0) write(*,*)               &
     &     trim(txt), ' position is successfully syncronizad'
!
      deallocate(x_test)
!
      end subroutine check_element_position
!
!-----------------------------------------------------------------------
!
      end module const_global_element_ids
