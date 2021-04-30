!>@file   collect_SR_int.f90
!!@brief  module collect_SR_int
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!@n      using reverse import table
!!
!!@verbatim
!!      subroutine count_collect_SR_num(NP, istack_NP, SR_sig)
!!        integer(kind = kint), intent(in) :: NP
!!        integer(kind = kint), intent(inout) :: istack_NP(0:nprocs)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!
!!      subroutine collect_send_recv_int                                &
!!     &         (dest_rank, NP, iX, istack_NP, X_dest, SR_sig)
!!      integer, intent(in) :: dest_rank
!!      integer(kind = kint), intent(in) :: NP
!!      integer(kind = kint), intent(in) :: istack_NP(0:nprocs)
!!      integer(kind = kint), intent(in) :: iX(NP)
!!      integer(kind = kint), intent(inout) :: iX_dest(istack_NP(nprocs))
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!
      module collect_SR_int
!
      use m_precision
      use calypso_mpi
      use t_solver_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_collect_SR_num(NP, istack_NP, SR_sig)
!
      integer(kind = kint), intent(in) :: NP
!
      integer(kind = kint), intent(inout) :: istack_NP(0:nprocs)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint), allocatable :: nums_NP(:)
      integer(kind = kint) :: ntmp(1)
      integer :: id_rank, ip
!
!
      call resize_SR_flag(1, nprocs, SR_sig)
!
      ntmp(1) = NP
      call MPI_ISEND(ntmp, 1, CALYPSO_INTEGER,                          &
     &    0, 0, CALYPSO_COMM, SR_sig%req1(1), ierr_MPI)
      if (my_rank .eq. 0) then
        allocate(nums_NP(nprocs))
!
        do ip = 1, nprocs
          id_rank = int(ip - 1)
          call MPI_IRECV(nums_NP(ip), 1, CALYPSO_INTEGER,               &
     &        id_rank, 0, CALYPSO_COMM, SR_sig%req2(ip), ierr_MPI)
        end do
        call MPI_WAITALL                                                &
     &     (nprocs, SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
      end if
      call MPI_WAITALL(1, SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
!
      if (my_rank .eq. 0) then
        istack_NP(0) = 0
        do ip = 1, nprocs
          istack_NP(ip) = istack_NP(ip-1) + nums_NP(ip)
        end do
!
        deallocate(nums_NP)
      end if
!
      end subroutine count_collect_SR_num
!
! ----------------------------------------------------------------------
!
      subroutine collect_send_recv_int                                  &
     &         (dest_rank, NP, iX, istack_NP, iX_dest, SR_sig)
!
      integer, intent(in) :: dest_rank
      integer(kind = kint), intent(in) :: NP
!
      integer(kind = kint), intent(in) :: istack_NP(0:nprocs)
      integer(kind = kint), intent(in) :: iX(NP)
!
      integer(kind = kint), intent(inout) :: iX_dest(istack_NP(nprocs))
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: ist, ip
      integer :: num, id_rank
!
!
      call resize_SR_flag(1, nprocs, SR_sig)
!
      call MPI_ISEND(iX, int(NP), CALYPSO_INTEGER, dest_rank,           &
     &               0, CALYPSO_COMM, SR_sig%req1(ione), ierr_MPI)
!
      if (my_rank .eq. dest_rank) then
        do ip = 1, nprocs
          id_rank = int(ip - 1)
          ist = istack_NP(ip-1) + 1
          num = int(istack_NP(ip) - istack_NP(ip-1))
          call MPI_IRECV(iX_dest(ist), num, CALYPSO_INTEGER, id_rank,   &
     &                   0, CALYPSO_COMM, SR_sig%req2(ip), ierr_MPI)
        end do
        call MPI_WAITALL                                                &
     &     (nprocs, SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
      end if
      call MPI_WAITALL(1, SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
!
      end subroutine collect_send_recv_int
!
! ----------------------------------------------------------------------
!
      end module collect_SR_int
