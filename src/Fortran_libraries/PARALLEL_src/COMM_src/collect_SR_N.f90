!>@file   collect_SR_N.f90
!!@brief  module collect_SR_N
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!@n      using reverse import table
!!
!!@verbatim
!!      subroutine collect_send_recv_N                                  &
!!     &         (dest_rank, NB, NP, X, istack_NP, X_dest, SR_sig)
!!      integer, intent(in) :: dest_rank
!!      integer(kind = kint), intent(in) :: NB, NP
!!      integer(kind = kint), intent(in) :: istack_NP(0:nprocs)
!!      real(kind = kreal), intent(in) :: X(NB*NP)
!!      real(kind = kreal), intent(inout)                               &
!!     &                   :: X_dest(NB*istack_NP(nprocs))
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!
      module collect_SR_N
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
      subroutine collect_send_recv_N                                    &
     &         (dest_rank, NB, NP, X, istack_NP, X_dest, SR_sig)
!
      integer, intent(in) :: dest_rank
      integer(kind = kint), intent(in) :: NB, NP
!
      integer(kind = kint), intent(in) :: istack_NP(0:nprocs)
      real(kind = kreal), intent(in) :: X(NB*NP)
!
      real(kind = kreal), intent(inout) :: X_dest(NB*istack_NP(nprocs))
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: ist, ip
      integer :: num, id_rank
      
!
!
      call resize_SR_flag(1, nprocs, SR_sig)
!
      num = int(NB * NP)
      call MPI_ISEND(X, num, CALYPSO_REAL, dest_rank,                   &
     &               0, CALYPSO_COMM, SR_sig%req1(ione), ierr_MPI)
!
      if (my_rank .eq. dest_rank) then
        do ip = 1, nprocs
          id_rank = int(ip - 1)
          ist = NB*istack_NP(ip-1) + 1
          num = int(NB * (istack_NP(ip) - istack_NP(ip-1)))
          call MPI_IRECV(X_dest(ist), num, CALYPSO_REAL, id_rank,       &
     &                   0, CALYPSO_COMM, SR_sig%req2(ip), ierr_MPI)
        end do
        call MPI_WAITALL                                                &
     &     (nprocs, SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
      end if
      call MPI_WAITALL(1, SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
!
      end subroutine collect_send_recv_N
!
! ----------------------------------------------------------------------
!
      end module collect_SR_N
