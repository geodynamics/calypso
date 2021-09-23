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
!!
!!      subroutine collect_small_send_recv                              &
!!     &         (istack_send, nitem_send, X_send,                      &
!!     &          ntot_recv, X_recv, SR_sig)
!!        integer(kind = kint), intent(in) :: istack_send(0:nprocs)
!!        integer(kind = kint), intent(in) :: nitem_send
!!        real(kind = kreal), intent(in) :: X_send(nitem_send)
!!        integer(kind = kint), intent(in) :: ntot_recv
!!        real(kind = kreal), intent(inout) :: X_recv(ntot_recv)
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
! ----------------------------------------------------------------------
!
      subroutine collect_small_send_recv                                &
     &         (istack_send, nitem_send, X_send,                        &
     &          ntot_recv, X_recv, SR_sig)
!
      integer(kind = kint), intent(in) :: istack_send(0:nprocs)
      integer(kind = kint), intent(in) :: nitem_send
      real(kind = kreal), intent(in) :: X_send(nitem_send)
!
      integer(kind = kint), intent(in) :: ntot_recv
      real(kind = kreal), intent(inout) :: X_recv(ntot_recv)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer :: npe_recv, id_rank, ncomp
      integer(kind = kint) :: ip, ist, num, icou
!
!
      if(my_rank .eq. 0) then
        npe_recv = 0
        do ip = 2, nprocs
          num = istack_send(ip) - istack_send(ip-1)
          if(num .gt. 0) npe_recv = npe_recv + 1
        end do
      else
        npe_recv = 1
      end if
!
      call resize_SR_flag(1, npe_recv, SR_sig)
!
      if(nitem_send .gt. 0) then
        call MPI_ISEND(X_send, nitem_send, CALYPSO_REAL, 0,             &
     &                 0, CALYPSO_COMM, SR_sig%req1(ione), ierr_MPI)
      end if
!
      if(my_rank .eq. 0) then
        icou = 0
        do ip = 2, nprocs
          id_rank = ip - 1
          ist = istack_send(ip-1)
          ncomp = istack_send(ip) - ist
          if(ncomp .gt. 0) then
            icou = icou + 1
            call MPI_IRECV(X_recv(ist+1), ncomp, CALYPSO_REAL, id_rank, &
     &          0, CALYPSO_COMM, SR_sig%req2(icou), ierr_MPI)
          end if
        end do
        call MPI_WAITALL                                                &
     &     (npe_recv, SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
!
        num = istack_send(1) - istack_send(0)
        if(num .gt. 0) X_recv(1:num) = X_send(1:num)
      end if
!
      if(my_rank .gt. 0 .and. nitem_send .gt. 0) then
        call MPI_WAITALL(1, SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
      end if
!
      end subroutine collect_small_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine collect_small_send_recv_N                              &
     &         (NB, istack_send, nitem_send, X_send,                    &
     &          ntot_recv, X_recv, SR_sig)
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: istack_send(0:nprocs)
      integer(kind = kint), intent(in) :: nitem_send
      real(kind = kreal), intent(in) :: X_send(NB*nitem_send)
!
      integer(kind = kint), intent(in) :: ntot_recv
      real(kind = kreal), intent(inout) :: X_recv(NB*ntot_recv)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer :: npe_recv, id_rank, ncomp
      integer(kind = kint) :: ip, ist, num, icou
!
!
      if(my_rank .eq. 0) then
        npe_recv = 0
        do ip = 2, nprocs
          num = istack_send(ip) - istack_send(ip-1)
          if(num .gt. 0) npe_recv = npe_recv + 1
        end do
      else
        npe_recv = 1
      end if
!
      call resize_SR_flag(1, npe_recv, SR_sig)
!
      if(nitem_send .gt. 0) then
        ncomp = NB * nitem_send
        call MPI_ISEND(X_send, ncomp, CALYPSO_REAL, 0,                  &
     &                 0, CALYPSO_COMM, SR_sig%req1(ione), ierr_MPI)
      end if
!
      if(my_rank .eq. 0) then
        icou = 0
        do ip = 2, nprocs
          id_rank = ip - 1
          ist =  NB*istack_send(ip-1)
          ncomp = NB*istack_send(ip) - ist
          if(ncomp .gt. 0) then
            icou = icou + 1
            call MPI_IRECV(X_recv(ist+1), ncomp, CALYPSO_REAL, id_rank, &
     &          0, CALYPSO_COMM, SR_sig%req2(icou), ierr_MPI)
          end if
        end do
        call MPI_WAITALL                                                &
     &     (npe_recv, SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
!
        num = NB*(istack_send(1) - istack_send(0))
        if(num .gt. 0) X_recv(1:num) = X_send(1:num)
      end if
!
      if(my_rank .gt. 0 .and. nitem_send .gt. 0) then
        call MPI_WAITALL(1, SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
      end if
!
      end subroutine collect_small_send_recv_N
!
! -----------------------------------------------------------------------
!
      end module collect_SR_N
