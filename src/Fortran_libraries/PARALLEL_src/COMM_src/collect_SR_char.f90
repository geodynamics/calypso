!>@file   collect_SR_char.f90
!!@brief  module collect_SR_char
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!@n      using reverse import table
!!
!!@verbatim
!!      subroutine collect_small_send_recv_char                         &
!!     &         (istack_send, nitem_send, cX_send,                     &
!!     &          ntot_recv, cX_recv, SR_sig)
!!        integer(kind = kint), intent(in) :: istack_send(0:nprocs)
!!        integer(kind = kint), intent(in) :: nitem_send
!!        character(len = 1), intent(in) :: cX_send(nitem_send)
!!        integer(kind = kint), intent(in) :: ntot_recv
!!        character(len = 1), intent(inout) :: cX_recv(ntot_recv)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine collect_small_send_recv_mulchar                      &
!!     &         (length, istack_send, nitem_send, cX_send,             &
!!     &          ntot_recv, cX_recv, SR_sig)
!!        integer(kind = kint), intent(in) :: length
!!        integer(kind = kint), intent(in) :: istack_send(0:nprocs)
!!        integer(kind = kint), intent(in) :: nitem_send
!!        character(len = length), intent(in) :: cX_send(nitem_send)
!!        integer(kind = kint), intent(in) :: ntot_recv
!!        character(len = length), intent(inout) :: cX_recv(ntot_recv)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!
      module collect_SR_char
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
      subroutine collect_small_send_recv_char                           &
     &         (istack_send, nitem_send, cX_send,                       &
     &          ntot_recv, cX_recv, SR_sig)
!
!
      integer(kind = kint), intent(in) :: istack_send(0:nprocs)
      integer(kind = kint), intent(in) :: nitem_send
      character(len = 1), intent(in) :: cX_send(nitem_send)
!
      integer(kind = kint), intent(in) :: ntot_recv
      character(len = 1), intent(inout) :: cX_recv(ntot_recv)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer :: npe_recv, id_rank, len_send, ncomp
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
        len_send = nitem_send
        call MPI_ISEND(cX_send, len_send, CALYPSO_CHARACTER, 0,         &
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
            call MPI_IRECV(cX_recv(ist+1), ncomp, CALYPSO_CHARACTER,    &
     &          id_rank,0, CALYPSO_COMM, SR_sig%req2(icou), ierr_MPI)
          end if
        end do
        call MPI_WAITALL                                                &
     &     (npe_recv, SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
!
        num = istack_send(1) - istack_send(0)
        if(num .gt. 0) cX_recv(1:num) = cX_send(1:num)
      end if
!
      if(nitem_send .gt. 0) then
        call MPI_WAITALL(1, SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
      end if
!
      end subroutine collect_small_send_recv_char
!
! -----------------------------------------------------------------------
!
      subroutine collect_small_send_recv_mulchar                        &
     &         (length, istack_send, nitem_send, cX_send,               &
     &          ntot_recv, cX_recv, SR_sig)
!
!
      integer(kind = kint), intent(in) :: length
      integer(kind = kint), intent(in) :: istack_send(0:nprocs)
      integer(kind = kint), intent(in) :: nitem_send
      character(len = length), intent(in) :: cX_send(nitem_send)
!
      integer(kind = kint), intent(in) :: ntot_recv
      character(len = length), intent(inout) :: cX_recv(ntot_recv)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer :: npe_recv, id_rank, len_send, len_recv
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
        len_send = nitem_send * length
        call MPI_ISEND(cX_send, len_send, CALYPSO_CHARACTER, 0,         &
     &                 0, CALYPSO_COMM, SR_sig%req1(ione), ierr_MPI)
      end if
!
      if(my_rank .eq. 0) then
        icou = 0
        do ip = 2, nprocs
          id_rank = ip - 1
          ist = istack_send(ip-1)
          len_recv = length * (istack_send(ip) - ist)
          if(len_recv .gt. 0) then
            icou = icou + 1
            call MPI_IRECV(cX_recv(ist+1), len_recv, CALYPSO_CHARACTER, &
     &          id_rank, 0, CALYPSO_COMM, SR_sig%req2(icou), ierr_MPI)
          end if
        end do
        call MPI_WAITALL                                                &
     &     (npe_recv, SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
!
        num = istack_send(1) - istack_send(0)
        if(num .gt. 0) cX_recv(1:num) = cX_send(1:num)
      end if
!
      if(my_rank .gt. 0 .and. nitem_send .gt. 0) then
        call MPI_WAITALL(1, SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
      end if
!
      end subroutine collect_small_send_recv_mulchar
!
! -----------------------------------------------------------------------
!
      end module collect_SR_char
