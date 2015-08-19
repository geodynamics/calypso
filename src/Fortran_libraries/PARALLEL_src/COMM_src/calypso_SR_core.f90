!>@file   calypso_SR_core.f90
!!@brief  module calypso_SR_core
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!@n      using reverse import table
!!
!!@verbatim
!!      subroutine calypso_send_recv_core                               &
!!     &         (NB, npe_send, isend_self, id_pe_send, istack_send,    &
!!     &              npe_recv, irecv_self, id_pe_recv, istack_recv)
!!      subroutine calypso_send_recv_intcore                            &
!!     &             (npe_send, isend_self, id_pe_send, istack_send,    &
!!     &              npe_recv, irecv_self, id_pe_recv, istack_recv)
!!
!!      subroutine calypso_send_recv_fin(npe_send, isend_self)
!!
!!      subroutine calypso_send_recv_check                              &
!!     &         (NB, npe_send, isend_self, istack_send,                &
!!     &              npe_recv, irecv_self, istack_recv)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n @param  nnod_new    Number of components for destination
!!@n
!!@n @param  npe_send    Number of processses to send
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  id_pe_send(npe_send)      Process ID to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_export(istack_send(npe_send))
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  irecv_self  Integer flag to copy within own process
!!@n @param  id_pe_recv(npe_send)      Process ID to receive
!!@n @param  istack_recv(0:npe_send)
!!                    End points of receive buffer for each process
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!
      module calypso_SR_core
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine calypso_send_recv_core                                 &
     &         (NB, npe_send, isend_self, id_pe_send, istack_send,      &
     &              npe_recv, irecv_self, id_pe_recv, istack_recv)
!
      use calypso_mpi
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
      integer (kind = kint) :: neib, ist, num, i
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
      ncomm_send = npe_send - isend_self
      ncomm_recv = npe_recv - irecv_self
!
      do neib = 1, ncomm_send
        ist = NB * istack_send(neib-1) + 1
        num = NB * (istack_send(neib  ) - istack_send(neib-1))
        call MPI_ISEND(WS(ist), num, CALYPSO_REAL,                      &
     &      id_pe_send(neib), 0, CALYPSO_COMM, req1(neib), ierr_MPI)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib = ncomm_recv, 1, -1
          ist= NB * istack_recv(neib-1) + 1
          num  = NB * (istack_recv(neib  ) - istack_recv(neib-1))
          call MPI_IRECV(WR(ist), num, CALYPSO_REAL,                    &
     &        id_pe_recv(neib), 0, CALYPSO_COMM, req2(neib), ierr_MPI)
        end do
      end if
!
      call MPI_WAITALL (ncomm_recv, req2, sta2, ierr_MPI)
!
      if (isend_self .eq. 0) return
      ist_send= NB * istack_send(npe_send-1)
      ist_recv= NB * istack_recv(npe_recv-1)
      num = NB * (istack_send(npe_send  ) - istack_send(npe_send-1))
!$omp parallel do
      do i = 1, num
        WR(ist_recv+i) = WS(ist_send+i)
      end do
!$omp end parallel do
!
      end subroutine calypso_send_recv_core
!
! ----------------------------------------------------------------------
!
      subroutine calypso_send_recv_intcore                              &
     &             (npe_send, isend_self, id_pe_send, istack_send,      &
     &              npe_recv, irecv_self, id_pe_recv, istack_recv)
!
      use calypso_mpi
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
      integer (kind = kint) :: neib, ist, num, i
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
      ncomm_send = npe_send - isend_self
      ncomm_recv = npe_recv - irecv_self
!
      do neib = 1, ncomm_send
        ist= istack_send(neib-1) + 1
        num  = istack_send(neib  ) - istack_send(neib-1)
        call MPI_ISEND(iWS(ist), num, CALYPSO_INTEGER,                  &
     &      id_pe_send(neib), 0, CALYPSO_COMM, req1(neib), ierr_MPI)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib = ncomm_recv, 1, -1
          ist= istack_recv(neib-1) + 1
          num  = istack_recv(neib  ) - istack_recv(neib-1)
          call MPI_IRECV(iWR(ist), num, CALYPSO_INTEGER,                &
     &        id_pe_recv(neib), 0, CALYPSO_COMM, req2(neib), ierr_MPI)
        end do
      end if
!
      call MPI_WAITALL (ncomm_recv, req2, sta2, ierr_MPI)
!
      if (isend_self .eq. 0) return
!
      ist_send= istack_send(npe_send-1)
      ist_recv= istack_recv(npe_recv-1)
      num  =   istack_send(npe_send  ) - istack_send(npe_send-1) 
!$omp parallel do
      do i = 1, num
        iWR(ist_recv+i) = iWS(ist_send+i)
      end do
!$omp end parallel do
!
      end subroutine calypso_send_recv_intcore
!
! ----------------------------------------------------------------------
!
      subroutine calypso_send_recv_fin(npe_send, isend_self)
!
      use calypso_mpi
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
!
      integer(kind = kint) :: ncomm_send
!
!
      ncomm_send = npe_send - isend_self
      call MPI_WAITALL (ncomm_send, req1, sta1, ierr_MPI)
!
      end subroutine calypso_send_recv_fin
!
! ----------------------------------------------------------------------
!
      subroutine calypso_send_recv_check                                &
     &         (NB, npe_send, isend_self, istack_send,                  &
     &              npe_recv, irecv_self, istack_recv)
!
      use calypso_mpi
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
      ncomm_send = npe_send - isend_self
      ncomm_recv = npe_recv - irecv_self
!
      do neib = 1, ncomm_send
        ist = NB * istack_send(neib-1) + 1
        num = NB * (istack_send(neib  ) - istack_send(neib-1))
        if(ist .lt. 0) write(*,*) 'wrong istack_send(0)', my_rank
        if(ist .gt. size(WS)) write(*,*) 'wrong istack_send(neib)',     &
     &       my_rank, neib, ist, size(WS)
        if((ist+num-1) .le. 0) write(*,*)                               &
     &       'negative num_send(0)', my_rank, neib, ist, num, size(WS)
        if((ist+num-1) .gt. size(WS)) write(*,*)                        &
     &       'large num_send(neib)', my_rank, neib, ist, num, size(WS)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib = ncomm_recv, 1, -1
          ist= NB * istack_recv(neib-1) + 1
          num  = NB * (istack_recv(neib  ) - istack_recv(neib-1))
          if(ist .lt. 0) write(*,*) 'wrong istack_recv(0)', my_rank
          if(ist .gt. size(WR)) write(*,*) 'wrong istack_recv(neib)',   &
     &       my_rank, neib, ist, size(WR)
          if((ist+num-1) .le. 0) write(*,*)                             &
     &       'negative num_recv(0)', my_rank, neib, ist, num, size(WR)
          if((ist+num-1) .gt. size(WR)) write(*,*)                      &
     &       'large num_recv(neib)' ,my_rank, neib, ist, num, size(WR)
        end do
      end if
!
      if (isend_self .eq. 0) return
      ist_send= NB * istack_send(npe_send-1)
      ist_recv= NB * istack_recv(npe_recv-1)
      num = NB * (istack_send(npe_send  ) - istack_send(npe_send-1))
        if(ist_send .lt. 0) write(*,*) 'wrong istack_send(0)', my_rank
        if(ist_send .gt. size(WS)) write(*,*)                           &
     &      'wrong istack_send(npe_send)',                              &
     &       my_rank, npe_send, ist_send, size(WS)
        if((ist_send+num-1) .le. 0) write(*,*) 'negative num_send(0)',  &
     &       my_rank, npe_send, ist_send, num, size(WS)
        if((ist_send+num-1) .gt. size(WS)) write(*,*)                   &
     &      'large num_send(npe_send)',                                 &
     &       my_rank, npe_send, ist_send, num, size(WS)
!
        if(ist_recv .lt. 0) write(*,*) 'wrong istack_recv(0)', my_rank
        if(ist_recv .gt. size(WR)) write(*,*)                           &
     &      'wrong istack_recv(npe_recv)',                              &
     &       my_rank, npe_recv, ist_recv, size(WR)
        if((ist_recv+num-1) .le. 0) write(*,*) 'negative num_send(0)',  &
     &       my_rank, npe_recv, ist_recv, num, size(WR)
        if((ist_recv+num-1) .gt. size(WR)) write(*,*)                   &
     &       'large num_send(npe_recv)',                                &
     &      my_rank, npe_recv, ist_recv, num, size(WR)
!
      end subroutine calypso_send_recv_check
!
! ----------------------------------------------------------------------
!
      end module calypso_SR_core
