!>@file   spherical_SR_rev_6.f90
!!@brief  module spherical_SR_rev_6
!!
!!@author H. Matsui
!!@date Programmed in March, 2013
!
!>@brief  Six components data communication
!!@n      for spherical harmonics transform using reverse import table
!!
!!@verbatim
!!      subroutine sph_send_recv_by_rev_6(nnod_org, nnod_new,           &
!!     &                           npe_send, isend_self, nnod_send,     &
!!     &                           id_pe_send, istack_send, inod_export,&
!!     &                           npe_recv, irecv_self, nnod_recv,     &
!!     &                           id_pe_recv, istack_recv, irev_import,&
!!     &                           X_org, X_new)
!!@endverbatim
!!
!!@n @param  nnod_org    Number of data points for origin
!!@n @param  nnod_new    Number of components for destination
!!@n
!!@n @param  npe_send    Number of processses to send
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  nnod_send   Number of data points to send
!!@n @param  id_pe_send(npe_send)      Process ID to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_export(nnod_send)
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  irecv_self  Integer flag to copy within own process
!!@n @param  nnod_recv   Number of data points to receive
!!@n @param  id_pe_recv(npe_send)      Process ID to receive
!!@n @param  istack_recv(0:npe_send)
!!                    End points of receive buffer for each process
!!@n @param  irev_import(nnod_recv)
!!                    import buffer ID for each data point
!!@n
!!@n @param  X_org(6*nnod_org)   Send data
!!@n @param  X_new(6*nnod_new)   Received data
!
      module spherical_SR_rev_6
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_send_recv_by_rev_6(nnod_org, nnod_new,             &
     &                           npe_send, isend_self, nnod_send,       &
     &                           id_pe_send, istack_send, inod_export,  &
     &                           npe_recv, irecv_self, nnod_recv,       &
     &                           id_pe_recv, istack_recv, irev_import,  &
     &                           X_org, X_new)
!
      use calypso_mpi
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(isix*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(isix*nnod_new)
!
      integer (kind = kint) :: neib, ist, inum
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
      call resize_work_sph_SR(isix, npe_send, npe_recv,                 &
     &    nnod_send, nnod_recv)
!
      ncomm_send = npe_send - isend_self
      ncomm_recv = npe_recv - irecv_self
!
!C-- SEND
!
!$omp parallel do private(k,j)
      do k = 1, istack_send(npe_send)
        j = inod_export(k)
        WS(isix*k-5)= X_org(isix*j-5)
        WS(isix*k-4)= X_org(isix*j-4)
        WS(isix*k-3)= X_org(isix*j-3)
        WS(isix*k-2)= X_org(isix*j-2)
        WS(isix*k-1)= X_org(isix*j-1)
        WS(isix*k  )= X_org(isix*j  )
      end do
!$omp end parallel do
!C
      do neib = 1, ncomm_send
        ist= isix * istack_send(neib-1) + 1
        inum  = isix * (istack_send(neib  ) - istack_send(neib-1))
        call MPI_ISEND(WS(ist), inum, CALYPSO_REAL,                     &
     &      id_pe_send(neib), 0, CALYPSO_COMM, req1(neib), ierr_MPI)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib= 1, ncomm_recv
          ist= isix * istack_recv(neib-1) + 1
          inum  = isix * (istack_recv(neib  ) - istack_recv(neib-1))
          call MPI_IRECV(WR(ist), inum, CALYPSO_REAL,                   &
     &      id_pe_recv(neib), 0, CALYPSO_COMM, req2(neib), ierr_MPI)
        end do
!
        call MPI_WAITALL (ncomm_recv, req2, sta2, ierr_MPI)
      end if
!
      if (isend_self .eq. 1) then
        ist_send= isix * istack_send(npe_send-1)
        ist_recv= isix * istack_recv(npe_recv-1)
        inum = isix*(istack_send(npe_send) - istack_send(npe_send-1))
!$omp parallel do
        do i = 1, inum
          WR(ist_recv+i) = WS(ist_send+i)
        end do
!$omp end parallel do
      end if
!
      ist = isix*istack_recv(npe_recv)
      WR(ist+1) = 0.0d0
      WR(ist+2) = 0.0d0
      WR(ist+3) = 0.0d0
      WR(ist+4) = 0.0d0
      WR(ist+5) = 0.0d0
      WR(ist+6) = 0.0d0
!
!$omp parallel do private(j,k)
      do k = 1, nnod_new
        j = irev_import(k)
        X_new(isix*k-5) = WR(isix*j-5)
        X_new(isix*k-4) = WR(isix*j-4)
        X_new(isix*k-3) = WR(isix*j-3)
        X_new(isix*k-2) = WR(isix*j-2)
        X_new(isix*k-1) = WR(isix*j-1)
        X_new(isix*k  ) = WR(isix*j  )
      end do
!$omp end parallel do
!
      if(ncomm_send .gt. 0) then
        call MPI_WAITALL (ncomm_send, req1, sta1, ierr_MPI)
      end if
!
      end subroutine sph_send_recv_by_rev_6
!
! ----------------------------------------------------------------------
!
      end module spherical_SR_rev_6
