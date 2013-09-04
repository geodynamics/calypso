!>@file   spherical_SR_rev_3.f90
!!@brief  module spherical_SR_rev_3
!!
!!@author H. Matsui
!!@date Programmed in March, 2013
!
!>@brief  Three components data communication
!!@n      for spherical harmonics transform using reverse import table
!!
!!@verbatim
!!      subroutine sph_send_recv_by_rev_3(nnod_org, nnod_new,           &
!!     &                           npe_send, isend_self, nnod_send,     &
!!     &                           id_pe_send, istack_send, inod_export,&
!!     &                           npe_recv, irecv_self, nnod_recv,     &
!!     &                           id_pe_recv, istack_recv, irev_import,&
!!     &                           X_org, X_new, SOLVER_COMM)
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
!!@n @param  X_org(3*nnod_org)   Send data
!!@n @param  X_new(3*nnod_new)   Received data
!!@n
!!@n @param  SOLVER_COMM          MPI communicator
!
      module spherical_SR_rev_3
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
      subroutine sph_send_recv_by_rev_3(nnod_org, nnod_new,             &
     &                           npe_send, isend_self, nnod_send,       &
     &                           id_pe_send, istack_send, inod_export,  &
     &                           npe_recv, irecv_self, nnod_recv,       &
     &                           id_pe_recv, istack_recv, irev_import,  &
     &                           X_org, X_new, SOLVER_COMM)
!
      use calypso_mpi
!
      use m_solver_SR
!
      integer, intent(in)   :: SOLVER_COMM
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
      real (kind=kreal), intent(in)::    X_org(ithree*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(ithree*nnod_new)
!
      integer (kind = kint) :: neib, ist, inum, ierr
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
      call resize_work_sph_SR(ithree, npe_send, npe_recv,               &
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
        WS(ithree*k-2)= X_org(ithree*j-2)
        WS(ithree*k-1)= X_org(ithree*j-1)
        WS(ithree*k  )= X_org(ithree*j  )
      end do
!$omp end parallel do
!C
      do neib = 1, ncomm_send
        ist= ithree * istack_send(neib-1) + 1
        inum  = ithree * (istack_send(neib  ) - istack_send(neib-1))
        call MPI_ISEND(WS(ist), inum, MPI_DOUBLE_PRECISION,             &
     &      id_pe_send(neib), 0, SOLVER_COMM, req1(neib), ierr)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib= 1, ncomm_recv
          ist= ithree * istack_recv(neib-1) + 1
          inum  = ithree * (istack_recv(neib  ) - istack_recv(neib-1))
          call MPI_IRECV(WR(ist), inum, MPI_DOUBLE_PRECISION,           &
     &      id_pe_recv(neib), 0, SOLVER_COMM, req2(neib), ierr)
        end do
!
        call MPI_WAITALL (ncomm_recv, req2, sta2, ierr)
      end if
!
      if (isend_self .eq. 1) then
        ist_send= ithree * istack_send(npe_send-1)
        ist_recv= ithree * istack_recv(npe_recv-1)
        inum = ithree*(istack_send(npe_send) - istack_send(npe_send-1))
!$omp parallel do
        do i = 1, inum
          WR(ist_recv+i) = WS(ist_send+i)
        end do
!$omp end parallel do
      end if
!
      ist = ithree*istack_recv(npe_recv)
      WR(ist+1) = 0.0d0
      WR(ist+2) = 0.0d0
      WR(ist+3) = 0.0d0
!
!$omp parallel do private(j,k)
      do k = 1, nnod_new
        j = irev_import(k)
        X_new(ithree*k-2) = WR(ithree*j-2)
        X_new(ithree*k-1) = WR(ithree*j-1)
        X_new(ithree*k  ) = WR(ithree*j  )
      end do
!$omp end parallel do
!
      if(ncomm_send .gt. 0) then
        call MPI_WAITALL (ncomm_send, req1, sta1, ierr)
      end if
!
      end subroutine sph_send_recv_by_rev_3
!
! ----------------------------------------------------------------------
!
      end module spherical_SR_rev_3
