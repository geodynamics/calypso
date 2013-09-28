!>@file   spherical_SR_rev_N.f90
!!@brief  module spherical_SR_rev_N
!!
!!@author H. Matsui
!!@date Programmed in March, 2013
!
!>@brief  Arbitrary components data communication
!!@n      for spherical harmonics transform using reverse import table
!!
!!@verbatim
!!      subroutine sph_send_recv_by_rev_N(NB, nnod_org, nnod_new,       &
!!     &                           npe_send, isend_self, nnod_send,     &
!!     &                           id_pe_send, istack_send, inod_export,&
!!     &                           npe_recv, irecv_self, nnod_recv,     &
!!     &                           id_pe_recv, istack_recv, irev_import,&
!!     &                           X_org, X_new)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n @param  nnod_new    Number of components for destination
!!
!!@n @param  npe_send    Number of processses to send
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  nnod_send   Number of data points to send
!!@n @param  id_pe_send(npe_send)      Process ID to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_export(nnod_send)
!!                    local node ID to copy in send buffer
!!
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  irecv_self  Integer flag to copy within own process
!!@n @param  nnod_recv   Number of data points to receive
!!@n @param  id_pe_recv(npe_send)      Process ID to receive
!!@n @param  istack_recv(0:npe_send)
!!                    End points of receive buffer for each process
!!@n @param  irev_import(nnod_recv)
!!                    import buffer ID for each data point
!!
!!@n @param  X_org(NB*nnod_org)   Send data
!!@n @param  X_new(NB*nnod_new)   Received data
!
      module spherical_SR_rev_N
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
      subroutine sph_send_recv_by_rev_N(NB, nnod_org, nnod_new,         &
     &                           npe_send, isend_self, nnod_send,       &
     &                           id_pe_send, istack_send, inod_export,  &
     &                           npe_recv, irecv_self, nnod_recv,       &
     &                           id_pe_recv, istack_recv, irev_import,  &
     &                           X_org, X_new)
!
      use calypso_mpi
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: NB
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
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
      real (kind=kreal) :: elaps3(3)
!
      real(kind = kreal) :: s1time, s2time
      integer (kind = kint) :: neib, ist, inum, ierr
      integer (kind = kint) :: i, k, nd, jj, kk
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
      call resize_work_sph_SR(NB, npe_send, npe_recv,                   &
     &    nnod_send, nnod_recv)
!
      ncomm_send = npe_send - isend_self
      ncomm_recv = npe_recv - irecv_self
!
!C-- SEND
!
      s1time = MPI_WTIME()
!$omp parallel do private(k,jj,kk,nd)
      do kk = 1, NB*istack_send(npe_send)
        nd = 1 + mod(kk-1,NB)
        k =  1 + (kk-nd) / NB
        jj = NB*(inod_export(k)-1) + nd
        WS(kk) = X_org(jj)
      end do
!$omp end parallel do
      elaps3(1) = elaps3(1) + MPI_WTIME() - s1time
!C
!C
      s1time = MPI_WTIME()
      do neib = 1, ncomm_send
        ist= NB * istack_send(neib-1) + 1
        inum  = NB * (istack_send(neib  ) - istack_send(neib-1))
        call MPI_ISEND(WS(ist), inum, MPI_DOUBLE_PRECISION,             &
     &      id_pe_send(neib), 0, CALYPSO_COMM, req1(neib), ierr)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib = 1, ncomm_recv
          ist= NB * istack_recv(neib-1) + 1
          inum  = NB * (istack_recv(neib  ) - istack_recv(neib-1))
          call MPI_IRECV(WR(ist), inum, MPI_DOUBLE_PRECISION,           &
     &        id_pe_recv(neib), 0, CALYPSO_COMM, req2(neib), ierr)
        end do
!
        call MPI_WAITALL (ncomm_recv, req2, sta2, ierr)
      end if
!
      if (isend_self .eq. 1) then
        ist_send= NB * istack_send(npe_send-1)
        ist_recv= NB * istack_recv(npe_recv-1)
        inum = NB * (istack_send(npe_send  ) - istack_send(npe_send-1))
!$omp parallel do
        do i = 1, inum
          WR(ist_recv+i) = WS(ist_send+i)
        end do
!$omp end parallel do
      end if
!
      ist = NB * istack_recv(npe_recv)
!$omp parallel do
      do nd = ist+1, ist+NB
        WR(nd) = 0.0d0
      end do
!$omp end parallel do
!
!
      s2time = MPI_WTIME()
!$omp parallel do private(k,jj,kk,nd)
      do kk = 1, NB*nnod_new
        nd = 1 + mod(kk-1,NB)
        k =  1 + (kk-nd) / NB
        jj = NB*(irev_import(k)-1) + nd
        X_new(kk) = WR(jj)
      end do
!$omp end parallel do
      elaps3(2) = elaps3(2) + MPI_WTIME() - s2time
!
      if(ncomm_send .gt. 0) then
        call MPI_WAITALL (ncomm_send, req1, sta1, ierr)
      end if
      elaps3(3) = elaps3(3) + MPI_WTIME() - s1time
!
      end subroutine sph_send_recv_by_rev_N
!
! ----------------------------------------------------------------------
!
      end module spherical_SR_rev_N
