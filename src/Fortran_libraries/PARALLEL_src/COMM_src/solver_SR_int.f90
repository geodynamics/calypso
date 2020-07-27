!>@file   solver_SR_int.f90
!!@brief  module solver_SR_int
!!
!!@author coded by K.Nakajima (RIST)
!!@date coded by K.Nakajima (RIST) in jul. 1999 (ver 1.0)
!!@n    modified by H. Matsui (U. of Chicago) in july 2007 (ver 1.1)
!!@n    modified by H. Matsui (UC Davis) in june 2015 (ver 1.2)
!
!>@brief  MPI SEND and RECEIVE routine for integer field
!!        in overlapped partitioning
!!
!!@verbatim
!!      subroutine calypso_send_recv_intcore                            &
!!     &         (npe_send, isend_self, id_pe_send, istack_send,        &
!!     &          npe_recv, irecv_self, id_pe_recv, istack_recv,        &
!!     &          SR_sig, SR_i)
!!      subroutine calypso_AllToAllv_intcore                            &
!!     &         (npe_sr, istack_send, istack_recv,                     &
!!     &          CALYPSO_SUB_COMM, SR_i)
!!      subroutine calypso_AllToAll_intcore                             &
!!     &         (nitem_SR, CALYPSO_SUB_COMM, SR_i)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!
!!      subroutine  solver_send_recv_i                                  &
!!     &          (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,     &
!!     &           STACK_EXPORT, NOD_EXPORT, SR_sig, SR_i, ix)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!
!!      subroutine  solver_send_recv_num                                &
!!     &          (NEIBPETOT, NEIBPE, nSEND, nRECV, SR_sig)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!!
!!@n @param  NP     Number of data points
!!
!!@n @param  NEIBPETOT    Number of processses to communicate
!!@n @param  NEIBPE(NEIBPETOT)      Process ID to communicate
!!@n @param  STACK_IMPORT(0:NEIBPETOT)
!!                    End points of import buffer for each process
!!@n @param  NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
!!                    local node ID to copy in import buffer
!!@n @param  STACK_EXPORT(0:NEIBPETOT)
!!                    End points of export buffer for each process
!!@n @param  NOD_EXPORT(STACK_IMPORT(NEIBPETOT))
!!                    local node ID to copy in export buffer
!!
!!@n @param  ix(N)      integer data with NB components
!!@n @param  ix8(N)     8 byte integer data with NB components
!!
!!@n @param  nSEND(NEIBPETOT)   Number of component to send
!!@n @param  nRECV(NEIBPETOT)   Number of component to recv
!
      module solver_SR_int
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_solver_SR
      use t_solver_SR_int
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine calypso_send_recv_intcore                              &
     &         (npe_send, isend_self, id_pe_send, istack_send,          &
     &          npe_recv, irecv_self, id_pe_recv, istack_recv,          &
     &          SR_sig, SR_i)
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer (kind = kint) :: ist
      integer :: num, i
      integer :: ncomm_send, ncomm_recv, neib
      integer (kind = kint) :: ist_send, ist_recv
!
!
      ncomm_send = int(npe_send - isend_self)
      ncomm_recv = int(npe_recv - irecv_self)
!
      do neib = 1, ncomm_send
        ist= istack_send(neib-1) + 1
        num  = int(istack_send(neib  ) - istack_send(neib-1))
        call MPI_ISEND(SR_i%iWS(ist), num, CALYPSO_INTEGER,             &
     &                 int(id_pe_send(neib)), 0, CALYPSO_COMM,          &
     &                 SR_sig%req1(neib), ierr_MPI)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib = ncomm_recv, 1, -1
          ist= istack_recv(neib-1) + 1
          num  = int(istack_recv(neib  ) - istack_recv(neib-1))
          call MPI_IRECV(SR_i%iWR(ist), num, CALYPSO_INTEGER,           &
     &                   int(id_pe_recv(neib)), 0, CALYPSO_COMM,        &
     &                   SR_sig%req2(neib), ierr_MPI)
        end do
      end if
!
      if(ncomm_recv .gt. 0) then
        call MPI_WAITALL                                                &
     &     (ncomm_recv, SR_sig%req2, SR_sig%sta2, ierr_MPI)
      end if
!
      if (isend_self .eq. 0) return
!
      ist_send= istack_send(npe_send-1)
      ist_recv= istack_recv(npe_recv-1)
      num  =   int(istack_send(npe_send  ) - istack_send(npe_send-1))
!$omp parallel do
      do i = 1, num
        SR_i%iWR(ist_recv+i) = SR_i%iWS(ist_send+i)
      end do
!$omp end parallel do
!
      end subroutine calypso_send_recv_intcore
!
! ----------------------------------------------------------------------
!
      subroutine calypso_AllToAllv_intcore                              &
     &         (npe_sr, istack_send, istack_recv,                       &
     &          CALYPSO_SUB_COMM, SR_i)
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: npe_sr
      integer(kind = kint), intent(in) :: istack_send(0:npe_sr)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_sr)
!
!>      Structure of communication buffer for integer
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint) :: num_send(npe_sr), num_recv(npe_sr)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, npe_sr
        num_send(ip) = (istack_send(ip) - istack_send(ip-1))
        num_recv(ip) = (istack_recv(ip) - istack_recv(ip-1))
      end do
!
      call MPI_AllToAllV                                                &
     &   (SR_i%iWS(1), num_send, istack_send(0), CALYPSO_INTEGER,       &
     &    SR_i%iWR(1), num_recv, istack_recv(0), CALYPSO_INTEGER,       &
     &    CALYPSO_SUB_COMM, ierr_MPI)
!
      end subroutine calypso_AllToAllv_intcore
!
! ----------------------------------------------------------------------
!
      subroutine calypso_AllToAll_intcore                               &
     &         (nitem_SR, CALYPSO_SUB_COMM, SR_i)
!
      use calypso_mpi
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: nitem_SR
!
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      call MPI_AllToAll(SR_i%iWS(1), nitem_SR, CALYPSO_INTEGER,         &
     &    SR_i%iWR(1), nitem_SR, CALYPSO_INTEGER,                       &
     &    CALYPSO_SUB_COMM, ierr_MPI)
!
      end subroutine calypso_AllToAll_intcore
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine  solver_send_recv_i                                    &
     &          (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,       &
     &           STACK_EXPORT, NOD_EXPORT, SR_sig, SR_i, ix)
!
!>       number of nodes
      integer(kind=kint )                , intent(in)   ::  NP
!>       total neighboring pe count
      integer(kind=kint )                , intent(in)   ::  NEIBPETOT
!>       neighboring pe id                        (i-th pe)
      integer(kind=kint ), dimension(NEIBPETOT) :: NEIBPE
!>       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_IMPORT
!>       imported node                            (i-th dof)
      integer(kind=kint ), dimension(STACK_IMPORT(NEIBPETOT))           &
     &        :: NOD_IMPORT
!>       exported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_EXPORT
!>       exported node                            (i-th dof)
      integer(kind=kint ), dimension(STACK_EXPORT(NEIBPETOT))           &
     &        :: NOD_EXPORT
!>       communicated result vector
      integer (kind=kint), dimension(NP)  , intent(inout):: iX
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for integer
      type(send_recv_int_buffer), intent(inout) :: SR_i
!C
!
      integer (kind = kint) :: neib, istart, iend, k
      integer :: inum
!
!
      call resize_iwork_SR_t(NEIBPETOT, NEIBPETOT,                      &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT),             &
     &    SR_sig , SR_i)
!
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1)
        iend =  STACK_EXPORT(neib  ) 
        inum  = int(iend - istart)
!
!$omp parallel do
        do k= istart+1, iend
           SR_i%iWS(k)= iX(NOD_EXPORT(k))
        end do
!$omp end parallel do
!
        call MPI_ISEND(SR_i%iWS(istart+1), inum, CALYPSO_INTEGER,       &
     &                 int(NEIBPE(neib)), 0, CALYPSO_COMM,              &
     &                 SR_sig%req1(neib), ierr_MPI)
      enddo

!C
!C-- RECEIVE
      
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        inum  = int(STACK_IMPORT(neib  ) - istart)
        call MPI_IRECV(SR_i%iWR(istart+1), inum, CALYPSO_INTEGER,       &
     &                  int(NEIBPE(neib)), 0, CALYPSO_COMM,             &
     &                  SR_sig%req2(neib), ierr_MPI)
      enddo

      call MPI_WAITALL                                                  &
     &   (int(NEIBPETOT), SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
   
!$omp parallel private(neib,istart,inum)
      do neib = 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        iend =  STACK_IMPORT(neib  ) 
!$omp do
        do k= istart+1, iend
          iX(NOD_IMPORT(k))= SR_i%iWR(k)
        end do
!$omp end do nowait
      end do
!$omp end parallel

      call MPI_WAITALL                                                  &
     &   (int(NEIBPETOT), SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)

      end subroutine solver_send_recv_i
!
!  ---------------------------------------------------------------------
!
      subroutine  solver_send_recv_num                                  &
     &          (NEIBPETOT, NEIBPE, nSEND, nRECV, SR_sig)
!
!>       total neighboring pe count
      integer(kind=kint )                , intent(in)   ::  NEIBPETOT
!>       neighboring pe id                        (i-th pe)
      integer(kind=kint ), dimension(NEIBPETOT) :: NEIBPE
!>       imported node count for each neighbor pe (i-th pe)
!
!>       Number of componennt to send
      integer (kind=kint), dimension(NEIBPETOT), intent(in):: nSEND
!>       Number of componennt to recv
      integer (kind=kint), dimension(NEIBPETOT), intent(inout):: nRECV
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!C
!
      integer (kind = kint) :: neib
!
!
      call resize_SR_flag(NEIBPETOT, NEIBPETOT, SR_sig)
!
!C-- SEND
      
      do neib= 1, NEIBPETOT
        call MPI_ISEND(nSEND(neib), 1, CALYPSO_INTEGER,                 &
     &                 int(NEIBPE(neib)), 0, CALYPSO_COMM,              &
     &                 SR_sig%req1(neib), ierr_MPI)
      end do
!C
!C-- RECEIVE
      
      do neib= 1, NEIBPETOT
        call MPI_IRECV(nRECV(neib), 1, CALYPSO_INTEGER,                 &
     &                 int(NEIBPE(neib)), 0, CALYPSO_COMM,              &
     &                 SR_sig%req2(neib), ierr_MPI)
      end do
!
      call MPI_WAITALL                                                  &
     &   (int(NEIBPETOT), SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (int(NEIBPETOT), SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
!
      end subroutine solver_send_recv_num
!
!  ---------------------------------------------------------------------
!
      end module solver_SR_int
