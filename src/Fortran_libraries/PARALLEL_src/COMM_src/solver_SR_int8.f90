!>@file   solver_SR_int8.f90
!!@brief  module solver_SR_int8
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!@n     Modified in Aug., 2007
!!@n     Modified in Sep., 2013
!!@n     Modified in July, 2020
!
!>@brief  Data communication for 8-byte integer
!!
!!@verbatim
!!      subroutine calypso_send_recv_i8core                             &
!!     &         (npe_send, isend_self, id_pe_send, istack_send,        &
!!     &          npe_recv, irecv_self, id_pe_recv, istack_recv,        &
!!     &          SR_sig, SR_il)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!
!!      subroutine  solver_send_recv_i8                                 &
!!     &          (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,     &
!!     &           STACK_EXPORT, NOD_EXPORT, SR_sig, SR_il, iX8)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!@endverbatim
!!
!!@n @param  NB           Number of components
!!@n @param  NTOT_SEND    Total number of data points for export
!!@n @param  NTOT_RECV    Total number of data points for import
!!@n @param  NPE_SEND      Number of processses to receive
!!@n @param  NPE_RECV      Number of processses to send
!!
!!@n @param  ITEM_IMPORT  import table
!!@n @param  REV_IMPORT   reversed import table
!
      module solver_SR_int8
!
      use m_precision
      use calypso_mpi
      use t_solver_SR
      use t_solver_SR_int8
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine calypso_send_recv_i8core                               &
     &         (npe_send, isend_self, id_pe_send, istack_send,          &
     &          npe_recv, irecv_self, id_pe_recv, istack_recv,          &
     &          SR_sig, SR_il)
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
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer(kind = kint) :: ist
      integer :: num, i
      integer :: ncomm_send, ncomm_recv, neib
      integer(kind = kint) :: ist_send, ist_recv
!
!
      ncomm_send = int(npe_send - isend_self)
      ncomm_recv = int(npe_recv - irecv_self)
!
      do neib = 1, ncomm_send
        ist= istack_send(neib-1) + 1
        num  = int(istack_send(neib  ) - istack_send(neib-1))
        call MPI_ISEND(SR_il%i8WS(ist), num, CALYPSO_GLOBAL_INT,        &
     &                 int(id_pe_send(neib)), 0, CALYPSO_COMM,          &
     &                 SR_sig%req1(neib), ierr_MPI)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib = ncomm_recv, 1, -1
          ist= istack_recv(neib-1) + 1
          num  = int(istack_recv(neib  ) - istack_recv(neib-1))
          call MPI_IRECV(SR_il%i8WR(ist), num, CALYPSO_GLOBAL_INT,      &
     &                   int(id_pe_recv(neib)),0, CALYPSO_COMM,         &
     &                   SR_sig%req2(neib), ierr_MPI)
        end do
      end if
!
      if(ncomm_recv .gt. 0) then
        call MPI_WAITALL                                                &
     &    (ncomm_recv, SR_sig%req2, SR_sig%sta2, ierr_MPI)
      end if
!
      if (isend_self .eq. 0) return
!
      ist_send= istack_send(npe_send-1)
      ist_recv= istack_recv(npe_recv-1)
      num  =  int(istack_send(npe_send  ) - istack_send(npe_send-1))
!$omp parallel do
      do i = 1, num
        SR_il%i8WR(ist_recv+i) = SR_il%i8WS(ist_send+i)
      end do
!$omp end parallel do
!
      end subroutine calypso_send_recv_i8core
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine  solver_send_recv_i8                                   &
     &          (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,       &
     &           STACK_EXPORT, NOD_EXPORT, SR_sig, SR_il, iX8)
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
!
!>       communicated result vector
      integer (kind=kint_gl), dimension(NP)  , intent(inout):: iX8
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte integer
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      integer (kind = kint) :: neib, istart, iend, k
      integer :: inum
!
!
      call resize_i8work_SR(NEIBPETOT, NEIBPETOT,                       &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT),             &
     &    SR_sig, SR_il)
!
!C-- SEND
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1)
        iend  = STACK_EXPORT(neib  )
        inum  = int(iend - istart)

!$omp parallel do
        do k= istart+1, iend
          SR_il%i8WS(k)= iX8(NOD_EXPORT(k))
        end do
!$omp end parallel do
!
        call MPI_ISEND(SR_il%i8WS(istart+1), inum, CALYPSO_GLOBAL_INT,  &
     &                 int(NEIBPE(neib)), 0, CALYPSO_COMM,              &
     &                 SR_sig%req1(neib), ierr_MPI)
      end do
!C
!C-- RECEIVE
      do neib = 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        inum  = int(STACK_IMPORT(neib  ) - istart)
        call MPI_IRECV(SR_il%i8WR(istart+1), inum, CALYPSO_GLOBAL_INT,  &
     &                 int(NEIBPE(neib)), 0, CALYPSO_COMM,              &
     &                 SR_sig%req2(neib), ierr_MPI)
      end do

      call MPI_WAITALL                                                  &
     &  (int(NEIBPETOT), SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)

!$omp parallel private(neib,istart,inum)
      do neib = 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        iend  = STACK_IMPORT(neib  )
!$omp do
        do k = istart+1, iend
          iX8(NOD_IMPORT(k))= SR_il%i8WR(k)
        end do
!$omp end do nowait
      end do
!$omp end parallel

      call MPI_WAITALL                                                  &
     &   (int(NEIBPETOT), SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)

      end subroutine solver_send_recv_i8
!
!  ---------------------------------------------------------------------
!
      end module solver_SR_int8
