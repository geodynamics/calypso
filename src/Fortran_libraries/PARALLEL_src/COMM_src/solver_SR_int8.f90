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
!!     &         (npe_send, id_pe_send, istack_send, i8Wsend,           &
!!     &          iflag_self, npe_recv, id_pe_recv, istack_recv,        &
!!     &          i8Wrecv, SR_sig)
!!        type(send_recv_status), intent(inout) :: SR_sig
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
     &         (npe_send, id_pe_send, istack_send, i8Wsend,             &
     &          iflag_self, npe_recv, id_pe_recv, istack_recv,          &
     &          i8Wrecv, SR_sig)
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint_gl), intent(in)                               &
     &                        :: i8Wsend(istack_send(npe_send))
!
      integer(kind = kint), intent(in) :: npe_recv, iflag_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
      integer(kind = kint_gl), intent(inout)                            &
     &                        :: i8Wrecv(istack_recv(npe_recv))
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: ist
      integer :: num, i
      integer :: ncomm_send, ncomm_recv, neib
      integer(kind = kint) :: ist_send, ist_recv
!
!
      ncomm_send = int(npe_send - iflag_self)
      ncomm_recv = int(npe_recv - iflag_self)
!
      do neib = 1, ncomm_send
        ist= istack_send(neib-1) + 1
        num  = int(istack_send(neib  ) - istack_send(neib-1))
        call MPI_ISEND(i8Wsend(ist), num, CALYPSO_GLOBAL_INT,           &
     &                 int(id_pe_send(neib)), 0, CALYPSO_COMM,          &
     &                 SR_sig%req1(neib), ierr_MPI)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib = ncomm_recv, 1, -1
          ist= istack_recv(neib-1) + 1
          num  = int(istack_recv(neib  ) - istack_recv(neib-1))
          call MPI_IRECV(i8Wrecv(ist), num, CALYPSO_GLOBAL_INT,         &
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
      if (iflag_self .eq. 0) return
!
      ist_send= istack_send(npe_send-1)
      ist_recv= istack_recv(npe_recv-1)
      num  =  int(istack_send(npe_send  ) - istack_send(npe_send-1))
!$omp parallel do
      do i = 1, num
        i8Wrecv(ist_recv+i) = i8Wsend(ist_send+i)
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
      use calypso_SR_core
      use set_to_send_buffer
      use set_from_recv_buffer
!
!>       number of nodes and components
      integer(kind=kint ), intent(in)   ::  NP
!>       total neighboring pe count
      integer(kind=kint ), intent(in)   ::  NEIBPETOT
!>       neighboring pe id                        (i-th pe)
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
!>       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
!>       imported node                            (i-th dof)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
!>       exported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
!>       exported node                            (i-th dof)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
!>       communicated result vector
      integer (kind=kint_gl), dimension(NP)  , intent(inout):: iX8
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte integer
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
      call resize_i8work_SR(NEIBPETOT, NEIBPETOT,                       &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT),             &
     &    SR_sig, SR_il)
!
      call set_to_send_buf_i8(NP, STACK_EXPORT(NEIBPETOT),              &
     &                        NOD_EXPORT, iX8(1), SR_il%i8WS(1))
!
      call calypso_send_recv_i8core                                     &
     &   (NEIBPETOT, NEIBPE, STACK_EXPORT, SR_il%i8WS(1), izero,        &
     &    NEIBPETOT, NEIBPE, STACK_IMPORT, SR_il%i8WR(1), SR_sig)

      call set_from_recv_buf_i8(NP, STACK_IMPORT(NEIBPETOT),            &
     &                          NOD_IMPORT, SR_il%i8WR(1), iX8(1))
      call calypso_send_recv_fin(NEIBPETOT, izero, SR_sig)

      end subroutine solver_send_recv_i8
!
!  ---------------------------------------------------------------------
!
      end module solver_SR_int8
