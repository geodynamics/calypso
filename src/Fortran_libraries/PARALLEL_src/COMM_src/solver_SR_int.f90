!>@file   solver_SR_int.f90
!!@brief  module solver_SR_int
!!
!!@author coded by K.Nakajima (RIST)
!!@date coded by K.Nakajima (RIST) in jul. 1999 (ver 1.0)
!!@n    modified by H. Matsui (U. of Chicago) in july 2007 (ver 1.1)
!!@n    modified by H. Matsui (UC Davis) in june 2015 (ver 1.2)
!!@n    modified by H. Matsui (UC Davis) in Apr. 2021 (ver 1.3)
!
!>@brief  MPI SEND and RECEIVE routine for integer field
!!        in overlapped partitioning
!!
!!@verbatim
!!      subroutine calypso_send_recv_intcore                            &
!!     &         (npe_send, id_pe_send, istack_send, iWsend, iflag_self,&
!!     &          npe_recv, id_pe_recv, istack_recv, iWrecv, SR_sig)
!!      subroutine calypso_send_recv_num(npe_send, irank_send, num_send,&
!!     &                               npe_recv, irank_recv, iflag_self,&
!!     &                               num_recv, SR_sig)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!
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
     &         (npe_send, id_pe_send, istack_send, iWsend, iflag_self,  &
     &          npe_recv, id_pe_recv, istack_recv, iWrecv, SR_sig)
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: iWsend(istack_send(npe_send))
!
      integer(kind = kint), intent(in) :: npe_recv, iflag_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
!
      integer(kind = kint), intent(inout)                               &
     &                                 :: iWrecv(istack_recv(npe_recv))
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer (kind = kint) :: ist
      integer :: num, i
      integer :: ncomm_send, ncomm_recv, neib
      integer (kind = kint) :: ist_send, ist_recv
!
!
      ncomm_send = int(npe_send - iflag_self)
      ncomm_recv = int(npe_recv - iflag_self)
!
      do neib = 1, ncomm_send
        ist = istack_send(neib-1)
        num = int(istack_send(neib  ) - istack_send(neib-1))
        call MPI_ISEND(iWsend(ist+1), num, CALYPSO_INTEGER,             &
     &                 int(id_pe_send(neib)), 0, CALYPSO_COMM,          &
     &                 SR_sig%req1(neib), ierr_MPI)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib = ncomm_recv, 1, -1
          ist = istack_recv(neib-1)
          num = int(istack_recv(neib  ) - istack_recv(neib-1))
          call MPI_IRECV(iWrecv(ist+1), num, CALYPSO_INTEGER,           &
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
      if(iflag_self .eq. 0) return
!
      ist_send= istack_send(npe_send-1)
      ist_recv= istack_recv(npe_recv-1)
      num  =   int(istack_send(npe_send  ) - istack_send(npe_send-1))
!$omp parallel do
      do i = 1, num
        iWrecv(ist_recv+i) = iWsend(ist_send+i)
      end do
!$omp end parallel do
!
      end subroutine calypso_send_recv_intcore
!
! ----------------------------------------------------------------------
!
      subroutine calypso_send_recv_num(npe_send, irank_send, num_send,  &
     &                               npe_recv, irank_recv, iflag_self,  &
     &                               num_recv, SR_sig)
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: npe_recv, iflag_self
      integer(kind = kint), intent(in) :: irank_send(npe_send)
      integer(kind = kint), intent(in) :: irank_recv(npe_recv)
!
      integer(kind = kint), intent(in) :: num_send(npe_send)
!
      integer(kind = kint), intent(inout) :: num_recv(npe_recv)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer :: ncomm_send, ncomm_recv, ip
!
!
      ncomm_send = int(npe_send - iflag_self)
      ncomm_recv = int(npe_recv - iflag_self)
!
      do ip = 1, ncomm_send
        call MPI_ISEND(num_send(ip), 1, CALYPSO_INTEGER,                &
     &                 int(irank_send(ip)), 0, CALYPSO_COMM,            &
     &                 SR_sig%req1(ip), ierr_MPI)
      end do
!
      do ip = 1, ncomm_recv
        call MPI_IRECV (num_recv(ip), 1, CALYPSO_INTEGER,               &
     &                 int(irank_recv(ip)), 0, CALYPSO_COMM,            &
     &                 SR_sig%req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL                                                  &
     &   (ncomm_recv, SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (ncomm_send, SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
!
      if(iflag_self .gt. 0) num_recv(npe_recv) = num_send(npe_send)
!
      end subroutine  calypso_send_recv_num
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
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
!>       communicated result vector
      integer (kind=kint), dimension(NP)  , intent(inout):: iX
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for integer
      type(send_recv_int_buffer), intent(inout) :: SR_i
!C
!
      call resize_iwork_SR_t(NEIBPETOT, NEIBPETOT,                      &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT),             &
     &    SR_sig , SR_i)
!
      call set_to_send_buf_int(NP, STACK_EXPORT(NEIBPETOT),             &
     &                        NOD_EXPORT, iX(1), SR_i%iWS(1))
!
      call calypso_send_recv_intcore                                    &
     &   (NEIBPETOT, NEIBPE, STACK_EXPORT, SR_i%iWS(1), izero,          &
     &    NEIBPETOT, NEIBPE, STACK_IMPORT, SR_i%iWR(1), SR_sig)

      call set_from_recv_buf_int(NP, STACK_IMPORT(NEIBPETOT),           &
     &                           NOD_IMPORT, SR_i%iWR(1), iX(1))
      call calypso_send_recv_fin(NEIBPETOT, izero, SR_sig)

      end subroutine solver_send_recv_i
!
!  ---------------------------------------------------------------------
!
      end module solver_SR_int
