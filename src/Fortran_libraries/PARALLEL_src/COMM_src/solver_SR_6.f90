!>@file   solver_SR_6.f90
!!@brief  module solver_SR_6
!!
!!@author coded by K.Nakajima (RIST)
!!@date coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!!@n    modified by H. Matsui (U. of Chicago) on july 2007 (ver 1.1)
!
!>@brief  MPI SEND and RECEIVE routine for six components fields
!!        in overlapped partitioning
!!
!!@verbatim
!!      subroutine  SOLVER_SEND_RECV_6(NP, NEIBPETOT, NEIBPE,           &
!!     &                               STACK_IMPORT, NOD_IMPORT,        &
!!     &                               STACK_EXPORT, NOD_EXPORT,        &
!!     &                               SR_sig, SR_r, X)
!!@endverbatim
!!
!!@n @param  NP     Number of data points
!!@n
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
!!@n @param  X(6*NP)   field data with 6 components
!
      module solver_SR_6
!
      use m_precision
      use m_constants
      use t_solver_SR
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!C
      subroutine  SOLVER_SEND_RECV_6(NP, NEIBPETOT, NEIBPE,             &
     &                               STACK_IMPORT, NOD_IMPORT,          &
     &                               STACK_EXPORT, NOD_EXPORT,          &
     &                               SR_sig, SR_r, X)
!
      use calypso_SR_core
      use set_to_send_buffer
      use set_from_recv_buffer
!
!>       number of nodes
      integer(kind=kint )                , intent(in)   ::  NP
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
      real   (kind=kreal), dimension(6*NP), intent(inout):: X
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_work_SR(isix, NEIBPETOT, NEIBPETOT,                   &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT),             &
     &    SR_sig, SR_r)
!C
!C-- SEND
      call set_to_send_buf_6(NP, STACK_EXPORT(NEIBPETOT), NOD_EXPORT,   &
     &                       X(1), SR_r%WS(1))
!
!C-- COMM
      call calypso_send_recv_core                                       &
     &   (isix, NEIBPETOT, NEIBPE, STACK_EXPORT, SR_r%WS(1),            &
     &            NEIBPETOT, NEIBPE, STACK_IMPORT, izero,               &
     &            SR_r%WR(1), SR_sig)
!C-- RECEIVE
      call set_from_recv_buf_6(NP, STACK_IMPORT(NEIBPETOT), NOD_IMPORT, &
     &                         SR_r%WR(1), X(1))

      call calypso_send_recv_fin(NEIBPETOT, izero, SR_sig)

      end subroutine SOLVER_SEND_RECV_6
!
! ----------------------------------------------------------------------
!
      end module solver_SR_6
