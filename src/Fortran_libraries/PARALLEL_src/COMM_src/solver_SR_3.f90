!>@file   solver_SR_3.f90
!!@brief  module solver_SR_3
!!
!!@author coded by K.Nakajima (RIST)
!!@date coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!!@n    modified by H. Matsui (U. of Chicago) on july 2007 (ver 1.1)
!
!>@brief  MPI SEND and RECEIVE routine for vector fields
!!        in overlapped partitioning
!!
!!@verbatim
!!      subroutine  SOLVER_SEND_RECV_3(NP, NEIBPETOT, NEIBPE,           &
!!     &                               STACK_IMPORT, NOD_IMPORT,        &
!!     &                               STACK_EXPORT, NOD_EXPORT,        &
!!     &                               SR_sig, SR_r, X)
!!      subroutine  solver_send_recv_3x3(NP, NEIBPETOT, NEIBPE,         &
!!     &                                 STACK_IMPORT, NOD_IMPORT,      &
!!     &                                 STACK_EXPORT, NOD_EXPORT,      &
!!     &                                 SR_sig, SR_r, X1, X2, X3)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
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
!!@n @param  X(3*NP)   vector field data
!!@n @param  X1(3*NP)  1st vector field data
!!@n @param  X2(3*NP)  2nd vector field data
!!@n @param  X3(3*NP)  3rd vector field data
!
      module solver_SR_3
!
      use m_precision
      use m_constants
      use t_solver_SR
      use calypso_mpi
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!C
!C*** SOLVER_SEND_RECV_3
      subroutine  SOLVER_SEND_RECV_3(NP, NEIBPETOT, NEIBPE,             &
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
      real   (kind=kreal), dimension(3*NP), intent(inout):: X
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_work_SR(ithree, NEIBPETOT, NEIBPETOT,                 &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT),             &
     &    SR_sig, SR_r)
!C
!C-- SEND
      call set_to_send_buf_3(NP, STACK_EXPORT(NEIBPETOT), NOD_EXPORT,   &
     &                       X(1), SR_r%WS(1))
!
!C-- COMM
      call calypso_send_recv_core                                       &
     &   (ithree, NEIBPETOT, NEIBPE, STACK_EXPORT, SR_r%WS(1),          &
     &            NEIBPETOT, NEIBPE, STACK_IMPORT, izero,               &
     &            SR_r%WR(1), SR_sig)
!C
!C-- RECEIVE
      call set_from_recv_buf_3(NP, STACK_IMPORT(NEIBPETOT), NOD_IMPORT, &
     &                         SR_r%WR(1), X(1))

      call calypso_send_recv_fin(NEIBPETOT, izero, SR_sig)

      end subroutine solver_send_recv_3
!
!  ---------------------------------------------------------------------
!
      subroutine  solver_send_recv_3x3(NP, NEIBPETOT, NEIBPE,           &
     &                                 STACK_IMPORT, NOD_IMPORT,        &
     &                                 STACK_EXPORT, NOD_EXPORT,        &
     &                                 SR_sig, SR_r, X1, X2, X3)
!
      use calypso_SR_core
      use set_to_send_buf_tri
      use set_from_recv_buff_tri
!
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
!
      real   (kind=kreal), dimension(3*NP), intent(inout):: X1
      real   (kind=kreal), dimension(3*NP), intent(inout):: X2
      real   (kind=kreal), dimension(3*NP), intent(inout):: X3
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_work_SR(inine, NEIBPETOT, NEIBPETOT,                  &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT),             &
     &    SR_sig, SR_r)
!C
!C-- SEND
      call set_to_send_buf_3x3(NP, STACK_EXPORT(NEIBPETOT), NOD_EXPORT, &
     &                         X1(1), X1(2), X1(3), SR_r%WS(1))
!
!C-- COMM
      call calypso_send_recv_core                                       &
     &   (inine, NEIBPETOT, NEIBPE, STACK_EXPORT, SR_r%WS(1),           &
     &            NEIBPETOT, NEIBPE, STACK_IMPORT, izero,               &
     &            SR_r%WR(1), SR_sig)
!C
!C-- RECEIVE
      call set_from_recv_buf_3x3                                        &
     &   (NP, STACK_IMPORT(NEIBPETOT), NOD_IMPORT,                      &
     &    SR_r%WR(1), X1(1), X2(1), X3(1))
!
      call calypso_send_recv_fin(NEIBPETOT, izero, SR_sig)
!
      end subroutine solver_send_recv_3x3
!
!  ---------------------------------------------------------------------
!
      end module solver_SR_3
