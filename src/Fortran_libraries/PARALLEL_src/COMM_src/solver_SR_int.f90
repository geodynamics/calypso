!>@file   solver_SR_int.f90
!!@brief  module solver_SR_int
!!
!!@author coded by K.Nakajima (RIST)
!!@date coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!!@n    modified by H. Matsui (U. of Chicago) on july 2007 (ver 1.1)
!
!>@brief  MPI SEND and RECEIVE routine for integer field
!!        in overlapped partitioning
!!
!!@verbatim
!!      subroutine  solver_send_recv_i                                  &
!!     &          (N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,      &
!!     &           STACK_EXPORT, NOD_EXPORT, ix)
!!@endverbatim
!!
!!@n @param  N     Number of data points
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
!!@n @param  ix(N)     integer data with NB components
!
      module solver_SR_int
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine  solver_send_recv_i                                    &
     &          (N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,        &
     &           STACK_EXPORT, NOD_EXPORT, ix)
!
      use calypso_mpi
      use m_solver_SR
!
!>       number of nodes
      integer(kind=kint )                , intent(in)   ::  N
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
      integer (kind=kint), dimension(N)  , intent(inout):: iX
!C
!
      integer (kind = kint) :: neib, istart, inum, ierr, k
!
!
      call resize_iwork_4_SR(NEIBPETOT,                                 &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1)
        inum  = STACK_EXPORT(neib  ) - istart
        
        do k= istart+1, istart+inum
           iWS(k)= iX(NOD_EXPORT(k))
        enddo
        call MPI_ISEND (iWS(istart+1), inum, CALYPSO_INTEGER,           &
     &                  NEIBPE(neib), 0, CALYPSO_COMM,                  &
     &                  req1(neib), ierr)
      enddo

!C
!C-- RECEIVE
      
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        inum  = STACK_IMPORT(neib  ) - istart
        call MPI_IRECV (iWR(istart+1), inum, CALYPSO_INTEGER,           &
     &                  NEIBPE(neib), 0, CALYPSO_COMM,                  &
     &                  req2(neib), ierr)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        inum  = STACK_IMPORT(neib  ) - istart
      do k= istart+1, istart+inum
        iX(NOD_IMPORT(k))= iWR(k)
      enddo
      enddo

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr)

      end subroutine solver_send_recv_i
!
!  ---------------------------------------------------------------------
!
      end module solver_SR_int
