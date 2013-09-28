!>@file   solver_SR.f90
!!@brief  module solver_SR
!!
!!@author coded by K.Nakajima (RIST)
!!@date coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!!@n    modified by H. Matsui (U. of Chicago) on july 2007 (ver 1.1)
!!@n    modified by H. Matsui (UC Davis) on Sep. 2013 (ver 1.2)
!
!>@brief  MPI SEND and RECEIVE routine for scalar fields
!!        in overlapped partitioning
!!
!!@verbatim
!!      subroutine  SOLVER_SEND_RECV                                    &
!!     &          (N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,      &
!!     &                                 STACK_EXPORT, NOD_EXPORT, X)
!!      subroutine  SOLVER_SEND_RECVx3                                  &
!!     &          (N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,      &
!!     &                                 STACK_EXPORT, NOD_EXPORT,      &
!!     &           X1, X2, X3)
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
!!@n @param  X(N)   scalar field data
!!@n @param  X1(N)  1st scalar field data
!!@n @param  X2(N)  2nd scalar field data
!!@n @param  X3(N)  3rd scalar field data
!
      module solver_SR
!
      use m_precision
      use m_constants
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
!C*** SOLVER_SEND_RECV
!C
      subroutine  SOLVER_SEND_RECV                                      &
     &         ( N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,        &
     &                                 STACK_EXPORT, NOD_EXPORT, X)

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
      real   (kind=kreal), dimension(N)  , intent(inout):: X
!
      integer (kind = kint) :: neib, istart, inum, iend, ierr, k
!
!
      call resize_work_4_SR(ione, NEIBPETOT,                            &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!
!C-- SEND
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        iend  = STACK_EXPORT(neib  )
        
        do k= istart, iend
           WS(k)= X(NOD_EXPORT(k))
        enddo
        istart= STACK_EXPORT(neib-1) + 1
        inum  = STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1)
        call MPI_ISEND (WS(istart), inum, MPI_DOUBLE_PRECISION,         &
     &                  NEIBPE(neib), 0, CALYPSO_COMM,                  &
     &                  req1(neib), ierr)
      enddo

!C
!C-- RECEIVE
      
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        inum  = STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1)
        call MPI_IRECV (WR(istart), inum, MPI_DOUBLE_PRECISION,         &
     &                  NEIBPE(neib), 0, CALYPSO_COMM,                  &
     &                  req2(neib), ierr)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
        do k= istart, iend
          X(NOD_IMPORT(k))= WR(k)
        enddo
      enddo

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr)

      end subroutine solver_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine  SOLVER_SEND_RECVx3                                    &
     &                ( N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT, &
     &                                        STACK_EXPORT, NOD_EXPORT, &
     &                  X1,  X2, X3)

      use m_solver_SR
!
      integer(kind=kint )                , intent(in)   ::  N
      integer(kind=kint )                , intent(in)   ::  NEIBPETOT
      integer(kind=kint ), dimension(NEIBPETOT) :: NEIBPE
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_IMPORT
      integer(kind=kint ), dimension(STACK_IMPORT(NEIBPETOT))           &
     &        :: NOD_IMPORT
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_EXPORT
      integer(kind=kint ), dimension(STACK_EXPORT(NEIBPETOT))           &
     &        :: NOD_EXPORT
      real   (kind=kreal), dimension(N)  , intent(inout):: X1
      real   (kind=kreal), dimension(N)  , intent(inout):: X2
      real   (kind=kreal), dimension(N)  , intent(inout):: X3
!C
!
      integer (kind = kint) :: neib, istart, inum, iend, ierr, k
!
!
      call resize_work_4_SR(ithree, NEIBPETOT,                          &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        iend  = STACK_EXPORT(neib  )
        
        do k= istart, iend
           WS(3*k-2)= X1(NOD_EXPORT(k))
           WS(3*k-1)= X2(NOD_EXPORT(k))
           WS(3*k  )= X3(NOD_EXPORT(k))
        enddo
        istart= 3 *   STACK_EXPORT(neib-1) + 1
        inum  = 3 * ( STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1) )
        call MPI_ISEND (WS(istart), inum, MPI_DOUBLE_PRECISION,         &
     &                  NEIBPE(neib), 0, CALYPSO_COMM,                  &
     &                  req1(neib), ierr)
      enddo

!C
!C-- RECEIVE
      
      do neib= 1, NEIBPETOT
        istart= 3 *   STACK_IMPORT(neib-1) + 1
        inum  = 3 * ( STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1) )
        call MPI_IRECV (WR(istart), inum, MPI_DOUBLE_PRECISION,         &
     &                  NEIBPE(neib), 0, CALYPSO_COMM,                  &
     &                  req2(neib), ierr)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
        do k= istart, iend
          X1(NOD_IMPORT(k))= WR(3*k-2)
          X2(NOD_IMPORT(k))= WR(3*k-1)
          X3(NOD_IMPORT(k))= WR(3*k  )
        enddo
      enddo

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr)

      end subroutine solver_send_recvx3
!
!  ---------------------------------------------------------------------
!
      end module     solver_SR
