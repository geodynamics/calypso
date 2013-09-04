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
!!      subroutine  SOLVER_SEND_RECV_6                                  &
!!     &                (N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,&
!!     &                                       STACK_EXPORT, NOD_EXPORT,&
!!     &                 X, SOLVER_COMM,my_rank)
!!@endverbatim
!!
!!@n @param  N     Number of data points
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
!!@n @param  X(6*N)   field data with 6 components
!!
!!@n @param  SOLVER_COMM      MPI communicator
!!@n @param  my_rank          own process rank
!
      module solver_SR_6
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!C
      subroutine  SOLVER_SEND_RECV_6                                    &
     &                ( N, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT, &
     &                                        STACK_EXPORT, NOD_EXPORT, &
     &                  X, SOLVER_COMM,my_rank)
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
      real   (kind=kreal), dimension(6*N), intent(inout):: X
!>       communicator for mpi
      integer                            , intent(in)   ::SOLVER_COMM
!>       Process ID
      integer                            , intent(in)   :: my_rank
!
      integer (kind = kint) :: neib, istart, inum, ierr, k, ii
!
!
      call resize_work_4_SR(isix, NEIBPETOT,                            &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!C
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1)
        inum  = STACK_EXPORT(neib  ) - istart
        
        do k= istart+1, istart+inum
               ii   = 6*NOD_EXPORT(k)
           WS(6*k-5)= X(ii-5)
           WS(6*k-4)= X(ii-4)
           WS(6*k-3)= X(ii-3)
           WS(6*k-2)= X(ii-2)
           WS(6*k-1)= X(ii-1)
           WS(6*k  )= X(ii  )
        enddo
        call MPI_ISEND (WS(6*istart+1), 6*inum,MPI_DOUBLE_PRECISION,    &
     &                  NEIBPE(neib), 0, SOLVER_COMM, req1(neib), ierr)
      enddo

!C
!C-- RECEIVE
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        inum  = STACK_IMPORT(neib  ) - istart
        call MPI_IRECV (WR(6*istart+1), 6*inum, MPI_DOUBLE_PRECISION,   &
     &                  NEIBPE(neib), 0, SOLVER_COMM, req2(neib), ierr)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        inum  = STACK_IMPORT(neib  ) - istart
      do k= istart+1, istart+inum
          ii   = 6*NOD_IMPORT(k)
        X(ii-5)= WR(6*k-5)
        X(ii-4)= WR(6*k-4)
        X(ii-3)= WR(6*k-3)
        X(ii-2)= WR(6*k-2)
        X(ii-1)= WR(6*k-1)
        X(ii  )= WR(6*k  )
      enddo
      enddo

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr)

      end subroutine SOLVER_SEND_RECV_6
!
! ----------------------------------------------------------------------
!
      end module     solver_SR_6
