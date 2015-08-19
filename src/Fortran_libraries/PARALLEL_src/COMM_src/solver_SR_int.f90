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
!!      subroutine  solver_send_recv_i                                  &
!!     &          (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,     &
!!     &           STACK_EXPORT, NOD_EXPORT, ix)
!!      subroutine  solver_send_recv_i8                                 &
!!     &          (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,     &
!!     &           STACK_EXPORT, NOD_EXPORT, iX8)
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
     &          (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,       &
     &           STACK_EXPORT, NOD_EXPORT, ix)
!
      use calypso_mpi
      use m_solver_SR
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
!C
!
      integer (kind = kint) :: neib, istart, inum, k
!
!
      call resize_iwork_4_SR(NEIBPETOT,                                 &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1)
        inum  = STACK_EXPORT(neib  ) - istart
        
!$omp parallel do
        do k= istart+1, istart+inum
           iWS(k)= iX(NOD_EXPORT(k))
        end do
!$omp end parallel do
!
        call MPI_ISEND (iWS(istart+1), inum, CALYPSO_INTEGER,           &
     &                  NEIBPE(neib), 0, CALYPSO_COMM,                  &
     &                  req1(neib), ierr_MPI)
      enddo

!C
!C-- RECEIVE
      
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        inum  = STACK_IMPORT(neib  ) - istart
        call MPI_IRECV (iWR(istart+1), inum, CALYPSO_INTEGER,           &
     &                  NEIBPE(neib), 0, CALYPSO_COMM,                  &
     &                  req2(neib), ierr_MPI)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr_MPI)
   
!$omp parallel private(neib,istart,inum)
      do neib = 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        inum  = STACK_IMPORT(neib  ) - istart
!$omp do
        do k= istart+1, istart+inum
          iX(NOD_IMPORT(k))= iWR(k)
        end do
!$omp end do nowait
      end do
!$omp end parallel

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr_MPI)

      end subroutine solver_send_recv_i
!
!  ---------------------------------------------------------------------
!
      subroutine  solver_send_recv_i8                                   &
     &          (NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,       &
     &           STACK_EXPORT, NOD_EXPORT, iX8)
!
      use calypso_mpi
      use m_solver_SR
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
      integer (kind=kint_gl), dimension(NP)  , intent(inout):: iX8
!C
!
      integer (kind = kint) :: neib, istart, inum, k
!
!
      call resize_i8work_4_SR(NEIBPETOT,                                &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1)
        inum  = STACK_EXPORT(neib  ) - istart
        
!$omp parallel do
        do k= istart+1, istart+inum
           i8WS(k)= iX8(NOD_EXPORT(k))
        end do
!$omp end parallel do
!
        call MPI_ISEND (i8WS(istart+1), inum, CALYPSO_GLOBAL_INT,       &
     &                  NEIBPE(neib), 0, CALYPSO_COMM,                  &
     &                  req1(neib), ierr_MPI)
      enddo

!C
!C-- RECEIVE
      
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        inum  = STACK_IMPORT(neib  ) - istart
        call MPI_IRECV (i8WR(istart+1), inum, CALYPSO_GLOBAL_INT,       &
     &                  NEIBPE(neib), 0, CALYPSO_COMM,                  &
     &                  req2(neib), ierr_MPI)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr_MPI)

!$omp parallel private(neib,istart,inum)
      do neib = 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1)
        inum  = STACK_IMPORT(neib  ) - istart
!$omp do
        do k = istart+1, istart+inum
          iX8(NOD_IMPORT(k))= i8WR(k)
        end do
!$omp end do nowait
      end do
!$omp end parallel

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr_MPI)

      end subroutine solver_send_recv_i8
!
!  ---------------------------------------------------------------------
      end module solver_SR_int
