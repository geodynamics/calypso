!>@file   solver_SR_N.f90
!!@brief  module solver_SR_N
!!
!!@author coded by K.Nakajima (RIST)
!!@date coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!!@n    modified by H. Matsui (U. of Chicago) on july 2007 (ver 1.1)
!
!>@brief  MPI SEND and RECEIVE routine for arbitrary components fields
!!        in overlapped partitioning
!!
!!@verbatim
!!      subroutine  SOLVER_SEND_RECV_N                                  &
!!     &            (N, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,&
!!     &                                       STACK_EXPORT, NOD_EXPORT,&
!!     &             X, SOLVER_COMM,my_rank)
!!      subroutine  SOLVER_SEND_RECV_Nx3                                &
!!     &            (N, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,&
!!     &                                       STACK_EXPORT, NOD_EXPORT,&
!!     &             X1, X2, X3, SOLVER_COMM,my_rank)
!!@endverbatim
!!
!!@n @param  N     Number of data points
!!@n @param  NB    Number of components for communication
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
!!@n @param  X(NB*N)   field data with NB components
!!@n @param  X1(NB*N)  1st field data with NB components
!!@n @param  X2(NB*N)  2nd field data with NB components
!!@n @param  X3(NB*N)  3rd field data with NB components
!!
!!@n @param  SOLVER_COMM      MPI communicator
!!@n @param  my_rank          own process rank
!
!
      module solver_SR_N
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
      subroutine  SOLVER_SEND_RECV_N                                    &
     &            ( N, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT, &
     &                                        STACK_EXPORT, NOD_EXPORT, &
     &              X, SOLVER_COMM, my_rank)
!
      use calypso_mpi
      use m_solver_SR
!
!>       number of nodes and number of components
      integer(kind=kint )                , intent(in)   ::  N, NB
!>       total neighboring pe count
      integer(kind=kint )                , intent(in)   ::  NEIBPETOT
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
      real   (kind=kreal), intent(inout):: X(NB*N)
!>       communicator for mpi
      integer                            , intent(in)   ::SOLVER_COMM
!>       Own process
      integer                            , intent(in)   :: my_rank
!
      integer (kind = kint) :: neib, istart, inum, iend
      integer (kind = kint) :: ierr, k, ii, ix, nd
!
!
      call resize_work_4_SR(NB, NEIBPETOT,                              &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!
!C
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        iend  = STACK_EXPORT(neib  )
        do nd = 1, NB
          do k= istart, iend
                 ii   = NB * (NOD_EXPORT(k)-1) + nd
                 ix   = NB * (k-1) + nd
             WS(ix)= X(ii)
           end do
         end do
!
        istart= NB *  STACK_EXPORT(neib-1) + 1
        inum  = NB * (STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1) )
        call MPI_ISEND(WS(istart), inum, MPI_DOUBLE_PRECISION,          &
     &                  NEIBPE(neib), 0, SOLVER_COMM, req1(neib), ierr)
      end do

!C
!C-- RECEIVE
      do neib= 1, NEIBPETOT
        istart= NB *  STACK_IMPORT(neib-1) + 1
        inum  = NB * (STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1) )
        call MPI_IRECV(WR(istart), inum, MPI_DOUBLE_PRECISION,          &
     &                 NEIBPE(neib), 0, SOLVER_COMM, req2(neib), ierr)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
        do nd = 1, NB
          do k= istart, iend
            ii   = NB * (NOD_IMPORT(k)-1) + nd
            ix   = NB * (k-1) + nd
            X(ii)= WR(ix)
          end do
        enddo
      enddo

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr)

      end subroutine solver_send_recv_N
!
! ----------------------------------------------------------------------
!
      subroutine  SOLVER_SEND_RECV_Nx3                                  &
     &            ( N, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT, &
     &                                        STACK_EXPORT, NOD_EXPORT, &
     &              X1, X2, X3, SOLVER_COMM, my_rank)

      use calypso_mpi
!
      use m_solver_SR
!
! ......................................................................

      integer(kind=kint )                , intent(in)   ::  N, NB
      integer(kind=kint )                , intent(in)   ::  NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
      real   (kind=kreal), intent(inout):: X1(NB*N)
      real   (kind=kreal), intent(inout):: X2(NB*N)
      real   (kind=kreal), intent(inout):: X3(NB*N)
!
      integer                            , intent(in)   ::SOLVER_COMM
      integer                            , intent(in)   :: my_rank
!
      integer (kind = kint) :: neib, istart, inum, iend
      integer (kind = kint) :: ierr, k, ii, ix, nd, NB3
!
!
      NB3 = 3 * NB
      call resize_work_4_SR(NB3, NEIBPETOT,                             &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT) )
!
!C
!C-- SEND
      
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        iend  = STACK_EXPORT(neib  )
        do nd = 1, NB
          do k= istart, iend
                 ii   = NB * (NOD_EXPORT(k)-1) + nd
                 ix   = 3*NB * (k-1) + 3*nd
             WS(ix-2)= X1(ii)
             WS(ix-1)= X2(ii)
             WS(ix  )= X3(ii)
           end do
         end do
!
        istart= 3*NB *  STACK_EXPORT(neib-1) + 1
        inum  = 3*NB * (STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1) )
        call MPI_ISEND(WS(istart), inum, MPI_DOUBLE_PRECISION,          &
     &                  NEIBPE(neib), 0, SOLVER_COMM, req1(neib), ierr)
      end do

!C
!C-- RECEIVE
      do neib= 1, NEIBPETOT
        istart= 3*NB *  STACK_IMPORT(neib-1) + 1
        inum  = 3*NB * (STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1) )
        call MPI_IRECV(WR(istart), inum, MPI_DOUBLE_PRECISION,          &
     &                 NEIBPE(neib), 0, SOLVER_COMM, req2(neib), ierr)
      enddo

      call MPI_WAITALL (NEIBPETOT, req2(1), sta2(1,1), ierr)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
        do nd = 1, NB
          do k= istart, iend
            ii   = NB * (NOD_IMPORT(k)-1) + nd
            ix   = 3*NB * (k-1) + 3*nd
            X1(ii)= WR(ix-2)
            X2(ii)= WR(ix-1)
            X3(ii)= WR(ix  )
          end do
        enddo
      enddo

      call MPI_WAITALL (NEIBPETOT, req1(1), sta1(1,1), ierr)

      end subroutine solver_send_recv_Nx3
!
! ----------------------------------------------------------------------
!
      end module     solver_SR_N
