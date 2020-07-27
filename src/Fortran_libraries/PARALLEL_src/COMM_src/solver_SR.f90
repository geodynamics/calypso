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
!!      subroutine  SOLVER_SEND_RECV(N, NEIBPETOT, NEIBPE,              &
!!     &                             STACK_IMPORT, NOD_IMPORT,          &
!!     &                             STACK_EXPORT, NOD_EXPORT,          &
!!     &                             SR_sig, SR_r, X)
!!      subroutine  SOLVER_SEND_RECVx3(N, NEIBPETOT, NEIBPE,            &
!!     &                               STACK_IMPORT, NOD_IMPORT,        &
!!     &                               STACK_EXPORT, NOD_EXPORT,        &
!!     &                               SR_sig, SR_r, X1, X2, X3)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
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
      use t_solver_SR
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
      subroutine  SOLVER_SEND_RECV(N, NEIBPETOT, NEIBPE,                &
     &                             STACK_IMPORT, NOD_IMPORT,            &
     &                             STACK_EXPORT, NOD_EXPORT,            &
     &                             SR_sig, SR_r, X)
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
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer (kind = kint) :: neib, istart, iend, k
      integer :: inum
!
!
      call resize_work_SR(ione, NEIBPETOT, NEIBPETOT,                   &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT),             &
     &    SR_sig, SR_r)
!
!C-- SEND
      do neib= 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        iend  = STACK_EXPORT(neib  )
        
        do k= istart, iend
           SR_r%WS(k)= X(NOD_EXPORT(k))
        enddo
        istart= STACK_EXPORT(neib-1) + 1
        inum  = int(STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1))
        call MPI_ISEND (SR_r%WS(istart), inum, CALYPSO_REAL,            &
     &                  int(NEIBPE(neib)), 0, CALYPSO_COMM,             &
     &                  SR_sig%req1(neib), ierr_MPI)
      enddo

!C
!C-- RECEIVE
      
      do neib= 1, NEIBPETOT
        istart = STACK_IMPORT(neib-1) + 1
        inum  = int(STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1))
        call MPI_IRECV (SR_r%WR(istart), inum, CALYPSO_REAL,            &
     &                  int(NEIBPE(neib)), 0, CALYPSO_COMM,             &
     &                  SR_sig%req2(neib), ierr_MPI)
      enddo

      call MPI_WAITALL                                                  &
     &   (int(NEIBPETOT), SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
        do k= istart, iend
          X(NOD_IMPORT(k))= SR_r%WR(k)
        enddo
      enddo

      call MPI_WAITALL                                                  &
     &   (int(NEIBPETOT), SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)

      end subroutine solver_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine  SOLVER_SEND_RECVx3(N, NEIBPETOT, NEIBPE,              &
     &                               STACK_IMPORT, NOD_IMPORT,          &
     &                               STACK_EXPORT, NOD_EXPORT,          &
     &                               SR_sig, SR_r, X1, X2, X3)
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
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!C
!
      integer (kind = kint) :: neib, istart, iend, k
      integer :: inum
!
!
      call resize_work_SR(ithree, NEIBPETOT, NEIBPETOT,                 &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT)              &
     &    , SR_sig, SR_r)
!
!C-- SEND
      
      do neib = 1, NEIBPETOT
        istart= STACK_EXPORT(neib-1) + 1
        iend  = STACK_EXPORT(neib  )
        
        do k= istart, iend
           SR_r%WS(3*k-2)= X1(NOD_EXPORT(k))
           SR_r%WS(3*k-1)= X2(NOD_EXPORT(k))
           SR_r%WS(3*k  )= X3(NOD_EXPORT(k))
        enddo
        istart= 3 *   STACK_EXPORT(neib-1) + 1
        inum  = int(3 * (STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1)))
        call MPI_ISEND (SR_r%WS(istart), inum, CALYPSO_REAL,            &
     &                  int(NEIBPE(neib)), 0, CALYPSO_COMM,             &
     &                  SR_sig%req1(neib), ierr_MPI)
      enddo

!C
!C-- RECEIVE
      
      do neib= 1, NEIBPETOT
        istart= 3 *   STACK_IMPORT(neib-1) + 1
        inum  = int(3 * (STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1)))
        call MPI_IRECV (SR_r%WR(istart), inum, CALYPSO_REAL,            &
     &                  int(NEIBPE(neib)), 0, CALYPSO_COMM,             &
     &                  SR_sig%req2(neib), ierr_MPI)
      enddo

      call MPI_WAITALL                                                  &
     &   (int(NEIBPETOT), SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
        do k= istart, iend
          X1(NOD_IMPORT(k))= SR_r%WR(3*k-2)
          X2(NOD_IMPORT(k))= SR_r%WR(3*k-1)
          X3(NOD_IMPORT(k))= SR_r%WR(3*k  )
        enddo
      enddo

      call MPI_WAITALL                                                  &
     &   (int(NEIBPETOT), SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)

      end subroutine solver_send_recvx3
!
!  ---------------------------------------------------------------------
!
      end module     solver_SR
