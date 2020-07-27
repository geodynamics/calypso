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
!!      subroutine  SOLVER_SEND_RECV_N(N, NB, NEIBPETOT, NEIBPE,        &
!!     &                               STACK_IMPORT, NOD_IMPORT,        &
!!     &                               STACK_EXPORT, NOD_EXPORT,        &
!!     &                               SR_sig, SR_r, X)
!!      subroutine  SOLVER_SEND_RECV_Nx3(N, NB, NEIBPETOT, NEIBPE,      &
!!     &                                STACK_IMPORT, NOD_IMPORT,       &
!!     &                                STACK_EXPORT, NOD_EXPORT,       &
!!     &                                SR_sig, SR_r, X1, X2, X3)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
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
!
      module solver_SR_N
!
      use m_precision
      use calypso_mpi
      use t_solver_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine  SOLVER_SEND_RECV_N(N, NB, NEIBPETOT, NEIBPE,          &
     &                               STACK_IMPORT, NOD_IMPORT,          &
     &                               STACK_EXPORT, NOD_EXPORT,          &
     &                               SR_sig, SR_r, X)
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
      real   (kind=kreal), intent(inout) :: X(NB*N)
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer (kind = kint) :: neib, istart, iend
      integer (kind = kint) :: k, ii, ix, nd
      integer :: inum
!
!
      call resize_work_SR(NB, NEIBPETOT, NEIBPETOT,                     &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT),             &
     &    SR_sig, SR_r)
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
             SR_r%WS(ix)= X(ii)
           end do
         end do
!
        istart= NB *  STACK_EXPORT(neib-1) + 1
        inum  = int(NB * (STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1)))
        call MPI_ISEND(SR_r%WS(istart), inum, CALYPSO_REAL,             &
     &                 int(NEIBPE(neib)), 0, CALYPSO_COMM,              &
     &                 SR_sig%req1(neib), ierr_MPI)
      end do

!C
!C-- RECEIVE
      do neib= 1, NEIBPETOT
        istart= NB *  STACK_IMPORT(neib-1) + 1
        inum  = int(NB * (STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1)))
        call MPI_IRECV(SR_r%WR(istart), inum, CALYPSO_REAL,             &
     &                 int(NEIBPE(neib)), 0, CALYPSO_COMM,              &
     &                 SR_sig%req2(neib), ierr_MPI)
      enddo

      call MPI_WAITALL(int                                              &
     &   (NEIBPETOT), SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
        do nd = 1, NB
          do k= istart, iend
            ii   = NB * (NOD_IMPORT(k)-1) + nd
            ix   = NB * (k-1) + nd
            X(ii)= SR_r%WR(ix)
          end do
        enddo
      enddo

      call MPI_WAITALL                                                  &
         (NEIBPETOT, SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)

      end subroutine solver_send_recv_N
!
! ----------------------------------------------------------------------
!
      subroutine  SOLVER_SEND_RECV_Nx3(N, NB, NEIBPETOT, NEIBPE,        &
     &                                STACK_IMPORT, NOD_IMPORT,         &
     &                                STACK_EXPORT, NOD_EXPORT,         &
     &                                SR_sig, SR_r, X1, X2, X3)
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
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer (kind = kint) :: neib, istart, inum, iend
      integer (kind = kint) :: k, ii, ix, nd, NB3
!
!
      NB3 = 3 * NB
      call resize_work_SR(NB3, NEIBPETOT, NEIBPETOT,                    &
     &    STACK_EXPORT(NEIBPETOT), STACK_IMPORT(NEIBPETOT),             &
     &    SR_sig, SR_r)
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
             SR_r%WS(ix-2)= X1(ii)
             SR_r%WS(ix-1)= X2(ii)
             SR_r%WS(ix  )= X3(ii)
           end do
         end do
!
        istart= 3*NB *  STACK_EXPORT(neib-1) + 1
        inum  = 3*NB * (STACK_EXPORT(neib  ) - STACK_EXPORT(neib-1) )
        call MPI_ISEND(SR_r%WS(istart), inum, CALYPSO_REAL,             &
     &                 int(NEIBPE(neib)), 0, CALYPSO_COMM,              &
     &                 SR_sig%req1(neib), ierr_MPI)
      end do

!C
!C-- RECEIVE
      do neib= 1, NEIBPETOT
        istart= 3*NB *  STACK_IMPORT(neib-1) + 1
        inum  = 3*NB * (STACK_IMPORT(neib  ) - STACK_IMPORT(neib-1) )
        call MPI_IRECV(SR_r%WR(istart), inum, CALYPSO_REAL,             &
     &                 int(NEIBPE(neib)), 0, CALYPSO_COMM,              &
     &                 SR_sig%req2(neib), ierr_MPI)
      enddo

      call MPI_WAITALL(int                                              &
     &   (NEIBPETOT), SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
   
      do neib= 1, NEIBPETOT
        istart= STACK_IMPORT(neib-1) + 1
        iend  = STACK_IMPORT(neib  )
        do nd = 1, NB
          do k= istart, iend
            ii   = NB * (NOD_IMPORT(k)-1) + nd
            ix   = 3*NB * (k-1) + 3*nd
            X1(ii)= SR_r%WR(ix-2)
            X2(ii)= SR_r%WR(ix-1)
            X3(ii)= SR_r%WR(ix  )
          end do
        enddo
      enddo

      call MPI_WAITALL                                                  &
     &   (NEIBPETOT, SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)

      end subroutine solver_send_recv_Nx3
!
! ----------------------------------------------------------------------
!
      end module     solver_SR_N
