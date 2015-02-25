!>@file   calypso_solver_SR.f90
!!@brief  module calypso_solver_SR
!!
!!@author H. Matsui
!!@date Programmed in March, 2013
!
!>@brief  Select communication routines for spherical harmonics transform
!!
!!@verbatim
!!      subroutine init_solver_send_recv(NP, N, NEIBPETOT,              &
!!     &                 STACK_IMPORT, NOD_IMPORT, REV_IMPORT)
!!      subroutine finalize_solver_send_recv(N, NEIBPETOT,              &
!!     &                 STACK_IMPORT, NOD_IMPORT)
!!
!!      subroutine solver_send_recv_N(iflag_SR, NP, N, NB, NEIBPETOT,   &
!!     &                 NEIBPE, STACK_IMPORT, NOD_IMPORT, REV_IMPORT,  &
!!     &                         STACK_EXPORT, NOD_EXPORT, X)
!!      subroutine solver_send_recv_6(iflag_SR, NP, N, NEIBPETOT,       &
!!     &                 NEIBPE, STACK_IMPORT, NOD_IMPORT, REV_IMPORT,  &
!!     &                         STACK_EXPORT, NOD_EXPORT, X)
!!      subroutine solver_send_recv_3(iflag_SR, NP, N, NEIBPETOT,       &
!!     &                 NEIBPE, STACK_IMPORT, NOD_IMPORT, REV_IMPORT,  &
!!     &                         STACK_EXPORT, NOD_EXPORT, X)
!!      subroutine solver_send_recv_2(iflag_SR, NP, N, NEIBPETOT,       &
!!     &                 NEIBPE, STACK_IMPORT, NOD_IMPORT, REV_IMPORT,  &
!!     &                         STACK_EXPORT, NOD_EXPORT, X)
!!      subroutine solver_send_recv(iflag_SR, NP, N, NEIBPETOT,         &
!!     &                 NEIBPE, STACK_IMPORT, NOD_IMPORT, REV_IMPORT,  &
!!     &                         STACK_EXPORT, NOD_EXPORT, X)
!!
!!      subroutine solver_send_recv_int(iflag_SR, NP, N, NEIBPETOT,     &
!!     &                 NEIBPE, STACK_IMPORT, NOD_IMPORT, REV_IMPORT,  &
!!     &                         STACK_EXPORT, NOD_EXPORT, iX)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  NP          Number of node
!!@n @param  N           Number of internal node
!!@n
!!@n @param  NEIBPETOT    Number of processses to send
!!@n @param  NEIBPE(NEIBPETOT)      Process ID to communicate
!!@n @param  STACK_EXPORT(0:NEIBPETOT)
!!                    End points of send buffer for each process
!!@n @param  NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  STACK_IMPORT(0:NEIBPETOT)
!!                    End points of receive buffer for each process
!!@n @param  NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
!!                    local node ID to copy from receive buffer
!!@n @param  REV_IMPORT(NP-N)
!!                    import buffer ID for each data point
!!@n
!!@n @param  X(NB*NP)  vector to syncronize
!!@n
!!@n @param  X(6*NP)  Six components of received data
!!@n
!!@n @param  X(3*NP)  Three components of received data
!!@n
!!@n @param  X(2*NP)  Two components of received data
!!@n
!!@n @param  X(NP)  Scalar received data
!!@n
!!@n @param  iX(NP)  Integer received data
!
      module calypso_solver_SR
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_solver_send_recv(NP, N, NEIBPETOT,                &
     &                 STACK_IMPORT, NOD_IMPORT, REV_IMPORT)
!
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NEIBPETOT
!
      integer(kind = kint), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind = kint), intent(inout)                               &
     &                    :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind = kint), intent(inout) :: REV_IMPORT(NP-N)
!
      integer(kind = kint) :: N_ext
      integer(kind = kint) :: i, j
!
!
      N_ext = NP - N
      do i = 1, STACK_IMPORT(NEIBPETOT)
        j = NOD_IMPORT(i) - N
        REV_IMPORT(j) = i
        NOD_IMPORT(i) = j
      end do
!
      call set_reverse_import_table(N_ext, STACK_IMPORT(NEIBPETOT),     &
     &          NOD_IMPORT, REV_IMPORT)
!
      end subroutine init_solver_send_recv
!
!-----------------------------------------------------------------------
!
      subroutine finalize_solver_send_recv(N, NEIBPETOT,                &
     &                 STACK_IMPORT, NOD_IMPORT)
!
      integer(kind = kint), intent(in) :: N
      integer(kind = kint), intent(in) :: NEIBPETOT
!
      integer(kind = kint), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind = kint), intent(inout)                               &
     &                    :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
!
      integer(kind = kint) :: i
!
!
      do i = 1, STACK_IMPORT(NEIBPETOT)
        NOD_IMPORT(i) = NOD_IMPORT(i) + N
      end do
!
      end subroutine finalize_solver_send_recv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine solver_send_recv_N(iflag_SR, NP, N, NB, NEIBPETOT,     &
     &                 NEIBPE, STACK_IMPORT, NOD_IMPORT, REV_IMPORT,    &
     &                         STACK_EXPORT, NOD_EXPORT, X)
      use select_calypso_SR
!
      integer(kind = kint), intent(in) :: iflag_SR
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NEIBPETOT
      integer(kind = kint), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind = kint), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind = kint), intent(in)                                  &
     &                    :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
      integer(kind = kint), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind = kint), intent(in)                                  &
     &                    :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind = kint), intent(in) :: REV_IMPORT(NP-N)
!
      real (kind=kreal), intent(inout):: X(NB*NP)
!
      integer(kind = kint) :: N_ext
!
!
      N_ext = NP - N
      if(N_ext .le. 0) return
      call sel_calypso_send_recv_N(iflag_SR, NB, N, N_ext,              &
     &    NEIBPETOT, izero, NEIBPE, STACK_EXPORT, NOD_EXPORT,           &
     &    NEIBPETOT, izero, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &    REV_IMPORT, X(1), X(NB*N+NB))
!
      call finish_calypso_send_recv(NEIBPETOT, izero)
!
      end subroutine solver_send_recv_N
!
!-----------------------------------------------------------------------
!
      subroutine solver_send_recv_6(iflag_SR, NP, N, NEIBPETOT,         &
     &                 NEIBPE, STACK_IMPORT, NOD_IMPORT, REV_IMPORT,    &
     &                         STACK_EXPORT, NOD_EXPORT, X)
!
      use calypso_SR_6
!
      integer(kind = kint), intent(in) :: iflag_SR
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NEIBPETOT
      integer(kind = kint), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind = kint), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind = kint), intent(in)                                  &
     &                    :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
      integer(kind = kint), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind = kint), intent(in)                                  &
     &                    :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind = kint), intent(in) :: REV_IMPORT(NP-N)
!
      real (kind=kreal), intent(inout):: X(isix*NP)
!
      integer(kind = kint) :: N_ext
!
!
      N_ext = NP - N
      if(N_ext .le. 0) return
      call calypso_send_recv_6(iflag_SR, N, N_ext,                      &
     &    NEIBPETOT, izero, NEIBPE, STACK_EXPORT, NOD_EXPORT,           &
     &    NEIBPETOT, izero, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &    REV_IMPORT,  X(1), X(6*N+6))
!
      end subroutine solver_send_recv_6
!
!-----------------------------------------------------------------------
!
      subroutine solver_send_recv_3(iflag_SR, NP, N, NEIBPETOT,         &
     &                 NEIBPE, STACK_IMPORT, NOD_IMPORT, REV_IMPORT,    &
     &                         STACK_EXPORT, NOD_EXPORT, X)
!
      use calypso_SR_3
!
      integer(kind = kint), intent(in) :: iflag_SR
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NEIBPETOT
      integer(kind = kint), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind = kint), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind = kint), intent(in)                                  &
     &                    :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
      integer(kind = kint), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind = kint), intent(in)                                  &
     &                    :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind = kint), intent(in) :: REV_IMPORT(NP-N)
!
      real (kind=kreal), intent(inout):: X(ithree*NP)
!
      integer(kind = kint) :: N_ext
!
!
      N_ext = NP - N
      if(N_ext .le. 0) return
      call calypso_send_recv_3(iflag_SR, N, N_ext,                      &
     &    NEIBPETOT, izero, NEIBPE, STACK_EXPORT, NOD_EXPORT,           &
     &    NEIBPETOT, izero, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &    REV_IMPORT,  X(1), X(3*N+3))
!
      end subroutine solver_send_recv_3
!
!-----------------------------------------------------------------------
!
      subroutine solver_send_recv_2(iflag_SR, NP, N, NEIBPETOT,         &
     &                 NEIBPE, STACK_IMPORT, NOD_IMPORT, REV_IMPORT,    &
     &                         STACK_EXPORT, NOD_EXPORT, X)
!
      use calypso_SR_2
!
      integer(kind = kint), intent(in) :: iflag_SR
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NEIBPETOT
      integer(kind = kint), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind = kint), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind = kint), intent(in)                                  &
     &                    :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
      integer(kind = kint), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind = kint), intent(in)                                  &
     &                    :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind = kint), intent(in) :: REV_IMPORT(NP-N)
!
      real (kind=kreal), intent(inout):: X(itwo*NP)
!
      integer(kind = kint) :: N_ext
!
!
      N_ext = NP - N
      if(N_ext .le. 0) return
      call calypso_send_recv_2(iflag_SR, N, N_ext,                      &
     &    NEIBPETOT, izero, NEIBPE, STACK_EXPORT, NOD_EXPORT,           &
     &    NEIBPETOT, izero, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &    REV_IMPORT, X(1), X(2*N+2))
!
      end subroutine solver_send_recv_2
!
! ----------------------------------------------------------------------
!
      subroutine solver_send_recv(iflag_SR, NP, N,NEIBPETOT,            &
     &                 NEIBPE, STACK_IMPORT, NOD_IMPORT, REV_IMPORT,    &
     &                         STACK_EXPORT, NOD_EXPORT, X)
!
      use calypso_SR
!
      integer(kind = kint), intent(in) :: iflag_SR
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NEIBPETOT
      integer(kind = kint), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind = kint), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind = kint), intent(in)                                  &
     &                    :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
      integer(kind = kint), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind = kint), intent(in)                                  &
     &                    :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind = kint), intent(in) :: REV_IMPORT(NP-N)
!
      real (kind=kreal), intent(inout):: X(NP)
!
      integer(kind = kint) :: N_ext
!
!
      N_ext = NP - N
      if(N_ext .le. 0) return
      call calypso_send_recv(iflag_SR, N, N_ext,                        &
     &    NEIBPETOT, izero, NEIBPE, STACK_EXPORT, NOD_EXPORT,           &
     &    NEIBPETOT, izero, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &    REV_IMPORT, X(1), X(N+1))
!
      end subroutine solver_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine solver_send_recv_int(iflag_SR, NP, N, NEIBPETOT,       &
     &                 NEIBPE, STACK_IMPORT, NOD_IMPORT, REV_IMPORT,    &
     &                         STACK_EXPORT, NOD_EXPORT, iX)
!
      use calypso_SR_int
!
      integer(kind = kint), intent(in) :: iflag_SR
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NEIBPETOT
      integer(kind = kint), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind = kint), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind = kint), intent(in)                                  &
     &                    :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
      integer(kind = kint), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind = kint), intent(in)                                  &
     &                    :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind = kint), intent(in) :: REV_IMPORT(NP-N)
!
      integer (kind=kint), intent(inout):: iX(NP)
!
      integer(kind = kint) :: N_ext
!
!
      N_ext = NP - N
      if(N_ext .le. 0) return
      call calypso_send_recv_int(iflag_SR, N, N_ext,                    &
     &    NEIBPETOT, izero, NEIBPE, STACK_EXPORT, NOD_EXPORT,           &
     &    NEIBPETOT, izero, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
     &    REV_IMPORT, iX(1), iX(N+1))
!
      end subroutine solver_send_recv_int
!
! ----------------------------------------------------------------------
!
      end module calypso_solver_SR
