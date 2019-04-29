!> @file  solver_SR_type.f90
!!      module solver_SR_type
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Data communication using communication table structure
!!
!!@verbatim
!!      subroutine SOLVER_SEND_RECV_type(NP, comm_tbl, X)
!!      subroutine SOLVER_SEND_RECV_3_type(NP, comm_tbl, X)
!!      subroutine SOLVER_SEND_RECV_6_type(NP, comm_tbl, X)
!!      subroutine SOLVER_SEND_RECV_N_type(NP, NB, comm_tbl, X)
!!
!!      subroutine SOLVER_SEND_RECV_int_type(NP, comm_tbl, iX)
!!      subroutine SOLVER_SEND_RECV_int8_type(NP, comm_tbl, i8X)
!!      subroutine SOLVER_SEND_RECV_num_type(comm_tbl, nSEND, nRECV)
!!        type(communication_table), intent(in) :: comm_tbl
!!        integer(kind = kint), intent(in) :: NP
!!        real(kind = kreal), intent(inout) :: X(NP)
!!        integer(kind = kint), intent(inout) :: iX(NP)
!!        integer(kind = kint_gl), intent(inout) :: i8X(NP)
!!        integer(kind = kint), intent(in) :: nSEND(comm_tbl%num_neib)
!!        integer(kind = kint), intent(inout) :: nRECV(comm_tbl%num_neib)
!!@endverbatim
!
      module solver_SR_type
!
      use m_precision
      use m_work_time
      use m_elapsed_labels_SEND_RECV
!
      implicit none
!
      logical, save, private :: iflag_F3D_time = .FALSE.
      integer(kind = kint), save, private :: ist_elapsed_F3D = 0
      integer(kind = kint), save, private :: ied_elapsed_F3D = 0
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine SOLVER_SEND_RECV_type(NP, comm_tbl, X)
!
      use t_comm_table
      use solver_SR
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP
!
      real(kind = kreal), intent(inout) :: X(NP)
!
!
      if(iflag_FSR_time) call start_elapsed_time(ist_elapsed_FSR+1)
      call SOLVER_SEND_RECV                                             &
     &   (NP, comm_tbl%num_neib, comm_tbl%id_neib,                      &
     &    comm_tbl%istack_import, comm_tbl%item_import,                 &
     &    comm_tbl%istack_export, comm_tbl%item_export, X(1) )
      if(iflag_FSR_time) call end_elapsed_time(ist_elapsed_FSR+1)
!
      end subroutine SOLVER_SEND_RECV_type
!
! ----------------------------------------------------------------------
!
      subroutine SOLVER_SEND_RECV_3_type(NP, comm_tbl, X)
!
      use t_comm_table
      use solver_SR_3
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP
!
      real(kind = kreal), intent(inout) :: X(3*NP)
!
!
      if(iflag_FSR_time) call start_elapsed_time(ist_elapsed_FSR+1)
      call SOLVER_SEND_RECV_3                                           &
     &   (NP, comm_tbl%num_neib, comm_tbl%id_neib,                      &
     &    comm_tbl%istack_import, comm_tbl%item_import,                 &
     &    comm_tbl%istack_export, comm_tbl%item_export, X(1) )
      if(iflag_FSR_time) call end_elapsed_time(ist_elapsed_FSR+1)
!
      end subroutine SOLVER_SEND_RECV_3_type
!
! ----------------------------------------------------------------------
!
      subroutine SOLVER_SEND_RECV_6_type(NP, comm_tbl, X)
!
      use t_comm_table
      use solver_SR_6
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP
!
      real(kind = kreal), intent(inout) :: X(6*NP)
!
!
      if(iflag_FSR_time) call start_elapsed_time(ist_elapsed_FSR+1)
      call SOLVER_SEND_RECV_6                                           &
     &   (NP, comm_tbl%num_neib, comm_tbl%id_neib,                      &
     &   comm_tbl%istack_import, comm_tbl%item_import,                  &
     &   comm_tbl%istack_export, comm_tbl%item_export, X(1) )
      if(iflag_FSR_time) call end_elapsed_time(ist_elapsed_FSR+1)
!
      end subroutine SOLVER_SEND_RECV_6_type
!
! ----------------------------------------------------------------------
!
      subroutine SOLVER_SEND_RECV_N_type(NP, NB, comm_tbl, X)
!
      use t_comm_table
      use solver_SR_N
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP, NB
!
      real(kind = kreal), intent(inout) :: X(NB*NP)
!
!
      if(iflag_FSR_time) call start_elapsed_time(ist_elapsed_FSR+1)
      call SOLVER_SEND_RECV_N                                           &
     &   (NP, NB, comm_tbl%num_neib, comm_tbl%id_neib,                  &
     &   comm_tbl%istack_import, comm_tbl%item_import,                  &
     &   comm_tbl%istack_export, comm_tbl%item_export, X(1))
      if(iflag_FSR_time) call end_elapsed_time(ist_elapsed_FSR+1)
!
      end subroutine SOLVER_SEND_RECV_N_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SOLVER_SEND_RECV_int_type(NP, comm_tbl, iX)
!
      use t_comm_table
      use solver_SR_int
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP
!
      integer(kind = kint), intent(inout) :: iX(NP)
!
!
      if(iflag_FSR_time) call start_elapsed_time(ist_elapsed_FSR+1)
      call SOLVER_SEND_RECV_i                                           &
     &   (NP, comm_tbl%num_neib, comm_tbl%id_neib,                      &
     &    comm_tbl%istack_import, comm_tbl%item_import,                 &
     &    comm_tbl%istack_export, comm_tbl%item_export, iX(1))
      if(iflag_FSR_time) call end_elapsed_time(ist_elapsed_FSR+1)
!
      end subroutine SOLVER_SEND_RECV_int_type
!
! ----------------------------------------------------------------------
!
      subroutine SOLVER_SEND_RECV_int8_type(NP, comm_tbl, i8X)
!
      use t_comm_table
      use solver_SR_int
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP
!
      integer(kind = kint_gl), intent(inout) :: i8X(NP)
!
!
      if(iflag_FSR_time) call start_elapsed_time(ist_elapsed_FSR+1)
      call SOLVER_SEND_RECV_i8                                          &
     &   (NP, comm_tbl%num_neib, comm_tbl%id_neib,                      &
     &   comm_tbl%istack_import, comm_tbl%item_import,                  &
     &   comm_tbl%istack_export, comm_tbl%item_export, i8X(1))
      if(iflag_FSR_time) call end_elapsed_time(ist_elapsed_FSR+1)
!
      end subroutine SOLVER_SEND_RECV_int8_type
!
! ----------------------------------------------------------------------
!
      subroutine SOLVER_SEND_RECV_num_type(comm_tbl, nSEND, nRECV)
!
      use t_comm_table
      use solver_SR_int
!
      type(communication_table), intent(in) :: comm_tbl
!
      integer(kind = kint), intent(in) :: nSEND(comm_tbl%num_neib)
      integer(kind = kint), intent(inout) :: nRECV(comm_tbl%num_neib)
!
!
      if(iflag_FSR_time) call start_elapsed_time(ist_elapsed_FSR+1)
      call SOLVER_SEND_RECV_num                                         &
     &   (comm_tbl%num_neib, comm_tbl%id_neib, nSEND(1), nRECV(1))
      if(iflag_FSR_time) call end_elapsed_time(ist_elapsed_FSR+1)
!
      end subroutine SOLVER_SEND_RECV_num_type
!
! ----------------------------------------------------------------------
!
      end module solver_SR_type
