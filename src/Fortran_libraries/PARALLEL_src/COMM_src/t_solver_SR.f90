!>@file   t_solver_SR.f90
!!@brief  module t_solver_SR
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!@n     Modified in Aug., 2007
!!@n     Modified in Sep., 2013
!!@n     Modified in July, 2020
!
!>@brief  Work area for data communications
!!
!!@verbatim
!!      subroutine resize_work_SR(NB, NPE_SEND, NPE_RECV,               &
!!     &          NTOT_SEND, NTOT_RECV, SR_sig, SR_r)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine resize_work_itp_SR                                   &
!!     &         (NB, NPE_SEND, NPE_RECV, NTOT_RECV, SR_sig, SR_r)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine resize_SR_flag(NPE_SEND, NPE_RECV, SR_sig)
!!      subroutine dealloc_SR_flag(SR_sig)
!!        integer(kind = kint), intent(in)   ::  NPE_SEND, NPE_RECV
!!        type(send_recv_status), intent(inout) :: SR_sig
!!
!!      subroutine calypso_send_recv_fin(npe_send, isend_self, SR_sig)
!!        integer(kind = kint), intent(in) :: npe_send, isend_self
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!!
!!@n @param  NB           Number of components
!!@n @param  NTOT_SEND    Total number of data points for export
!!@n @param  NTOT_RECV    Total number of data points for import
!!@n @param  NPE_SEND      Number of processses to receive
!!@n @param  NPE_RECV      Number of processses to send
!!
!!@n @param  N_SHIFT      number of shifting of the reversed import table
!!@n @param  ITEM_IMPORT  import table
!!@n @param  REV_IMPORT   reversed import table
!
      module t_solver_SR
!
      use m_precision
      use calypso_mpi
!
      implicit none
!
!>      Structure of communication flags
      type send_recv_status
!>        status flag for sending
        integer, allocatable :: sta1(:,:)
!>        status flag for recieving
        integer, allocatable :: sta2(:,:)
!>        status flag for sending
        integer, allocatable :: req1(:  )
!>        status flag for recieving
        integer, allocatable :: req2(:  )
      end type send_recv_status
!
!
!>      Structure of communication buffer for real
      type send_recv_real_buffer
!>        size of send buffer
        integer(kind = kint) :: n_WS = 0
!>        size of kint buffer
        integer(kind = kint) :: n_WR = 0
!
!>         work array for send buffer
        real(kind = kreal), allocatable :: WS(:)
!>         work array for recieve buffer
        real(kind = kreal), allocatable :: WR(:)
      end type send_recv_real_buffer
!
      private :: resize_wsend_SR, resize_wrecv_SR
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine resize_work_SR(NB, NPE_SEND, NPE_RECV,                 &
     &          NTOT_SEND, NTOT_RECV, SR_sig, SR_r)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NB, NTOT_SEND, NTOT_RECV
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_SR_flag(NPE_SEND, NPE_RECV, SR_sig)
      call resize_wsend_SR(NB, NTOT_SEND+1, SR_r)
      call resize_wrecv_SR(NB, NTOT_RECV+1, SR_r)
!
      end subroutine resize_work_SR
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_work_itp_SR                                     &
     &         (NB, NPE_SEND, NPE_RECV, NTOT_RECV, SR_sig, SR_r)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NB, NTOT_RECV
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_SR_flag(NPE_SEND, NPE_RECV, SR_sig)
      call resize_wrecv_SR(NB, NTOT_RECV, SR_r)
!
      end subroutine resize_work_itp_SR
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine calypso_send_recv_fin(npe_send, isend_self, SR_sig)
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer :: ncomm_send
!
!
      ncomm_send = int(npe_send - isend_self)
      if(ncomm_send .gt. 0) then
        call MPI_WAITALL                                                &
     &     (ncomm_send, SR_sig%req1, SR_sig%sta1, ierr_MPI)
      end if
!
      end subroutine calypso_send_recv_fin
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_SR_flag(NPE_SEND, NPE_RECV, SR_sig)
!
      integer(kind = kint), intent(in)   ::  NPE_SEND, NPE_RECV
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      if(allocated(SR_sig%req1)                                         &
     &       .and. (size(SR_sig%req1) .lt. NPE_SEND)) then
        deallocate(SR_sig%sta1, SR_sig%req1)
      end if
      if(allocated(SR_sig%req1) .neqv. .true.) then
        allocate(SR_sig%sta1(MPI_STATUS_SIZE,NPE_SEND))
        allocate(SR_sig%req1(NPE_SEND))
      end if
!
      if(allocated(SR_sig%req2)                                         &
     &      .and. (size(SR_sig%req2) .lt. NPE_RECV)) then
        deallocate(SR_sig%sta2, SR_sig%req2)
      end if
      if(allocated(SR_sig%req2) .neqv. .true.) then
        allocate(SR_sig%sta2(MPI_STATUS_SIZE,NPE_RECV))
        allocate(SR_sig%req2(NPE_RECV))
      end if
!
      end subroutine resize_SR_flag
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_SR_flag(SR_sig)
!
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      deallocate(SR_sig%sta1, SR_sig%req1)
      deallocate(SR_sig%sta2, SR_sig%req2)
!
      end subroutine dealloc_SR_flag
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_wsend_SR(NB, NTOT_SEND, SR_r)
!
      integer(kind=kint), intent(in)   ::  NB, NTOT_SEND
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(allocated(SR_r%WS)                                             &
     &      .and. (size(SR_r%WS) .lt. (NB*NTOT_SEND)) ) then
        deallocate(SR_r%WS)
        SR_r%n_WS = -1
      end if
      if(allocated(SR_r%WS) .eqv. .false.) then
        allocate (SR_r%WS(NB*NTOT_SEND))
        SR_r%n_WS = size(SR_r%WS)
      end if
!
      end subroutine resize_wsend_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_wrecv_SR(NB, NTOT_RECV, SR_r)
!
      integer(kind=kint), intent(in) ::  NB, NTOT_RECV
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(allocated(SR_r%WR)                                             &
     &     .and. (size(SR_r%WR) .lt. (NB*NTOT_RECV)) ) then
        deallocate(SR_r%WR)
        SR_r%n_WR = -1
      end if
      if(allocated(SR_r%WR) .eqv. .false.) then
        allocate (SR_r%WR(NB*NTOT_RECV))
        SR_r%n_WR = size(SR_r%WR)
      end if
!
      end subroutine resize_wrecv_SR
!
! ----------------------------------------------------------------------
!
      end module t_solver_SR
