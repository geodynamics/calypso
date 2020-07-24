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
!!      subroutine resize_work_SR_t(NB, NPE_SEND, NPE_RECV,             &
!!     &          NTOT_SEND, NTOT_RECV, SR_sig, SR_r)
!!      subroutine resize_iwork_SR_t(NPE_SEND, NPE_RECV,                &
!!     &          NTOT_SEND, NTOT_RECV, SR_sig, SR_i)
!!      subroutine resize_i8work_SR_t(NPE_SEND, NPE_RECV,               &
!!     &          NTOT_SEND, NTOT_RECV, SR_sig, SR_il)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!
!!      subroutine resize_work_itp_SR_t                                 &
!!     &         (NB, NPE_SEND, NPE_RECV, NTOT_RECV, SR_sig, SR_r)
!!      subroutine resize_iwork_itp_SR_t                                &
!!     &         (NPE_SEND, NPE_RECV, NTOT_RECV, SR_sig, SR_i)
!!      subroutine resize_i8work_itp_SR_t                               &
!!     &         (NPE_SEND, NPE_RECV, NTOT_RECV, SR_sig, SR_il)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
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
!>      Structure of communication buffer for integer
      type send_recv_int_buffer
!>        size of send buffer
        integer(kind = kint) :: n_iWS = 0
!>        size of recieve buffer
        integer(kind = kint) :: n_iWR = 0
!
!>        work array for integer send buffer
        integer(kind = kint), allocatable :: iWS(:)
!>        work array for integer recieve buffer
        integer(kind = kint), allocatable :: iWR(:)
      end type send_recv_int_buffer
!
!
!>      Structure of communication buffer for 8-byte integer
      type send_recv_int8_buffer
!>        size of send buffer
        integer(kind = kint) :: n_i8WS = 0
!>        size of recieve buffer
        integer(kind = kint) :: n_i8WR = 0
!
!>        work array for 8 byte integer send buffer
        integer(kind = kint_gl), allocatable :: i8WS(:)
!>        work array for 8 byte integer recieve buffer
        integer(kind = kint_gl), allocatable :: i8WR(:)
      end type send_recv_int8_buffer
!
!
      private :: resize_flag_4_SR
      private :: resize_wsend_SR, resize_wrecv_SR
!      private :: resize_isend_SR, resize_irecv_SR
!      private :: resize_i8send_SR, resize_i8recv_SR
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine resize_work_SR_t(NB, NPE_SEND, NPE_RECV,               &
     &          NTOT_SEND, NTOT_RECV, SR_sig, SR_r)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NB, NTOT_SEND, NTOT_RECV
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_flag_4_SR(NPE_SEND, NPE_RECV, SR_sig)
      call resize_wsend_SR(NB, NTOT_SEND+1, SR_r)
      call resize_wrecv_SR(NB, NTOT_RECV+1, SR_r)
!
      end subroutine resize_work_SR_t
!
! ----------------------------------------------------------------------
!
      subroutine resize_iwork_SR_t(NPE_SEND, NPE_RECV,                  &
     &          NTOT_SEND, NTOT_RECV, SR_sig, SR_i)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NTOT_SEND, NTOT_RECV
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      call resize_flag_4_SR(NPE_SEND, NPE_RECV, SR_sig)
      call resize_isend_SR(NTOT_SEND+1, SR_i)
      call resize_irecv_SR(NTOT_RECV+1, SR_i)
!
      end subroutine resize_iwork_SR_t
!
! ----------------------------------------------------------------------
!
      subroutine resize_i8work_SR_t(NPE_SEND, NPE_RECV,                 &
     &          NTOT_SEND, NTOT_RECV, SR_sig, SR_il)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NTOT_SEND, NTOT_RECV
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
      call resize_flag_4_SR(NPE_SEND, NPE_RECV, SR_sig)
      call resize_i8send_SR(NTOT_SEND+1, SR_il)
      call resize_i8recv_SR(NTOT_RECV+1, SR_il)
!
      end subroutine resize_i8work_SR_t
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_work_itp_SR_t                                   &
     &         (NB, NPE_SEND, NPE_RECV, NTOT_RECV, SR_sig, SR_r)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NB, NTOT_RECV
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_flag_4_SR(NPE_SEND, NPE_RECV, SR_sig)
      call resize_wrecv_SR(NB, NTOT_RECV, SR_r)
!
      end subroutine resize_work_itp_SR_t
!
! ----------------------------------------------------------------------
!
      subroutine resize_iwork_itp_SR_t                                  &
     &         (NPE_SEND, NPE_RECV, NTOT_RECV, SR_sig, SR_i)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NTOT_RECV
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      call resize_flag_4_SR(NPE_SEND, NPE_RECV, SR_sig)
      call resize_irecv_SR(NTOT_RECV, SR_i)
!
      end subroutine resize_iwork_itp_SR_t
!
! ----------------------------------------------------------------------
!
      subroutine resize_i8work_itp_SR_t                                 &
     &         (NPE_SEND, NPE_RECV, NTOT_RECV, SR_sig, SR_il)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NTOT_RECV
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
      call resize_flag_4_SR(NPE_SEND, NPE_RECV, SR_sig)
      call resize_i8recv_SR(NTOT_RECV, SR_il)
!
      end subroutine resize_i8work_itp_SR_t
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_flag_4_SR(NPE_SEND, NPE_RECV, SR_sig)
!
      use calypso_mpi
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
      end subroutine resize_flag_4_SR
!
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
! ----------------------------------------------------------------------
!
      subroutine resize_isend_SR(NTOT_SEND, SR_i)
!
      integer(kind=kint), intent(in) :: NTOT_SEND
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      if(allocated(SR_i%iWS)                                            &
     &      .and. (size(SR_i%iWS) .lt. (NTOT_SEND)) ) then
        deallocate (SR_i%iWS)
        SR_i%n_iWS = -1
      end if
      if(allocated(SR_i%iWS) .eqv. .false.) then
        allocate (SR_i%iWS(NTOT_SEND))
        SR_i%n_iWS = size(SR_i%iWS)
      end if
!
      end subroutine resize_isend_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_irecv_SR(NTOT_RECV, SR_i)
!
      integer(kind=kint), intent(in) :: NTOT_RECV
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      if(allocated(SR_i%iWR)                                            &
     &     .and. (size(SR_i%iWR) .lt. (NTOT_RECV)) ) then
        deallocate(SR_i%iWR)
        SR_i%n_iWR = -1
      end if
      if(allocated(SR_i%iWR) .eqv. .false.) then
        allocate (SR_i%iWR(NTOT_RECV))
        SR_i%n_iWR = size(SR_i%iWR)
      end if
!
      end subroutine resize_irecv_SR
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_i8send_SR(NTOT_SEND, SR_il)
!
      integer(kind=kint), intent(in) :: NTOT_SEND
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      if(allocated(SR_il%i8WS)                                          &
     &    .and. (size(SR_il%i8WS) .lt. (NTOT_SEND)) ) then
        deallocate (SR_il%i8WS)
        SR_il%n_i8WS = -1
      end if
      if(allocated(SR_il%i8WS) .eqv. .false.) then
        allocate (SR_il%i8WS(NTOT_SEND))
        SR_il%n_i8WS = size(SR_il%i8WS)
     end if
!
      end subroutine resize_i8send_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_i8recv_SR(NTOT_RECV, SR_il)
!
      integer(kind=kint), intent(in) :: NTOT_RECV
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
      if(allocated(SR_il%i8WR)                                          &
     &   .and. (size(SR_il%i8WR).lt.(NTOT_RECV))) then
        deallocate (SR_il%i8WR)
        SR_il%n_i8WR = -1
      end if
      if(allocated(SR_il%i8WR) .eqv. .false.) then
        allocate (SR_il%i8WR(NTOT_RECV))
        SR_il%n_i8WR = size(SR_il%i8WR)
      end if
!
      end subroutine resize_i8recv_SR
!
! ----------------------------------------------------------------------
!
      end module t_solver_SR
