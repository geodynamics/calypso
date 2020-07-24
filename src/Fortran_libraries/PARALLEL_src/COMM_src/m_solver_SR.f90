!>@file   m_solver_SR.f90
!!@brief  module m_solver_SR
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!@n     Modified in Aug., 2007
!!@n     Modified in Sep., 2013
!
!>@brief  Work area for data communications
!!
!!@verbatim
!!      subroutine resize_work_4_SR                                     &
!!     &         (NB, NPE_SEND, NPE_RECV, NTOT_SEND, NTOT_RECV)
!!
!!      subroutine resize_work_itp_SR(NB, NPE_SEND, NPE_RECV, NTOT_RECV)
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
      module m_solver_SR
!
      use m_precision
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
!
      implicit none
!
!>       status flag for sending
      integer, save, allocatable :: sta1(:,:)
!>       status flag for recieving
      integer, save, allocatable :: sta2(:,:)
!>       status flag for sending
      integer, save, allocatable :: req1(:  )
!>       status flag for recieving
      integer, save, allocatable :: req2(:  )
!
!
!>       size of send buffer
      integer(kind = kint) :: n_WS = 0
!>       size of kint buffer
      integer(kind = kint) :: n_WR = 0
!
!>       work array for send buffer
      real(kind = kreal), allocatable :: WS(:)
!>       work array for recieve buffer
      real(kind = kreal), allocatable :: WR(:)
!
!>      Structure of communication flags
      type(send_recv_status), save :: SR_sig1
!
!>      Structure of communication buffer for 8-byte integer
      type(send_recv_int_buffer), save :: SR_i1
!>      Structure of communication buffer for 8-byte integer
      type(send_recv_int8_buffer), save :: SR_il1
!
      private :: resize_flag_4_SR
      private :: resize_wsend_SR, resize_wrecv_SR
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine resize_work_4_SR                                       &
     &         (NB, NPE_SEND, NPE_RECV, NTOT_SEND, NTOT_RECV)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NB, NTOT_SEND, NTOT_RECV
!
!
      call resize_flag_4_SR(NPE_SEND, NPE_RECV)
      call resize_wsend_SR(NB, NTOT_SEND+1)
      call resize_wrecv_SR(NB, NTOT_RECV+1)
!
      end subroutine resize_work_4_SR
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_work_itp_SR(NB, NPE_SEND, NPE_RECV, NTOT_RECV)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NB, NTOT_RECV
!
!
      call resize_flag_4_SR(NPE_SEND, NPE_RECV)
      call resize_wrecv_SR(NB, NTOT_RECV)
!
      end subroutine resize_work_itp_SR
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_flag_4_SR( NPE_SEND, NPE_RECV )
!
      use calypso_mpi
!
      integer(kind = kint) , intent(in)   ::  NPE_SEND, NPE_RECV
!
!
      if (allocated(req1) .and. (size(req1) .lt. NPE_SEND)) then
        deallocate (sta1, req1)
      end if
      if (allocated(req1) .neqv. .true.) then
        allocate (sta1(MPI_STATUS_SIZE,NPE_SEND))
        allocate (req1(NPE_SEND))
      end if
!
      if (allocated(req2) .and. (size(req2) .lt. NPE_RECV)) then
        deallocate (sta2, req2)
      end if
      if (allocated(req2) .neqv. .true.) then
        allocate (sta2(MPI_STATUS_SIZE,NPE_RECV))
        allocate (req2(NPE_RECV))
      end if
!
      end subroutine resize_flag_4_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_wsend_SR(NB, NTOT_SEND)
!
      integer(kind=kint), intent(in)   ::  NB, NTOT_SEND
!
      if(allocated(WS) .and. (size(WS) .lt. (NB*NTOT_SEND)) )           &
     &                                 deallocate(WS)
      if (allocated(WS) .eqv. .false.) allocate (WS(NB*NTOT_SEND))
      n_WS = size(WS)
!
      end subroutine resize_wsend_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_wrecv_SR(NB, NTOT_RECV)
!
      integer(kind=kint), intent(in) ::  NB, NTOT_RECV
!
!
      if(allocated(WR) .and. (size(WR) .lt. (NB*NTOT_RECV)) )           &
     &                              deallocate (WR)
      if(allocated(WR) .eqv. .false.) allocate (WR(NB*NTOT_RECV))
      n_WR = size(WR)
!
      end subroutine resize_wrecv_SR
!
! ----------------------------------------------------------------------
!
      end module m_solver_SR
