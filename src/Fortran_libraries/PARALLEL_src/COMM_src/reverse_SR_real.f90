!>@file   reverse_SR_real.f90
!!@brief  module reverse_SR_real
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!@n     Modified in Aug., 2007
!!@n     Modified in Sep., 2013
!!@n     Modified in July, 2020
!
!>@brief  Data communication for 8-byte integer
!!
!!@verbatim
!!      subroutine real_items_send_recv                                 &
!!     &         (npe_send, irank_send, istack_send, x_send,            &
!!     &          npe_recv, irank_recv, istack_recv, iflag_self, x_recv)
!!        integer(kind = kint), intent(in) :: iflag_self
!!        integer(kind = kint), intent(in) :: npe_send, npe_recv
!!        integer(kind = kint), intent(in) :: irank_send(npe_send)
!!        integer(kind = kint), intent(in) :: irank_recv(npe_recv)
!!        integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!!        integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!!        real(kind = kreal), intent(in) :: x_send(istack_send(npe_send))
!!        real(kind = kreal), intent(inout)                             &
!!     &                 :: x_recv(istack_recv(npe_recv))
!!      subroutine real_items_send_recv_3                               &
!!     &         (npe_send, irank_send, istack_send, x_send,            &
!!     &          npe_recv, irank_recv, istack_recv, iflag_self, x_recv)
!!        integer(kind = kint), intent(in) :: num_neib
!!        integer(kind = kint), intent(in) :: id_neib(num_neib)
!!        integer(kind = kint), intent(in) :: istack_send(0:num_neib)
!!        integer(kind = kint), intent(in) :: istack_recv(0:num_neib)
!!        real(kind = kreal), intent(in)                                &
!!           &                 :: x_send(3*istack_send(num_neib))
!!        real(kind = kreal), intent(inout)                             &
!!      &                 :: x_recv(3*istack_recv(num_neib))
!!@endverbatim
!
      module reverse_SR_real
!
      use m_precision
      use m_constants
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
      subroutine real_items_send_recv                                   &
     &         (npe_send, irank_send, istack_send, x_send,              &
     &          npe_recv, irank_recv, istack_recv, iflag_self, x_recv)
!
      use calypso_SR_core
!
      integer(kind = kint), intent(in) :: iflag_self
      integer(kind = kint), intent(in) :: npe_send, npe_recv
      integer(kind = kint), intent(in) :: irank_send(npe_send)
      integer(kind = kint), intent(in) :: irank_recv(npe_recv)
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
      real(kind = kreal), intent(in) :: x_send(istack_send(npe_send))
!
      real(kind = kreal), intent(inout)                                 &
     &                 :: x_recv(istack_recv(npe_recv))
!
      type(send_recv_status) :: rSR_sig
!
!
      call resize_SR_flag(npe_send, npe_recv, rSR_sig)
      call calypso_send_recv_core                                       &
     &   (ione, npe_send, irank_send, istack_send, x_send(1),           &
     &          npe_recv, irank_recv, istack_recv, iflag_self,          &
     &          x_recv(1), rSR_sig)
      call calypso_send_recv_fin(npe_send, iflag_self, rSR_sig)
      call dealloc_SR_flag(rSR_sig)
!
      end subroutine real_items_send_recv
!
!-----------------------------------------------------------------------
!
      subroutine real_items_send_recv_3                                 &
     &         (npe_send, irank_send, istack_send, x_send,              &
     &          npe_recv, irank_recv, istack_recv, iflag_self, x_recv)
!
      use calypso_SR_core
!
      integer(kind = kint), intent(in) :: iflag_self
      integer(kind = kint), intent(in) :: npe_send, npe_recv
      integer(kind = kint), intent(in) :: irank_send(npe_send)
      integer(kind = kint), intent(in) :: irank_recv(npe_recv)
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
      real(kind = kreal), intent(in)                                    &
     &                 :: x_send(3*istack_send(npe_send))
!
      real(kind = kreal), intent(inout)                                 &
     &                 :: x_recv(3*istack_recv(npe_recv))
!
      type(send_recv_status) :: rSR_sig
!
!
      call resize_SR_flag(npe_send, npe_recv, rSR_sig)
      call calypso_send_recv_core                                       &
     &   (ithree, npe_send, irank_send, istack_send, x_send(1),         &
     &            npe_recv, irank_recv, istack_recv, iflag_self,        &
     &            x_recv(1), rSR_sig)
      call calypso_send_recv_fin(npe_send, iflag_self, rSR_sig)
      call dealloc_SR_flag(rSR_sig)
!
      end subroutine real_items_send_recv_3
!
!-----------------------------------------------------------------------
!
      end module reverse_SR_real
