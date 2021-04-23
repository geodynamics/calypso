!>@file   reverse_SR_int.f90
!!@brief  module reverse_SR_int
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Routines to construca element communication table
!!
!!@verbatim
!!      subroutine num_items_send_recv(npe_send, irank_send, num_send,  &
!!     &                               npe_recv, irank_recv, iflag_self,&
!!     &                               num_recv, istack_recv, ntot_recv)
!!      subroutine comm_items_send_recv                                 &
!!     &         (npe_send, irank_send, istack_send, item_send,         &
!!     &          npe_recv, irank_recv, istack_recv, iflag_self,        &
!!     &          item_recv)
!!      subroutine int8_items_send_recv                                 &
!!     &         (npe_send, irank_send, istack_send, item8_send,        &
!!     &          npe_recv, irank_recv, istack_recv, iflag_self,        &
!!     &          item8_recv)
!!
!!      subroutine local_node_id_reverse_SR(numnod, num_neib, id_neib,  &
!!     &         istack_import, item_import, istack_export, item_export,&
!!     &         item_local, inod_local)
!!@endverbatim
!!
      module reverse_SR_int
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_solver_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine num_items_send_recv(npe_send, irank_send, num_send,    &
     &                               npe_recv, irank_recv, iflag_self,  &
     &                               num_recv, istack_recv, ntot_recv)
!
      use solver_SR_int
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: iflag_self
      integer(kind = kint), intent(in) :: npe_send, npe_recv
      integer(kind = kint), intent(in) :: irank_send(npe_send)
      integer(kind = kint), intent(in) :: irank_recv(npe_recv)
!
      integer(kind = kint), intent(in) :: num_send(npe_send)
!
      integer(kind = kint), intent(inout) :: ntot_recv
      integer(kind = kint), intent(inout) :: num_recv(npe_recv)
      integer(kind = kint), intent(inout) :: istack_recv(0:npe_recv)
!
      type(send_recv_status) :: iSR_sig
!
!
      call resize_SR_flag(npe_send, npe_recv, iSR_sig)
      call calypso_send_recv_num(npe_send, irank_send, num_send,        &
     &                           npe_recv, irank_recv, iflag_self,      &
     &                           num_recv, iSR_sig)
      call s_cal_total_and_stacks(npe_recv, num_recv, izero,            &
     &                            istack_recv, ntot_recv)
      call dealloc_SR_flag(iSR_sig)
!
      end subroutine  num_items_send_recv
!
!-----------------------------------------------------------------------
!
      subroutine comm_items_send_recv                                   &
     &         (npe_send, irank_send, istack_send, item_send,           &
     &          npe_recv, irank_recv, istack_recv, iflag_self,          &
     &          item_recv)
!
      use solver_SR_int
!
      integer(kind = kint), intent(in) :: iflag_self
      integer(kind = kint), intent(in) :: npe_send, npe_recv
      integer(kind = kint), intent(in) :: irank_send(npe_send)
      integer(kind = kint), intent(in) :: irank_recv(npe_recv)
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
      integer(kind = kint), intent(in)                                  &
     &                 :: item_send(istack_send(npe_send))
!
      integer(kind = kint), intent(inout)                               &
     &                 :: item_recv(istack_recv(npe_recv))
!
      type(send_recv_status) :: iSR_sig
!
!
      call resize_SR_flag(npe_send, npe_recv, iSR_sig)
      call calypso_send_recv_intcore                                    &
     &   (npe_send, irank_send, istack_send, item_send, iflag_self,     &
     &    npe_recv, irank_recv, istack_recv, item_recv, iSR_sig)
      call calypso_send_recv_fin(npe_send, iflag_self, iSR_sig)
      call dealloc_SR_flag(iSR_sig)
!
      end subroutine comm_items_send_recv
!
!-----------------------------------------------------------------------
!
      subroutine int8_items_send_recv                                   &
     &         (npe_send, irank_send, istack_send, item8_send,          &
     &          npe_recv, irank_recv, istack_recv, iflag_self,          &
     &          item8_recv)
!
      use solver_SR_int8
!
      integer(kind = kint), intent(in) :: iflag_self
      integer(kind = kint), intent(in) :: npe_send, npe_recv
      integer(kind = kint), intent(in) :: irank_send(npe_send)
      integer(kind = kint), intent(in) :: irank_recv(npe_recv)
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
      integer(kind = kint_gl), intent(in)                               &
     &                 :: item8_send(istack_send(npe_send))
!
      integer(kind = kint_gl), intent(inout)                            &
     &                 :: item8_recv(istack_recv(npe_recv))
!
      type(send_recv_status) :: iSR_sig
!
!
      call resize_SR_flag(npe_send, npe_recv, iSR_sig)
      call calypso_send_recv_i8core                                     &
     &   (npe_send, irank_send, istack_send, item8_send, iflag_self,    &
     &    npe_recv, irank_recv, istack_recv, item8_recv, iSR_sig)
      call calypso_send_recv_fin(npe_send, iflag_self, iSR_sig)
      call dealloc_SR_flag(iSR_sig)
!
      end subroutine int8_items_send_recv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine local_node_id_reverse_SR(numnod,                       &
     &         npe_import, irank_import, istack_import, item_import,    &
     &         npe_export, irank_export, istack_export, item_export,    &
     &         iflag_self, item_local, inod_local)
!
      use solver_SR_int
!
      integer(kind = kint), intent(in) :: numnod
!
      integer(kind = kint), intent(in) :: iflag_self
      integer(kind = kint), intent(in) :: npe_import, npe_export
      integer(kind = kint), intent(in) :: irank_import(npe_import)
      integer(kind = kint), intent(in) :: irank_export(npe_export)
!
      integer(kind = kint), intent(in) :: istack_import(0:npe_import)
      integer(kind = kint), intent(in)                                  &
     &                 :: item_import(istack_import(npe_import))
!
      integer(kind = kint), intent(in) :: istack_export(0:npe_export)
      integer(kind = kint), intent(in)                                  &
     &                 :: item_export(istack_export(npe_export))
!
      integer(kind = kint), intent(inout)                               &
     &                 :: item_local(istack_export(npe_export))
      integer(kind = kint), intent(inout) :: inod_local(numnod)
!
      type(send_recv_status) :: iSR_sig
      integer(kind = kint) :: i, inod
!
!
      call resize_SR_flag(npe_import, npe_export, iSR_sig)
      call calypso_send_recv_intcore(npe_import, irank_import,          &
     &    istack_import, item_import, iflag_self,                       &
     &    npe_export, irank_export, istack_export, item_local,          &
     &    iSR_sig)
      call calypso_send_recv_fin(npe_import, izero, iSR_sig)
      call dealloc_SR_flag(iSR_sig)
!
      inod_local = 0
      do i = 1, istack_export(npe_export)
        inod = item_export(i)
        inod_local(inod) = item_local(i)
      end do
!
      end subroutine local_node_id_reverse_SR
!
!-----------------------------------------------------------------------
!
      end module reverse_SR_int
