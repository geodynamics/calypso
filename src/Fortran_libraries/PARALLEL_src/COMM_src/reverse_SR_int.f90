!>@file   reverse_SR_int.f90
!!@brief  module reverse_SR_int
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Routines to construca element communication table
!!
!!@verbatim
!!      subroutine element_num_reverse_SR                               &
!!     &         (num_neib, id_neib, num_import, SR_sig,                &
!!     &          num_export, istack_export, ntot_export)
!!      subroutine reverse_send_recv_int(num_neib, id_neib,             &
!!     &          istack_import, istack_export, inod_import,            &
!!     &          SR_sig, inod_export)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!
!!      subroutine local_node_id_reverse_SR(numnod, num_neib, id_neib,  &
!!     &         istack_import, item_import, istack_export, item_export,&
!!     &         SR_sig, item_local, inod_local)
!!        type(send_recv_status), intent(inout) :: SR_sig
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
      subroutine element_num_reverse_SR                                 &
     &         (num_neib, id_neib, num_import, SR_sig,                  &
     &          num_export, istack_export, ntot_export)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: num_import(num_neib)
!
      type(send_recv_status), intent(inout) :: SR_sig
      integer(kind = kint), intent(inout) :: ntot_export
      integer(kind = kint), intent(inout) :: num_export(num_neib)
      integer(kind = kint), intent(inout) :: istack_export(0:num_neib)
!
      integer(kind = kint) :: ip
!
!
      call resize_SR_flag(num_neib, num_neib, SR_sig)
!
      do ip = 1, num_neib
        call MPI_ISEND(num_import(ip), 1, CALYPSO_INTEGER,              &
     &                 int(id_neib(ip)), 0, CALYPSO_COMM,               &
     &                 SR_sig%req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib
        call MPI_IRECV (num_export(ip), 1, CALYPSO_INTEGER,             &
     &                 int(id_neib(ip)), 0, CALYPSO_COMM,               &
     &                 SR_sig%req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL                                                  &
     &   (int(num_neib), SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (int(num_neib), SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
!
      do ip = 1, num_neib
        istack_export(ip) = istack_export(ip-1) + num_export(ip)
      end do
      ntot_export = istack_export(num_neib)
!
      end subroutine  element_num_reverse_SR
!
!-----------------------------------------------------------------------
!
      subroutine reverse_send_recv_int(num_neib, id_neib,               &
     &          istack_import, istack_export, inod_import,              &
     &          SR_sig, inod_export)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
!
      integer(kind = kint), intent(in)                                  &
     &                 :: inod_import(istack_import(num_neib))
!
      type(send_recv_status), intent(inout) :: SR_sig
      integer(kind = kint), intent(inout)                               &
     &                 :: inod_export(istack_export(num_neib))
!
      integer(kind = kint) :: ip, ist
      integer :: num
!
!
      call resize_SR_flag(num_neib, num_neib, SR_sig)
!
      do ip = 1, num_neib
        ist = istack_import(ip-1)
        num = int(istack_import(ip  ) - istack_import(ip-1))
        call MPI_ISEND(inod_import(ist+1), num,                         &
     &                 CALYPSO_INTEGER,int(id_neib(ip)), 0,             &
     &                 CALYPSO_COMM, SR_sig%req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib
        ist = istack_export(ip-1)
        num = int(istack_export(ip  ) - istack_export(ip-1))
        call MPI_IRECV(inod_export(ist+1), num,                         &
     &                 CALYPSO_INTEGER, int(id_neib(ip)), 0,            &
     &                 CALYPSO_COMM, SR_sig%req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL                                                  &
     &   (int(num_neib), SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (int(num_neib), SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
!
      end subroutine reverse_send_recv_int
!
!-----------------------------------------------------------------------
!
      subroutine local_node_id_reverse_SR(numnod, num_neib, id_neib,    &
     &         istack_import, item_import, istack_export, item_export,  &
     &         SR_sig, item_local, inod_local)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                 :: item_import(istack_import(num_neib))
!
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                 :: item_export(istack_export(num_neib))
!
      type(send_recv_status), intent(inout) :: SR_sig
      integer(kind = kint), intent(inout)                               &
     &                 :: item_local(istack_export(num_neib))
      integer(kind = kint), intent(inout) :: inod_local(numnod)
!
      integer(kind = kint) :: ip, ist, i, inod
      integer :: num
!
!
      call resize_SR_flag(num_neib, num_neib, SR_sig)
!
      do ip = 1, num_neib
        ist = istack_import(ip-1)
        num = int(istack_import(ip  ) - istack_import(ip-1))
        call MPI_ISEND(item_import(ist+1), num,                         &
     &                 CALYPSO_INTEGER, int(id_neib(ip)), 0,            &
     &                 CALYPSO_COMM, SR_sig%req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib
        ist = istack_export(ip-1)
        num = int(istack_export(ip  ) - istack_export(ip-1))
        call MPI_IRECV(item_local(ist+1), num,                          &
     &                 CALYPSO_INTEGER, int(id_neib(ip)), 0,            &
     &                 CALYPSO_COMM, SR_sig%req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL                                                  &
     &   (int(num_neib), SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (int(num_neib), SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
!
      inod_local = 0
      do i = 1, istack_export(num_neib)
        inod = item_export(i)
        inod_local(inod) = item_local(i)
      end do
!
      end subroutine local_node_id_reverse_SR
!
!-----------------------------------------------------------------------
!
      end module reverse_SR_int
