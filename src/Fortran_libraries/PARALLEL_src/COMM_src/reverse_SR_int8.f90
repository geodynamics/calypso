!>@file   reverse_SR_int8.f90
!!@brief  module reverse_SR_int8
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
!!      subroutine global_id_reverse_SR                                 &
!!     &         (num_neib, id_neib, istack_import, istack_export,      &
!!     &          inod_import, SR_sig, inod_export)
!!      type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!!
!!@n @param  NB           Number of components
!!@n @param  NTOT_SEND    Total number of data points for export
!!@n @param  NTOT_RECV    Total number of data points for import
!!@n @param  NPE_SEND      Number of processses to receive
!!@n @param  NPE_RECV      Number of processses to send
!!
!!@n @param  ITEM_IMPORT  import table
!!@n @param  REV_IMPORT   reversed import table
!
      module reverse_SR_int8
!
      use m_precision
      use calypso_mpi
      use t_solver_SR
      use t_solver_SR_int8
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine global_id_reverse_SR                                   &
     &         (num_neib, id_neib, istack_import, istack_export,        &
     &          inod_import, SR_sig, inod_export)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
!
      integer(kind = kint_gl), intent(in)                               &
     &                 :: inod_import(istack_import(num_neib))
!
      integer(kind = kint_gl), intent(inout)                            &
     &                 :: inod_export(istack_export(num_neib))
      type(send_recv_status), intent(inout) :: SR_sig
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
     &                 CALYPSO_GLOBAL_INT, int(id_neib(ip)), 0,         &
     &                 CALYPSO_COMM, SR_sig%req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib
        ist = istack_export(ip-1)
        num = int(istack_export(ip  ) - istack_export(ip-1))
        call MPI_IRECV(inod_export(ist+1), num,                         &
     &                 CALYPSO_GLOBAL_INT, int(id_neib(ip)), 0,         &
     &                 CALYPSO_COMM, SR_sig%req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL                                                  &
     &   (int(num_neib), SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (int(num_neib), SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
!
      end subroutine global_id_reverse_SR
!
!-----------------------------------------------------------------------
!
      end module reverse_SR_int8
