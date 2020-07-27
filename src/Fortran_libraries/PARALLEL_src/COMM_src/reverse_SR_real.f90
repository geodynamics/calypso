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
!!      subroutine reverse_send_recv_3(num_neib, id_neib,               &
!!     &          istack_import, istack_export, x_import,               &
!!     &          SR_sig, x_export)
!!      type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!
      module reverse_SR_real
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
      subroutine reverse_send_recv_3(num_neib, id_neib,                 &
     &          istack_import, istack_export, x_import,                 &
     &          SR_sig, x_export)
!
      use t_solver_SR
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
!
      real(kind = kreal), intent(in)                                    &
     &                 :: x_import(3*istack_import(num_neib))
!
      real(kind = kreal), intent(inout)                                 &
     &                 :: x_export(3*istack_export(num_neib))
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: ip, ist
      integer :: num
!
!
      call resize_SR_flag(num_neib, num_neib, SR_sig)
!
      do ip = 1, num_neib
        ist = 3*istack_import(ip-1)
        num = int(3*(istack_import(ip  ) - istack_import(ip-1)))
        call MPI_ISEND (x_import(ist+1), num, CALYPSO_REAL,             &
     &                  int(id_neib(ip)), 0, CALYPSO_COMM,              &
     &                  SR_sig%req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib
        ist = 3* istack_export(ip-1)
        num = int(3*(istack_export(ip  ) - istack_export(ip-1)))
        call MPI_IRECV (x_export(ist+1), num, CALYPSO_REAL,             &
     &                 int(id_neib(ip)), 0, CALYPSO_COMM,               &
     &                 SR_sig%req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL                                                  &
     &   (int(num_neib), SR_sig%req2(1), SR_sig%sta2(1,1), ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (int(num_neib), SR_sig%req1(1), SR_sig%sta1(1,1), ierr_MPI)
!
      end subroutine reverse_send_recv_3
!
!-----------------------------------------------------------------------
!
      end module reverse_SR_real
