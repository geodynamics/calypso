!>@file   make_element_comm_table_SR.f90
!!@brief  module make_element_comm_table_SR
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Routines to construca element communication table
!!
!!@verbatim
!!      subroutine element_data_reverse_SR(num_neib_e, id_neib_e,       &
!!     &          istack_import_e, istack_export_e,                     &
!!     &          inod_import_e, inod_import_l, xe_import,              &
!!     &          inod_export_e, inod_export_l, xe_export)
!!@endverbatim
!!
      module make_element_comm_table_SR
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_solver_SR
!
      implicit none
!
      private :: element_position_reverse_SR
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine element_data_reverse_SR(num_neib_e, id_neib_e,         &
     &          istack_import_e, istack_export_e,                       &
     &          inod_import_e, inod_import_l, xe_import,                &
     &          inod_export_e, inod_export_l, xe_export)
!
      use m_solver_SR
      use reverse_SR_int
      use reverse_SR_int8
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: id_neib_e(num_neib_e)
!
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
!
      integer(kind = kint_gl), intent(in)                               &
     &         :: inod_import_e(istack_import_e(num_neib_e))
      integer(kind = kint), intent(in)                                  &
     &         :: inod_import_l(istack_import_e(num_neib_e))
      real(kind = kreal), intent(in)                                    &
     &         :: xe_import(3*istack_import_e(num_neib_e))
!
      integer(kind = kint_gl), intent(inout)                            &
     &         :: inod_export_e(istack_export_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &         :: inod_export_l(istack_export_e(num_neib_e))
      real(kind = kreal), intent(inout)                                 &
     &         :: xe_export(3*istack_export_e(num_neib_e))
!
      integer(kind = kint) :: ip
!
!      do ip = 1, istack_import_e(num_neib_e)
!        write(*,*) ip, inod_import_e(ip), xe_import(3*ip-2:3*ip)
!      end do
!
!
      call global_id_reverse_SR(num_neib_e, id_neib_e,                  &
     &    istack_import_e, istack_export_e, inod_import_e,              &
     &    SR_sig1, inod_export_e)
!
      call local_element_id_reverse_SR(num_neib_e, id_neib_e,           &
     &    istack_import_e, istack_export_e, inod_import_l,              &
     &    SR_sig1, inod_export_l)
!
      call element_position_reverse_SR(num_neib_e, id_neib_e,           &
     &    istack_import_e, istack_export_e, xe_import, xe_export)
!
      end subroutine element_data_reverse_SR
!
!-----------------------------------------------------------------------
!
      subroutine element_position_reverse_SR(num_neib_e, id_neib_e,     &
     &          istack_import_e, istack_export_e, xe_import, xe_export)
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: id_neib_e(num_neib_e)
!
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
!
      real(kind = kreal), intent(in)                                    &
     &                 :: xe_import(3*istack_import_e(num_neib_e))
!
      real(kind = kreal), intent(inout)                                 &
     &                 :: xe_export(3*istack_export_e(num_neib_e))
!
      integer(kind = kint) :: ip, ist
      integer :: num
!
!
      call resize_work_4_SR(ithree, num_neib_e, num_neib_e,             &
     &    istack_import_e(num_neib_e), istack_export_e(num_neib_e))
!
      do ip = 1, num_neib_e
        ist = 3*istack_import_e(ip-1)
        num = int(3*(istack_import_e(ip  ) - istack_import_e(ip-1)))
        call MPI_ISEND (xe_import(ist+1), num, CALYPSO_REAL,            &
     &                  int(id_neib_e(ip)), 0, CALYPSO_COMM,            &
     &                  req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib_e
        ist = 3* istack_export_e(ip-1)
        num = int(3*(istack_export_e(ip  ) - istack_export_e(ip-1)))
        call MPI_IRECV (xe_export(ist+1), num, CALYPSO_REAL,            &
     &                 int(id_neib_e(ip)), 0, CALYPSO_COMM,             &
     &                 req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL(int(num_neib_e), req2(1), sta2(1,1), ierr_MPI)
      call MPI_WAITALL(int(num_neib_e), req1(1), sta1(1,1), ierr_MPI)
!
      end subroutine element_position_reverse_SR
!
!-----------------------------------------------------------------------
!
      end module make_element_comm_table_SR
