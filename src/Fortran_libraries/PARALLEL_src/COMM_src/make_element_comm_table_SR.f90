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
      use reverse_SR_real
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
      call reverse_send_recv_int8(num_neib_e, id_neib_e,                &
     &    istack_import_e, istack_export_e, inod_import_e,              &
     &    SR_sig1, inod_export_e)
!
      call reverse_send_recv_int(num_neib_e, id_neib_e,                 &
     &    istack_import_e, istack_export_e, inod_import_l,              &
     &    SR_sig1, inod_export_l)
!
      call reverse_send_recv_3(num_neib_e, id_neib_e,                   &
     &    istack_import_e, istack_export_e, xe_import,                  &
     &    SR_sig1, xe_export)
!
      end subroutine element_data_reverse_SR
!
!-----------------------------------------------------------------------
!
      end module make_element_comm_table_SR
