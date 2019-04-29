!>@file   copy_communication_table.f90
!!@brief  module copy_communication_table
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief Copy routines of communication table
!!
!!@verbatim
!!      subroutine copy_neighboring_pe(num_neib, id_neib_org, id_neib)
!!      subroutine copy_num_import(num_neib, istack_import_org,         &
!!     &          istack_import, ntot_import)
!!      subroutine copy_num_export(num_neib, istack_export_org,         &
!!     &          istack_export, ntot_export)
!!      subroutine copy_import_item                                     &
!!     &         (ntot_import, item_import_org, item_import)
!!      subroutine copy_export_item                                     &
!!     &         (ntot_export, item_export_org, item_export)
!!@endverbatim
!
      module copy_communication_table
!
      use m_precision
      use m_constants
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_neighboring_pe(num_neib, id_neib_org, id_neib)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib_org(num_neib)
!
      integer(kind = kint), intent(inout) :: id_neib(num_neib)
!
!
      if(num_neib .gt. 0) id_neib(1:num_neib) = id_neib_org(1:num_neib)
!
      end subroutine copy_neighboring_pe
!
!-----------------------------------------------------------------------
!
      subroutine copy_num_import(num_neib, istack_import_org,           &
     &          istack_import, ntot_import)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: istack_import_org(0:num_neib)
!
      integer(kind = kint), intent(inout) :: istack_import(0:num_neib)
      integer(kind = kint), intent(inout) :: ntot_import
!
!
      if (num_neib .gt. 0) then
        istack_import(0:num_neib) = istack_import_org(0:num_neib)
        ntot_import = istack_import(num_neib)
      else
        ntot_import = izero
      end if
!
      end subroutine copy_num_import
!
!-----------------------------------------------------------------------
!
      subroutine copy_num_export(num_neib, istack_export_org,           &
     &          istack_export, ntot_export)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: istack_export_org(0:num_neib)
!
      integer(kind = kint), intent(inout) :: istack_export(0:num_neib)
      integer(kind = kint), intent(inout) :: ntot_export
!
!
      if (num_neib .gt. 0) then
        istack_export(0:num_neib) = istack_export_org(0:num_neib)
        ntot_export = istack_export(num_neib)
      else
        ntot_export = izero
      end if
!
      end subroutine copy_num_export
!
!-----------------------------------------------------------------------
!
      subroutine copy_import_item                                       &
     &         (ntot_import, item_import_org, item_import)
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: item_import_org(ntot_import)
!
      integer(kind = kint), intent(inout) :: item_import(ntot_import)
!
!
      if (ntot_import .gt. 0) then
        item_import(1:ntot_import) = item_import_org(1:ntot_import)
      end if
!
      end subroutine copy_import_item
!
!-----------------------------------------------------------------------
!
      subroutine copy_export_item                                       &
     &         (ntot_export, item_export_org, item_export)
!
      integer(kind = kint), intent(in) :: ntot_export
      integer(kind = kint), intent(in) :: item_export_org(ntot_export)
!
      integer(kind = kint), intent(inout) :: item_export(ntot_export)
!
!
      if (ntot_export .gt. 0) then
        item_export(1:ntot_export) = item_export_org(1:ntot_export)
      end if
!
      end subroutine copy_export_item
!
!-----------------------------------------------------------------------
!
      end module copy_communication_table
