!copy_communication_table.f90
!      module copy_communication_table
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine copy_num_communication(num_neib, id_neib,             &
!     &          istack_import, istack_export, ntot_import, ntot_export,&
!     &          id_neib_org, istack_import_org, istack_export_org)
!      subroutine copy_num_import(num_neib, id_neib,                    &
!     &          istack_import, istack_export, ntot_import, ntot_export,&
!     &          id_neib_org, istack_import_org)
!
!      subroutine copy_num_import_export(num_neib,                      &
!     &          num_import, num_export, istack_import, istack_export)
!
!      subroutine copy_communication_item(ntot_import, ntot_export,     &
!     &          item_import, item_export,                              &
!     &          item_import_org, item_export_org)
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
      subroutine copy_num_communication(num_neib, id_neib,              &
     &          istack_import, istack_export, ntot_import, ntot_export, &
     &          id_neib_org, istack_import_org, istack_export_org)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib_org(num_neib)
      integer(kind = kint), intent(in) :: istack_import_org(0:num_neib)
      integer(kind = kint), intent(in) :: istack_export_org(0:num_neib)
!
      integer(kind = kint), intent(inout) :: id_neib(num_neib)
      integer(kind = kint), intent(inout) :: istack_import(0:num_neib)
      integer(kind = kint), intent(inout) :: istack_export(0:num_neib)
      integer(kind = kint), intent(inout) :: ntot_import, ntot_export
!
      integer(kind = kint) :: i
!
!
      if (num_neib .gt. 0) then
        id_neib(1:num_neib) = id_neib_org(1:num_neib)
!
        istack_import(0:num_neib) = istack_import_org(0:num_neib)
        istack_export(0:num_neib) = istack_export_org(0:num_neib)
        ntot_import = istack_import(num_neib)
        ntot_export = istack_export(num_neib)
      else
        ntot_import = izero
        ntot_export = izero
      end if
!
!
      end subroutine copy_num_communication
!
!-----------------------------------------------------------------------
!
      subroutine copy_num_import(num_neib, id_neib,                     &
     &          istack_import, istack_export, ntot_import, ntot_export, &
     &          id_neib_org, istack_import_org)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib_org(num_neib)
      integer(kind = kint), intent(in) :: istack_import_org(0:num_neib)
!
      integer(kind = kint), intent(inout) :: id_neib(num_neib)
      integer(kind = kint), intent(inout) :: istack_import(0:num_neib)
      integer(kind = kint), intent(inout) :: istack_export(0:num_neib)
      integer(kind = kint), intent(inout) :: ntot_import, ntot_export
!
      integer(kind = kint) :: i
!
!
      if (num_neib .gt. 0) then
        id_neib(1:num_neib) = id_neib_org(1:num_neib)
!
        istack_import(0:num_neib) = istack_import_org(0:num_neib)
        istack_export(0:num_neib) = izero
!
        ntot_import = istack_import(num_neib)
        ntot_export = izero
      else
        ntot_import = izero
        ntot_export = izero
      end if
!
      end subroutine copy_num_import
!
!-----------------------------------------------------------------------
!
      subroutine copy_num_import_export(num_neib,                       &
     &          num_import, num_export, istack_import, istack_export)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
!
      integer(kind = kint), intent(inout) :: num_import(num_neib)
      integer(kind = kint), intent(inout) :: num_export(num_neib)
!
      integer(kind = kint) :: i
!
!
      if (num_neib .gt. 0) then
        do i = 1, num_neib
          num_import(i) = istack_import(i) - istack_import(i-1)
          num_export(i) = istack_export(i) - istack_export(i-1)
        end do
      end if
!
!
      end subroutine copy_num_import_export
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_communication_item(ntot_import, ntot_export,      &
     &          item_import, item_export,                               &
     &          item_import_org, item_export_org)
!
      integer(kind = kint), intent(in) :: ntot_import, ntot_export
      integer(kind = kint), intent(in) :: item_import_org(ntot_import)
      integer(kind = kint), intent(in):: item_export_org(ntot_export)
!
      integer(kind = kint), intent(inout) :: item_import(ntot_import)
      integer(kind = kint), intent(inout) :: item_export(ntot_export)
!
!
      if (ntot_import .gt. 0) then
        item_import(1:ntot_import) = item_import_org(1:ntot_import)
      end if
      if (ntot_export .gt. 0) then
        item_export(1:ntot_export) = item_export_org(1:ntot_export)
      end if
!
      end subroutine copy_communication_item
!
!-----------------------------------------------------------------------
!
      end module copy_communication_table
