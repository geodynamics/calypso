!>@file   t_calypso_comm_table.f90
!!@brief  module t_calypso_comm_table
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!> @brief Structure for communication table
!!
!!@verbatim
!!      subroutine alloc_calypso_import_num(cps_tbl)
!!      subroutine alloc_calypso_export_num(cps_tbl)
!!      subroutine alloc_calypso_import_item(NP, cps_tbl)
!!      subroutine alloc_calypso_export_item(cps_tbl)
!!        type(calypso_comm_table), intent(inout) :: cps_tbl
!!
!!      subroutine dealloc_calypso_comm_table(cps_tbl)
!!      subroutine dealloc_calypso_import(cps_tbl)
!!      subroutine dealloc_calypso_export(cps_tbl)
!!        type(calypso_comm_table), intent(inout) :: cps_tbl
!!
!!      subroutine empty_calypso_import(NP, cps_tbl)
!!      subroutine empty_calypso_export(cps_tbl)
!!        type(calypso_comm_table), intent(inout) :: cps_tbl
!!@endverbatim
!
      module t_calypso_comm_table
!
      use m_precision
!
      implicit  none
!
!> data structure for communication table
      type calypso_comm_table
!>     number of neighboring domain
        integer(kind = kint) :: nrank_import
!>    total number of import data 
        integer(kind = kint) :: ntot_import
!>    integer flag for self copy
        integer(kind = kint) :: iflag_self_copy = 0
!>     neighboring pe id
        integer(kind = kint), allocatable :: irank_import(:)
!>     import data count for each neighbor pe (i-th pe)
        integer(kind = kint), allocatable :: num_import(:)
!>     import data end point for each neighbor pe (i-th pe)
        integer(kind = kint), allocatable :: istack_import(:)
!>      local id for import data                     (i-th)
        integer(kind = kint), allocatable :: item_import(:)
!>      local id for import data                     (i-th)
        integer(kind = kint), allocatable :: irev_import(:)
!
!>     number of neighboring domain
        integer(kind = kint) :: nrank_export
!>     total number of export data 
        integer(kind = kint) :: ntot_export
!>     neighboring pe id
        integer(kind = kint), allocatable :: irank_export(:)
!>     export data count for each neighbor pe (i-th pe)
        integer(kind = kint), allocatable :: num_export(:)
!>     export data end point for each neighbor pe (i-th pe)
        integer(kind = kint), allocatable :: istack_export(:)
!>     local id for export data                     (i-th)
        integer(kind = kint), allocatable :: item_export(:)
      end type calypso_comm_table
!
      private :: dealloc_calypso_import_num
      private :: dealloc_calypso_export_num
      private :: dealloc_calypso_import_item
      private :: dealloc_calypso_export_item
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_calypso_import_num(cps_tbl)
!
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
      allocate(cps_tbl%irank_import(cps_tbl%nrank_import))
      allocate(cps_tbl%num_import(cps_tbl%nrank_import))
      allocate(cps_tbl%istack_import(0:cps_tbl%nrank_import))
!
      cps_tbl%istack_import = 0
!
      if(cps_tbl%nrank_import .le. 0) return
      cps_tbl%irank_import = 0
      cps_tbl%num_import = 0
!
      end subroutine alloc_calypso_import_num
!
!------------------------------------------------------------------
!
      subroutine alloc_calypso_export_num(cps_tbl)
!
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
      allocate(cps_tbl%irank_export(cps_tbl%nrank_export))
      allocate(cps_tbl%num_export(cps_tbl%nrank_export))
      allocate(cps_tbl%istack_export(0:cps_tbl%nrank_export))
!
      cps_tbl%istack_export = 0
!
      if(cps_tbl%nrank_export .le. 0) return
      cps_tbl%irank_export = 0
      cps_tbl%num_export = 0
!
      end subroutine alloc_calypso_export_num
!
!------------------------------------------------------------------
!
      subroutine alloc_calypso_import_item(NP, cps_tbl)
!
      integer(kind = kint), intent(in) :: NP
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
!
      allocate(cps_tbl%item_import(cps_tbl%ntot_import))
      if (cps_tbl%ntot_import .gt. 0) cps_tbl%item_import = 0
!
      allocate(cps_tbl%irev_import(NP))
      if(NP .gt. 0) cps_tbl%irev_import = 0
!
      end subroutine alloc_calypso_import_item
!
!------------------------------------------------------------------
!
      subroutine alloc_calypso_export_item(cps_tbl)
!
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
!
      allocate(cps_tbl%item_export(cps_tbl%ntot_export))
      if (cps_tbl%ntot_export .gt. 0) cps_tbl%item_export = 0
!
      end subroutine alloc_calypso_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_calypso_comm_table(cps_tbl)
!
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
!
      call dealloc_calypso_export(cps_tbl)
      call dealloc_calypso_import(cps_tbl)
!
      end subroutine dealloc_calypso_comm_table
!
!------------------------------------------------------------------
!
      subroutine dealloc_calypso_import(cps_tbl)
!
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
!
      call dealloc_calypso_import_item(cps_tbl)
      call dealloc_calypso_import_num(cps_tbl)
!
      end subroutine dealloc_calypso_import
!
!------------------------------------------------------------------
!
      subroutine dealloc_calypso_export(cps_tbl)
!
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
!
      call dealloc_calypso_export_item(cps_tbl)
      call dealloc_calypso_export_num(cps_tbl)
!
      end subroutine dealloc_calypso_export
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine empty_calypso_import(NP, cps_tbl)
!
      integer(kind = kint), intent(in) :: NP
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
!
      cps_tbl%iflag_self_copy = 0
      cps_tbl%nrank_import = 0
      call alloc_calypso_import_num(cps_tbl)
!
      cps_tbl%ntot_import = 0
      call alloc_calypso_import_item(NP, cps_tbl)
!
      end subroutine empty_calypso_import
!
!-----------------------------------------------------------------------
!
      subroutine empty_calypso_export(cps_tbl)
!
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
!
      cps_tbl%nrank_export = 0
      call alloc_calypso_export_num(cps_tbl)
!
      cps_tbl%ntot_export = 0
      call alloc_calypso_export_item(cps_tbl)
!
      end subroutine empty_calypso_export
!
!-----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_calypso_import_num(cps_tbl)
!
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
      deallocate(cps_tbl%irank_import)
      deallocate(cps_tbl%num_import)
      deallocate(cps_tbl%istack_import)
!
      end subroutine dealloc_calypso_import_num
!
!------------------------------------------------------------------
!
      subroutine dealloc_calypso_export_num(cps_tbl)
!
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
      deallocate(cps_tbl%irank_export)
      deallocate(cps_tbl%num_export)
      deallocate(cps_tbl%istack_export)
!
      end subroutine dealloc_calypso_export_num
!
!------------------------------------------------------------------
!
      subroutine dealloc_calypso_import_item(cps_tbl)
!
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
      if(allocated(cps_tbl%item_import)) then
        deallocate(cps_tbl%irev_import)
        deallocate(cps_tbl%item_import)
      end if
!
      end subroutine dealloc_calypso_import_item
!
!------------------------------------------------------------------
!
      subroutine dealloc_calypso_export_item(cps_tbl)
!
      type(calypso_comm_table), intent(inout) :: cps_tbl
!
      if(allocated(cps_tbl%item_export)) then
        deallocate(cps_tbl%item_export)
      end if
!
      end subroutine dealloc_calypso_export_item
!
!------------------------------------------------------------------
!
      end module t_calypso_comm_table
