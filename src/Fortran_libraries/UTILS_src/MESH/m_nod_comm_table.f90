!>@file   m_nod_comm_table.f90
!!@brief  module m_nod_comm_table
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n     Modified in 2006 
!
!> @brief Communication table for node
!!
!!@verbatim
!!      subroutine allocate_neib_id
!!
!!      subroutine allocate_nod_import_num
!!      subroutine allocate_nod_export_num
!!      subroutine allocate_nod_import_item
!!      subroutine allocate_nod_export_item
!!
!!      subroutine deallocate_neib_id
!!
!!      subroutine deallocate_nod_import_item
!!      subroutine deallocate_nod_export_item
!!@endverbatim
!
      module m_nod_comm_table
!
      use m_precision
!
      implicit  none
!
!
!>     number of neighboring domain
      integer(kind = kint) :: num_neib
!>     neighboring pe id
      integer(kind = kint), allocatable, target :: id_neib(:)
!
!
!>     total number of import node 
      integer(kind = kint) :: ntot_import
!>     import node count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable, target :: num_import(:)
!>     import node end point for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable, target :: istack_import(:)
!>     local id for import node                     (i-th)
      integer(kind = kint), allocatable, target :: item_import(:)
!
!>     total number of export node 
      integer(kind = kint) :: ntot_export
!>     export node count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable, target :: num_export(:)
!>     export node end point for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable, target :: istack_export(:)
!>     local id for export node                     (i-th)
      integer(kind = kint), allocatable, target :: item_export(:)
!
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_neib_id
!
      allocate(id_neib(num_neib))
      if (num_neib .gt. 0) id_neib = -1
!
      end subroutine allocate_neib_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_nod_import_num
!
      allocate(num_import(num_neib))
      allocate(istack_import(0:num_neib))
!
      if (num_neib .gt. 0) num_import = 0
      istack_import = 0
!
      end subroutine allocate_nod_import_num
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_export_num
!
      allocate(num_export(num_neib))
      allocate(istack_export(0:num_neib))
!
      if (num_neib .gt. 0) num_export = 0
      istack_export = 0
!
      end subroutine allocate_nod_export_num
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_import_item
!
      ntot_import = istack_import(num_neib)
      allocate(item_import(ntot_import))
!
      if (ntot_import .gt. 0) item_import = 0
!
      end subroutine allocate_nod_import_item
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_export_item
!
      ntot_export = istack_export(num_neib)
      allocate(item_export(ntot_export))
!
      if (ntot_export .gt. 0) item_export = 0
!
      end subroutine allocate_nod_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_neib_id
!
      deallocate(id_neib)
!
      end subroutine deallocate_neib_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_nod_import_item
!
      deallocate(num_import)
      deallocate(istack_import)
      deallocate(item_import)
!
      end subroutine deallocate_nod_import_item
!
!------------------------------------------------------------------
!
      subroutine deallocate_nod_export_item
!
      deallocate(num_export)
      deallocate(istack_export)
      deallocate(item_export)
!
      end subroutine deallocate_nod_export_item
!
!------------------------------------------------------------------
!
      end module m_nod_comm_table
