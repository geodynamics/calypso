!
!      module m_ele_comm_table
!> @brief Communication table for element
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine allocate_ele_neib_id
!      subroutine deallocate_ele_neib_id
!
!      subroutine allocate_ele_import_num
!      subroutine allocate_ele_export_num
!      subroutine allocate_ele_import_item
!      subroutine allocate_ele_export_item
!
!      subroutine deallocate_ele_import_item
!      subroutine deallocate_ele_export_item
!
      module m_ele_comm_table
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint) :: num_neib_ele
!<     number of neighboring domain
      integer(kind = kint), allocatable :: id_neib_ele(:)
!<     neighboring pe id
!
      integer(kind = kint) :: ntot_import_ele
!<     total number of import element 
      integer(kind = kint), allocatable :: num_import_ele(:)
!<     import element count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: istack_import_ele(:)
!<     import element end point for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: item_import_ele(:)
!<     local id for import element                     (i-th)
!
      integer(kind = kint) :: ntot_export_ele
!<     total number of export element 
      integer(kind = kint), allocatable :: num_export_ele(:)
!<     export element count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: istack_export_ele(:)
!<     export element end point for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: item_export_ele(:)
!<     local id for export element                     (i-th)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_ele_neib_id
!
      allocate(id_neib_ele(num_neib_ele))
!
      if (num_neib_ele .gt. 0) id_neib_ele = -1
!
      end subroutine allocate_ele_neib_id
!
!------------------------------------------------------------------
!
      subroutine deallocate_ele_neib_id
!
      deallocate(id_neib_ele)
!
      end subroutine deallocate_ele_neib_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_ele_import_num
!
!
      allocate(num_import_ele(num_neib_ele))
      allocate(istack_import_ele(0:num_neib_ele))
!
      if (num_neib_ele .gt. 0) num_import_ele = 0
      istack_import_ele = 0
!
      end subroutine allocate_ele_import_num
!
!------------------------------------------------------------------
!
      subroutine allocate_ele_export_num
!
!
      allocate(num_export_ele(num_neib_ele))
      allocate(istack_export_ele(0:num_neib_ele))
!
      if (num_neib_ele .gt. 0) num_export_ele = 0
      istack_export_ele = 0
!
      end subroutine allocate_ele_export_num
!
!------------------------------------------------------------------
!
      subroutine allocate_ele_import_item
!
!
      ntot_import_ele = istack_import_ele(num_neib_ele)
      allocate(item_import_ele(ntot_import_ele))
!
      if (ntot_import_ele .gt. 0) item_import_ele = 0
!
      end subroutine allocate_ele_import_item
!
!------------------------------------------------------------------
!
      subroutine allocate_ele_export_item
!
!
      ntot_export_ele = istack_export_ele(num_neib_ele)
      allocate(item_export_ele(ntot_export_ele))
!
      if (ntot_export_ele .gt. 0) item_export_ele = 0
!
      end subroutine allocate_ele_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_ele_import_item
!
!
      deallocate(num_import_ele)
      deallocate(istack_import_ele)
      deallocate(item_import_ele)
!
      end subroutine deallocate_ele_import_item
!
!------------------------------------------------------------------
!
      subroutine deallocate_ele_export_item
!
!
      deallocate(num_export_ele)
      deallocate(istack_export_ele)
      deallocate(item_export_ele)
!
      end subroutine deallocate_ele_export_item
!
!------------------------------------------------------------------
!
      end module m_ele_comm_table
