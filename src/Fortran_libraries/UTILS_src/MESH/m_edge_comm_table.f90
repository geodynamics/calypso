!
!      module m_edge_comm_table
!
!     Written by H. Matsui on Aug., 2007
!> @brief Communication table for edge
!
!      subroutine allocate_edge_neib_id
!      subroutine deallocate_edge_neib_id
!
!      subroutine allocate_edge_import_num
!      subroutine allocate_edge_export_num
!      subroutine allocate_edge_import_item
!      subroutine allocate_edge_export_item
!
!      subroutine deallocate_edge_import_item
!      subroutine deallocate_edge_export_item
!
      module m_edge_comm_table
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint) :: num_neib_edge
!<     number of neighboring domain
      integer(kind = kint), allocatable :: id_neib_edge(:)
!<     neighboring pe id
!
      integer(kind = kint) :: ntot_import_edge
!<     total number of import edge 
      integer(kind = kint), allocatable :: num_import_edge(:)
!<     import edge count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: istack_import_edge(:)
!<     import edge end point for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: item_import_edge(:)
!<     local id for import edge                     (i-th)
!
      integer(kind = kint) :: ntot_export_edge
!<     total number of export edge 
      integer(kind = kint), allocatable :: num_export_edge(:)
!<     export edge count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: istack_export_edge(:)
!<     export edge end point for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: item_export_edge(:)
!<     local id for export edge                     (i-th)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_edge_neib_id
!
      allocate(id_neib_edge(num_neib_edge))
!
      if (num_neib_edge .gt. 0) id_neib_edge = -1
!
      end subroutine allocate_edge_neib_id
!
!------------------------------------------------------------------
!
      subroutine deallocate_edge_neib_id
!
      deallocate(id_neib_edge)
!
      end subroutine deallocate_edge_neib_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_edge_import_num
!
!
      allocate(num_import_edge(num_neib_edge))
      allocate(istack_import_edge(0:num_neib_edge))
!
      if (num_neib_edge .gt. 0) num_import_edge = 0
      istack_import_edge = 0
!
      end subroutine allocate_edge_import_num
!
!------------------------------------------------------------------
!
      subroutine allocate_edge_export_num
!
!
      allocate(num_export_edge(num_neib_edge))
      allocate(istack_export_edge(0:num_neib_edge))
!
      if (num_neib_edge .gt. 0) num_export_edge = 0
      istack_export_edge = 0
!
      end subroutine allocate_edge_export_num
!
!------------------------------------------------------------------
!
      subroutine allocate_edge_import_item
!
!
      ntot_import_edge = istack_import_edge(num_neib_edge)
      allocate(item_import_edge(ntot_import_edge))
!
      if (ntot_import_edge .gt. 0) item_import_edge = 0
!
      end subroutine allocate_edge_import_item
!
!------------------------------------------------------------------
!
      subroutine allocate_edge_export_item
!
!
      ntot_export_edge = istack_export_edge(num_neib_edge)
      allocate(item_export_edge(ntot_export_edge))
!
      if (ntot_export_edge .gt. 0) item_export_edge = 0
!
      end subroutine allocate_edge_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_edge_import_item
!
!
      deallocate(num_import_edge)
      deallocate(istack_import_edge)
      deallocate(item_import_edge)
!
      end subroutine deallocate_edge_import_item
!
!------------------------------------------------------------------
!
      subroutine deallocate_edge_export_item
!
!
      deallocate(num_export_edge)
      deallocate(istack_export_edge)
      deallocate(item_export_edge)
!
      end subroutine deallocate_edge_export_item
!
!------------------------------------------------------------------
!
      end module m_edge_comm_table
