!
!      module m_surf_comm_table
!> @brief Communication table for surface
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine allocate_surf_neib_id
!      subroutine deallocate_surf_neib_id
!
!      subroutine allocate_surf_import_num
!      subroutine allocate_surf_export_num
!      subroutine allocate_surf_import_item
!      subroutine allocate_surf_export_item
!
!      subroutine deallocate_surf_import_item
!      subroutine deallocate_surf_export_item
!
      module m_surf_comm_table
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint) :: num_neib_surf
!<     number of neighboring domain
      integer(kind = kint), allocatable :: id_neib_surf(:)
!<     neighboring pe id
!
      integer(kind = kint) :: ntot_import_surf
!<     total number of import surface 
      integer(kind = kint), allocatable :: num_import_surf(:)
!<     import surface count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: istack_import_surf(:)
!<     import surface end point for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: item_import_surf(:)
!<     local id for import surface                     (i-th)
!
      integer(kind = kint) :: ntot_export_surf
!<     total number of export surface 
      integer(kind = kint), allocatable :: num_export_surf(:)
!<     export surface count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: istack_export_surf(:)
!<     export surface end point for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: item_export_surf(:)
!<     local id for export surface                     (i-th)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_surf_neib_id
!
      allocate(id_neib_surf(num_neib_surf))
!
      if (num_neib_surf .gt. 0) id_neib_surf = -1
!
      end subroutine allocate_surf_neib_id
!
!------------------------------------------------------------------
!
      subroutine deallocate_surf_neib_id
!
      deallocate(id_neib_surf)
!
      end subroutine deallocate_surf_neib_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_surf_import_num
!
!
      allocate(num_import_surf(num_neib_surf))
      allocate(istack_import_surf(0:num_neib_surf))
!
      if (num_neib_surf .gt. 0) num_import_surf = 0
      istack_import_surf = 0
 !
      end subroutine allocate_surf_import_num
!
!------------------------------------------------------------------
!
      subroutine allocate_surf_export_num
!
!
      allocate(num_export_surf(num_neib_surf))
      allocate(istack_export_surf(0:num_neib_surf))
!
      if (num_neib_surf .gt. 0) num_export_surf = 0
      istack_export_surf = 0
 !
      end subroutine allocate_surf_export_num
!
!------------------------------------------------------------------
!
      subroutine allocate_surf_import_item
!
!
      ntot_import_surf = istack_import_surf(num_neib_surf)
      allocate(item_import_surf(ntot_import_surf))
!
      if (ntot_import_surf .gt. 0) item_import_surf = 0
!
      end subroutine allocate_surf_import_item
!
!------------------------------------------------------------------
!
      subroutine allocate_surf_export_item
!
!
      ntot_export_surf = istack_export_surf(num_neib_surf)
      allocate(item_export_surf(ntot_export_surf))
!
      if (ntot_export_surf .gt. 0) item_export_surf = 0
!
      end subroutine allocate_surf_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_surf_import_item
!
!
      deallocate(num_import_surf)
      deallocate(istack_import_surf)
      deallocate(item_import_surf)
!
      end subroutine deallocate_surf_import_item
!
!------------------------------------------------------------------
!
      subroutine deallocate_surf_export_item
!
!
      deallocate(num_export_surf)
      deallocate(istack_export_surf)
      deallocate(item_export_surf)
!
      end subroutine deallocate_surf_export_item
!
!------------------------------------------------------------------
!
      end module m_surf_comm_table
