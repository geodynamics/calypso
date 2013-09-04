!
!      module m_edge_hash
!
!      Written by H. Matsui on March, 2006
!
!      subroutine allocate_edge_hash(numnod, numsurf, nnod_4_edge)
!      subroutine cleear_edge_hash
!      subroutine deallocate_edge_hash
!
      module m_edge_hash
!
      use m_precision
!
      implicit none
!
      integer(kind=kint ) :: iend_edge_hash
      integer(kind=kint ), allocatable :: inum_edge_hash(:)
      integer(kind=kint ), allocatable :: istack_edge_hash(:)
      integer(kind=kint ), allocatable :: iedge_hash(:,:)
      integer(kind=kint ), allocatable :: iedge_flag(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_edge_hash(numnod, numsurf, nnod_4_edge)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod, numsurf
      integer(kind = kint), intent(in) :: nnod_4_edge
!
      allocate(inum_edge_hash(nnod_4_edge*numnod))
      allocate(istack_edge_hash(0:nnod_4_edge*numnod))
      allocate(iedge_hash(nedge_4_surf*numsurf,2) )
      allocate(iedge_flag(nedge_4_surf*numsurf) )
!
!
      call cleear_edge_hash
!
      end subroutine allocate_edge_hash
!
!------------------------------------------------------------------
!
      subroutine cleear_edge_hash
!
      inum_edge_hash = 0
      istack_edge_hash = 0
      iedge_hash = 0
      iedge_flag = 0
!
      end subroutine cleear_edge_hash
!
!------------------------------------------------------------------
!
      subroutine deallocate_edge_hash
!
      deallocate(iedge_hash)
      deallocate(iedge_flag)
      deallocate(inum_edge_hash)
      deallocate(istack_edge_hash)
!
      end subroutine deallocate_edge_hash
!
!------------------------------------------------------------------
!
      end module m_edge_hash
