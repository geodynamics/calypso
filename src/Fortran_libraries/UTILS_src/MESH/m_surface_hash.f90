!
!      module m_surface_hash
!
!      Written by H. Matsui on March, 2006
!
!      subroutine allocate_surface_hash(numnod, numele, nnod_4_surf)
!      subroutine deallocate_surface_hash
!
      module m_surface_hash
!
      use m_precision
!
      implicit none
!
      integer(kind=kint ) :: iend_surf_hash
      integer(kind=kint ), allocatable :: inum_surf_hash(:)
      integer(kind=kint ), allocatable :: istack_surf_hash(:)
      integer(kind=kint ), allocatable :: isurf_hash(:,:)
      integer(kind=kint ), allocatable :: isurf_flag(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_surface_hash(numnod, numele, nnod_4_surf)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: nnod_4_surf
!
!
      allocate(inum_surf_hash(nnod_4_surf*numnod))
      allocate(istack_surf_hash(0:nnod_4_surf*numnod))
      allocate(isurf_hash(nsurf_4_ele*numele,2) )
      allocate(isurf_flag(nsurf_4_ele*numele) )
!
      call clear_surface_hash
!
      end subroutine allocate_surface_hash
!
!------------------------------------------------------------------
!
      subroutine clear_surface_hash
!
      inum_surf_hash = 0
      istack_surf_hash = 0
      isurf_hash = 0
      isurf_flag = 0
!
      end subroutine clear_surface_hash
!
!------------------------------------------------------------------
!
      subroutine deallocate_surface_hash
!
      deallocate(isurf_hash)
      deallocate(isurf_flag)
      deallocate(inum_surf_hash)
      deallocate(istack_surf_hash)
!
      end subroutine deallocate_surface_hash
!
!------------------------------------------------------------------
!
      end module m_surface_hash
