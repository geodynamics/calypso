!set_edge_hash.f90
!      module set_edge_hash
!
!      Written by H. Matsui
!
!      subroutine count_edge_hash_4_sf(numnod, numsurf, nnod_4_surf,    &
!     &          nnod_4_edge, ie_surf)
!      subroutine set_edge_hash_4_sf(numsurf, nnod_4_surf, ie_surf)
!      subroutine count_part_edge_hash_4_sf(numnod, numsurf,            &
!     &          numsurf_part, nnod_4_surf, nnod_4_edge,                &
!     &          ie_surf, isf_part)
!      subroutine set_part_edge_hash_4_sf(numsurf, numsurf_part,        &
!     &          nnod_4_surf, ie_surf, isf_part)
!
      module set_edge_hash
!
      use m_precision
!
      use m_geometry_constants
      use m_edge_hash
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine count_edge_hash_4_sf(numnod, numsurf, nnod_4_surf,     &
     &          nnod_4_edge, ie_surf)
!
      integer(kind = kint), intent(in) :: numnod, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
!
      integer(kind = kint) :: isurf, inod1, inod2, is1, is2, k1
      integer(kind = kint) :: ihash
!
!
      do isurf = 1, numsurf
        do k1 = 1, nedge_4_surf
          is1 = node_on_edge_sf_l(1,k1)
          is2 = node_on_edge_sf_l(2,k1)
          ihash = ie_surf(isurf,is1) + ie_surf(isurf,is2)
!
          inum_edge_hash(ihash) = inum_edge_hash(ihash) + 1
!
        end do
      end do
!
      istack_edge_hash = 0
      do ihash = 1, nnod_4_edge*numnod
        istack_edge_hash(ihash) = istack_edge_hash(ihash-1)             &
     &                               + inum_edge_hash(ihash)
        if (istack_edge_hash(ihash) .le. (nedge_4_surf*numsurf) ) then
          iend_edge_hash = ihash
        end if
      end do
!
      end subroutine count_edge_hash_4_sf
!
!------------------------------------------------------------------
!
      subroutine set_edge_hash_4_sf(numsurf, nnod_4_surf, ie_surf)
!
      integer(kind = kint), intent(in) :: numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
!
      integer(kind = kint) :: j
      integer(kind = kint) :: isurf, inod1, inod2, is1, is2, k1
      integer(kind = kint) :: ihash
!
!
      inum_edge_hash = 0
      do isurf = 1, numsurf
        do k1 = 1, nedge_4_surf
          is1 = node_on_edge_sf_l(1,k1)
          is2 = node_on_edge_sf_l(2,k1)
          ihash = ie_surf(isurf,is1) + ie_surf(isurf,is2)
!
          inum_edge_hash(ihash) = inum_edge_hash(ihash) + 1
          j = istack_edge_hash(ihash-1) + inum_edge_hash(ihash)
          iedge_hash(j,1) = isurf
          iedge_hash(j,2) = k1
!
        end do
      end do
!
      end subroutine set_edge_hash_4_sf
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_part_edge_hash_4_sf(numnod, numsurf,             &
     &          numsurf_part, nnod_4_surf, nnod_4_edge,                 &
     &          ie_surf, isf_part)
!
      integer(kind = kint), intent(in) :: numnod, numsurf, numsurf_part
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
      integer(kind = kint), intent(in) :: isf_part(numsurf_part)
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
!
      integer(kind = kint) :: inum, isurf, inod1, inod2, is1, is2, k1
      integer(kind = kint) :: ihash
!
!
      do inum = 1, numsurf_part
        isurf = abs( isf_part(inum) )
        do k1 = 1, nedge_4_surf
          is1 = node_on_edge_sf_l(1,k1)
          is2 = node_on_edge_sf_l(2,k1)
          ihash = ie_surf(isurf,is1) + ie_surf(isurf,is2)
!
          inum_edge_hash(ihash) = inum_edge_hash(ihash) + 1
!
        end do
      end do
!
      istack_edge_hash = 0
      do ihash = 1, nnod_4_edge*numnod
        istack_edge_hash(ihash) = istack_edge_hash(ihash-1)             &
     &                               + inum_edge_hash(ihash)
        if (istack_edge_hash(ihash) .le. (nedge_4_surf*numsurf_part) )  &
     &   then
          iend_edge_hash = ihash
        end if
      end do
!
      end subroutine count_part_edge_hash_4_sf
!
!------------------------------------------------------------------
!
      subroutine set_part_edge_hash_4_sf(numsurf, numsurf_part,         &
     &          nnod_4_surf, ie_surf, isf_part)
!
      integer(kind = kint), intent(in) :: numsurf, numsurf_part
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: isf_part(numsurf_part)
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
!
      integer(kind = kint) :: j
      integer(kind = kint) :: inum, isurf, inod1, inod2, is1, is2, k1
      integer(kind = kint) :: ihash
!
!
      inum_edge_hash = 0
      do inum = 1, numsurf_part
        isurf = abs( isf_part(inum) )
        do k1 = 1, nedge_4_surf
          is1 = node_on_edge_sf_l(1,k1)
          is2 = node_on_edge_sf_l(2,k1)
          ihash = ie_surf(isurf,is1) + ie_surf(isurf,is2)
!
          inum_edge_hash(ihash) = inum_edge_hash(ihash) + 1
          j = istack_edge_hash(ihash-1) + inum_edge_hash(ihash)
          iedge_hash(j,1) = isurf
          iedge_hash(j,2) = k1
!
        end do
      end do
!
      end subroutine set_part_edge_hash_4_sf
!
!------------------------------------------------------------------
!
      end module set_edge_hash
