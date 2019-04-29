!set_surface_hash.f90
!      module set_surface_hash
!
!      Written by H. Matsui
!
!!      subroutine count_surface_hash(numele, nnod_4_ele, ie, ntot_id,  &
!!     &          num_surf_hash, istack_surf_hash, iend_surf_hash)
!!      subroutine set_surf_hash                                        &
!!     &         (numele, nnod_4_ele, ie, ntot_id, ntot_list,           &
!!     &         num_surf_hash, istack_surf_hash, isurf_hash)
!!
!!      subroutine count_part_surface_hash(numele, nnod_4_ele, ie,      &
!!     &          numele_part, iele_part, ntot_id,                      &
!!     &          num_surf_hash, istack_surf_hash, iend_surf_hash)
!!      subroutine set_part_surf_hash(numele, nnod_4_ele, ie,           &
!!     &          numele_part, iele_part, ntot_id, ntot_list,           &
!!     &          num_surf_hash, istack_surf_hash, isurf_hash)
!
      module set_surface_hash
!
      use m_precision
      use m_geometry_constants
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine count_surface_hash(numele, nnod_4_ele, ie, ntot_id,    &
     &          num_surf_hash, istack_surf_hash, iend_surf_hash)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint_gl), intent(in) :: ntot_id
!
      integer(kind = kint_gl), intent(inout) :: iend_surf_hash
      integer(kind = kint_gl), intent(inout) :: num_surf_hash(ntot_id)
      integer(kind = kint_gl), intent(inout)                            &
     &                     :: istack_surf_hash(0:ntot_id)
!
      integer(kind = kint) :: iele, is
      integer(kind = kint) :: i1, i2, i3, i4
      integer(kind = kint) :: is1, is2, is3, is4
      integer(kind = kint_gl) :: ihash
!
!
      num_surf_hash = 0
      do iele = 1, numele
        do is = 1, nsurf_4_ele
          is1 = node_on_sf_4(1,is)
          is2 = node_on_sf_4(2,is)
          is3 = node_on_sf_4(3,is)
          is4 = node_on_sf_4(4,is)
          i1 = ie(iele,is1)
          i2 = ie(iele,is2)
          i3 = ie(iele,is3)
          i4 = ie(iele,is4)
!
          ihash = int(i1+i2+i3+i4,KIND(ihash))
          num_surf_hash(ihash) = num_surf_hash(ihash) + 1
!
        end do
      end do
!
      istack_surf_hash = 0
      do ihash = 1, ntot_id
        istack_surf_hash(ihash) = istack_surf_hash(ihash-1)             &
     &                           + num_surf_hash(ihash)
        if ( istack_surf_hash(ihash) .le. (nsurf_4_ele*numele) ) then
          iend_surf_hash = ihash
        end if
      end do
!
      end subroutine count_surface_hash
!
!------------------------------------------------------------------
!
      subroutine set_surf_hash                                          &
     &         (numele, nnod_4_ele, ie, ntot_id, ntot_list,             &
     &         num_surf_hash, istack_surf_hash, isurf_hash)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint_gl), intent(in) :: ntot_id, ntot_list
!
      integer(kind = kint_gl), intent(inout) :: num_surf_hash(ntot_id)
      integer(kind = kint_gl), intent(inout)                            &
     &                     :: istack_surf_hash(0:ntot_id)
      integer(kind = kint), intent(inout) :: isurf_hash(ntot_list,2)
!
      integer(kind = kint) :: iele, is
      integer(kind = kint) :: i1, i2, i3, i4
      integer(kind = kint) :: is1, is2, is3, is4
      integer(kind = kint_gl) :: ihash, icou
!
!
      num_surf_hash = 0
      isurf_hash = 0
      do iele = 1, numele
        do is = 1, nsurf_4_ele
          is1 = node_on_sf_4(1,is)
          is2 = node_on_sf_4(2,is)
          is3 = node_on_sf_4(3,is)
          is4 = node_on_sf_4(4,is)
          i1 = ie(iele,is1)
          i2 = ie(iele,is2)
          i3 = ie(iele,is3)
          i4 = ie(iele,is4)
!
          ihash = int(i1+i2+i3+i4, KIND(ihash))
          num_surf_hash(ihash) = num_surf_hash(ihash) + 1
          icou = istack_surf_hash(ihash-1) + num_surf_hash(ihash)
          isurf_hash(icou,1) = iele
          isurf_hash(icou,2) = is
!
        end do
      end do
!
      end subroutine set_surf_hash
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_part_surface_hash(numele, nnod_4_ele, ie,        &
     &          numele_part, iele_part, ntot_id,                        &
     &          num_surf_hash, istack_surf_hash, iend_surf_hash)
!
      integer(kind = kint), intent(in) :: numele, numele_part
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: iele_part(numele_part)
      integer(kind = kint_gl), intent(in) :: ntot_id
!
      integer(kind = kint_gl), intent(inout) :: iend_surf_hash
      integer(kind = kint_gl), intent(inout) :: num_surf_hash(ntot_id)
      integer(kind = kint_gl), intent(inout)                            &
     &                     :: istack_surf_hash(0:ntot_id)
!
      integer(kind = kint) :: inum, iele, is
      integer(kind = kint) :: i1, i2, i3, i4
      integer(kind = kint) :: is1, is2, is3, is4
      integer(kind = kint_gl) :: ihash
!
!
      num_surf_hash = 0
      do inum = 1, numele_part
        iele = iele_part(inum)
        do is = 1, nsurf_4_ele
          is1 = node_on_sf_4(1,is)
          is2 = node_on_sf_4(2,is)
          is3 = node_on_sf_4(3,is)
          is4 = node_on_sf_4(4,is)
          i1 = ie(iele,is1)
          i2 = ie(iele,is2)
          i3 = ie(iele,is3)
          i4 = ie(iele,is4)
!
          ihash = int(i1+i2+i3+i4, KIND(ihash))
          num_surf_hash(ihash) = num_surf_hash(ihash) + 1
!
        end do
      end do
!
      istack_surf_hash = 0
      do ihash = 1, ntot_id
        istack_surf_hash(ihash) = istack_surf_hash(ihash-1)             &
     &                           + num_surf_hash(ihash)
        if ( istack_surf_hash(ihash) .le. (nsurf_4_ele*numele_part) )   &
     &    then
          iend_surf_hash = ihash
        end if
      end do
!
!
      end subroutine count_part_surface_hash
!
!------------------------------------------------------------------
!
      subroutine set_part_surf_hash(numele, nnod_4_ele, ie,             &
     &          numele_part, iele_part, ntot_id, ntot_list,             &
     &          num_surf_hash, istack_surf_hash, isurf_hash)
!
      integer(kind = kint), intent(in) :: numele, numele_part
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: iele_part(numele_part)
      integer(kind = kint_gl), intent(in) :: ntot_id, ntot_list
!
      integer(kind = kint_gl), intent(inout) :: num_surf_hash(ntot_id)
      integer(kind = kint_gl), intent(inout)                            &
     &                     :: istack_surf_hash(0:ntot_id)
      integer(kind = kint), intent(inout) :: isurf_hash(ntot_list,2)
!
      integer(kind = kint) :: inum, iele, is
      integer(kind = kint) :: i1, i2, i3, i4
      integer(kind = kint) :: is1, is2, is3, is4
      integer(kind = kint_gl) :: ihash, icou
!
!
      num_surf_hash = 0
      isurf_hash = 0
      do inum = 1, numele_part
        iele = iele_part(inum)
        do is = 1, nsurf_4_ele
          is1 = node_on_sf_4(1,is)
          is2 = node_on_sf_4(2,is)
          is3 = node_on_sf_4(3,is)
          is4 = node_on_sf_4(4,is)
          i1 = ie(iele,is1)
          i2 = ie(iele,is2)
          i3 = ie(iele,is3)
          i4 = ie(iele,is4)
!
          ihash = int(i1+i2+i3+i4,KIND(ihash))
          num_surf_hash(ihash) = num_surf_hash(ihash) + 1
          icou = istack_surf_hash(ihash-1) + num_surf_hash(ihash)
          isurf_hash(icou,1) = iele
          isurf_hash(icou,2) = is
!
        end do
      end do
!
      end subroutine set_part_surf_hash
!
!------------------------------------------------------------------
!
      end module set_surface_hash
