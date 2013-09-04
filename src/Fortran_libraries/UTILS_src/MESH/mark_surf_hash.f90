!mark_surf_hash.f90
!      module mark_surf_hash
!
!      Written by H. Matsui
!
!      subroutine mark_all_surfaces(numele, nnod_4_ele, ie)
!
!      subroutine mark_independent_surface(numele, nnod_4_ele, ie)
!      subroutine mark_external_surface(internal_node, numele,          &
!     &          nnod_4_ele, ie)
!
      module mark_surf_hash
!
      use m_precision
!
      use m_surface_hash
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mark_all_surfaces(numele, nnod_4_ele, ie)
!
      use m_geometry_constants
      use compare_indices
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint) :: iele, is
      integer(kind = kint) :: jele, js
      integer(kind = kint) :: i1, i2, i3, i4
      integer(kind = kint) :: j1, j2, j3, j4
      integer(kind = kint) :: is1, is2, is3, is4
      integer(kind = kint) :: js1, js2, js3, js4
      integer(kind = kint) :: ihash, iflag_inside
      integer(kind = kint) :: ist, ied, k1, k2
!
!
      isurf_flag = 0
      do ihash = 1, iend_surf_hash
        ist = istack_surf_hash(ihash-1)+1
        ied = istack_surf_hash(ihash)
!
        if (ied .eq. ist) then
          isurf_flag(ist) = ist
!
        else if (ied .gt. ist) then
!
          do k1 = ist, ied
            if( isurf_flag(k1) .eq. 0 ) then
!
              isurf_flag(k1) = k1
              iele = isurf_hash(k1,1)
              is =   isurf_hash(k1,2)
!
              is1 = node_on_sf_4(1,is)
              is2 = node_on_sf_4(2,is)
              is3 = node_on_sf_4(3,is)
              is4 = node_on_sf_4(4,is)
!
              i1 = ie(iele,is1)
              i2 = ie(iele,is2)
              i3 = ie(iele,is3)
              i4 = ie(iele,is4)
              do k2 = k1+1, ied
                jele = isurf_hash(k2,1)
                js =   isurf_hash(k2,2)
!
                js1 =  node_on_sf_4(1,js)
                js2 =  node_on_sf_4(2,js)
                js3 =  node_on_sf_4(3,js)
                js4 =  node_on_sf_4(4,js)
                j1 = ie(jele,js1)
                j2 = ie(jele,js2)
                j3 = ie(jele,js3)
                j4 = ie(jele,js4)
                iflag_inside = check_4_on_3(i1, i2, i3, j1, j2, j3, j4)
                if (iflag_inside .eq. 1) then
                  isurf_flag(k1) =  k1
                  isurf_flag(k2) = -k1
                  go to 20
                end if
              end do
  20          continue
            end if
          end do
        end if
!
      end do
!
      end subroutine mark_all_surfaces
!
!------------------------------------------------------------------
!
      subroutine mark_independent_surface(numele, nnod_4_ele, ie)
!
      use m_geometry_constants
      use compare_indices
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint) :: iele, is
      integer(kind = kint) :: jele, js
      integer(kind = kint) :: i1, i2, i3, i4
      integer(kind = kint) :: j1, j2, j3, j4
      integer(kind = kint) :: is1, is2, is3, is4
      integer(kind = kint) :: js1, js2, js3, js4
      integer(kind = kint) :: ihash, iflag_inside
      integer(kind = kint) :: ist, ied, k1, k2
!
!
      isurf_flag = 0
      do ihash = 1, iend_surf_hash
        ist = istack_surf_hash(ihash-1)+1
        ied = istack_surf_hash(ihash)
!
        if (ied .gt. ist) then
          do k1 = ist, ied
            iele = isurf_hash(k1,1)
            is =   isurf_hash(k1,2)
            if( isurf_flag(k1) .eq. 0 ) then
              is1 = node_on_sf_4(1,is)
              is2 = node_on_sf_4(2,is)
              is3 = node_on_sf_4(3,is)
              is4 = node_on_sf_4(4,is)
!
              i1 = ie(iele,is1)
              i2 = ie(iele,is2)
              i3 = ie(iele,is3)
              i4 = ie(iele,is4)
              do k2 = k1+1, ied
                jele = isurf_hash(k2,1)
                js =   isurf_hash(k2,2)
!
                js1 =  node_on_sf_4(1,js)
                js2 =  node_on_sf_4(2,js)
                js3 =  node_on_sf_4(3,js)
                js4 =  node_on_sf_4(4,js)
                j1 = ie(jele,js1)
                j2 = ie(jele,js2)
                j3 = ie(jele,js3)
                j4 = ie(jele,js4)
                iflag_inside = check_4_on_3(i1, i2, i3, j1, j2, j3, j4)
                if (iflag_inside .eq. 1) then
                  isurf_flag(k1) =  iflag_inside
                  isurf_flag(k2) =  iflag_inside
                  go to 20
                end if
              end do
  20          continue
            end if
          end do
        end if
!
      end do
!
      end subroutine mark_independent_surface
!
!------------------------------------------------------------------
!
      subroutine mark_external_surface(internal_node, numele,           &
     &          nnod_4_ele, ie)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint) :: iele, is
      integer(kind = kint) :: i1, i2, i3, i4
      integer(kind = kint) :: is1, is2, is3, is4
      integer(kind = kint) :: ihash
      integer(kind = kint) :: ist, ied, k1
!
!
      do ihash = 1, iend_surf_hash
        ist = istack_surf_hash(ihash-1)+1
        ied = istack_surf_hash(ihash)
!
        do k1 = ist, ied
          if( isurf_flag(k1) .eq. 0 ) then
            iele = isurf_hash(k1,1)
            is =   isurf_hash(k1,2)
!
            is1 = node_on_sf_4(1,is)
            is2 = node_on_sf_4(2,is)
            is3 = node_on_sf_4(3,is)
            is4 = node_on_sf_4(4,is)
            i1 = ie(iele,is1)
            i2 = ie(iele,is2)
            i3 = ie(iele,is3)
            i4 = ie(iele,is4)
!
            if ( ie(iele,1).gt.internal_node ) then
                isurf_flag(k1) = 2
            else if ( (i1 .le. internal_node)                           &
     &           .or. (i2 .le. internal_node)                           &
     &           .or. (i3 .le. internal_node)                           &
     &           .or. (i4 .le. internal_node) ) then
              isurf_flag(k1) = 3
            end if
!
          end if
        end do
!
      end do
!
      end subroutine mark_external_surface
!
!------------------------------------------------------------------
!
      end module mark_surf_hash
