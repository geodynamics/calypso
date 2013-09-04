!set_edge_data.f90
!      module set_edge_data
!
!      Written by H. Matsui
!
!      subroutine count_num_edges(numedge)
!      subroutine set_edges_connection(numsurf, numedge, nnod_4_surf,   &
!     &          nnod_4_edge, ie_surf, ie_edge, iedge_4_sf,             &
!     &          node_on_edge_sf)
!      subroutine set_edges_connect_4_ele(numele, numsurf, numedge,     &
!     &          nnod_4_ele, nnod_4_edge, ie, iedge_4_sf,               &
!     &          ie_edge, iedge_4_ele)
!
!      subroutine set_part_edges(numsurf, nunmedge_part,                &
!     &          iedge_4_sf, iedge_part)
!
      module set_edge_data
!
      use m_precision
!
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
      subroutine count_num_edges(numedge)
!
      integer(kind = kint), intent(inout)  :: numedge
      integer(kind = kint) :: ihash, k1, ist, ied
!
!
      numedge = 0
      do ihash = 1, iend_edge_hash
        ist = istack_edge_hash(ihash-1)+1
        ied = istack_edge_hash(ihash)
        do k1 = ist, ied
          if (iedge_flag(k1) .eq. k1) numedge = numedge + 1
        end do
      end do
!
      end subroutine count_num_edges
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_edges_connection(numsurf, numedge, nnod_4_surf,    &
     &          nnod_4_edge, ie_surf, ie_edge, iedge_4_sf,              &
     &          node_on_edge_sf)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numsurf, numedge
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_surf(numsurf, nnod_4_surf)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(inout)                               &
     &                     :: iedge_4_sf(numsurf,nedge_4_surf)
      integer(kind = kint), intent(in)                                  &
     &                     :: node_on_edge_sf(nnod_4_edge,nedge_4_surf)
!
      integer(kind = kint) :: k1, k2, ihash, ist, ied
      integer(kind = kint) :: i, isurf, is, iedge
      integer(kind = kint) :: j, jsurf, js
!
!
      iedge = 0
      do ihash = 1, iend_edge_hash
        ist = istack_edge_hash(ihash-1)+1
        ied = istack_edge_hash(ihash)
        do k1 = ist, ied
          if (iedge_flag(k1) .eq. k1) then
            iedge = iedge + 1
!
            isurf = iedge_hash(k1,1)
            is =    iedge_hash(k1,2)
            iedge_4_sf(isurf,is) = iedge
            do i = 1, nnod_4_edge
              j = node_on_edge_sf(i,is)
              ie_edge(iedge,i) = ie_surf(isurf,j)
            end do
!
          end if
        end do
      end do
!
!
!
      do ihash = 1, iend_edge_hash
        ist = istack_edge_hash(ihash-1)+1
        ied = istack_edge_hash(ihash)
        do k1 = ist, ied
!
          if (iedge_flag(k1) .ne. k1) then
!
            k2 = abs(iedge_flag(k1))
            isurf = iedge_hash(k1,1)
            is =   iedge_hash(k1,2)
            jsurf = iedge_hash(k2,1)
            js =   iedge_hash(k2,2)
            iedge_4_sf(isurf,is)                                        &
     &       = iedge_4_sf(jsurf,js) * (iedge_flag(k1) / k2)
!
          end if
        end do
      end do
!
      end subroutine set_edges_connection
!
!------------------------------------------------------------------
!
      subroutine set_edges_connect_4_ele(numele, numsurf, numedge,      &
     &          nnod_4_ele, nnod_4_edge, ie, iedge_4_sf,                &
     &          ie_edge, iedge_4_ele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numele, numsurf, numedge
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                     :: iedge_4_sf(numsurf,nedge_4_surf)
      integer(kind = kint), intent(in)                                  &
     &                     :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: iedge_4_ele(numele,nedge_4_ele)
!
      integer(kind = kint) :: iele, i, is1, is2, inod1, inod2
      integer(kind = kint) :: ihash, ist, ied, k1, isurf, is, iedge
!
!
      do iele = 1, numele
        do i = 1, nedge_4_ele
          is1 = node_on_edge_l(1,i)
          is2 = node_on_edge_l(2,i)
          inod1 = ie(iele,is1)
          inod2 = ie(iele,is2)
          ihash = inod1 + inod2
          ist = istack_edge_hash(ihash-1)+1
          ied = istack_edge_hash(ihash)
          do k1 = ist, ied
            if (iedge_flag(k1) .eq. k1) then
              isurf = iedge_hash(k1,1)
              is =    iedge_hash(k1,2)
              iedge = iedge_4_sf(isurf,is)
              if (      inod1 .eq. ie_edge(iedge,1)                     &
     &            .and. inod2 .eq. ie_edge(iedge,nnod_4_edge) ) then
                iedge_4_ele(iele,i) = iedge
              else if ( inod1 .eq. ie_edge(iedge,nnod_4_edge)           &
     &            .and. inod2 .eq. ie_edge(iedge,1) ) then
                iedge_4_ele(iele,i) = -iedge
              end if
            end if
          end do
        end do
      end do
!
      end subroutine set_edges_connect_4_ele
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_part_edges(numsurf, nunmedge_part,                 &
     &          iedge_4_sf, iedge_part)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nunmedge_part
      integer(kind = kint), intent(in) :: numsurf
      integer(kind = kint), intent(in)                                  &
     &                     :: iedge_4_sf(numsurf,nedge_4_surf)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: iedge_part(nunmedge_part)
!
      integer(kind = kint) :: k1, k2, ihash, ist, ied, iedge
      integer(kind = kint) :: jsurf, js
!
!
      iedge = 0
      do ihash = 1, iend_edge_hash
        ist = istack_edge_hash(ihash-1)+1
        ied = istack_edge_hash(ihash)
        do k1 = ist, ied
!
          if (iedge_flag(k1) .eq. k1) then
!
            iedge = iedge + 1
            k2 = abs(iedge_flag(k1))
!            isurf = iedge_hash(k1,1)
!            is =   iedge_hash(k1,2)
            jsurf = iedge_hash(k2,1)
            js =   iedge_hash(k2,2)
            iedge_part(iedge)                                        &
     &       = iedge_4_sf(jsurf,js) * (iedge_flag(k1) / k2)
!
          end if
        end do
      end do
!
      end subroutine set_part_edges
!
!------------------------------------------------------------------
!
      end module set_edge_data
