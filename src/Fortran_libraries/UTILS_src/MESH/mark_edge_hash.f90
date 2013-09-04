!mark_edge_hash.f90
!      module mark_edge_hash
!
!      Written by H. Matsui
!
!      subroutine mark_all_edges(numsurf, nnod_4_surf, ie_surf)
!
      module mark_edge_hash
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
      subroutine mark_all_edges(numsurf, nnod_4_surf, ie_surf)
!
      integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
!
      integer(kind = kint) :: isurf, iedge, inod1, inod2, is1, is2
      integer(kind = kint) :: jsurf, jedge, jnod1, jnod2, js1, js2
      integer(kind = kint) :: ihash
      integer(kind = kint) :: ist, ied, k1, k2
!
!
      iedge_flag = 0
      do ihash = 1, iend_edge_hash
        ist = istack_edge_hash(ihash-1)+1
        ied = istack_edge_hash(ihash)
        if (ied .eq. ist) then
          iedge_flag(ist) = ist
        else if (ied .gt. ist) then
          do k1 = ist, ied
            if( iedge_flag(k1) .eq. 0 ) then
              iedge_flag(k1) = k1
              isurf = iedge_hash(k1,1)
              iedge = iedge_hash(k1,2)
              is1 = node_on_edge_sf_l(1,iedge)
              is2 = node_on_edge_sf_l(2,iedge)
              inod1 = ie_surf(isurf,is1)
              inod2 = ie_surf(isurf,is2)
              do k2 = k1+1, ied
                jsurf = iedge_hash(k2,1)
                jedge = iedge_hash(k2,2)
                js1 = node_on_edge_sf_l(1,jedge)
                js2 = node_on_edge_sf_l(2,jedge)
                jnod1 = ie_surf(jsurf,js1)
                jnod2 = ie_surf(jsurf,js2)
                if ( (inod2-inod1) .eq. (jnod2-jnod1) ) then
                  iedge_flag(k2) = k1
                else if ( (inod2-inod1) .eq. (jnod1-jnod2) ) then
                  iedge_flag(k2) =-k1
                end if
              end do
            end if
          end do
        end if
      end do
!
      end subroutine mark_all_edges
!
!------------------------------------------------------------------
!
      end module mark_edge_hash
