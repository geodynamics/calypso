!set_local_id_table_4_1ele.f90
!     module set_local_id_table_4_1ele
!
!> @brief  Copy node table in each element from constants
!
!      written by H. Matsui
!
!      subroutine set_inod_in_surf(nnod_4_surf,                         &
!     &          node_on_sf, node_on_sf_n)
!      subroutine copy_inod_in_edge(nnod_4_edge,                        &
!     &          node_on_edge, node_on_edge_sf)
!
      module set_local_id_table_4_1ele
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_inod_in_surf(nnod_4_surf,                          &
     &          node_on_sf, node_on_sf_n)
!
      use m_geometry_constants
!
      integer (kind = kint), intent(in) ::  nnod_4_surf
      integer(kind = kint), intent(inout)                               &
     &                  :: node_on_sf(nnod_4_surf,nsurf_4_ele)
      integer(kind = kint), intent(inout)                               &
     &                  :: node_on_sf_n(nnod_4_surf,nsurf_4_ele)
      integer (kind = kint) :: j
!
!
      do j = 1, nsurf_4_ele
        node_on_sf(1:nnod_4_surf,j) =   node_on_sf_9(1:nnod_4_surf,j)
        node_on_sf_n(1:nnod_4_surf,j) = node_on_sf_n_9(1:nnod_4_surf,j)
      end do
!
      end subroutine set_inod_in_surf
!
!  ---------------------------------------------------------------------
!
      subroutine copy_inod_in_edge(nnod_4_edge,                         &
     &          node_on_edge, node_on_edge_sf)
!
      use m_geometry_constants
!
!
      integer(kind = kint), intent(in) :: nnod_4_edge
      integer(kind = kint), intent(inout)                               &
     &                  :: node_on_edge(nnod_4_edge,nedge_4_ele)
      integer(kind = kint), intent(inout)                               &
     &                  :: node_on_edge_sf(nnod_4_edge,nedge_4_surf)
!
!
       if (nnod_4_edge .eq. 2) then
         node_on_edge(1:nnod_4_edge,1:nedge_4_ele)                      &
     &         =   node_on_edge_l(1:nnod_4_edge,1:nedge_4_ele)
         node_on_edge_sf(1:nnod_4_edge,1:nedge_4_surf)                  &
     &         =   node_on_edge_sf_l(1:nnod_4_edge,1:nedge_4_surf)
!
       else if (nnod_4_edge .eq. 3) then
         node_on_edge(1:nnod_4_edge,1:nedge_4_ele)                      &
     &         =   node_on_edge_q(1:nnod_4_edge,1:nedge_4_ele)
         node_on_edge_sf(1:nnod_4_edge,1:nedge_4_surf)                  &
     &         =   node_on_edge_sf_q(1:nnod_4_edge,1:nedge_4_surf)
       end if
!
      end subroutine copy_inod_in_edge
!
!  ---------------------------------------------------------------------
!
      end module set_local_id_table_4_1ele
