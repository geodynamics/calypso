!check_geometries.f90
!     module check_geometries
!
!     Written by H. Matsui on Apr., 2006
!     Modified by H. Matsui on June, 2007
!
!!      subroutine check_node_data(id_rank, node)
!!      subroutine check_element_data(id_rank, ele)
!!      subroutine check_surface_data(id_rank, ele, surf)
!!
!!      subroutine check_edge_data(id_rank, surf, edge)
!!      subroutine check_edge_hexa_data(id_rank, ele, edge)
!!
!!      subroutine check_external_surface(id_rank, surf)
!!      subroutine check_iso_surface(id_rank, surf)
!!      subroutine check_bc_edge(id_rank, edge)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!
      module check_geometries
!
      use m_precision
!
      use m_machine_parameter
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine check_node_data(id_rank, node)
!
      type(node_data), intent(in) :: node
      integer, intent(in) :: id_rank
      integer(kind = kint) ::inod
!
!
      write(50+id_rank,*) 'numnod, internal_node'
      write(50+id_rank,'(2i16)') node%numnod, node%internal_node
!
      write(50+id_rank,*) 'inod, position'
      do inod = 1, node%numnod
        write(50+id_rank,'(i16,1p3e25.14)') inod, node%xx(inod,1:3)
      end do
!
      end subroutine check_node_data
!
!   ---------------------------------------------------------------------
!
      subroutine check_element_data(id_rank, ele)
!
      type(element_data), intent(in) :: ele
      integer, intent(in) :: id_rank
      integer(kind = kint) :: iele
!
      write(50+id_rank,*) 'numele, nnod_4_ele'
      write(50+id_rank,'(2i16)') ele%numele, ele%nnod_4_ele
!
      write(50+id_rank,*) 'iele, connection'
      do iele = 1, ele%numele
        write(50+id_rank,'(28i16)')                                     &
     &         iele, ele%ie(iele,1:ele%nnod_4_ele)
      end do
!
      end subroutine check_element_data
!
!   ---------------------------------------------------------------------
!
      subroutine check_surface_data(id_rank, ele, surf)
!
      use m_geometry_constants
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer, intent(in) :: id_rank
      integer(kind = kint) :: isurf, iele
!
!
      write(50+id_rank,*) 'numsurf, nnod_4_surf'
      write(50+id_rank,'(2i16)') surf%numsurf, surf%nnod_4_surf
!
      write(50+id_rank,*) 'isurf, connection'
      do isurf = 1, surf%numsurf
        write(50+id_rank,'(10i16)')                                     &
     &            isurf, surf%ie_surf(isurf,1:surf%nnod_4_surf)
      end do
!
      write(50+id_rank,*) 'numele, nsurf_4_ele'
      write(50+id_rank,'(2i16)') ele%numele, nsurf_4_ele
      write(50+id_rank,*) 'iele, edge ID for surface'
!
      do iele = 1, ele%numele
        write(50+id_rank,'(7i16)')                                     &
     &            iele, surf%isf_4_ele(iele,1:nsurf_4_ele)
      end do
!
!
      end subroutine check_surface_data
!
!   ---------------------------------------------------------------------
!
      subroutine check_edge_data(id_rank, surf, edge)
!
      use m_geometry_constants
!
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      integer, intent(in) :: id_rank
      integer(kind = kint) :: iedge, isurf
!
!
      write(50+id_rank,*) 'numedge, nnod_4_edge'
      write(50+id_rank,'(2i16)') edge%numedge, edge%nnod_4_edge
!
      write(50+id_rank,*) 'iedge, connection'
      do iedge = 1, edge%numedge
        write(50+id_rank,'(4i16)')                                      &
     &             iedge, edge%ie_edge(iedge,1:edge%nnod_4_edge)
      end do
!
!
      write(50+id_rank,*) 'numsurf, nedge_4_surf'
      write(50+id_rank,'(2i16)') surf%numsurf, nedge_4_surf
      write(50+id_rank,*) 'isurf, edge ID for surface'
!
      do isurf = 1, surf%numsurf
        write(50+id_rank,'(5i16)')                                      &
     &            isurf, edge%iedge_4_sf(isurf,1:nedge_4_surf)
      end do
!
      end subroutine check_edge_data
!
!   ---------------------------------------------------------------------
!
      subroutine check_edge_hexa_data(id_rank, ele, edge)
!
      use m_geometry_constants
!
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      integer, intent(in) :: id_rank
      integer(kind = kint) :: iele
!
!
      write(50+id_rank,*) 'numele, nedge_4_ele'
      write(50+id_rank,'(2i16)') ele%numele, nedge_4_ele
      write(50+id_rank,*) 'isurf, edge ID for surface'
!
      do iele = 1, ele%numele
        write(50+id_rank,'(13i16)')                                     &
     &            iele, edge%iedge_4_ele(iele,1:nedge_4_ele)
      end do
!
      end subroutine check_edge_hexa_data
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine check_external_surface(id_rank, surf)
!
      type(surface_data), intent(in) :: surf
      integer, intent(in) :: id_rank
!
!
      write(50+id_rank,*) 'numsurf_ext', surf%numsurf_ext
!
      write(50+id_rank,*) 'isf_external'
      write(50+id_rank,'(8i16)')                                        &
     &         surf%isf_external(1:surf%numsurf_ext)
!
      end subroutine check_external_surface
!
!   ---------------------------------------------------------------------
!
      subroutine check_iso_surface(id_rank, surf)
!
      type(surface_data), intent(in) :: surf
      integer, intent(in) :: id_rank
!
!
      write(50+id_rank,*) 'numsurf_iso', surf%numsurf_iso
!
      write(50+id_rank,*) 'isf_isolate'
      write(50+id_rank,'(8i16)') surf%isf_isolate(1:surf%numsurf_iso)
!
      end subroutine check_iso_surface
!
!   ---------------------------------------------------------------------
!
      subroutine check_bc_edge(id_rank, edge)
!
      type(edge_data), intent(in) :: edge
      integer, intent(in) :: id_rank
!
!
      write(50+id_rank,*) 'numedge_iso', edge%numedge_iso
!
      write(50+id_rank,*) 'iedge_isolate'
      write(50+id_rank,'(8i16)')                                        &
     &                 edge%iedge_isolate(1:edge%numedge_iso)
!
      end subroutine check_bc_edge
!
!   ---------------------------------------------------------------------
!
      end module check_geometries
