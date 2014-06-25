!check_geometries.f90
!     module check_geometries
!
!     Written by H. Matsui on Apr., 2006
!     Modified by H. Matsui on June, 2007
!
!      subroutine check_node_data(my_rank)
!      subroutine check_element_data(my_rank)
!      subroutine check_surface_data(my_rank)
!
!      subroutine check_edge_data(my_rank)
!      subroutine check_edge_hexa_data(my_rank)
!
!      subroutine check_external_surface(my_rank)
!      subroutine check_iso_surface(my_rank)
!      subroutine check_bc_edge(my_rank)
!
      module check_geometries
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
!
      implicit  none
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine check_node_data(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) ::inod
!
!
      write(50+my_rank,*) 'numnod, internal_node'
      write(50+my_rank,'(2i10)') numnod, internal_node
!
      write(50+my_rank,*) 'inod, position'
      do inod = 1, numnod
        write(50+my_rank,'(i10,1p3e25.14)') inod, xx(inod,1:3)
      end do
!
      end subroutine check_node_data
!
!   ---------------------------------------------------------------------
!
      subroutine check_element_data(my_rank)
!
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: iele
!
      write(50+my_rank,*) 'numele, nnod_4_ele'
      write(50+my_rank,'(2i10)') numele, nnod_4_ele
!
      write(50+my_rank,*) 'iele, connection'
      do iele = 1, numele
        write(50+my_rank,'(30i10)') iele, ie(iele,1:nnod_4_ele)
      end do
!
      end subroutine check_element_data
!
!   ---------------------------------------------------------------------
!
      subroutine check_surface_data(my_rank)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: isurf, iele
!
!
      write(50+my_rank,*) 'numsurf, nnod_4_surf'
      write(50+my_rank,'(2i10)') numsurf, nnod_4_surf
!
      write(50+my_rank,*) 'isurf, connection'
      do isurf = 1, numsurf
        write(50+my_rank,'(30i10)') isurf, ie_surf(isurf,1:nnod_4_surf)
      end do
!
      write(50+my_rank,*) 'numele, nsurf_4_ele'
      write(50+my_rank,'(2i10)') numele, nsurf_4_ele
      write(50+my_rank,*) 'iele, edge ID for surface'
!
      do iele = 1, numele
        write(50+my_rank,'(30i10)')                                     &
     &            iele, isf_4_ele(iele,1:nsurf_4_ele)
      end do
!
!
      end subroutine check_surface_data
!
!   ---------------------------------------------------------------------
!
      subroutine check_edge_data(my_rank)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: iedge, isurf
!
!
      write(50+my_rank,*) 'numedge, nnod_4_edge'
      write(50+my_rank,'(2i10)') numedge, nnod_4_edge
!
      write(50+my_rank,*) 'iedge, connection'
      do iedge = 1, numedge
        write(50+my_rank,'(30i10)') iedge, ie_edge(iedge,1:nnod_4_edge)
      end do
!
!
      write(50+my_rank,*) 'numsurf, nedge_4_surf'
      write(50+my_rank,'(2i10)') numsurf, nedge_4_surf
      write(50+my_rank,*) 'isurf, edge ID for surface'
!
      do isurf = 1, numsurf
        write(50+my_rank,'(30i10)')                                     &
     &            isurf, iedge_4_sf(isurf,1:nedge_4_surf)
      end do
!
      end subroutine check_edge_data
!
!   ---------------------------------------------------------------------
!
      subroutine check_edge_hexa_data(my_rank)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: iele
!
!
      write(50+my_rank,*) 'numele, nedge_4_ele'
      write(50+my_rank,'(2i10)') numele, nedge_4_ele
      write(50+my_rank,*) 'isurf, edge ID for surface'
!
      do iele = 1, numele
        write(50+my_rank,'(30i10)')                                     &
     &            iele, iedge_4_ele(iele,1:nedge_4_ele)
      end do
!
      end subroutine check_edge_hexa_data
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine check_external_surface(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,*) 'numsurf_ext', numsurf_ext
!
      write(50+my_rank,*) 'isf_external'
      write(50+my_rank,'(8i10)') isf_external(1:numsurf_ext)
!
      end subroutine check_external_surface
!
!   ---------------------------------------------------------------------
!
      subroutine check_iso_surface(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,*) 'numsurf_iso', numsurf_iso
!
      write(50+my_rank,*) 'isf_isolate'
      write(50+my_rank,'(8i10)') isf_isolate(1:numsurf_iso)
!
      end subroutine check_iso_surface
!
!   ---------------------------------------------------------------------
!
      subroutine check_bc_edge(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,*) 'numedge_iso', numedge_iso
!
      write(50+my_rank,*) 'iedge_isolate'
      write(50+my_rank,'(8i10)') iedge_isolate(1:numedge_iso)
!
      end subroutine check_bc_edge
!
!   ---------------------------------------------------------------------
!
      end module check_geometries
