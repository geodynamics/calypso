!
!     module   m_geometry_data
!
!> @brief geometry data for FEM mesh
!!   including node and element position, all connectivities
!
!     written by H. Matsui
!
!      subroutine allocate_geometry_data
!
!      subroutine allocate_node_geometry
!      subroutine allocate_element_connection
!      subroutine deallocate_node_geometry
!      subroutine deallocate_element_connection
!
!      subroutine allocate_element_geometry
!
!      subroutine allocate_surface_connect
!
!      subroutine allocate_edge_connect
!      subroutine allocate_edge_4_ele
!
!      subroutine allocate_ele_4_surf
!      subroutine allocate_ele_4_edge_num
!      subroutine allocate_ele_4_edge_item
!      subroutine allocate_surf_4_edge_num
!      subroutine allocate_surf_4_edge_item
!
!      subroutine deallocate_surface_connect
!
!      subroutine deallocate_edge_connect
!      subroutine deallocate_edge_4_ele
!      subroutine deallocate_ele_4_surf
!      subroutine deallocate_ele_4_edge_item
!      subroutine deallocate_surf_4_edge_item
!
!      subroutine allocate_ext_surface
!      subroutine allocate_iso_surface
!      subroutine allocate_iso_edge
!
!      subroutine deallocate_ext_surface
!      subroutine deallocate_iso_surface
!      subroutine deallocate_iso_edge
!
!>
!>@n@image html 1ele_node.png Node connectivity \p ie(iele,j)
!>                            for each element
!>@n@image html 1ele_edge.png Edge connectivity \p isurf_4_ele(iele,j)
!>                            for each element
!>@n@image html 1ele_surf.png Surface connectivity \p iedge_4_ele(iele,j)
!>                            for each element
!>@n@image html 1quad_ele_img.png Node connectivity \p ie(iele,j)
!>                            for quadrature element
!>
!
      module   m_geometry_data
!
      use m_precision
!
      implicit  none
!
!
!>   position of nodes (i:direction, j:node ID)
      real(kind=kreal)  , allocatable, target  :: xx(:,:)
!>   element connectivity ie(i:element ID,j:element index)
      integer(kind=kint), allocatable, target  :: ie(:,:)
!>   element type defined by the first element
      integer(kind=kint) ::  first_ele_type
!
!
!>   surface connectivity ie_surf(i:surface ID,j:surface index)
      integer(kind=kint), allocatable, target  :: ie_surf(:,:)
!>   edge connectivity ie_edge(i:edge ID,j:surface index)
      integer(kind=kint), allocatable, target  :: ie_edge(:,:)
!
!
!>   surface ID for element surface isf_4_ele(:,:)
!>          ...i:element ID, j:surface ID
!>@n          Positive: normal direction negative: reverse direction
      integer(kind=kint), allocatable, target  :: isf_4_ele(:,:)
!>   rotation ID for element surface isf_rot_ele(:,:)
!>          ...i:element ID, j:surface ID
!>@n          0: normal direction  1-4: rotation flag for reverse surface
      integer(kind=kint), allocatable, target  :: isf_rot_ele(:,:)
!>   edge ID for each surface
      integer(kind=kint), allocatable, target  :: iedge_4_sf(:,:)
!>   edge ID for each element
      integer(kind=kint), allocatable, target  :: iedge_4_ele(:,:)
!>   number of isolated edges
      integer(kind=kint) ::  numedge_iso
!>     isolated edge list
      integer(kind=kint), allocatable  ::  iedge_isolate(:)
!
!
!>   belonged element for surface(surface#,face#,
!>                                1:element or 2:local surface)
      integer(kind=kint), allocatable, target :: iele_4_surf(:,:,:)
!
!>   total number of element list for edge
      integer(kind=kint) :: ntot_iele_4_edge
!>   number of element list for each edge
      integer(kind=kint), allocatable, target :: num_iele_4_edge(:)
!>   end address of element list for each edge
      integer(kind=kint), allocatable, target :: istack_iele_4_edge(:)
!>   element id list for each edge (negative: opposite direction)
      integer(kind=kint), allocatable, target :: iele_4_edge(:,:)
!
!>   total number of surface list for edge
      integer(kind=kint) :: ntot_isurf_4_edge
!>   number of surface list for each edge
      integer(kind=kint), allocatable, target :: num_isurf_4_edge(:)
!>   end address of surface list for each edge
      integer(kind=kint), allocatable, target :: istack_isurf_4_edge(:)
!>   surafce id list for each edge (negative: opposite direction)
      integer(kind=kint), allocatable, target :: isurf_4_edge(:,:)
!
!>     number of external surface
      integer(kind=kint) ::  numsurf_ext
!>     number of isolated surface
      integer(kind=kint) ::  numsurf_iso
!>     external surface list
      integer(kind=kint), allocatable  ::  isf_external(:)
!>     isolated surface list
      integer(kind=kint), allocatable  ::  isf_isolate(:)
!

!>     element type id   (where i:element id)
      integer(kind=kint), allocatable, target  ::  elmtyp(:)
!>     element type id   (where i:element id)
      integer(kind=kint), allocatable, target  ::  nodelm(:)
!>     global node    id (where i:node id)
      integer(kind=kint_gl), allocatable, target  ::  inod_global(:)
!>     global element id (where i:element id)
      integer(kind=kint_gl), allocatable, target  ::  iele_global(:)
!>     global surface id (where i:surface id)
      integer(kind=kint), allocatable, target  ::  isurf_global(:)
!>     global edge id (where i:edge id)
      integer(kind=kint), allocatable, target  ::  iedge_global(:)
!
!>   distance from the center
      real(kind=kreal)  , allocatable, target  :: radius(:)
!>   1/radius
      real(kind=kreal)  , allocatable, target  :: a_radius(:)
!>   longitude of node
      real(kind=kreal)  , allocatable, target  :: longitude(:)
!>   colatitude of node
      real(kind=kreal)  , allocatable, target  :: colatitude(:)
!>   cylindorical radius of node
      real(kind=kreal)  , allocatable, target  :: s_cylinder(:)
!>   1 / (cylindorical radius)
      real(kind=kreal)  , allocatable, target  :: a_s_cylinder(:)
!
!>  integer flag for interior element 1...interior, 0...exterior
      integer(kind = kint), allocatable, target :: interior_ele(:)
!>  double flag for interior element  1.0...interior, 0.0...exterior
      real(kind=kreal)  , allocatable, target  :: e_multi(:)
!
!>  integer flag for interior surface 1...interior, 0...exterior
      integer(kind = kint), allocatable, target :: interior_surf(:)
!>  integer flag for interior edge 1...interior, 0...exterior
      integer(kind = kint), allocatable, target :: interior_edge(:)
!
!
!>   position of centre of element
      real(kind=kreal)  , allocatable, target :: x_ele(:,:)
!>   distance from the centre of element
      real(kind=kreal)  , allocatable, target :: r_ele(:)
!>   1/r_ele
      real(kind=kreal)  , allocatable, target :: ar_ele(:)
!>   longitude of element
      real(kind=kreal)  , allocatable, target :: phi_ele(:)
!>  colatitude of element
      real(kind=kreal)  , allocatable, target :: theta_ele(:)
!>  cylindorical radius of element
      real(kind=kreal)  , allocatable, target :: s_ele(:)
!>  1 / s_ele
      real(kind=kreal)  , allocatable, target :: as_ele(:)
!
!>  Volume of each element
      real (kind=kreal), allocatable, target :: volume_ele(:)
!>  1 / volume of each element
      real (kind=kreal), allocatable, target :: a_vol_ele(:)
!
!
!>  Total volume of domain
      real(kind=kreal) :: volume
!>  1 / volume of domain
      real(kind=kreal) :: a_vol
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_geometry_data
!
      use m_geometry_parameter
!
!
      call allocate_node_geometry
      call allocate_element_connection
      call allocate_element_geometry
!
      end subroutine allocate_geometry_data
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine allocate_node_geometry
!
      use m_geometry_parameter
!
!
      allocate(inod_global(numnod))
      allocate(xx(numnod,3))
!
      allocate(radius(numnod))
      allocate(a_radius(numnod))
      allocate(s_cylinder(numnod))
      allocate(a_s_cylinder(numnod))
      allocate(longitude(numnod))
      allocate(colatitude(numnod))
!
      inod_global = 0
      xx = 0.0d00
!
      radius = 0.0d00
      a_radius = 0.0d00
      s_cylinder = 0.0d00
      a_s_cylinder = 0.0d00
      longitude = 0.0d00
      colatitude = 0.0d00
!
      end subroutine allocate_node_geometry
!
! ------------------------------------------------------
!
      subroutine allocate_element_connection
!
      use m_geometry_parameter
!
!
      allocate(iele_global(numele))
      allocate(elmtyp(numele))
      allocate(nodelm(numele))
      allocate(ie(numele,nnod_4_ele))
!
      iele_global = 0
      elmtyp = 0
      nodelm = 0
      ie = 0
!
      end subroutine allocate_element_connection
!
! ------------------------------------------------------
!
      subroutine deallocate_node_geometry
!
      deallocate(inod_global)
      deallocate(xx)
!
      deallocate(radius)
      deallocate(a_radius)
      deallocate(s_cylinder)
      deallocate(a_s_cylinder)
      deallocate(longitude)
      deallocate(colatitude)
!
      end subroutine deallocate_node_geometry
!
! ------------------------------------------------------
!
      subroutine deallocate_element_connection
!
      deallocate(iele_global)
      deallocate(elmtyp)
      deallocate(nodelm)
      deallocate(ie)
!
      end subroutine deallocate_element_connection
!
! ------------------------------------------------------
! ------------------------------------------------------
!
       subroutine allocate_element_geometry
!
       use m_geometry_parameter
!
        allocate(x_ele(numele,3))
        allocate(r_ele(numele))
        allocate(ar_ele(numele))
        allocate(phi_ele(numele))
        allocate(theta_ele(numele))
        allocate(s_ele(numele))
        allocate(as_ele(numele))
!
        allocate ( interior_ele(numele) )
        allocate ( e_multi(numele) )
!
        allocate( volume_ele (numele))
        allocate( a_vol_ele (numele))
!
       interior_ele = 1
       e_multi = 1.0d0
!
       x_ele = 0.0d0
!
       r_ele = 0.0d0
       ar_ele = 0.0d0
       phi_ele = 0.0d0
       theta_ele = 0.0d0
       s_ele = 0.0d0
       as_ele = 0.0d0
!
       volume_ele = 0.0d0
       a_vol_ele = 0.0d0
!
       end subroutine allocate_element_geometry
!
! ------------------------------------------------------
!
       subroutine deallocate_element_geometry
!
        deallocate(x_ele)
        deallocate(r_ele)
        deallocate(ar_ele)
        deallocate(phi_ele)
        deallocate(theta_ele)
        deallocate(s_ele)
        deallocate(as_ele)
!
        deallocate ( interior_ele )
        deallocate ( e_multi )
!
        deallocate( volume_ele )
        deallocate( a_vol_ele )
!
       end subroutine deallocate_element_geometry
!
! ------------------------------------------------------
!
      subroutine allocate_surface_connect
!
      use m_geometry_constants
      use m_geometry_parameter
!
      allocate( isf_4_ele(numele,nsurf_4_ele) )
      allocate( isf_rot_ele(numele,nsurf_4_ele) )
      allocate( ie_surf(numsurf,nnod_4_surf) )
      allocate( isurf_global(numsurf) )
      allocate( interior_surf(numsurf) )
!
      isf_rot_ele =   0
      ie_surf =       0
      isurf_global =  0
      interior_surf = 0
!
      end subroutine allocate_surface_connect
!
! ------------------------------------------------------
!
      subroutine allocate_edge_connect
!
      use m_geometry_constants
      use m_geometry_parameter
!
      allocate( iedge_4_sf(numsurf,nedge_4_surf) )
      allocate( ie_edge(numedge,nnod_4_edge) )
      allocate( iedge_global(numedge) )
      allocate( interior_edge(numedge) )
!
      iedge_4_sf = 0
      ie_edge = 0
      iedge_global = 0
      interior_edge = 0
!
      end subroutine allocate_edge_connect
!
! ------------------------------------------------------
!
      subroutine allocate_edge_4_ele
!
      use m_geometry_constants
      use m_geometry_parameter
!
      allocate( iedge_4_ele(numele,nedge_4_ele) )
      iedge_4_ele = 0
!
      end subroutine allocate_edge_4_ele
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine allocate_ele_4_surf
!
      use m_geometry_parameter
!
      allocate( iele_4_surf(numsurf,2,2) )
      iele_4_surf = 0
!
      end subroutine allocate_ele_4_surf
!
! ------------------------------------------------------
!
      subroutine allocate_ele_4_edge_num
!
      use m_geometry_parameter
!
      allocate( num_iele_4_edge(numedge) )
      allocate( istack_iele_4_edge(0:numedge) )
      num_iele_4_edge =    0
      istack_iele_4_edge = 0
!
      end subroutine allocate_ele_4_edge_num
!
! ------------------------------------------------------
!
      subroutine allocate_ele_4_edge_item
!
      use m_geometry_parameter
!
      ntot_iele_4_edge = istack_iele_4_edge(numedge)
      allocate( iele_4_edge(ntot_iele_4_edge,2) )
      iele_4_edge = 0
!
      end subroutine allocate_ele_4_edge_item
!
! ------------------------------------------------------
!
      subroutine allocate_surf_4_edge_num
!
      use m_geometry_parameter
!
      allocate( num_isurf_4_edge(numedge) )
      allocate( istack_isurf_4_edge(0:numedge) )
      num_isurf_4_edge =    0
      istack_isurf_4_edge = 0
!
      end subroutine allocate_surf_4_edge_num
!
! ------------------------------------------------------
!
      subroutine allocate_surf_4_edge_item
!
      use m_geometry_parameter
!
      ntot_isurf_4_edge = istack_isurf_4_edge(numedge)
      allocate( isurf_4_edge(ntot_isurf_4_edge,2) )
      isurf_4_edge = 0
!
      end subroutine allocate_surf_4_edge_item
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_surface_connect
!
      deallocate( isf_4_ele, isf_rot_ele)
      deallocate( ie_surf )
      deallocate( isurf_global )
      deallocate( interior_surf )
!
      end subroutine deallocate_surface_connect
!
! ------------------------------------------------------
!
      subroutine deallocate_edge_connect
!
      deallocate( iedge_4_sf )
      deallocate( ie_edge )
      deallocate( iedge_global )
      deallocate( interior_edge )
!
      end subroutine deallocate_edge_connect
!
! ------------------------------------------------------
!
      subroutine deallocate_edge_4_ele
!
      deallocate( iedge_4_ele )
!
      end subroutine deallocate_edge_4_ele
!
! ------------------------------------------------------
!
      subroutine deallocate_ele_4_surf
!
!
      deallocate( iele_4_surf )
!
      end subroutine deallocate_ele_4_surf
!
! ------------------------------------------------------
!
      subroutine deallocate_ele_4_edge_item
!
!
      deallocate(num_iele_4_edge, istack_iele_4_edge, iele_4_edge)
!
      end subroutine deallocate_ele_4_edge_item
!
! ------------------------------------------------------
!
      subroutine deallocate_surf_4_edge_item
!
!
      deallocate( isurf_4_edge, num_isurf_4_edge, istack_isurf_4_edge)
!
      end subroutine deallocate_surf_4_edge_item
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine allocate_ext_surface
!
      allocate( isf_external(numsurf_ext) )
      isf_external = 0
!
      end subroutine allocate_ext_surface
!
! ------------------------------------------------------
!
      subroutine allocate_iso_surface
!
      allocate( isf_isolate(numsurf_iso) )
      isf_isolate = 0
!
      end subroutine allocate_iso_surface
!
! ------------------------------------------------------
!
      subroutine allocate_iso_edge
!
      allocate( iedge_isolate(numedge_iso) )
      iedge_isolate = 0
!
      end subroutine allocate_iso_edge
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_ext_surface
!
      deallocate( isf_external )
!
      end subroutine deallocate_ext_surface
!
! ------------------------------------------------------
!
      subroutine deallocate_iso_surface
!
      deallocate( isf_isolate )
!
      end subroutine deallocate_iso_surface
!
! ------------------------------------------------------
!
      subroutine deallocate_iso_edge
!
      deallocate( iedge_isolate )
!
      end subroutine deallocate_iso_edge
!
! ------------------------------------------------------
!
      end module m_geometry_data
