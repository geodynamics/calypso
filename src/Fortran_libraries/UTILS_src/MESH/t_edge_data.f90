!>@file   t_edge_data.f90
!!@brief  module t_edge_data
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!> @brief Structure of edge geometry data
!!
!!@verbatim
!!      subroutine alloc_numedge_stack(nprocs, edge)
!!      subroutine allocate_inod_in_edge(edge)
!!      subroutine allocate_edge_connect_type(edge, nsurf)
!!      subroutine allocate_edge_4_ele_type(edge, nele)
!!      subroutine allocate_iso_edge_type(edge)
!!      subroutine allocate_edge_geom_type(edge)
!!      subroutine allocate_edge_vect_type(edge)
!!      subroutine allocate_edge_vect_sph_type(edge)
!!      subroutine allocate_edge_vect_cyl_type(edge)
!!      subroutine allocate_edge_param_smp_type(edge)
!!
!!      subroutine alloc_ele_4_edge_num_type(edge)
!!      subroutine alloc_ele_4_edge_item_type(edge)
!!      subroutine alloc_surf_4_edge_num_type(edge)
!!      subroutine alloc_surf_4_edge_item_type(edge)
!!
!!
!!      subroutine dealloc_numedge_stack(nprocs, edge)
!!      subroutine deallocate_inod_in_edge_type(edge)
!!      subroutine deallocate_edge_connect_type(edge)
!!      subroutine deallocate_edge_4_ele_type(edge)
!!      subroutine deallocate_iso_edge_type(edge)
!!      subroutine deallocate_edge_geom_type(edge)
!!      subroutine deallocate_edge_vect_type(edge)
!!      subroutine deallocate_edge_vect_sph_type(edge)
!!      subroutine deallocate_edge_vect_cyl_type(edge)
!!      subroutine deallocate_edge_param_smp_type(edge)
!!
!!      subroutine dealloc_ele_4_edge_num_type(edge)
!!      subroutine dealloc_ele_4_edge_item_type(edge)
!!      subroutine dealloc_surf_4_edge_num_type(edge)
!!      subroutine dealloc_surf_4_edge_item_type(edge)
!!        integer(kind = kint), intent(in) :: nele
!!        integer(kind = kint), intent(in) :: nsurf
!!        type(edge_data), intent(inout) :: edge
!!@endverbatim
!
      module t_edge_data
!
      use m_precision
!
      implicit  none
!
!>     Structure for edge data
      type edge_data
!>     number of edge on local PE
        integer(kind=kint) ::  numedge
!>     number of internal edge on local PE
        integer(kind=kint) ::  internal_edge
!>     number of nodes in each edge
        integer(kind=kint) ::  nnod_4_edge
!>     number of isolated edge
        integer(kind=kint) ::  numedge_iso
!
!>        Stack list of number of edge
        integer(kind=kint_gl), allocatable  :: istack_numedge(:)
!>        Stack list of number of internal edge
        integer(kind=kint_gl), allocatable  :: istack_interedge(:)
!
!>     local index for edge on each element
        integer (kind=kint), allocatable :: node_on_edge(:,:)
!>     local index for edge on each surface
        integer (kind=kint), allocatable :: node_on_edge_sf(:,:)
!
!>     smp stack for edge on  local PE
        integer( kind=kint ), allocatable :: istack_edge_smp(:)
!>     maximum number of smp edge on local PE
        integer( kind=kint )  ::  max_edge_smp
!>     maximum number of internal smp edge on local PE
        integer( kind=kint )  ::  max_internal_edge_smp
!
!>       global edge id (where i:edge id)
        integer(kind=kint_gl), allocatable  ::  iedge_global(:)
!
!>   edge connectivity ie_edge(i:edge ID,j:surface index)
        integer(kind=kint), allocatable  :: ie_edge(:,:)
!>   edge ID for each surface
        integer(kind=kint), allocatable  :: iedge_4_sf(:,:)
!>   edge ID for each element
        integer(kind=kint), allocatable  :: iedge_4_ele(:,:)
!
!>     isolated edge list
        integer(kind=kint), allocatable  ::  iedge_isolate(:)
!>    integer flag for interior edge 1...interior, 0...exterior
        integer(kind = kint), allocatable :: interior_edge(:)
!
!>   total number of element list for edge
        integer(kind=kint) :: ntot_iele_4_edge
!>   number of element list for each edge
        integer(kind=kint), allocatable :: num_iele_4_edge(:)
!>   end address of element list for each edge
        integer(kind=kint), allocatable :: istack_iele_4_edge(:)
!>   element id list for each edge (negative: opposite direction)
        integer(kind=kint), allocatable  :: iele_4_edge(:,:)
!
!>   total number of surface list for edge
        integer(kind=kint) :: ntot_isurf_4_edge
!>   number of surface list for each edge
        integer(kind=kint), allocatable :: num_isurf_4_edge(:)
!>   end address of surface list for each edge
        integer(kind=kint), allocatable :: istack_isurf_4_edge(:)
!>   surafce id list for each edge (negative: opposite direction)
        integer(kind=kint), allocatable  :: isurf_4_edge(:,:)
!
!>   position of center of edge
        real(kind=kreal)  , allocatable  :: x_edge(:,:)
!
!>   distance from the center of edge
        real(kind=kreal)  , allocatable  :: r_edge(:)
!>   1/r_edge
        real(kind=kreal)  , allocatable  :: ar_edge(:)
!>   longitude of center of edge
        real(kind=kreal)  , allocatable  :: phi_edge(:)
!>   colatitude of center of edge
        real(kind=kreal)  , allocatable  :: theta_edge(:)
!>   cylindorical radius of center of edge
        real(kind=kreal)  , allocatable  :: s_edge(:)
!>   1 / s_edge
        real(kind=kreal)  , allocatable  :: as_edge(:)
!
!>   length of each edge
        real (kind=kreal), allocatable :: edge_length(:)
!>   1 / edge_length
        real (kind=kreal), allocatable :: a_edge_length(:)
!
!>   edge vector
        real (kind=kreal), allocatable :: edge_vect(:,:)
!>   edge vector (spherical coordinate)
        real (kind=kreal), allocatable :: edge_vect_sph(:,:)
!>   edge vector (cylindrical coordinate)
        real (kind=kreal), allocatable :: edge_vect_cyl(:,:)
!
      end type edge_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_numedge_stack(nprocs, edge)
!
      integer(kind = kint), intent(in) :: nprocs
      type(edge_data), intent(inout) :: edge
!
!
      allocate(edge%istack_numedge(0:nprocs))
      allocate(edge%istack_interedge(0:nprocs))
      edge%istack_numedge =   0
      edge%istack_interedge = 0
!
      end subroutine alloc_numedge_stack
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_inod_in_edge(edge)
!
      use m_geometry_constants
!
      type(edge_data), intent(inout) :: edge
!
!
      allocate ( edge%node_on_edge(edge%nnod_4_edge,nedge_4_ele) )
      allocate ( edge%node_on_edge_sf(edge%nnod_4_edge,nedge_4_surf) )
!
      end subroutine allocate_inod_in_edge
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_edge_connect_type(edge, nsurf)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nsurf
      type(edge_data), intent(inout) :: edge
!
      allocate( edge%iedge_4_sf(nsurf,nedge_4_surf) )
      allocate( edge%ie_edge(edge%numedge,edge%nnod_4_edge) )
      allocate( edge%iedge_global(edge%numedge) )
      allocate( edge%interior_edge(edge%numedge) )
!
      if(nsurf .gt. 0) edge%iedge_4_sf =    0
      if(edge%numedge .gt. 0) then
        edge%ie_edge =       0
        edge%iedge_global =  0
        edge%interior_edge = 0
      end if
!
      end subroutine allocate_edge_connect_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_edge_4_ele_type(edge, nele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele
      type(edge_data), intent(inout) :: edge
!
!
      allocate( edge%iedge_4_ele(nele,nedge_4_ele) )
      if(nele .gt. 0) edge%iedge_4_ele = 0
!
      end subroutine allocate_edge_4_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_iso_edge_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      allocate( edge%iedge_isolate(edge%numedge_iso) )
      if (edge%numedge_iso .gt. 0) edge%iedge_isolate = 0
!
      end subroutine allocate_iso_edge_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_edge_geom_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
      allocate( edge%x_edge(edge%numedge,3) )
!
      allocate( edge%r_edge(edge%numedge) )
      allocate( edge%ar_edge(edge%numedge) )
      allocate( edge%phi_edge(edge%numedge) )
      allocate( edge%theta_edge(edge%numedge) )
!
      allocate( edge%s_edge(edge%numedge) )
      allocate( edge%as_edge(edge%numedge) )
!
      if (edge%numedge .gt. 0) then
        edge%x_edge =      0.0d0
!
        edge%r_edge =      0.0d0
        edge%ar_edge =     0.0d0
        edge%phi_edge =    0.0d0
        edge%theta_edge =  0.0d0
!
        edge%s_edge =      0.0d0
        edge%as_edge =     0.0d0
      end if
!
      end subroutine allocate_edge_geom_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_edge_vect_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
      allocate( edge%edge_length(edge%numedge) )
      allocate( edge%a_edge_length(edge%numedge) )
      allocate( edge%edge_vect(edge%numedge,3) )
!
      if (edge%numedge .gt. 0) then
        edge%edge_length =   0.0d0
        edge%a_edge_length = 0.0d0
        edge%edge_vect =     0.0d0
      end if
!
      end subroutine allocate_edge_vect_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_edge_vect_sph_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
      allocate( edge%edge_vect_sph(edge%numedge,3) )
      if(edge%numedge .gt. 0) edge%edge_vect_sph =     0.0d0
!
      end subroutine allocate_edge_vect_sph_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_edge_vect_cyl_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
      allocate( edge%edge_vect_cyl(edge%numedge,3) )
      if(edge%numedge .gt. 0) edge%edge_vect_cyl =     0.0d0
!
      end subroutine allocate_edge_vect_cyl_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_edge_param_smp_type(edge)
!
      use m_machine_parameter
!
      type(edge_data), intent(inout) :: edge
!
      allocate( edge%istack_edge_smp(0:np_smp))
      edge%istack_edge_smp = 0
!
      end subroutine allocate_edge_param_smp_type
!
!-----------------------------------------------------------------------
!
      subroutine alloc_ele_4_edge_num_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      allocate( edge%num_iele_4_edge(edge%numedge) )
      allocate( edge%istack_iele_4_edge(0:edge%numedge) )
      if (edge%numedge .gt. 0) edge%num_iele_4_edge =    0
      edge%istack_iele_4_edge = 0
!
      end subroutine alloc_ele_4_edge_num_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ele_4_edge_item_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      edge%ntot_iele_4_edge = edge%istack_iele_4_edge(edge%numedge)
      allocate( edge%iele_4_edge(edge%ntot_iele_4_edge,2) )
      if (edge%ntot_iele_4_edge .gt. 0) edge%iele_4_edge = 0
!
      end subroutine alloc_ele_4_edge_item_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_surf_4_edge_num_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      allocate( edge%num_isurf_4_edge(edge%numedge) )
      allocate( edge%istack_isurf_4_edge(0:edge%numedge) )
      if (edge%numedge .gt. 0) edge%num_isurf_4_edge =    0
      edge%istack_isurf_4_edge = 0
!
      end subroutine alloc_surf_4_edge_num_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_surf_4_edge_item_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      edge%ntot_isurf_4_edge = edge%istack_isurf_4_edge(edge%numedge)
      allocate( edge%isurf_4_edge(edge%ntot_isurf_4_edge,2) )
      if (edge%ntot_isurf_4_edge .gt. 0) edge%isurf_4_edge = 0
!
      end subroutine alloc_surf_4_edge_item_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_numedge_stack(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      deallocate ( edge%istack_numedge, edge%istack_interedge)
!
      end subroutine dealloc_numedge_stack
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_inod_in_edge_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      deallocate ( edge%node_on_edge, edge%node_on_edge_sf)
!
      end subroutine deallocate_inod_in_edge_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_edge_connect_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
      deallocate( edge%iedge_4_sf )
      deallocate( edge%ie_edge )
      deallocate( edge%iedge_global )
      deallocate( edge%interior_edge )
!
      end subroutine deallocate_edge_connect_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_edge_4_ele_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      deallocate( edge%iedge_4_ele )
!
      end subroutine deallocate_edge_4_ele_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_iso_edge_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      deallocate( edge%iedge_isolate )
!
      end subroutine deallocate_iso_edge_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_edge_geom_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
      deallocate( edge%x_edge )
!
      deallocate( edge%r_edge )
      deallocate( edge%ar_edge )
      deallocate( edge%phi_edge )
      deallocate( edge%theta_edge )
!
      deallocate( edge%s_edge )
      deallocate( edge%as_edge )
!
      end subroutine deallocate_edge_geom_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_edge_vect_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
      deallocate( edge%edge_length   )
      deallocate( edge%a_edge_length )
      deallocate( edge%edge_vect     )
!
      end subroutine deallocate_edge_vect_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_edge_vect_sph_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
      deallocate( edge%edge_vect_sph )
!
      end subroutine deallocate_edge_vect_sph_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_edge_vect_cyl_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
      deallocate( edge%edge_vect_cyl )
!
      end subroutine deallocate_edge_vect_cyl_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_edge_param_smp_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
      deallocate( edge%istack_edge_smp )
!
      end subroutine deallocate_edge_param_smp_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_ele_4_edge_item_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      deallocate( edge%iele_4_edge )
      deallocate( edge%num_iele_4_edge, edge%istack_iele_4_edge)
!
      end subroutine dealloc_ele_4_edge_item_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_surf_4_edge_item_type(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      deallocate( edge%isurf_4_edge )
      deallocate( edge%num_isurf_4_edge, edge%istack_isurf_4_edge)
!
      end subroutine dealloc_surf_4_edge_item_type
!
!  ---------------------------------------------------------------------
!
      end module t_edge_data
