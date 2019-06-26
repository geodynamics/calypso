!>@file   t_edge_data.f90
!!@brief  module t_edge_data
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!> @brief Structure of edge geometry data
!!
!!@verbatim
!!      subroutine alloc_numedge_stack(num_pe, edge)
!!      subroutine alloc_inod_in_edge(edge)
!!      subroutine alloc_edge_connect(edge, nsurf)
!!      subroutine alloc_edge_4_ele(edge, nele)
!!      subroutine alloc_isolate_edge(edge)
!!      subroutine alloc_edge_geometory(edge)
!!      subroutine alloc_edge_vect(edge)
!!      subroutine alloc_edge_vect_sph(edge)
!!      subroutine alloc_edge_vect_cyl(edge)
!!      subroutine alloc_edge_param_smp(edge)
!!
!!      subroutine dealloc_numedge_stack(edge)
!!      subroutine dealloc_inod_in_edge(edge)
!!      subroutine dealloc_edge_connect(edge)
!!      subroutine dealloc_edge_4_ele(edge)
!!      subroutine dealloc_isolate_edge(edge)
!!      subroutine dealloc_edge_geometory(edge)
!!      subroutine dealloc_edge_vect(edge)
!!      subroutine dealloc_edge_vect_sph(edge)
!!      subroutine dealloc_edge_vect_cyl(edge)
!!      subroutine dealloc_edge_param_smp(edge)
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
      subroutine alloc_numedge_stack(num_pe, edge)
!
      integer, intent(in) :: num_pe
      type(edge_data), intent(inout) :: edge
!
!
      allocate(edge%istack_numedge(0:num_pe))
      allocate(edge%istack_interedge(0:num_pe))
      edge%istack_numedge =   0
      edge%istack_interedge = 0
!
      end subroutine alloc_numedge_stack
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_inod_in_edge(edge)
!
      use m_geometry_constants
!
      type(edge_data), intent(inout) :: edge
!
!
      allocate ( edge%node_on_edge(edge%nnod_4_edge,nedge_4_ele) )
      allocate ( edge%node_on_edge_sf(edge%nnod_4_edge,nedge_4_surf) )
!
      end subroutine alloc_inod_in_edge
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_edge_connect(edge, nsurf)
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
      end subroutine alloc_edge_connect
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_edge_4_ele(edge, nele)
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
      end subroutine alloc_edge_4_ele
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_isolate_edge(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      allocate( edge%iedge_isolate(edge%numedge_iso) )
      if (edge%numedge_iso .gt. 0) edge%iedge_isolate = 0
!
      end subroutine alloc_isolate_edge
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_edge_geometory(edge)
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
      end subroutine alloc_edge_geometory
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_edge_vect(edge)
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
      end subroutine alloc_edge_vect
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_edge_vect_sph(edge)
!
      type(edge_data), intent(inout) :: edge
!
      allocate( edge%edge_vect_sph(edge%numedge,3) )
      if(edge%numedge .gt. 0) edge%edge_vect_sph =     0.0d0
!
      end subroutine alloc_edge_vect_sph
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_edge_vect_cyl(edge)
!
      type(edge_data), intent(inout) :: edge
!
      allocate( edge%edge_vect_cyl(edge%numedge,3) )
      if(edge%numedge .gt. 0) edge%edge_vect_cyl =     0.0d0
!
      end subroutine alloc_edge_vect_cyl
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_edge_param_smp(edge)
!
      use m_machine_parameter
!
      type(edge_data), intent(inout) :: edge
!
      allocate( edge%istack_edge_smp(0:np_smp))
      edge%istack_edge_smp = 0
!
      end subroutine alloc_edge_param_smp
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
      subroutine dealloc_inod_in_edge(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      deallocate ( edge%node_on_edge, edge%node_on_edge_sf)
!
      end subroutine dealloc_inod_in_edge
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_connect(edge)
!
      type(edge_data), intent(inout) :: edge
!
      deallocate( edge%iedge_4_sf )
      deallocate( edge%ie_edge )
      deallocate( edge%iedge_global )
      deallocate( edge%interior_edge )
!
      end subroutine dealloc_edge_connect
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_4_ele(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      deallocate( edge%iedge_4_ele )
!
      end subroutine dealloc_edge_4_ele
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_isolate_edge(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      deallocate( edge%iedge_isolate )
!
      end subroutine dealloc_isolate_edge
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_geometory(edge)
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
      end subroutine dealloc_edge_geometory
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_vect(edge)
!
      type(edge_data), intent(inout) :: edge
!
      deallocate( edge%edge_length   )
      deallocate( edge%a_edge_length )
      deallocate( edge%edge_vect     )
!
      end subroutine dealloc_edge_vect
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_vect_sph(edge)
!
      type(edge_data), intent(inout) :: edge
!
      deallocate( edge%edge_vect_sph )
!
      end subroutine dealloc_edge_vect_sph
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_vect_cyl(edge)
!
      type(edge_data), intent(inout) :: edge
!
      deallocate( edge%edge_vect_cyl )
!
      end subroutine dealloc_edge_vect_cyl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_param_smp(edge)
!
      type(edge_data), intent(inout) :: edge
!
      deallocate( edge%istack_edge_smp )
!
      end subroutine dealloc_edge_param_smp
!
!-----------------------------------------------------------------------
!
      end module t_edge_data
