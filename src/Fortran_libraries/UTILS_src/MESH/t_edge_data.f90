!>@file   t_edge_data.f90
!!@brief  module t_edge_data
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!> @brief Structure of edge geometry data
!!
!!@verbatim
!!      subroutine alloc_inod_in_edge(edge)
!!      subroutine alloc_edge_connect(edge, nsurf)
!!      subroutine alloc_interior_edge(edge)
!!      subroutine alloc_edge_4_ele(edge, nele)
!!      subroutine alloc_isolate_edge(edge)
!!      subroutine alloc_edge_geometory(edge)
!!      subroutine alloc_edge_vect(edge)
!!      subroutine alloc_edge_vect_sph(edge)
!!      subroutine alloc_edge_param_smp(edge)
!!        type(edge_data), intent(inout) :: edge
!!        type(edge_position), intent(inout) :: edge_pt
!!
!!      subroutine dealloc_inod_in_edge(edge)
!!      subroutine dealloc_edge_connect(edge)
!!      subroutine dealloc_interior_edge(edge)
!!      subroutine dealloc_edge_4_ele(edge)
!!      subroutine dealloc_isolate_edge(edge)
!!      subroutine dealloc_edge_geometory(edge)
!!      subroutine dealloc_edge_vect(edge)
!!        type(edge_position), intent(inout) :: edge_pt
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
!>     number of nodes in each edge
        integer(kind=kint) ::  nnod_4_edge
!>     number of isolated edge
        integer(kind=kint) ::  numedge_iso
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
!>    integer flag for interior edge 1...interior, 0...exterior
        integer(kind = kint), allocatable :: interior_edge(:)
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
!
!>   position of center of edge
        real(kind = kreal), allocatable  :: x_edge(:,:)
!
!>   length of each edge
        real(kind = kreal), allocatable :: edge_length(:)
!>   1 / edge_length
        real(kind = kreal), allocatable :: a_edge_length(:)
!
!>   edge vector
        real(kind = kreal), allocatable :: edge_vect(:,:)
      end type edge_data
!
!  ---------------------------------------------------------------------
!
      contains
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
!
      if(nsurf .gt. 0)        edge%iedge_4_sf =    0
      if(edge%numedge .gt. 0) edge%ie_edge =       0
!
      end subroutine alloc_edge_connect
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_interior_edge(edge)
!
      use m_geometry_constants
!
      type(edge_data), intent(inout) :: edge
!
      allocate(edge%iedge_global(edge%numedge))
      allocate(edge%interior_edge(edge%numedge))
!
      if(edge%numedge .gt. 0) then
        edge%iedge_global =  0
        edge%interior_edge = 0
      end if
!
      end subroutine alloc_interior_edge
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
!
      if (edge%numedge .gt. 0) edge%x_edge = 0.0d0
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
!$omp parallel workshare
        edge%edge_length =   0.0d0
        edge%a_edge_length = 0.0d0
        edge%edge_vect =     0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_edge_vect
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
      subroutine dealloc_inod_in_edge(edge)
!
      type(edge_data), intent(inout) :: edge
!
!
      if(allocated(edge%node_on_edge) .eqv. .FALSE.) return
      deallocate(edge%node_on_edge, edge%node_on_edge_sf)
!
      end subroutine dealloc_inod_in_edge
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_connect(edge)
!
      type(edge_data), intent(inout) :: edge
!
      if(allocated(edge%iedge_4_sf) .eqv. .FALSE.) return
      deallocate(edge%iedge_4_sf, edge%ie_edge)
!
      end subroutine dealloc_edge_connect
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_interior_edge(edge)
!
      type(edge_data), intent(inout) :: edge
!
      if(allocated(edge%interior_edge) .eqv. .FALSE.) return
      deallocate(edge%iedge_global, edge%interior_edge)
!
      end subroutine dealloc_interior_edge
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_4_ele(edge)
!
      type(edge_data), intent(inout) :: edge
!
      if(allocated(edge%iedge_4_ele)) deallocate(edge%iedge_4_ele)
!
      end subroutine dealloc_edge_4_ele
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_isolate_edge(edge)
!
      type(edge_data), intent(inout) :: edge
!
      if(allocated(edge%iedge_isolate)) deallocate(edge%iedge_isolate)
!
      end subroutine dealloc_isolate_edge
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_geometory(edge)
!
      type(edge_data), intent(inout) :: edge
!
      if(allocated(edge%x_edge)) deallocate(edge%x_edge)
!
      end subroutine dealloc_edge_geometory
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_vect(edge)
!
      type(edge_data), intent(inout) :: edge
!
      if(allocated(edge%edge_length) .eqv. .FALSE.) return
      deallocate(edge%edge_length, edge%a_edge_length)
      deallocate(edge%edge_vect)
!
      end subroutine dealloc_edge_vect
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_edge_param_smp(edge)
!
      type(edge_data), intent(inout) :: edge
!
      if(allocated(edge%x_edge)) deallocate(edge%istack_edge_smp)
!
      end subroutine dealloc_edge_param_smp
!
!-----------------------------------------------------------------------
!
      end module t_edge_data
