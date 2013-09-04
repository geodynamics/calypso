!
!   module   m_geometry_parameter
!>   number of node, element, surface, and edge data
!>   and SMP stack
!
!   Written by H. Matsui & H. Okuda
!
!      subroutine allocate_inod_in_surf
!      subroutine allocate_inod_in_edge
!      subroutine deallocate_inod_in_surf
!      subroutine deallocate_inod_in_edge
!
!      subroutine allocate_geometry_param_smp
!      subroutine allocate_surf_param_smp
!      subroutine allocate_edge_param_smp
!
!      subroutine deallocate_geometry_param_smp
!      subroutine deallocate_surf_param_smp
!      subroutine deallocate_edge_param_smp
!
!      subroutine check_size_4_sheard_para(my_rank)
!      subroutine check_edge_surf_size_4_smp
!
!
      module   m_geometry_parameter
!
      use m_precision
!
      implicit  none
!
!>     number of node on local PE (include external node)
      integer( kind=kint )  ::  numnod
!>     number of node on local PE
      integer( kind=kint )  ::  internal_node
!>     number of element on local PE
      integer( kind=kint )  ::  numele
!>     number of internal element on local PE
      integer( kind=kint )  ::  internal_ele
!>     number of surface on local PE
      integer( kind=kint )  ::  numsurf
!>     number of internal surface on local PE
      integer( kind=kint )  ::  internal_surf
!>     number of edge on local PE
      integer( kind=kint )  ::  numedge
!>     number of internal edge on local PE
      integer( kind=kint )  ::  internal_edge
!
!
!   number of nodes in each edge
!
!>   number of nodes in each element
      integer(kind=kint) :: nnod_4_ele
!>   number of nodes in each surface
      integer(kind=kint) :: nnod_4_surf
!>   number of nodes in each edge
      integer(kind=kint) :: nnod_4_edge
!
!>   local index for surface on each element
      integer (kind=kint), allocatable, target :: node_on_sf(:,:)
!>   local index for opposite surface on each element
      integer (kind=kint), allocatable, target :: node_on_sf_n(:,:)
!
!>   local index for edge on each element
      integer (kind=kint), allocatable, target :: node_on_edge(:,:)
!>   local index for edge on each surface
      integer (kind=kint), allocatable, target :: node_on_edge_sf(:,:)
!
!
!>     smp stack for total node on  local PE
      integer( kind=kint ), allocatable, target :: inod_smp_stack(:)
!>     smp stack for internal node on  local PE
      integer( kind=kint ), allocatable, target :: inter_smp_stack(:)
!>     maximum number of smp node on local PE
      integer( kind=kint )  ::  maxnod_4_smp = 0
!>     maximum number of smp internal node on local PE
      integer( kind=kint )  ::  max_in_nod_4_smp = 0
!
!
!>     smp stack for element on  local PE
      integer( kind=kint ), allocatable, target :: iele_smp_stack(:)
!>     maximum number of smp element on local PE
      integer( kind=kint )  ::  maxele_4_smp = 0
!
!>     smp stack for surface on  local PE
      integer( kind=kint ), allocatable, target :: isurf_smp_stack(:)
!>     maximum number of smp surface on local PE
      integer( kind=kint )  ::  maxsurf_4_smp = 0
!
!>     smp stack for edge on  local PE
      integer( kind=kint ), allocatable, target :: iedge_smp_stack(:)
!>     maximum number of smp edge on local PE
      integer(kind = kint)  ::  maxedge_4_smp = 0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine allocate_inod_in_surf
!
       use m_geometry_constants
!
!
       allocate ( node_on_sf  (nnod_4_surf,nsurf_4_ele) )
       allocate ( node_on_sf_n(nnod_4_surf,nsurf_4_ele) )
!
       node_on_sf =   0
       node_on_sf_n = 0
!
       end subroutine allocate_inod_in_surf
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_inod_in_edge
!
       use m_geometry_constants
!
!
       allocate ( node_on_edge(nnod_4_edge,nedge_4_ele) )
       allocate ( node_on_edge_sf(nnod_4_edge,nedge_4_surf) )
!
       node_on_edge =    0
       node_on_edge_sf = 0
!
       end subroutine allocate_inod_in_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine deallocate_inod_in_surf
!
!
       deallocate (node_on_sf, node_on_sf_n)
!
       end subroutine deallocate_inod_in_surf
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_inod_in_edge
!
!
       deallocate ( node_on_edge, node_on_edge_sf)
!
       end subroutine deallocate_inod_in_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!>    allocate smp stack table for mesh
       subroutine allocate_geometry_param_smp
!
      use m_machine_parameter
!
       allocate( iele_smp_stack(0:np_smp))
       allocate( inod_smp_stack(0:np_smp))
       allocate( inter_smp_stack(0:np_smp))
!
!       allocate( iele_4_smp(numele,2))
!
       iele_smp_stack = 0
       inod_smp_stack = 0
!       iele_4_smp = 0
!
       end subroutine allocate_geometry_param_smp
!
!-----------------------------------------------------------------------
!
!>    allocate smp stack table for surface
       subroutine allocate_surf_param_smp
!
      use m_machine_parameter
!
       allocate( isurf_smp_stack(0:np_smp))
       isurf_smp_stack = 0
!
       end subroutine allocate_surf_param_smp
!
!-----------------------------------------------------------------------
!
!>    allocate smp stack table for edge
      subroutine allocate_edge_param_smp
!
      use m_machine_parameter
!
      allocate( iedge_smp_stack(0:np_smp))
      iedge_smp_stack = 0
!
      end subroutine allocate_edge_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!>    release smp stack table for mesh
      subroutine deallocate_geometry_param_smp
!
      deallocate( iele_smp_stack  )
      deallocate( inod_smp_stack  )
      deallocate( inter_smp_stack )
!
!       deallocate( iele_4_smp(numele,2))
!
      end subroutine deallocate_geometry_param_smp
!
!-----------------------------------------------------------------------
!
!>    release smp stack table for surface
       subroutine deallocate_surf_param_smp
!
       deallocate( isurf_smp_stack)
!
       end subroutine deallocate_surf_param_smp
!
!-----------------------------------------------------------------------
!
!>    release smp stack table for edge
       subroutine deallocate_edge_param_smp
!
       deallocate( iedge_smp_stack)
!
       end subroutine deallocate_edge_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!>    check smp stack data for mesh
!!@n   @param my_rank domain (MPI rank) ID
      subroutine check_size_4_sheard_para(my_rank)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'np_smp: ', np_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inod_smp_stack ', inod_smp_stack
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inter_smp_stack ', inter_smp_stack
       write(*,*) 'PE: ', my_rank,                                      &
     &           'iele_smp_stack ', iele_smp_stack
!
      end subroutine check_size_4_sheard_para
!
!-----------------------------------------------------------------------
!
!>    check smp stack data for surface and edge
      subroutine check_edge_surf_size_4_smp
!
!
        write(*,*) 'isurf_smp_stack ', isurf_smp_stack
        write(*,*) 'iedge_smp_stack ', iedge_smp_stack
!
      end subroutine check_edge_surf_size_4_smp
!
!-----------------------------------------------------------------------
!
      end module   m_geometry_parameter
