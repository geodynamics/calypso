!>@file   t_viewer_mesh.f90
!!@brief  module t_viewer_mesh
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Structure of surface information for pickup surface
!!
!!@verbatim
!!      subroutine alloc_nod_position_viewer(view_mesh)
!!      subroutine alloc_surf_type_viewer(view_mesh)
!!      subroutine alloc_edge_type_viewer(view_mesh)
!!      subroutine alloc_surf_connect_viewer(nnod_4_surf, view_mesh)
!!      subroutine alloc_edge_data_4_sf(nnod_4_edge, view_mesh)
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!
!!      subroutine dealloc_nod_position_viewer(view_mesh)
!!      subroutine dealloc_surf_type_viewer(view_mesh)
!!      subroutine dealloc_edge_type_viewer(view_mesh)
!!      subroutine dealloc_surf_connect_viewer(view_mesh)
!!      subroutine dealloc_edge_data_4_sf(view_mesh)
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!@endverbatim
!
      module t_viewer_mesh
!
      use m_precision
      use m_geometry_constants
!
      implicit none
!
!
      integer (kind = kint), parameter :: surface_id = 15
!
!
      type viewer_mesh_data
        integer(kind = kint)  ::  nnod_viewer
        integer(kind = kint)  ::  nsurf_viewer
        integer(kind = kint)  ::  nedge_viewer
!
        integer(kind = kint)  ::  nnod_v_surf
        integer(kind = kint)  ::  nnod_v_edge
!
        integer(kind = kint), allocatable  ::  ie_sf_viewer(:,:)
        integer(kind = kint), allocatable  ::  ie_edge_viewer(:,:)
        integer(kind = kint), allocatable  ::  surftyp_viewer(:  )
        integer(kind = kint), allocatable  ::  edgetyp_viewer(:  )
        integer(kind = kint), allocatable  ::  iedge_sf_viewer(:,:)
!
        integer(kind = kint_gl), allocatable :: inod_gl_view(:)
        integer(kind = kint_gl), allocatable :: isurf_gl_view(:)
        integer(kind = kint_gl), allocatable :: iedge_gl_view(:)
!
        real(kind=kreal), allocatable  ::  xx_view(:,:)
      end type viewer_mesh_data
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_nod_position_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      allocate( view_mesh%xx_view(view_mesh%nnod_viewer,3) )
      allocate( view_mesh%inod_gl_view(view_mesh%nnod_viewer) )
      if(view_mesh%nnod_viewer .gt. 0) view_mesh%xx_view = 0.0d0
      if(view_mesh%nnod_viewer .gt. 0) view_mesh%inod_gl_view = 0
!
      end subroutine alloc_nod_position_viewer
!
!------------------------------------------------------------------
!
      subroutine alloc_surf_type_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      allocate(view_mesh%surftyp_viewer(view_mesh%nsurf_viewer))
      if(view_mesh%nsurf_viewer .gt. 0) then
        view_mesh%surftyp_viewer = 0
      end if
!
      end subroutine alloc_surf_type_viewer
!
!------------------------------------------------------------------
!
      subroutine alloc_edge_type_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
!
      allocate(view_mesh%edgetyp_viewer(view_mesh%nedge_viewer))
      if(view_mesh%nedge_viewer .gt. 0) then
        view_mesh%edgetyp_viewer = 0
      end if
!
      end subroutine alloc_edge_type_viewer
!
!------------------------------------------------------------------
!
      subroutine alloc_surf_connect_viewer(nnod_4_surf, view_mesh)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind  = kint) :: num
!
      num = view_mesh%nsurf_viewer
      view_mesh%nnod_v_surf = nnod_4_surf
      allocate( view_mesh%ie_sf_viewer(num,nnod_4_surf) )
      allocate( view_mesh%isurf_gl_view(num) )
      if(num .gt. 0) view_mesh%ie_sf_viewer = 0
      if(num .gt. 0) view_mesh%isurf_gl_view = 0
!
      end subroutine alloc_surf_connect_viewer
!
!------------------------------------------------------------------
!
      subroutine alloc_edge_data_4_sf(nnod_4_edge, view_mesh)
!
      integer(kind = kint), intent(in) :: nnod_4_edge
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: num
!
!
      num = view_mesh%nedge_viewer
      view_mesh%nnod_v_edge = nnod_4_edge
      allocate(view_mesh%ie_edge_viewer(num,nnod_4_edge))
      allocate(view_mesh%iedge_gl_view(num))
      if(num .gt. 0) view_mesh%ie_edge_viewer = 0
      if(num .gt. 0) view_mesh%iedge_gl_view = 0
!
      num = view_mesh%nsurf_viewer
      allocate(view_mesh%iedge_sf_viewer(num,nedge_4_surf))
      if(num .gt. 0) view_mesh%iedge_sf_viewer = 0
!
      end subroutine alloc_edge_data_4_sf
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_nod_position_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      deallocate( view_mesh%xx_view, view_mesh%inod_gl_view )
!
      end subroutine dealloc_nod_position_viewer
!
!------------------------------------------------------------------
!
      subroutine dealloc_surf_type_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      deallocate( view_mesh%surftyp_viewer )
!
      end subroutine dealloc_surf_type_viewer
!
!------------------------------------------------------------------
!
      subroutine dealloc_edge_type_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
!
      deallocate(view_mesh%edgetyp_viewer)
!
      end subroutine dealloc_edge_type_viewer
!
!------------------------------------------------------------------
!
      subroutine dealloc_surf_connect_viewer(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      deallocate( view_mesh%ie_sf_viewer, view_mesh%isurf_gl_view )
!
      end subroutine dealloc_surf_connect_viewer
!
!------------------------------------------------------------------
!
      subroutine dealloc_edge_data_4_sf(view_mesh)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
!
      deallocate(view_mesh%ie_edge_viewer, view_mesh%iedge_gl_view)
      deallocate(view_mesh%iedge_sf_viewer)
!
      end subroutine dealloc_edge_data_4_sf
!
!------------------------------------------------------------------
!
      end module t_viewer_mesh
