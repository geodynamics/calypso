!>@file  t_rendering_vr_image.f90
!!       module t_rendering_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine alloc_multi_view_parameters(num_views, pvr_param)
!!      subroutine dealloc_multi_view_parameters(num_views, pvr_param)
!!        integer(kind = kint), intent(in) :: num_views
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!      subroutine flush_rendering_4_fixed_view(pvr_proj)
!!        type(PVR_projection_data), intent(inout) :: pvr_proj
!!@endverbatim
!
      module t_rendering_vr_image
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use m_work_time
!
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surf_grp_list_each_surf
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_pvr_stencil_buffer
      use t_pvr_field_data
      use t_control_params_stereo_pvr
      use t_mesh_SR
      use generate_vr_image
!
      implicit  none
!
!>      Structure of PVR control parameters
      type PVR_control_params
!>        Structure for rendering area by element group
        type(viz_area_parameter) :: area_def
!>        Structure for field parameter for PVR
        type(pvr_field_parameter) :: field_def
!
!>        Parameters for image pixels
        type(pvr_pixel_position_type) :: pixel
!>        Structure for rough serch of subdomains
        type(pvr_domain_outline) :: outline
!>        Field data for volume rendering
        type(rendering_parameter) :: draw_param
!>        Structure for PVR colormap
        type(pvr_colorbar_parameter):: colorbar
!
!>        Color paramter for volume rendering
        type(pvr_colormap_parameter) :: color
!>        Movie parameters
        type(pvr_movie_parameter) :: movie_def
!>        Stereo view parameters
        type(pvr_stereo_parameter) :: stereo_def
!
!>        Logical flag to use multi view paramter from movie block
        logical :: flag_mulview_movie = .FALSE.
!>        Number of mulitple view parameters
        integer(kind = kint) :: num_multi_views = 0
!>        Multiple viewer coordinate information
        type(pvr_view_parameter), allocatable :: multi_view(:)
      end type PVR_control_params
!
!
!>      Structure for projection data
      type PVR_projection_data
!>        Data on screen coordinate
        type(pvr_projected_position) :: screen
!>        Parallel stencil buffer
        type(pvr_stencil_buffer) :: stencil
!>        Start point structure for volume rendering with fixed view
        type(pvr_ray_start_type) :: start_fix
!
!>        Start point structure for volume rendering with fixed view
        type(pvr_ray_start_type) :: start_save
      end type PVR_projection_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_multi_view_parameters(num_views, pvr_param)
!
      integer(kind = kint), intent(in) :: num_views
      type(PVR_control_params), intent(inout) :: pvr_param
!
      pvr_param%num_multi_views = num_views
      allocate(pvr_param%multi_view(pvr_param%num_multi_views))
!
      end subroutine alloc_multi_view_parameters
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_multi_view_parameters(pvr_param)
!
      type(PVR_control_params), intent(inout) :: pvr_param
!
      if(allocated(pvr_param%multi_view) .eqv. .FALSE.) return
      deallocate(pvr_param%multi_view)
      pvr_param%num_multi_views = 0
!
      end subroutine dealloc_multi_view_parameters
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine flush_rendering_4_fixed_view(pvr_proj)
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
!
!
      call dealloc_pvr_stencil_buffer(pvr_proj%stencil)
!
      end subroutine flush_rendering_4_fixed_view
!
!  ---------------------------------------------------------------------
!
      end module t_rendering_vr_image
