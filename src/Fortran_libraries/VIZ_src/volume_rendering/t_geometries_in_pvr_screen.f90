!>@file  t_geometries_in_pvr_screen.f90
!!       module t_geometries_in_pvr_screen
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine alloc_iflag_pvr_used_ele(ele, draw_param)
!!      subroutine dealloc_iflag_pvr_used_ele(draw_param)
!!        type(element_data), intent(in) :: ele
!!        type(rendering_parameter), intent(inout) :: draw_param
!!      subroutine alloc_iflag_pvr_boundaries(surf_grp, draw_param)
!!      subroutine dealloc_iflag_pvr_boundaries(draw_param)
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(rendering_parameter), intent(inout) :: draw_param
!!
!!      subroutine alloc_pvr_sections(draw_param)
!!      subroutine alloc_pvr_isosurfaces(draw_param)
!!        type(rendering_parameter), intent(inout) :: draw_param
!!
!!      subroutine alloc_pixel_position_pvr(n_pvr_pixel, pixel_xy)
!!        type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!!      subroutine dealloc_data_4_pvr(draw_param)
!!        type(rendering_parameter), intent(inout) :: draw_param
!!
!!      subroutine deallocate_projected_data_pvr                        &
!!      &        (num_pvr, proj, draw_param)
!!      subroutine dealloc_pixel_position_pvr(pixel_xy)
!!      subroutine set_pixel_on_pvr_screen(view_param, pixel_xy)
!!        type(pvr_view_parameter), intent(in) :: view_param
!!        type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!!@endverbatim
!
      module t_geometries_in_pvr_screen
!
      use m_precision
      use m_constants
      use t_ctl_param_tracer_render
!
      implicit  none
!
!>  Structure for start points of ray tracing
      type pvr_projected_position
!>        viewpoint
        real(kind = kreal) :: viewpoint_vec(3)
!>        modelview matrix
        real(kind = kreal) :: modelview_mat(4,4)
!>        perspective projection matrix
        real(kind = kreal) :: projection_mat(4,4)
!
!>    Direction of three axis in screen coordinate
        real(kind = kreal) :: axis_view(3,4)
!>    Order of three axis in screen coordinate
        integer(kind = kint) :: axis_order(3)
      end type pvr_projected_position
!
!
!>  Structure for field data on projected coordinate
      type rendering_parameter
!>    flag for rendering element
        integer(kind = kint), allocatable :: iflag_used_ele(:)
!
!>    Number of surface to be enhansed
        integer(kind = kint) :: num_enhanse
!>    integer flag for surface boundaries
        integer(kind = kint), allocatable :: iflag_enhanse(:)
!>    Opacity value for surface boundaries
        real(kind = kreal), allocatable :: enhansed_opacity(:)
!
!>  Structure for tracer rendering
        type(tracer_render_param) :: tracer_pvr_prm
!>  Structure for fiel line rendering
        type(tracer_render_param) :: fline_pvr_prm
!
!>    Number of sections
        integer(kind = kint) :: num_sections
!>    Number of sections
        integer(kind = kint), allocatable :: iflag_psf_zeoline(:)
!>    fiale value for isosurfaces
        real(kind = kreal), allocatable :: coefs(:,:)
!>    Opacity value for isosurfaces
        real(kind = kreal), allocatable :: sect_opacity(:)
!
!>    Number of isosurfaces
        integer(kind = kint) :: num_isosurf
!>    Number of isosurfaces
        integer(kind = kint), allocatable :: itype_isosurf(:)
!>    fiale value for isosurfaces
        real(kind = kreal), allocatable :: iso_value(:)
!>    Opacity value for isosurfaces
        real(kind = kreal), allocatable :: iso_opacity(:)
      end type rendering_parameter
!
!>  Structure for pixel position
      type pvr_pixel_position_type
!>    Number of horizontal pixels
        integer(kind = kint) :: num_pixel_x
!>    Number of vertical pixels
        integer(kind = kint) :: num_pixel_y
!>    Position of horizontal pixels
        real(kind = kreal), allocatable :: pixel_point_x(:)
!>    Position of vertical pixels
        real(kind = kreal), allocatable :: pixel_point_y(:)
      end type pvr_pixel_position_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_iflag_pvr_used_ele(ele, draw_param)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: ele
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      allocate(draw_param%iflag_used_ele(ele%numele))
      if(ele%numele .gt. 0) draw_param%iflag_used_ele = 0
!
      end subroutine alloc_iflag_pvr_used_ele
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_iflag_pvr_used_ele(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      deallocate(draw_param%iflag_used_ele)
!
      end subroutine dealloc_iflag_pvr_used_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_iflag_pvr_boundaries(surf_grp, draw_param)
!
      use t_group_data
!
      type(surface_group_data), intent(in) :: surf_grp
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      draw_param%num_enhanse = surf_grp%num_grp
      allocate(draw_param%iflag_enhanse(surf_grp%num_grp))
      allocate(draw_param%enhansed_opacity(surf_grp%num_grp))
!
      if(surf_grp%num_grp .gt. 0) draw_param%iflag_enhanse = 0
      if(surf_grp%num_grp .gt. 0) draw_param%enhansed_opacity = 0.0d0
!
      end subroutine alloc_iflag_pvr_boundaries
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_iflag_pvr_boundaries(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      deallocate(draw_param%iflag_enhanse, draw_param%enhansed_opacity)
!
      end subroutine dealloc_iflag_pvr_boundaries
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_sections(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      allocate(draw_param%coefs(10,draw_param%num_sections))
      allocate(draw_param%sect_opacity(draw_param%num_sections))
      allocate(draw_param%iflag_psf_zeoline(draw_param%num_sections))
!
      if(draw_param%num_sections .le. 0) return
!
      draw_param%coefs(1:10,1:draw_param%num_sections) = zero
      draw_param%sect_opacity(1:draw_param%num_sections) = zero
      draw_param%iflag_psf_zeoline(1:draw_param%num_sections) = izero
!
      end subroutine alloc_pvr_sections
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_isosurfaces(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      allocate(draw_param%itype_isosurf(draw_param%num_isosurf))
      allocate(draw_param%iso_value(draw_param%num_isosurf))
      allocate(draw_param%iso_opacity(draw_param%num_isosurf))
!
      if(draw_param%num_isosurf .gt. 0) draw_param%itype_isosurf = 0
      if(draw_param%num_isosurf .gt. 0) draw_param%iso_value = zero
      if(draw_param%num_isosurf .gt. 0) draw_param%iso_opacity = zero
!
      end subroutine alloc_pvr_isosurfaces
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_sections(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      deallocate(draw_param%coefs, draw_param%sect_opacity)
      deallocate(draw_param%iflag_psf_zeoline)
!
      end subroutine dealloc_pvr_sections
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_isosurfaces(draw_param)
!
      type(rendering_parameter), intent(inout) :: draw_param
!
!
      deallocate(draw_param%itype_isosurf)
      deallocate(draw_param%iso_value, draw_param%iso_opacity)
!
      end subroutine dealloc_pvr_isosurfaces
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_pixel_position_pvr(n_pvr_pixel, pixel_xy)
!
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!
!
      pixel_xy%num_pixel_x = n_pvr_pixel(1)
      pixel_xy%num_pixel_y = n_pvr_pixel(2)
      allocate(pixel_xy%pixel_point_x(pixel_xy%num_pixel_x))
      allocate(pixel_xy%pixel_point_y(pixel_xy%num_pixel_y))
!
      pixel_xy%pixel_point_x =  0.0d0
      pixel_xy%pixel_point_y =  0.0d0
!
      end subroutine alloc_pixel_position_pvr
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pixel_position_pvr(pixel_xy)
!
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!
!
      deallocate(pixel_xy%pixel_point_x, pixel_xy%pixel_point_y)
!
      end subroutine dealloc_pixel_position_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_pixel_on_pvr_screen(view_param, pixel_xy)
!
      use t_control_params_4_pvr
      use set_projection_matrix
!
      type(pvr_view_parameter), intent(in) :: view_param
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!
!
      call alloc_pixel_position_pvr(view_param%n_pvr_pixel, pixel_xy)
!
      call set_pixel_points_on_project                                  &
     &   (view_param%n_pvr_pixel(1), view_param%n_pvr_pixel(2),         &
          pixel_xy%pixel_point_x, pixel_xy%pixel_point_y)
!
      end subroutine set_pixel_on_pvr_screen
!
!  ---------------------------------------------------------------------
!
      end module t_geometries_in_pvr_screen
