!>@file  t_control_params_4_pvr.f90
!!       module t_control_params_4_pvr
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Structures for parameteres for volume rendering
!!
!!@verbatim
!!      subroutine alloc_pvr_element_group(pvr_area)
!!      subroutine dealloc_pvr_element_group(pvr_area)
!!        type(viz_area_parameter), intent(inout) :: pvr_area
!!
!!      subroutine alloc_pvr_color_parameteres(color)
!!      subroutine alloc_pvr_opacity_list(color)
!!      subroutine alloc_light_posi_in_view(color)
!!      subroutine dealloc_pvr_color_parameteres(color)
!!
!!      integer(kind = kint) function num_flag_pvr_movie_mode()
!!      integer(kind = kint) function num_flag_LIC_movie_mode()
!!      subroutine set_flag_pvr_movie_mode(names)
!!      subroutine set_flag_LIC_movie_mode(names)
!!@endverbatim
!
      module t_control_params_4_pvr
!
      use m_precision
      use m_constants
      use output_image_sel_4_png
!
      implicit  none
!
!
      real(kind = kreal), parameter :: SMALL_RAY_TRACE = 0.1d0
      real(kind = kreal), parameter :: SMALL_NORM = -0.1d0
!
      integer(kind = kint), parameter :: n_flag_pvr_movie_mode =   3
      integer(kind = kint), parameter :: n_flag_LIC_movie_mode =   4
      character(len=kchara), parameter                                  &
     &                      :: FLAG_ROTATE_MOVIE =    'rotation'
      character(len=kchara), parameter :: FLAG_ZOOM = 'zoom'
      character(len=kchara), parameter                                  &
     &                      :: FLAG_START_END_VIEW =  'view_matrices'
      character(len=kchara), parameter                                  &
     &                      :: FLAG_LIC_KERNEL = 'LIC_kernel'
!
      integer(kind = kint), parameter :: IFLAG_NO_MOVIE =   0
      integer(kind = kint), parameter :: I_ROTATE_MOVIE =   1
      integer(kind = kint), parameter :: I_ZOOM =           2
      integer(kind = kint), parameter :: I_START_END_VIEW = 3
      integer(kind = kint), parameter :: I_LOOKINGLASS =    4
      integer(kind = kint), parameter :: I_LIC_KERNEL =     5
!
!>  Structure for field parameter for PVR
      type pvr_field_parameter
!>     Field type for PVR data
        integer(kind = kint) :: id_field =          0
!>     Component flag for PVR data
        integer(kind = kint) :: id_component =      0
!>     Number of component of data for Rendering
        integer(kind = kint) :: num_original_comp = 0
!>     Field name of data for Rendering
        character(len = kchara) :: field_name
      end type pvr_field_parameter
!
!>  Structure for rendering area by element group
      type viz_area_parameter
!>     Number of Element group for volume rendering
        integer(kind = kint) :: nele_grp_area_pvr = 0
!>     Element group list for volume rendering
        integer(kind = kint), allocatable :: id_ele_grp_area_pvr(:)
      end type viz_area_parameter
!
!>  Structure for view parameteres
      type pvr_view_parameter
!>    Number of pixels for image
        integer(kind = kint) :: n_pvr_pixel(2) = (/0,0/)
!
!>    Defined flag for orthogonal view
        integer(kind = kint) :: iflag_ortho_mat = 0
!>    Orthogonal projection parameter
        real(kind = kreal) :: ortho_mat(3) = (/zero,zero,zero/)
!>    Orthogonal projection matrix
        real(kind = kreal) :: ortho_pvr(2) = (/zero,zero/)
!
!>    Defined flag for perspective view
        integer(kind = kint) :: iflag_perspective = 0
!>    Apature of perspective view
        real(kind = kreal) :: perspective_angle = zero
!>    Aspect ratio between horiaontal and vertical
        real(kind = kreal) :: perspective_xy_ratio = zero
!>    Near distance for perspective view
        real(kind = kreal) :: perspective_near = zero
!>    Farther distance for perspective view
        real(kind = kreal) :: perspective_far = zero
!
!
!>    Defined flag for modelview matrix
        integer(kind = kint) :: iflag_modelview_mat = 0
!>    Modelview matrix
        real(kind = kreal) :: modelview(4,4)
!
!
!>    Defined flag for view rotation
        integer(kind = kint) :: iflag_rotation = 0
!>    View rotatin
        real(kind = kreal) :: rotation_pvr(4) = (/zero,zero,zero,zero/)
!
!>    Defined flag for scale factor
        integer(kind = kint) :: iflag_scale_fact = 0
!>    Scale factor
        real(kind = kreal) :: scale_factor_pvr(3) = (/zero,zero,zero/)
!
!>    Defined flag for eye point in viewer coordinate
        integer(kind = kint) :: iflag_viewpt_in_view = 0
!>    Position of eye point in viewer coordinate
        real(kind = kreal) :: viewpt_in_viewer_pvr(4)                   &
     &                       = (/zero,zero,zero,zero/)
!
!>    Defined flag for lookatpoint
        integer(kind = kint) :: iflag_lookpoint = 0
!>    Position to look at
        real(kind = kreal) :: lookat_vec(3) = (/zero,zero,zero/)
!
!>    Defined flag for up-direction
        integer(kind = kint) :: iflag_updir = 0
!>    Vector for up-direction
        real(kind = kreal) :: up_direction_vec(3) = (/zero,zero,zero/)
!
!>    Defined flag for viewpoint
        integer(kind = kint) :: iflag_viewpoint = 0
!>    Position of viewpoint
        real(kind = kreal) :: viewpoint(3) = (/zero,zero,zero/)
      end type pvr_view_parameter
!
!>  movie parameters
      type pvr_movie_parameter
!>    Integer flag for movie output
        integer(kind = kint) :: iflag_movie_mode = IFLAG_NO_MOVIE
!
!>     Number of frames
        integer(kind = kint) :: num_frame =   0
!>    Number of row and column of image array (horizontal, vertical)
        integer(kind = kint) :: n_column_row_movie(2) = 0
!
!>     Rotatin axis:    id_rot_axis
        integer(kind = kint) :: id_rot_axis = 3
!>     Rotation range
        real(kind = kreal) :: angle_range(2) = 0.0d0
!
!>     Apature range
        real(kind = kreal) :: apature_range(2) = 0.0d0
!>     Apature range
        real(kind = kreal) :: peak_range(2) =    0.0d0
      end type pvr_movie_parameter
!
!>  Structure for PVR colormap parameters
      type pvr_colormap_parameter
!>    Colormap IDs
!!@n        pvr_colormap(:) =        id_pvr_color(1)
!!@n        pvr_data_mapping(:) =    id_pvr_color(2)
!!@n        opacity_style(:) =       find_dis_minmax(3)
        integer(kind = kint) :: id_pvr_color(3) = (/0,0,0/)
!
!>    Number of data points to define color
        integer(kind = kint) :: num_pvr_datamap_pnt = 0
!>    Data and corresponding color value
!!@n        Field data:  pvr_datamap_param(1,:)
!!@n        Color data:  pvr_datamap_param(2,:)
        real(kind = kreal), allocatable :: pvr_datamap_param(:,:)
!
!>    Number of data points to define color
        integer(kind = kint) :: num_opacity_pnt = 0
!>     Maximum opacity for colorbar
        real(kind = kreal) :: pvr_max_opacity = zero
!>     Opacity data table
!!@n        pvr_opacity_dat_low(:) =  pvr_opacity_param(1,:)
!!@n        pvr_opacity_dat_high(:) = pvr_opacity_param(2,:)
!!@n        pvr_opacity_opacity(:) =  pvr_opacity_param(3,:)
!!@n        ambient_opacity:  pvr_opacity_param(3,(num_opacity_pnt(:)+1))
        real(kind = kreal), allocatable :: pvr_opacity_param(:,:)
!
!>    Defined flag for lights
        integer(kind = kint) :: iflag_pvr_lights = 0
!>        Number of lights
        integer(kind = kint) :: num_pvr_lights = 0
!!@n        ambient_coef(:) =  pvr_lighting_real(1,:)
!!@n        diffuse_coef(:) =  pvr_lighting_real(2,:)
!!@n        specular_coef(:) = pvr_lighting_real(3,:)
        real(kind = kreal) :: pvr_lighting_real(3) = (/zero,zero,zero/)
!>    Position of lights
        real(kind = kreal), allocatable :: xyz_pvr_lights(:,:)
      end type pvr_colormap_parameter
!
!>  Structure for PVR colorbar parameters
      type pvr_colorbar_parameter
!>    Draw flag for color bar
        logical :: iflag_pvr_colorbar =  .FALSE.
!>    Bottom colorbar flag
        logical :: flag_pvr_cbar_bottom =  .FALSE.
!>    Draw flag for color bar numbers
        integer(kind = kint) :: iflag_pvr_cbar_nums = 0
!>    Draw flag for zero line in color bar
        integer(kind = kint) :: iflag_pvr_zero_mark = 0
!>    Flag of colorbar with opacity
        integer(kind = kint) :: iflag_opacity = 1
!>    Scaling for number font
        integer(kind = kint) :: iscale_font = 1
!>    Thicknsess of colorbar
        integer(kind = kint) :: ntick_pvr_colorbar =  3
!>    Range of colorbar
        real(kind = kreal) :: cbar_range(2) = (/zero,one/)
!
!>    Draw flag for axis label
        logical :: iflag_pvr_axis = .FALSE.
!>    Draw flag for time label
        logical :: iflag_draw_time = .FALSE.
      end type pvr_colorbar_parameter
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_element_group(pvr_area)
!
      type(viz_area_parameter), intent(inout) :: pvr_area
!
      allocate(pvr_area%id_ele_grp_area_pvr(pvr_area%nele_grp_area_pvr))
!
      if(pvr_area%nele_grp_area_pvr .le. 0) return
      pvr_area%id_ele_grp_area_pvr = 0
!
      end subroutine alloc_pvr_element_group
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_element_group(pvr_area)
!
      type(viz_area_parameter), intent(inout) :: pvr_area
!
      deallocate(pvr_area%id_ele_grp_area_pvr)
!
      end subroutine dealloc_pvr_element_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_color_parameteres(color)
!
      type(pvr_colormap_parameter), intent(inout) :: color
!
!
      allocate(color%pvr_datamap_param(2,color%num_pvr_datamap_pnt) )
      if(color%num_pvr_datamap_pnt .gt. 0) then
        color%pvr_datamap_param = 0.0d0
      end if
!
      end subroutine alloc_pvr_color_parameteres
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_opacity_list(color)
!
      type(pvr_colormap_parameter), intent(inout) :: color
!
!
      allocate(color%pvr_opacity_param(3,color%num_opacity_pnt+1) )
      if(color%num_opacity_pnt .gt. 0) then
        color%pvr_opacity_param = 0.0d0
      end if
!
      end subroutine alloc_pvr_opacity_list
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_light_posi_in_view(color)
!
      type(pvr_colormap_parameter), intent(inout) :: color
!
!
      allocate(color%xyz_pvr_lights(3,color%num_pvr_lights) )
      if (color%num_pvr_lights .le. 0) return
      color%xyz_pvr_lights =    0.0d0
!
      end subroutine alloc_light_posi_in_view
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_color_parameteres(color)
!
      type(pvr_colormap_parameter), intent(inout) :: color
!
!
      deallocate(color%pvr_datamap_param)
      deallocate(color%pvr_opacity_param)
      deallocate(color%xyz_pvr_lights)
!
      end subroutine dealloc_pvr_color_parameteres
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_flag_pvr_movie_mode()
      num_flag_pvr_movie_mode = n_flag_pvr_movie_mode
      return
      end function num_flag_pvr_movie_mode
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_flag_LIC_movie_mode()
      num_flag_LIC_movie_mode = n_flag_LIC_movie_mode
      return
      end function num_flag_LIC_movie_mode
!
!  ---------------------------------------------------------------------
!
      subroutine set_flag_pvr_movie_mode(names)
!
      use t_read_control_elements
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_flag_pvr_movie_mode)
!
!
      call set_control_labels(FLAG_ROTATE_MOVIE,   names( 1))
      call set_control_labels(FLAG_ZOOM,           names( 2))
      call set_control_labels(FLAG_START_END_VIEW, names( 3))
!
      end subroutine set_flag_pvr_movie_mode
!
! ----------------------------------------------------------------------
!
      subroutine set_flag_LIC_movie_mode(names)
!
      use t_read_control_elements
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_flag_LIC_movie_mode)
!
!
      call set_control_labels(FLAG_ROTATE_MOVIE,   names( 1))
      call set_control_labels(FLAG_ZOOM,           names( 2))
      call set_control_labels(FLAG_START_END_VIEW, names( 3))
      call set_control_labels(FLAG_LIC_KERNEL,     names( 4))
!
      end subroutine set_flag_LIC_movie_mode
!
! ----------------------------------------------------------------------
!
      end module t_control_params_4_pvr
