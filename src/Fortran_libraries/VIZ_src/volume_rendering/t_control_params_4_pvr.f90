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
        real(kind = kreal) :: scale_factor_pvr(3) = (/one,one,one/)
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
!  ---------------------------------------------------------------------
!
      contains
!
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
!
      end module t_control_params_4_pvr
