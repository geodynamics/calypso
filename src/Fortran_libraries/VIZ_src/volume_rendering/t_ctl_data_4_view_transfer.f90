!>@file   t_ctl_data_4_view_transfer.f90
!!@brief  module t_ctl_data_4_view_transfer
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR view parameter
!!
!!@verbatim
!!      subroutine read_view_transfer_ctl                               &
!!     &         (id_control, hd_block, mat, c_buf)
!!      subroutine dealloc_view_transfer_ctl(mat)
!!        type(modeview_ctl), intent(inout) :: mat
!!
!!      integer(kind = kint) function num_label_pvr_modelview()
!!      subroutine set_label_pvr_modelview(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Input example
!
!!  begin view_transform_ctl
!!
!!     begin image_size_ctl
!!       x_pixel_ctl  640
!!       y_pixel_ctl  480
!!     end image_size_ctl
!!
!!    array look_at_point_ctl
!!      look_at_point_ctl  x      3.0
!!      look_at_point_ctl  y     -8.0
!!      look_at_point_ctl  z      6.0 
!!    end  array look_at_point_ctl
!!
!!    array viewpoint_ctl
!!      viewpoint_ctl  x      3.0
!!      viewpoint_ctl  y     -8.0
!!      viewpoint_ctl  z      6.0 
!!    end array viewpoint_ctl
!!
!!    array up_direction_ctl
!!      up_direction_ctl  x      0.0
!!      up_direction_ctl  y      0.0
!!      up_direction_ctl  z      1.0
!!    end array up_direction_ctl
!!
!!    array view_rotation_vec_ctl
!!      view_rotation_vec_ctl  x      0.0
!!      view_rotation_vec_ctl  y      0.0
!!      view_rotation_vec_ctl  z      1.0
!!    end array view_rotation_vec_ctl
!!
!!    view_rotation_deg_ctl    60.0
!!
!!    scale_factor_ctl            1.0
!!    array scale_factor_vec_ctl
!!      scale_factor_vec_ctl  x      0.0
!!      scale_factor_vec_ctl  y      0.0
!!      scale_factor_vec_ctl  z      1.0
!!    end array scale_factor_vec_ctl
!!
!!    array viewpoint_in_viewer_ctl
!!      viewpoint_in_viewer_ctl  x      0.0
!!      viewpoint_in_viewer_ctl  y      0.0
!!      viewpoint_in_viewer_ctl  z      10.0
!!    end array viewpoint_in_viewer_ctl
!!
!!    array  modelview_matrix_ctl
!!      modelview_matrix_ctl   1  1  1.0  end
!!      modelview_matrix_ctl   2  1  0.0  end
!!      modelview_matrix_ctl   3  1  0.0  end
!!      modelview_matrix_ctl   4  1  0.0  end
!!
!!      modelview_matrix_ctl   1  2  0.0  end
!!      modelview_matrix_ctl   2  2  1.0  end
!!      modelview_matrix_ctl   3  2  0.0  end
!!      modelview_matrix_ctl   4  2  0.0  end
!!
!!      modelview_matrix_ctl   1  3  0.0  end
!!      modelview_matrix_ctl   2  3  0.0  end
!!      modelview_matrix_ctl   3  3  1.0  end
!!      modelview_matrix_ctl   4  3  0.0  end
!!
!!      modelview_matrix_ctl   1  4  0.0  end
!!      modelview_matrix_ctl   2  4  0.0  end
!!      modelview_matrix_ctl   3  4  0.0  end
!!      modelview_matrix_ctl   4  4  1.0  end
!!    end array modelview_matrix_ctl
!!
!!    Orthogonal view....( perspective_near_ctl = perspective_far_ctl)
!!
!!    begin projection_matrix_ctl
!!      perspective_angle_ctl     10.0
!!      perspective_xy_ratio_ctl   1.0
!!      perspective_near_ctl       0.5
!!      perspective_far_ctl     1000.0
!!    end projection_matrix_ctl
!!
!!    begin stereo_view_parameter_ctl
!!      focal_point_ctl           40.0
!!      eye_separation_ctl        0.5
!!    end stereo_view_parameter_ctl
!!
!!  end view_transform_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
!
      module t_ctl_data_4_view_transfer
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_real
      use t_control_array_charareal
      use t_control_array_chara2real
      use t_ctl_data_4_screen_pixel
      use t_ctl_data_4_projection
      use t_ctl_data_4_streo_view
      use skip_comment_f
!
      implicit  none
!
!
!>    Structure for modelview marices
      type modeview_ctl
!>      Structure of screen resolution
        type(screen_pixel_ctl) :: pixel
!>    Structure for projection parameters
        type(projection_ctl) :: proj
!>      Structure of streo view parameters
        type(streo_view_ctl) :: streo
!
!
!>    Structure for opacity controls
!!@n      modelview_mat_ctl%c1_tbl:  1st component name for matrix
!!@n      modelview_mat_ctl%c2_tbl:  2nd component name for matrix
!!@n      modelview_mat_ctl%vect:    Modelview matrix
        type(ctl_array_c2r) :: modelview_mat_ctl
!
!>      Structure for look at  controls
!!@n      lookpoint_ctl%c_tbl:   component of lookpoint
!!@n      lookpoint_ctl%vect:    Position of lookpoint
        type(ctl_array_cr) :: lookpoint_ctl
!
!>      Structure for viewpoint controls
!!@n      viewpoint_ctl%c_tbl:   Direction of viewpoint
!!@n      viewpoint_ctl%vect:    Position of viewpoint
        type(ctl_array_cr) :: viewpoint_ctl
!
!>      Structure for Up-directions controls
!!@n      up_dir_ctl%c_tbl:   Direction of  Up-directions
!!@n      up_dir_ctl%vect:    Position of  Up-directions
        type(ctl_array_cr) :: up_dir_ctl
!
!>      Structure for rotation of object
!!@n      view_rot_vec_ctl%c_tbl:   Direction of rotatin vector
!!@n      view_rot_vec_ctl%vect:    rotation vector
        type(ctl_array_cr) :: view_rot_vec_ctl
!
!>      Structure for rotation of rotatin angle of view
        type(read_real_item) :: view_rotation_deg_ctl
!>      Structure for scale factor
        type(read_real_item) :: scale_factor_ctl
!
!>      Structure for scale factor controls
!!@n      scale_vector_ctl%c_tbl:   Direction of scale factor
!!@n      scale_vector_ctl%vect:    Position of scale factor
        type(ctl_array_cr) :: scale_vector_ctl
!
!>      Structure for viewpoint in viewer controls
!!@n      viewpt_in_viewer_ctl%c_tbl:   Direction of viewpoint in viewer
!!@n      viewpt_in_viewer_ctl%vect:    Position of viewpoint in viewer
        type(ctl_array_cr) :: viewpt_in_viewer_ctl
!
!
!   entry label for this block
        integer (kind=kint) :: i_view_transform = 0
      end type modeview_ctl
!
!
!     3rd level for view_transform_define
      character(len=kchara), parameter, private                         &
     &             :: hd_image_size =    'image_size_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_model_mat =   'modelview_matrix_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_project_mat = 'projection_matrix_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_look_point =  'look_at_point_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_view_point =  'viewpoint_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_up_dir =      'up_direction_ctl'
!
!
      character(len=kchara), parameter, private                         &
     &             :: hd_view_rot_deg = 'view_rotation_deg_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_view_rot_dir = 'view_rotation_vec_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_scale_factor = 'scale_factor_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_scale_fac_dir = 'scale_factor_vec_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_viewpt_in_view = 'viewpoint_in_viewer_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_stereo_view = 'stereo_view_parameter_ctl'
!
      integer(kind = kint), parameter :: n_label_pvr_modelview =  12
      private :: n_label_pvr_modelview
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_view_transfer_ctl                                 &
     &         (id_control, hd_block, mat, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(modeview_ctl), intent(inout) :: mat
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if (mat%i_view_transform .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_projection_mat_ctl                                    &
     &     (id_control, hd_project_mat, mat%proj, c_buf)
        call read_image_size_ctl                                        &
     &     (id_control, hd_image_size, mat%pixel, c_buf)
        call read_stereo_view_ctl                                       &
     &     (id_control, hd_stereo_view, mat%streo, c_buf)
!
!
        call read_control_array_c_r(id_control,                         &
     &      hd_look_point, mat%lookpoint_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_view_point, mat%viewpoint_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_up_dir, mat%up_dir_ctl, c_buf)
!
        call read_control_array_c_r(id_control,                         &
     &      hd_view_rot_dir, mat%view_rot_vec_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_scale_fac_dir, mat%scale_vector_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_viewpt_in_view, mat%viewpt_in_viewer_ctl, c_buf)
!
        call read_control_array_c2_r(id_control,                        &
     &      hd_model_mat, mat%modelview_mat_ctl, c_buf)
!
        call read_real_ctl_type(c_buf, hd_view_rot_deg,                 &
     &        mat%view_rotation_deg_ctl)
        call read_real_ctl_type(c_buf, hd_scale_factor,                 &
     &        mat%scale_factor_ctl)
      end do
      mat%i_view_transform = 1
!
      end subroutine read_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_view_transfer_ctl(mat)
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      mat%i_view_transform = 0
!
      call dealloc_control_array_c2_r(mat%modelview_mat_ctl)
      mat%modelview_mat_ctl%num =    0
      mat%modelview_mat_ctl%icou =   0
!
      call dealloc_control_array_c_r(mat%lookpoint_ctl)
      call dealloc_control_array_c_r(mat%viewpoint_ctl)
      call dealloc_control_array_c_r(mat%up_dir_ctl)
      mat%lookpoint_ctl%num =   0
      mat%viewpoint_ctl%num =   0
      mat%up_dir_ctl%num =      0
      mat%lookpoint_ctl%icou =  0
      mat%viewpoint_ctl%icou =  0
      mat%up_dir_ctl%icou =     0
!
      call dealloc_control_array_c_r(mat%view_rot_vec_ctl)
      call dealloc_control_array_c_r(mat%scale_vector_ctl)
      call dealloc_control_array_c_r(mat%viewpt_in_viewer_ctl)
      mat%view_rot_vec_ctl%num =     0
      mat%scale_vector_ctl%num =     0
      mat%viewpt_in_viewer_ctl%num = 0
      mat%view_rot_vec_ctl%icou =     0
      mat%scale_vector_ctl%icou =     0
      mat%viewpt_in_viewer_ctl%icou = 0
!
!
      mat%view_rotation_deg_ctl%realvalue =      0.0d0
      mat%scale_factor_ctl%realvalue =           1.0d0
!
      mat%view_rotation_deg_ctl%iflag = 0
      mat%scale_factor_ctl%iflag = 0
!
      call reset_image_size_ctl(mat%pixel)
      call reset_projection_view_ctl(mat%proj)
      call reset_stereo_view_ctl(mat%streo)
!
      end subroutine dealloc_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_modelview()
      num_label_pvr_modelview = n_label_pvr_modelview
      return
      end function num_label_pvr_modelview
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_modelview(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_modelview)
!
!
      call set_control_labels(hd_image_size,   names( 1))
!
      call set_control_labels(hd_look_point,   names( 2))
      call set_control_labels(hd_view_point,   names( 3))
      call set_control_labels(hd_up_dir,       names( 4))
      call set_control_labels(hd_view_rot_dir, names( 5))
      call set_control_labels(hd_view_rot_deg, names( 6))
!
      call set_control_labels(hd_scale_factor,   names( 7))
      call set_control_labels(hd_scale_fac_dir,  names( 8))
      call set_control_labels(hd_viewpt_in_view, names( 9))
!
      call set_control_labels(hd_project_mat, names(10))
      call set_control_labels(hd_model_mat,   names(11))
!
      call set_control_labels(hd_stereo_view, names(12))
!
      end subroutine set_label_pvr_modelview
!
! ----------------------------------------------------------------------
!
      end module t_ctl_data_4_view_transfer
