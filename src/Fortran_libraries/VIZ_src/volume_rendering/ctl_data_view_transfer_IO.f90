!>@file   ctl_data_view_transfer_IO.f90
!!@brief  module ctl_data_view_transfer_IO
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR view parameter
!!
!!@verbatim
!!      subroutine read_view_transfer_ctl                               &
!!     &         (id_control, hd_block, mat, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(modeview_ctl), intent(inout) :: mat
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_view_transfer_ctl                              &
!!     &         (id_control, hd_block, mat, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(modeview_ctl), intent(in) :: mat
!!        integer(kind = kint), intent(inout) :: level
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
!!    array eye_position_ctl
!!      eye_position_ctl  x      3.0
!!      eye_position_ctl  y     -8.0
!!      eye_position_ctl  z      6.0
!!    end array eye_position_ctl
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
!!    array eye_position_in_viewer_ctl
!!      eye_position_in_viewer_ctl  x      0.0
!!      eye_position_in_viewer_ctl  y      0.0
!!      eye_position_in_viewer_ctl  z      10.0
!!    end array eye_position_in_viewer_ctl
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
!!    projection_type_ctl      Aitoff, xy_plane, xz_plane, yz_plane
!!    begin projection_matrix_ctl
!!      ...
!!    end projection_matrix_ctl
!!
!!    begin stereo_view_parameter_ctl
!!      focal_distance_ctl       40.0
!!      eye_separation_ctl        0.5
!!    end stereo_view_parameter_ctl
!!
!!  end view_transform_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
      module ctl_data_view_transfer_IO
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
      use t_ctl_data_4_view_transfer
      use skip_comment_f
!
      implicit  none
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
     &             :: hd_look_point =   'look_at_point_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_eye_position = 'eye_position_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_up_dir =       'up_direction_ctl'
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
     &             :: hd_eye_in_view = 'eye_position_in_viewer_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_stereo_view = 'stereo_view_parameter_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_projection_type = 'projection_type_ctl'
!
!      Old definision
      character(len=kchara), parameter, private                         &
     &             :: hd_view_point =  'viewpoint_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_viewpt_in_view = 'viewpoint_in_viewer_ctl'
!
      integer(kind = kint), parameter :: n_label_pvr_modelview =  13
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
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(mat%i_view_transform .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
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
!
        call read_control_array_c_r(id_control,                         &
     &      hd_eye_position, mat%viewpoint_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_view_point, mat%viewpoint_ctl, c_buf)
!
        call read_control_array_c_r(id_control,                         &
     &      hd_up_dir, mat%up_dir_ctl, c_buf)
!
        call read_control_array_c_r(id_control,                         &
     &      hd_view_rot_dir, mat%view_rot_vec_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_scale_fac_dir, mat%scale_vector_ctl, c_buf)
!
        call read_control_array_c_r(id_control,                         &
     &      hd_eye_in_view, mat%viewpt_in_viewer_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_viewpt_in_view, mat%viewpt_in_viewer_ctl, c_buf)
!
        call read_control_array_c2_r(id_control,                        &
     &      hd_model_mat, mat%modelview_mat_ctl, c_buf)
!
        call read_real_ctl_type(c_buf, hd_view_rot_deg,                 &
     &      mat%view_rotation_deg_ctl)
        call read_real_ctl_type(c_buf, hd_scale_factor,                 &
     &      mat%scale_factor_ctl)
        call read_chara_ctl_type(c_buf, hd_projection_type,             &
     &      mat%projection_type_ctl)
      end do
      mat%i_view_transform = 1
!
      end subroutine read_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_view_transfer_ctl                                &
     &         (id_control, hd_block, mat, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(modeview_ctl), intent(in) :: mat
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(mat%i_view_transform .le. 0) return
!
      maxlen = len_trim(hd_view_rot_deg)
      maxlen = max(maxlen, len_trim(hd_scale_factor))
      maxlen = max(maxlen, len_trim(hd_projection_type))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_projection_type, mat%projection_type_ctl)
      call write_projection_mat_ctl                                     &
     &   (id_control, hd_project_mat, mat%proj, level)
      call write_image_size_ctl                                         &
     &   (id_control, hd_image_size, mat%pixel, level)
      call write_stereo_view_ctl                                        &
     &   (id_control, hd_stereo_view, mat%streo, level)
!
      call write_control_array_c_r(id_control, level,                   &
     &    hd_look_point, mat%lookpoint_ctl)
      call write_control_array_c_r(id_control, level,                   &
     &    hd_eye_position, mat%viewpoint_ctl)
      call write_control_array_c_r(id_control, level,                   &
     &    hd_up_dir, mat%up_dir_ctl)
!
      call write_control_array_c_r(id_control, level,                   &
     &    hd_view_rot_dir, mat%view_rot_vec_ctl)
      call write_control_array_c_r(id_control, level,                   &
     &    hd_scale_fac_dir, mat%scale_vector_ctl)
      call write_control_array_c_r(id_control, level,                   &
     &    hd_eye_in_view, mat%viewpt_in_viewer_ctl)
!
      call write_control_array_c2_r(id_control, level,                  &
     &    hd_model_mat, mat%modelview_mat_ctl)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_view_rot_deg, mat%view_rotation_deg_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_scale_factor, mat%scale_factor_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_view_transfer_ctl
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
      call set_control_labels(hd_eye_position, names( 3))
      call set_control_labels(hd_up_dir,       names( 4))
      call set_control_labels(hd_view_rot_dir, names( 5))
      call set_control_labels(hd_view_rot_deg, names( 6))
!
      call set_control_labels(hd_scale_factor,   names( 7))
      call set_control_labels(hd_scale_fac_dir,  names( 8))
      call set_control_labels(hd_eye_in_view,    names( 9))
!
      call set_control_labels(hd_project_mat, names(10))
      call set_control_labels(hd_model_mat,   names(11))
!
      call set_control_labels(hd_stereo_view, names(12))
      call set_control_labels(hd_projection_type, names(13))
!
      end subroutine set_label_pvr_modelview
!
! ----------------------------------------------------------------------
!
      end module ctl_data_view_transfer_IO
