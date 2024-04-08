!>@file   t_ctl_data_4_view_transfer.f90
!!@brief  module t_ctl_data_4_view_transfer
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR view parameter
!!
!!@verbatim
!!      subroutine dealloc_view_transfer_ctl(mat)
!!        type(modeview_ctl), intent(inout) :: mat
!!      subroutine dup_view_transfer_ctl(org_mat, new_mat)
!!        type(modeview_ctl), intent(in) :: org_mat
!!        type(modeview_ctl), intent(inout) :: new_mat
!!      logical function cmp_modeview_ctl(mat1, mat2)
!!        type(modeview_ctl), intent(in) :: mat1, mat2
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
!!      perspective_angle_ctl     10.0
!!      perspective_xy_ratio_ctl   1.0
!!      perspective_near_ctl       0.5
!!      perspective_far_ctl     1000.0
!!
!!      horizontal_range_ctl       -2.4   2.4
!!      vertical_range_ctl         -1.2   1.2
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
      use t_control_array_character
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
!>        Control block name
        character(len = kchara) :: block_name = 'view_transform_ctl'
!
!>        Structure of screen resolution
        type(screen_pixel_ctl) :: pixel
!>        Structure for projection parameters
        type(projection_ctl) :: proj
!>        Structure of streo view parameters
        type(streo_view_ctl) :: streo
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
!>      Structure for projection type for 2D plot
        type(read_character_item) :: projection_type_ctl
!
!>        loaded flag
        integer (kind=kint) :: i_view_transform = 0
!
!>        Consistency check flag
        logical :: flag_checked = .FALSE.
      end type modeview_ctl
!
!  ---------------------------------------------------------------------
!
      contains
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
      mat%projection_type_ctl%iflag = 0
!
      call reset_image_size_ctl(mat%pixel)
      call reset_projection_view_ctl(mat%proj)
      call reset_stereo_view_ctl(mat%streo)
!
      end subroutine dealloc_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dup_view_transfer_ctl(org_mat, new_mat)
!
      type(modeview_ctl), intent(in) :: org_mat
      type(modeview_ctl), intent(inout) :: new_mat
!
!
      new_mat%block_name =       org_mat%block_name
      new_mat%i_view_transform = org_mat%i_view_transform
!
      call dup_control_array_c_r(org_mat%lookpoint_ctl,                 &
     &                           new_mat%lookpoint_ctl)
      call dup_control_array_c_r(org_mat%viewpoint_ctl,                 &
     &                           new_mat%viewpoint_ctl)
      call dup_control_array_c_r(org_mat%up_dir_ctl,                    &
     &                           new_mat%up_dir_ctl)
!
      call dup_control_array_c_r(org_mat%view_rot_vec_ctl,              &
     &                           new_mat%view_rot_vec_ctl)
      call dup_control_array_c_r(org_mat%scale_vector_ctl,              &
     &                           new_mat%scale_vector_ctl)
      call dup_control_array_c_r(org_mat%viewpt_in_viewer_ctl,          &
     &                           new_mat%viewpt_in_viewer_ctl)
!
      call dup_control_array_c2_r(org_mat%modelview_mat_ctl,            &
     &                            new_mat%modelview_mat_ctl)
!
      call copy_real_ctl(org_mat%view_rotation_deg_ctl,                 &
     &                   new_mat%view_rotation_deg_ctl)
      call copy_real_ctl(org_mat%scale_factor_ctl,                      &
     &                   new_mat%scale_factor_ctl)
!
      call copy_chara_ctl(org_mat%projection_type_ctl,                  &
     &                   new_mat%projection_type_ctl)
!
      call copy_projection_mat_ctl(org_mat%proj, new_mat%proj)
      call copy_image_size_ctl(org_mat%pixel, new_mat%pixel)
      call copy_stereo_view_ctl(org_mat%streo, new_mat%streo)
!
      end subroutine dup_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!
      logical function cmp_modeview_ctl(mat1, mat2)
!
      type(modeview_ctl), intent(in) :: mat1, mat2
!
      cmp_modeview_ctl = .FALSE.
      if(mat1%i_view_transform .ne. mat2%i_view_transform) return
      if(cmp_no_case(trim(mat1%block_name),                             &
     &               trim(mat2%block_name)) .eqv. .FALSE.) return
!
      if(cmp_screen_pixel_ctl(mat1%pixel, mat2%pixel)                   &
     &                                            .eqv. .FALSE.) return
      if(cmp_projection_ctl(mat1%proj, mat2%proj) .eqv. .FALSE.) return
      if(cmp_streo_view_ctl(mat1%streo, mat2%streo)                     &
     &                                            .eqv. .FALSE.) return
!
      if(cmp_control_array_c2_r(mat1%modelview_mat_ctl,                 &
     &                          mat2%modelview_mat_ctl)                 &
     &                                            .eqv. .FALSE.) return
!
      if(cmp_control_array_c_r(mat1%lookpoint_ctl, mat2%lookpoint_ctl)  &
     &                                            .eqv. .FALSE.) return
      if(cmp_control_array_c_r(mat1%viewpoint_ctl, mat2%viewpoint_ctl)  &
     &                                            .eqv. .FALSE.) return
      if(cmp_control_array_c_r(mat1%up_dir_ctl, mat2%up_dir_ctl)        &
     &                                            .eqv. .FALSE.) return
      if(cmp_control_array_c_r(mat1%view_rot_vec_ctl,                   &
     &                         mat2%view_rot_vec_ctl)                   &
     &                                            .eqv. .FALSE.) return
      if(cmp_control_array_c_r(mat1%scale_vector_ctl,                   &
     &                         mat2%scale_vector_ctl)                   &
     &                                            .eqv. .FALSE.) return
      if(cmp_control_array_c_r(mat1%viewpt_in_viewer_ctl,               &
     &                         mat2%viewpt_in_viewer_ctl)               &
     &                                            .eqv. .FALSE.) return
!
      if(cmp_read_real_item(mat1%view_rotation_deg_ctl,                 &
     &                      mat2%view_rotation_deg_ctl)                 &
     &                                            .eqv. .FALSE.) return
      if(cmp_read_real_item(mat1%scale_factor_ctl,                      &
     &                      mat2%scale_factor_ctl)                      &
     &                                            .eqv. .FALSE.) return
!
      if(cmp_read_chara_item(mat1%projection_type_ctl,                  &
     &                       mat2%projection_type_ctl)                  &
     &                                            .eqv. .FALSE.) return
!
      cmp_modeview_ctl = .TRUE.
!
      end function cmp_modeview_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_4_view_transfer
