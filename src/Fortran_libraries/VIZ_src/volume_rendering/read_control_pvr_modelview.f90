!>@file   read_control_pvr_modelview.f90
!!@brief  module read_control_pvr_modelview
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR view parameter
!!
!!@verbatim
!!      subroutine read_control_modelview_file                          &
!!     &         (id_control, viewctl_file_name, mat)
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
!!      eye_separation_angle      35.0
!!      eye_separation_step_by_angle     ON
!!    end stereo_view_parameter_ctl
!!
!!  end view_transform_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
!
      module read_control_pvr_modelview
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_ctl_data_4_view_transfer
      use t_read_control_elements
!
      implicit  none
!
      character(len=kchara), parameter                                  &
     &                      :: hd_view_transform = 'view_transform_ctl'
      private :: hd_view_transform
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_modelview_file                            &
     &         (id_control, viewctl_file_name, mat)
!
      use calypso_mpi
      use m_error_IDs
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: viewctl_file_name
      type(modeview_ctl), intent(inout) :: mat
!
      type(buffer_for_control) :: c_buf1
!
!
      write(*,*) 'Modelview control:', trim(viewctl_file_name)
      open(id_control, file = viewctl_file_name, status='old')
!
      do 
        call load_one_line_from_control(id_control, c_buf1)
        if(check_begin_flag(c_buf1, hd_view_transform)) then
          call read_view_transfer_ctl(id_control, hd_view_transform,    &
     &        mat, c_buf1)
        end if
        if(mat%i_view_transform .gt. 0) exit
      end do
      close(id_control)

      if(mat%i_view_transform .eq. 0) then
        call calypso_mpi_abort(ierr_PVR, 'Set view matrix file')
      end if
!
      end subroutine read_control_modelview_file
!
!  ---------------------------------------------------------------------
!
      end module read_control_pvr_modelview
