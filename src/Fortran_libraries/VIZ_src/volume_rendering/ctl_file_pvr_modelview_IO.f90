!>@file   ctl_file_pvr_modelview_IO.f90
!!@brief  module ctl_file_pvr_modelview_IO
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR view parameter
!!
!!@verbatim
!!      subroutine sel_read_ctl_modelview_file                          &
!!     &         (id_control, hd_block, icou, file_name, mat, c_buf)
!!      subroutine sel_write_ctl_modelview_file                         &
!!     &         (id_control, hd_block, file_name, mat, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(modeview_ctl), intent(in) :: mat
!!        character(len = kchara), intent(inout) :: file_name
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine write_control_modelview_file(id_control, file_name,  &
!!     &                                        hd_block, mat)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len = kchara), intent(in) :: file_name
!!        type(modeview_ctl), intent(in) :: mat
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
!!    begin projection_matrix_ctl
!!      ...
!!    end projection_matrix_ctl
!!
!!    begin stereo_view_parameter_ctl
!!      focal_distance_ctl           40.0
!!      eye_separation_ctl            0.5
!!      eye_separation_angle          35.0
!!      eye_separation_step_by_angle     ON
!!    end stereo_view_parameter_ctl
!!
!!  end view_transform_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
!
      module ctl_file_pvr_modelview_IO
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
      private :: read_control_modelview_file
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_ctl_modelview_file                            &
     &         (id_control, hd_block, icou, file_name, mat, c_buf)
!
      use ctl_data_view_transfer_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, icou
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(inout) :: file_name
      type(modeview_ctl), intent(inout) :: mat
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_block)) then
        file_name = third_word(c_buf)
!
        call write_multi_ctl_file_message(hd_block, icou, c_buf%level)
        write(*,'(2a)') ' is read from ... ', trim(file_name)
        call read_control_modelview_file(id_control+2, file_name,       &
     &                                   hd_block, mat, c_buf)
      else if(check_begin_flag(c_buf, hd_block)) then
        file_name = 'NO_FILE'
!
        write(*,'(a)') ' is included'
        call read_view_transfer_ctl(id_control, hd_block, mat, c_buf)
      end if
!
      end subroutine sel_read_ctl_modelview_file
!
!  ---------------------------------------------------------------------
!
      subroutine sel_write_ctl_modelview_file                           &
     &         (id_control, hd_block, file_name, mat, level)
!
      use skip_comment_f
      use ctl_data_view_transfer_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(modeview_ctl), intent(in) :: mat
      integer(kind = kint), intent(inout) :: level
!
!
      if(no_file_flag(file_name)) then
        write(*,'(3a)')  '!  ', trim(hd_block),  ' is included'
        call write_view_transfer_ctl(id_control, hd_block, mat, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(4a)') '!  ', trim(hd_block),                          &
     &        ' should be written to file ... ', trim(file_name)
        call write_view_transfer_ctl(id_control, hd_block, mat, level)
      else
        write(*,'(4a)') 'Write file for ', trim(hd_block),              &
     &                  '... ', trim(file_name)
        call write_control_modelview_file(id_control+2, file_name,      &
     &                                    hd_block, mat)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, file_name)
      end if
!
      end subroutine sel_write_ctl_modelview_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_modelview_file(id_control, file_name,     &
     &                                       hd_block, mat, c_buf)
!
      use skip_comment_f
      use ctl_data_view_transfer_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(modeview_ctl), intent(inout) :: mat
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      open(id_control, file = file_name, status='old')
!
      do 
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_view_transfer_ctl(id_control, hd_block, mat, c_buf)
        if(mat%i_view_transform .gt. 0) exit
      end do
      close(id_control)
!
      c_buf%level = c_buf%level - 1
!
      end subroutine read_control_modelview_file
!
!  ---------------------------------------------------------------------
!
      subroutine write_control_modelview_file(id_control, file_name,    &
     &                                        hd_block, mat)
!
      use ctl_data_view_transfer_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(modeview_ctl), intent(in) :: mat
!
      integer(kind = kint) :: level
!
!
      write(*,*) trim(file_name)
      open(id_control, file = file_name)
!
      level = 0
      call write_view_transfer_ctl(id_control, hd_block, mat, level)
      close(id_control)
!
      end subroutine write_control_modelview_file
!
!  ---------------------------------------------------------------------
!
      end module ctl_file_pvr_modelview_IO
