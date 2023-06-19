!>@file   ctl_data_pvr_movie_IO.f90
!!@brief  module ctl_data_pvr_movie_IO
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for PVR movie from snapshot
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_pvr_rotation_ctl                                &
!!     &         (id_control, hd_block, movie, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_movie_ctl), intent(inout) :: movie
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_pvr_rotation_ctl                               &
!!     &         (id_control, hd_block, movie, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_movie_ctl), intent(in) :: movie
!!        integer(kind = kint), intent(inout) :: level
!!
!!      integer(kind = kint) function num_label_pvr_movie()
!!      integer(kind = kint) function num_label_LIC_movie()
!!      subroutine set_label_pvr_movie(names)
!!      subroutine set_label_LIC_movie(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    Avaiable parameters for movie_mode_ctl:
!!        rotation, zoom, view_matrices, LIC_kernel, looking_glass
!!
!!  begin snapshot_movie_ctl
!!    movie_mode_ctl       rotation
!!    num_frames_ctl        120
!!
!!    rotation_axis_ctl       z
!!
!!    file start_view_control    'ctl_view_start'
!!    file end_view_control      'ctl_view_end'
!!
!!    array view_transform_ctl
!!      file  view_transform_ctl  control_view
!!
!!      begin view_transform_ctl
!!        ..
!!      end
!!    end array view_transform_ctl
!!
!!    angle_range             0.0   360.0
!!    apature_range           10.0  1.0
!!
!!    LIC_kernel_peak_range      -0.8  0.8
!!  end snapshot_movie_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    movie_mode_ctl:   view_matrices, rotation, apature, LIC_kernel
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_data_pvr_movie_IO
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_data_4_psf
      use t_control_array_character
      use t_control_array_integer
      use t_control_array_real2
      use t_control_array_integer2
      use t_ctl_data_4_view_transfer
      use t_ctl_data_view_transfers
      use t_ctl_data_pvr_movie
      use skip_comment_f
!
      implicit  none
!
!
!
!     3rd level for movie
!
      character(len=kchara), parameter, private                         &
     &             :: hd_movie_mode =      'movie_mode_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_movie_num_frame = 'num_frames_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_movie_rot_axis =  'rotation_axis_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_start_view_control = 'start_view_control'
      character(len=kchara), parameter, private                         &
     &             :: hd_end_view_control = 'end_view_control'
      character(len=kchara), parameter, private                         &
     &             :: hd_mview_transform =   'view_transform_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_angle_range =   'angle_range'
      character(len=kchara), parameter, private                         &
     &             :: hd_apature_range = 'apature_range'
      character(len=kchara), parameter, private                         &
     &             :: hd_LIC_kernel_peak = 'LIC_kernel_peak_range'
!
      integer(kind = kint), parameter :: n_label_pvr_movie =  8
      integer(kind = kint), parameter :: n_label_LIC_movie =  9
!
      private :: n_label_pvr_movie, n_label_LIC_movie
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_rotation_ctl                                  &
     &         (id_control, hd_block, movie, c_buf)
!
      use ctl_file_pvr_modelview_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_movie_ctl), intent(inout) :: movie
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (movie%i_pvr_rotation.gt.0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        if(check_file_flag(c_buf, hd_start_view_control)                &
     &        .or. check_begin_flag(c_buf, hd_start_view_control)) then
          call write_multi_ctl_file_message                             &
     &       (hd_start_view_control, izero, c_buf%level)
          call sel_read_ctl_modelview_file                              &
     &       (id_control, hd_start_view_control,                        &
     &        movie%fname_view_start_ctl, movie%view_start_ctl, c_buf)
        end if
!
        if(check_file_flag(c_buf, hd_end_view_control)                  &
     &        .or. check_begin_flag(c_buf, hd_end_view_control)) then
          call write_multi_ctl_file_message                             &
     &       (hd_end_view_control, izero, c_buf%level)
         call sel_read_ctl_modelview_file                               &
     &       (id_control, hd_end_view_control,                          &
     &        movie%fname_view_end_ctl, movie%view_end_ctl, c_buf)
        end if
!
!
        call read_chara_ctl_type(c_buf, hd_movie_mode,                  &
     &      movie%movie_mode_ctl)
!
        call read_integer_ctl_type(c_buf, hd_movie_num_frame,           &
     &      movie%num_frames_ctl)
        call read_chara_ctl_type(c_buf, hd_movie_rot_axis,              &
     &      movie%rotation_axis_ctl)
!
        call read_real2_ctl_type(c_buf, hd_angle_range,                 &
     &      movie%angle_range_ctl)
        call read_real2_ctl_type(c_buf, hd_apature_range,               &
     &      movie%apature_range_ctl)
        call read_real2_ctl_type(c_buf, hd_LIC_kernel_peak,             &
     &      movie%LIC_kernel_peak_range_ctl)
!
        call read_mul_view_transfer_ctl                                 &
     &     (id_control, hd_mview_transform, movie%mul_mmats_c, c_buf)
      end do
      movie%i_pvr_rotation = 1
!
      end subroutine read_pvr_rotation_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_pvr_rotation_ctl                                 &
     &         (id_control, hd_block, movie, level)
!
      use ctl_file_pvr_modelview_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_movie_ctl), intent(in) :: movie
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(movie%i_pvr_rotation .le. 0) return
!
      maxlen = len_trim(hd_movie_mode)
      maxlen = max(maxlen, len_trim(hd_movie_rot_axis))
      maxlen = max(maxlen, len_trim(hd_angle_range))
      maxlen = max(maxlen, len_trim(hd_apature_range))
      maxlen = max(maxlen, len_trim(hd_LIC_kernel_peak))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_movie_mode, movie%movie_mode_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_movie_num_frame, movie%num_frames_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_movie_rot_axis, movie%rotation_axis_ctl)
!
      write(*,'(2a)', ADVANCE='NO') '!  ', trim(hd_start_view_control)
      call sel_write_ctl_modelview_file                                 &
     &   (id_control, hd_start_view_control,                            &
     &    movie%fname_view_start_ctl, movie%view_start_ctl, level)
      write(*,'(2a)', ADVANCE='NO') '!  ', trim(hd_end_view_control)
      call sel_write_ctl_modelview_file                                 &
     &   (id_control, hd_end_view_control,                              &
     &    movie%fname_view_end_ctl, movie%view_end_ctl, level)
!
      call write_mul_view_transfer_ctl                                  &
     &   (id_control, hd_mview_transform, movie%mul_mmats_c, level)
!
      call write_real2_ctl_type(id_control, level, maxlen,              &
     &    hd_angle_range, movie%angle_range_ctl)
      call write_real2_ctl_type(id_control, level, maxlen,              &
     &    hd_apature_range, movie%apature_range_ctl)
      call write_real2_ctl_type(id_control, level, maxlen,              &
     &    hd_LIC_kernel_peak, movie%LIC_kernel_peak_range_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_pvr_rotation_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_movie()
      num_label_pvr_movie = n_label_pvr_movie
      return
      end function num_label_pvr_movie
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_LIC_movie()
      num_label_LIC_movie = n_label_LIC_movie
      return
      end function num_label_LIC_movie
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_movie(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_movie)
!
!
      call set_control_labels(hd_movie_mode,      names( 1))
      call set_control_labels(hd_movie_num_frame, names( 2))
!
      call set_control_labels(hd_movie_rot_axis,   names( 3))
!
      call set_control_labels(hd_start_view_control, names( 4))
      call set_control_labels(hd_end_view_control,   names( 5))
      call set_control_labels(hd_mview_transform,    names( 6))
!
      call set_control_labels(hd_angle_range,        names( 7))
      call set_control_labels(hd_apature_range,      names( 8))
!
      end subroutine set_label_pvr_movie
!
! ----------------------------------------------------------------------
!
      subroutine set_label_LIC_movie(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_LIC_movie)
!
!
      call set_control_labels(hd_movie_mode,      names( 1))
      call set_control_labels(hd_movie_num_frame, names( 2))
!
      call set_control_labels(hd_movie_rot_axis,   names( 3))
!
      call set_control_labels(hd_start_view_control, names( 4))
      call set_control_labels(hd_end_view_control,   names( 5))
      call set_control_labels(hd_mview_transform,    names( 6))
!
      call set_control_labels(hd_angle_range,        names( 7))
      call set_control_labels(hd_apature_range,      names( 8))
!
      call set_control_labels(hd_LIC_kernel_peak,   names( 9))
!
      end subroutine set_label_LIC_movie
!
! ----------------------------------------------------------------------
!
      end module ctl_data_pvr_movie_IO