!>@file   t_control_data_pvr_movie.f90
!!@brief  module t_control_data_pvr_movie
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
!!        type(pvr_movie_ctl), intent(inout) :: movie
!!
!!      subroutine dup_pvr_movie_control_flags(org_movie, new_movie)
!!        type(pvr_movie_ctl), intent(in) :: org_movie
!!        type(pvr_movie_ctl), intent(inout) :: new_movie
!!      subroutine dealloc_pvr_movie_control_flags(movie)
!!        type(pvr_movie_ctl), intent(inout) :: movie
!!
!!      subroutine bcast_pvr_rotation_ctl(movie)
!!        type(pvr_movie_ctl), intent(inout) :: movie
!!
!!      integer(kind = kint) function num_label_pvr_movie()
!!      integer(kind = kint) function num_label_LIC_movie()
!!      subroutine set_label_pvr_movie(names)
!!      subroutine set_label_LIC_movie(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    Avaiable parameters for movie_format_ctl:
!!        BMP, PNG, QUILT
!!    Avaiable parameters for movie_mode_ctl:
!!        rotation, zoom, view_matrices, LIC_kernel, looking_glass
!!
!!  begin image_rotation_ctl
!!    movie_format_ctl     QUILT
!!    movie_mode_ctl       rotation
!!    num_frames_ctl        120
!!    num_column_row_ctl       5     9
!!    num_row_column_ctl       9     5
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
!!  end image_rotation_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    movie_mode_ctl:   view_matrices, rotation, apature, LIC_kernel
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_pvr_movie
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
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_movie_ctl
!>        Structure of movie mode file format
        type(read_character_item) :: movie_format_ctl
!>        Structure of movie mode control
        type(read_character_item) :: movie_mode_ctl
!>        Structure of number of flame control
        type(read_integer_item) ::   num_frames_ctl
!>        Structure of number of columns and of image
        type(read_int2_item) :: quilt_column_row_ctl
!>        Structure of number of row and columns of image
        type(read_int2_item) :: quilt_row_column_ctl
!
!>        Structure of rotation axis control
        type(read_character_item) :: rotation_axis_ctl
!
!>        Structure of start and end of angle
        type(read_real2_item) :: angle_range_ctl
!>        Structure of start and end of apature
        type(read_real2_item) :: apature_range_ctl
!
!>        Structure of start and end of LIC kernel peak
        type(read_real2_item) :: LIC_kernel_peak_range_ctl
!
!>        file name for start modelview matrix
        character(len=kchara) :: start_view_file_ctl
!>        file name for end modelview matrix
        character(len=kchara) :: end_view_file_ctl
!>    Structure for start modelview marices
        type(modeview_ctl) :: view_start_ctl
!>    Structure for end modelview marices
        type(modeview_ctl) :: view_end_ctl
!
!         Lists of multiple view parameters
        type(multi_modeview_ctl) :: mul_mmats_c
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_pvr_rotation = 0
      end type pvr_movie_ctl
!
!     3rd level for rotation
!
      character(len=kchara), parameter, private                         &
     &             :: hd_movie_format =    'movie_format_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_movie_mode =      'movie_mode_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_movie_num_frame = 'num_frames_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_column_row =      'num_column_row_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_row_column =      'num_row_column_ctl'
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
      integer(kind = kint), parameter :: n_label_pvr_movie =  11
      integer(kind = kint), parameter :: n_label_LIC_movie =  12
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
      use read_control_pvr_modelview
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
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        if(check_file_flag(c_buf, hd_start_view_control)) then
          write(*,'(3a)', ADVANCE='NO')                                 &
     &            'Read file for ', trim(hd_start_view_control), '... '
          movie%start_view_file_ctl = third_word(c_buf)
          call read_control_modelview_file(id_control+2,                &
     &        movie%start_view_file_ctl, movie%view_start_ctl)
        else if(check_begin_flag(c_buf, hd_start_view_control)) then
          write(*,*)  'Start modelview control is included'
          call read_view_transfer_ctl(id_control,                       &
     &        hd_start_view_control, movie%view_start_ctl, c_buf)
        end if
!
        if(check_file_flag(c_buf, hd_end_view_control)) then
          write(*,'(3a)', ADVANCE='NO')                                 &
     &            'Read file for ', trim(hd_end_view_control), '... '
          movie%end_view_file_ctl = third_word(c_buf)
          call read_control_modelview_file(id_control+2,                &
     &        movie%end_view_file_ctl, movie%view_end_ctl)
        else if(check_begin_flag(c_buf, hd_end_view_control)) then
          write(*,*)  'End modelview control is included'
          call read_view_transfer_ctl(id_control,                       &
     &        hd_end_view_control, movie%view_end_ctl, c_buf)
        end if
!
!
        call read_chara_ctl_type(c_buf, hd_movie_format,                &
     &      movie%movie_format_ctl)
        call read_chara_ctl_type(c_buf, hd_movie_mode,                  &
     &      movie%movie_mode_ctl)
!
        call read_integer2_ctl_type(c_buf, hd_column_row,               &
     &      movie%quilt_column_row_ctl)
        call read_integer2_ctl_type(c_buf, hd_row_column,               &
     &      movie%quilt_row_column_ctl)
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
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_movie_control_flags(org_movie, new_movie)
!
      use bcast_dup_view_transfer_ctl
!
      type(pvr_movie_ctl), intent(in) :: org_movie
      type(pvr_movie_ctl), intent(inout) :: new_movie
!
!
      call dup_mul_view_trans_ctl(org_movie%mul_mmats_c,                &
     &                            new_movie%mul_mmats_c)
!
      call copy_chara_ctl(org_movie%movie_format_ctl,                   &
     &                    new_movie%movie_format_ctl)
      call copy_chara_ctl(org_movie%movie_mode_ctl,                     &
     &                    new_movie%movie_mode_ctl)
      call copy_integer_ctl(org_movie%num_frames_ctl,                   &
     &                      new_movie%num_frames_ctl)
      call copy_integer2_ctl(org_movie%quilt_column_row_ctl,            &
     &                       new_movie%quilt_column_row_ctl)
      call copy_integer2_ctl(org_movie%quilt_row_column_ctl,            &
     &                       new_movie%quilt_row_column_ctl)
!
      call copy_chara_ctl(org_movie%rotation_axis_ctl,                  &
     &                    new_movie%rotation_axis_ctl)
!
      call copy_real2_ctl(org_movie%angle_range_ctl,                    &
     &                    new_movie%angle_range_ctl)
      call copy_real2_ctl(org_movie%apature_range_ctl,                  &
     &                    new_movie%apature_range_ctl)
      call copy_real2_ctl(org_movie%LIC_kernel_peak_range_ctl,          &
     &                    new_movie%LIC_kernel_peak_range_ctl)
!
      new_movie%start_view_file_ctl = org_movie%start_view_file_ctl
      new_movie%end_view_file_ctl =   org_movie%end_view_file_ctl
      call dup_view_transfer_ctl(org_movie%view_start_ctl,              &
     &    new_movie%view_start_ctl)
      call dup_view_transfer_ctl(org_movie%view_end_ctl,                &
     &    new_movie%view_end_ctl)
!
      new_movie%i_pvr_rotation = org_movie%i_pvr_rotation
!
      end subroutine dup_pvr_movie_control_flags
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_movie_control_flags(movie)
!
      type(pvr_movie_ctl), intent(inout) :: movie
!
!
      call dealloc_multi_modeview_ctl(movie%mul_mmats_c)
!
      movie%movie_format_ctl%iflag =     0
      movie%movie_mode_ctl%iflag =       0
      movie%num_frames_ctl%iflag =       0
      movie%quilt_column_row_ctl%iflag = 0
      movie%quilt_row_column_ctl%iflag = 0
      movie%rotation_axis_ctl%iflag =    0
      movie%angle_range_ctl%iflag =      0
      movie%apature_range_ctl%iflag =    0
!
      movie%LIC_kernel_peak_range_ctl%iflag = 0
!
      call dealloc_view_transfer_ctl(movie%view_start_ctl)
      call dealloc_view_transfer_ctl(movie%view_end_ctl)
!
      movie%i_pvr_rotation = 0
!
      end subroutine dealloc_pvr_movie_control_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_rotation_ctl(movie)
!
      use calypso_mpi
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
      use bcast_dup_view_transfer_ctl
!
      type(pvr_movie_ctl), intent(inout) :: movie
!
!
      call calypso_mpi_bcast_one_int(movie%i_pvr_rotation, 0)
!
      call bcast_ctl_type_c1(movie%movie_format_ctl)
      call bcast_ctl_type_c1(movie%movie_mode_ctl)
      call bcast_ctl_type_i1(movie%num_frames_ctl)
      call bcast_ctl_type_c1(movie%rotation_axis_ctl)
      call bcast_ctl_type_i2(movie%quilt_column_row_ctl)
      call bcast_ctl_type_i2(movie%quilt_row_column_ctl)
!
      call bcast_ctl_type_r2(movie%angle_range_ctl)
      call bcast_ctl_type_r2(movie%apature_range_ctl)
      call bcast_ctl_type_r2(movie%LIC_kernel_peak_range_ctl)
!
      call calypso_mpi_bcast_character(movie%start_view_file_ctl,       &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_character(movie%end_view_file_ctl,         &
     &                                 cast_long(kchara), 0)
      call bcast_view_transfer_ctl(movie%view_start_ctl)
      call bcast_view_transfer_ctl(movie%view_end_ctl)
!
      call bcast_mul_view_trans_ctl(movie%mul_mmats_c)
!
      end subroutine bcast_pvr_rotation_ctl
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
      call set_control_labels(hd_movie_format,    names( 1))
      call set_control_labels(hd_movie_mode,      names( 2))
      call set_control_labels(hd_movie_num_frame, names( 3))
      call set_control_labels(hd_column_row,      names( 4))
      call set_control_labels(hd_row_column,      names( 5))
!
      call set_control_labels(hd_movie_rot_axis,   names( 6))
!
      call set_control_labels(hd_start_view_control, names( 7))
      call set_control_labels(hd_end_view_control,   names( 8))
      call set_control_labels(hd_mview_transform,    names( 9))
!
      call set_control_labels(hd_angle_range,        names(10))
      call set_control_labels(hd_apature_range,      names(11))
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
      call set_control_labels(hd_movie_format,    names( 1))
      call set_control_labels(hd_movie_mode,      names( 2))
      call set_control_labels(hd_movie_num_frame, names( 3))
      call set_control_labels(hd_column_row,      names( 4))
      call set_control_labels(hd_row_column,      names( 5))
!
      call set_control_labels(hd_movie_rot_axis,   names( 6))
!
      call set_control_labels(hd_start_view_control, names( 7))
      call set_control_labels(hd_end_view_control,   names( 8))
      call set_control_labels(hd_mview_transform,    names( 9))
!
      call set_control_labels(hd_angle_range,        names(10))
      call set_control_labels(hd_apature_range,      names(11))
!
      call set_control_labels(hd_LIC_kernel_peak,   names(12))
!
      end subroutine set_label_LIC_movie
!
! ----------------------------------------------------------------------
!
      end module t_control_data_pvr_movie
