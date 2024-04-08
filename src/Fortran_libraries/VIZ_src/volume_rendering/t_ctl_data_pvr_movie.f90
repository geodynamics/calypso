!>@file   t_ctl_data_pvr_movie.f90
!!@brief  module t_ctl_data_pvr_movie
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for PVR movie from snapshot
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine dup_pvr_movie_control_flags(org_movie, new_movie)
!!        type(pvr_movie_ctl), intent(in) :: org_movie
!!        type(pvr_movie_ctl), intent(inout) :: new_movie
!!      subroutine dealloc_pvr_movie_control_flags(movie)
!!        type(pvr_movie_ctl), intent(inout) :: movie
!!
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
      module t_ctl_data_pvr_movie
!
      use m_precision
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
!>        Control block name
        character(len = kchara) :: block_name = 'snapshot_movie_ctl'
!
!>        Structure of movie mode control
        type(read_character_item) :: movie_mode_ctl
!>        Structure of number of flame control
        type(read_integer_item) ::   num_frames_ctl
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
        character(len=kchara) :: fname_view_start_ctl = 'NO_FILE'
!>        file name for end modelview matrix
        character(len=kchara) :: fname_view_end_ctl = 'NO_FILE'
!>    Structure for start modelview marices
        type(modeview_ctl) :: view_start_ctl
!>    Structure for end modelview marices
        type(modeview_ctl) :: view_end_ctl
!
!         Lists of multiple view parameters
        type(multi_modelview_ctl) :: mul_mmats_c
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_pvr_rotation = 0
      end type pvr_movie_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_movie_control_flags(org_movie, new_movie)
!
      type(pvr_movie_ctl), intent(in) :: org_movie
      type(pvr_movie_ctl), intent(inout) :: new_movie
!
!
      call dup_mul_view_trans_ctl(org_movie%mul_mmats_c,                &
     &                            new_movie%mul_mmats_c)
!
      call copy_chara_ctl(org_movie%movie_mode_ctl,                     &
     &                    new_movie%movie_mode_ctl)
      call copy_integer_ctl(org_movie%num_frames_ctl,                   &
     &                      new_movie%num_frames_ctl)
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
      new_movie%fname_view_start_ctl = org_movie%fname_view_start_ctl
      new_movie%fname_view_end_ctl =   org_movie%fname_view_end_ctl
      call dup_view_transfer_ctl(org_movie%view_start_ctl,              &
     &    new_movie%view_start_ctl)
      call dup_view_transfer_ctl(org_movie%view_end_ctl,                &
     &    new_movie%view_end_ctl)
!
      new_movie%i_pvr_rotation = org_movie%i_pvr_rotation
      new_movie%block_name =     org_movie%block_name
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
      movie%movie_mode_ctl%iflag =       0
      movie%num_frames_ctl%iflag =       0
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
!
      end module t_ctl_data_pvr_movie
