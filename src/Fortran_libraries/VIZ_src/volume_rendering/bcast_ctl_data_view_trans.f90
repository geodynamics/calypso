!>@file   bcast_ctl_data_view_trans.f90
!!@brief  module bcast_ctl_data_view_trans
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR projection and streo parameter
!!
!!@verbatim
!!      subroutine bcast_pvr_moving_view_ctl(movie)
!!        type(pvr_movie_ctl), intent(inout) :: movie
!!      subroutine bcast_quilt_image_ctl(quilt_c)
!!        type(quilt_image_ctl), intent(inout) :: quilt_c
!!      subroutine bcast_mul_view_trans_ctl(mul_mats_c)
!!        type(multi_modelview_ctl), intent(inout) :: mul_mats_c
!!      subroutine bcast_view_transfer_ctl(mat)
!!        type(modeview_ctl), intent(inout) :: mat
!!
!!      subroutine bcast_image_size_ctl(pixel)
!!        type(screen_pixel_ctl), intent(inout) :: pixel
!!      subroutine bcast_projection_mat_ctl(proj)
!!        type(projection_ctl), intent(inout) :: proj
!!      subroutine bcast_stereo_view_ctl(streo)
!!        type(streo_view_ctl), intent(inout) :: streo
!!@endverbatim
!!
!
      module bcast_ctl_data_view_trans
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
      private :: bcast_projection_mat_ctl, bcast_image_size_ctl
      private :: bcast_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_moving_view_ctl(movie)
!
      use t_ctl_data_pvr_movie
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
!
      type(pvr_movie_ctl), intent(inout) :: movie
!
!
      call calypso_mpi_bcast_one_int(movie%i_pvr_rotation, 0)
      call calypso_mpi_bcast_character(movie%block_name,                &
     &                                 cast_long(kchara), 0)
!
      call bcast_ctl_type_c1(movie%movie_mode_ctl)
      call bcast_ctl_type_i1(movie%num_frames_ctl)
      call bcast_ctl_type_c1(movie%rotation_axis_ctl)
!
      call bcast_ctl_type_r2(movie%angle_range_ctl)
      call bcast_ctl_type_r2(movie%apature_range_ctl)
      call bcast_ctl_type_r2(movie%LIC_kernel_peak_range_ctl)
!
      call calypso_mpi_bcast_character(movie%fname_view_start_ctl,      &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_character(movie%fname_view_end_ctl,        &
     &                                 cast_long(kchara), 0)
      call bcast_view_transfer_ctl(movie%view_start_ctl)
      call bcast_view_transfer_ctl(movie%view_end_ctl)
!
      call bcast_mul_view_trans_ctl(movie%mul_mmats_c)
!
      end subroutine bcast_pvr_moving_view_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_quilt_image_ctl(quilt_c)
!
      use t_ctl_data_quilt_image
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
!
      type(quilt_image_ctl), intent(inout) :: quilt_c
!
!
      call calypso_mpi_bcast_one_int(quilt_c%i_quilt_image, 0)
      call calypso_mpi_bcast_character(quilt_c%block_name,              &
     &                                 cast_long(kchara), 0)
!
      call bcast_ctl_type_i2(quilt_c%num_column_row_ctl)
      call bcast_ctl_type_i2(quilt_c%num_row_column_ctl)
!
      call bcast_mul_view_trans_ctl(quilt_c%mul_qmats_c)
!
      end subroutine bcast_quilt_image_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_mul_view_trans_ctl(mul_mats_c)
!
      use t_ctl_data_view_transfers
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(multi_modelview_ctl), intent(inout) :: mul_mats_c
!
      integer(kind = kint) :: i, num
!
!
      call calypso_mpi_bcast_one_int(mul_mats_c%num_modelviews_c, 0)
      call calypso_mpi_bcast_character(mul_mats_c%block_name,           &
     &                                 cast_long(kchara), 0)
!
      if(mul_mats_c%num_modelviews_c .gt. 0 .and. my_rank .gt. 0) then
        num = mul_mats_c%num_modelviews_c
        call alloc_multi_modeview_ctl(mul_mats_c)
      end if
!
      call calypso_mpi_bcast_character(mul_mats_c%fname_mat_ctl,        &
     &    cast_long(kchara*mul_mats_c%num_modelviews_c), 0)
      do i = 1, mul_mats_c%num_modelviews_c
        call bcast_view_transfer_ctl(mul_mats_c%matrices(i))
      end do
!
      end subroutine bcast_mul_view_trans_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_view_transfer_ctl(mat)
!
      use t_ctl_data_4_view_transfer
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(modeview_ctl), intent(inout) :: mat
!
!
      call calypso_mpi_bcast_character(mat%block_name,                  &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(mat%i_view_transform, 0)
!
      call bcast_ctl_array_cr(mat%lookpoint_ctl)
      call bcast_ctl_array_cr(mat%viewpoint_ctl)
      call bcast_ctl_array_cr(mat%up_dir_ctl)
!
      call bcast_ctl_array_cr(mat%view_rot_vec_ctl)
      call bcast_ctl_array_cr(mat%scale_vector_ctl)
      call bcast_ctl_array_cr(mat%viewpt_in_viewer_ctl)
!
      call bcast_ctl_array_c2r(mat%modelview_mat_ctl)
!
      call bcast_ctl_type_r1(mat%view_rotation_deg_ctl)
      call bcast_ctl_type_r1(mat%scale_factor_ctl)
      call bcast_ctl_type_c1(mat%projection_type_ctl)
!
!
      call bcast_projection_mat_ctl(mat%proj)
      call bcast_image_size_ctl(mat%pixel)
      call bcast_stereo_view_ctl(mat%streo)
!
      end subroutine bcast_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_image_size_ctl(pixel)
!
      use t_ctl_data_4_screen_pixel
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(screen_pixel_ctl), intent(inout) :: pixel
!
!
      call calypso_mpi_bcast_character(pixel%block_name,                &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(pixel%i_image_size, 0)
!
      call bcast_ctl_type_i1(pixel%num_xpixel_ctl)
      call bcast_ctl_type_i1(pixel%num_ypixel_ctl)
!
      end subroutine bcast_image_size_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_projection_mat_ctl(proj)
!
      use t_ctl_data_4_projection
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(projection_ctl), intent(inout) :: proj
!
!
      call calypso_mpi_bcast_character(proj%block_name,                 &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(proj%i_project_mat, 0)
!
      call bcast_ctl_type_r1(proj%perspective_angle_ctl)
      call bcast_ctl_type_r1(proj%perspective_xy_ratio_ctl)
      call bcast_ctl_type_r1(proj%perspective_near_ctl)
      call bcast_ctl_type_r1(proj%perspective_far_ctl)
!
      call bcast_ctl_type_r2(proj%horizontal_range_ctl)
      call bcast_ctl_type_r2(proj%vertical_range_ctl)
!
      end subroutine bcast_projection_mat_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_stereo_view_ctl(streo)
!
      use t_ctl_data_4_streo_view
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(streo_view_ctl), intent(inout) :: streo
!
!
      call calypso_mpi_bcast_character(streo%block_name,                &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(streo%i_stereo_view, 0)
!
      call bcast_ctl_type_r1(streo%focalpoint_ctl)
      call bcast_ctl_type_r1(streo%eye_separation_ctl)
      call bcast_ctl_type_r1(streo%eye_sep_angle_ctl)
      call bcast_ctl_type_c1(streo%step_eye_sep_angle_ctl)
!
      end subroutine bcast_stereo_view_ctl
!
!  ---------------------------------------------------------------------
!
      end module bcast_ctl_data_view_trans
