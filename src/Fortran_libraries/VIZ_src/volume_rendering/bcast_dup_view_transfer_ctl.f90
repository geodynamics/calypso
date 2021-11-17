!>@file   bcast_dup_view_transfer_ctl.f90
!!@brief  module bcast_dup_view_transfer_ctl
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Control inputs for PVR view parameter
!!
!!@verbatim
!!      subroutine bcast_view_transfer_ctl(mat)
!!        type(modeview_ctl), intent(inout) :: mat
!!      subroutine dup_view_transfer_ctl(org_mat, new_mat)
!!        type(modeview_ctl), intent(in) :: org_mat
!!        type(modeview_ctl), intent(inout) :: new_mat
!!@endverbatim
!!
!
      module bcast_dup_view_transfer_ctl
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use t_ctl_data_4_view_transfer
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_view_transfer_ctl(mat)
!
      use calypso_mpi_int
      use bcast_control_arrays
      use t_ctl_data_4_screen_pixel
      use t_ctl_data_4_projection
      use t_ctl_data_4_streo_view
!
      type(modeview_ctl), intent(inout) :: mat
!
!
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
!
!
      call bcast_projection_mat_ctl(mat%proj)
      call bcast_image_size_ctl(mat%pixel)
      call bcast_stereo_view_ctl(mat%streo)
!
      end subroutine bcast_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dup_view_transfer_ctl(org_mat, new_mat)
!
      use t_ctl_data_4_screen_pixel
      use t_ctl_data_4_projection
      use t_ctl_data_4_streo_view
!
      type(modeview_ctl), intent(in) :: org_mat
      type(modeview_ctl), intent(inout) :: new_mat
!
!
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
      call copy_projection_mat_ctl(org_mat%proj, new_mat%proj)
      call copy_image_size_ctl(org_mat%pixel, new_mat%pixel)
      call copy_stereo_view_ctl(org_mat%streo, new_mat%streo)
!
      end subroutine dup_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!
      end module bcast_dup_view_transfer_ctl
