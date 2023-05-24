!>@file   write_multi_PVR_image.f90
!!@brief  module write_multi_PVR_image
!!
!!@date  Programmed by H.Matsui in May. 2006
!!       Modified by H.Matsui in May, 2021
!
!>@brief Main routines for volume renderings
!!
!!@verbatim
!!      subroutine output_quilt_PVR_images                              &
!!     &         (istep_pvr, num_pvr, ist_pvr, ied_pvr,                 &
!!     &          num_pvr_images, istack_pvr_images, pvr_param, pvr_rgb)
!!      subroutine output_PVR_images                                    &
!!     &         (istep_pvr, num_pvr, ist_pvr, ied_pvr,                 &
!!     &          num_pvr_images, istack_pvr_images, pvr_rgb)
!!      subroutine output_rotation_PVR_images(istep_pvr,                &
!!     &                                      num_frame, pvr_rgb)
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        integer(kind = kint), intent(in) :: num_pvr, num_pvr_images
!!        integer(kind = kint), intent(in) :: ist_pvr, ied_pvr
!!        integer(kind = kint), intent(in)                              &
!!       &                     :: istack_pvr_images(0:num_pvr)
!!        type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!!@endverbatim
!
      module write_multi_PVR_image
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      use t_pvr_image_array
      use t_rendering_vr_image
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_quilt_PVR_images                                &
     &         (istep_pvr, num_pvr, ist_pvr, ied_pvr,                   &
     &          num_pvr_images, istack_pvr_images, pvr_param, pvr_rgb)
!
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      integer(kind = kint), intent(in) :: num_pvr, num_pvr_images
      integer(kind = kint), intent(in) :: ist_pvr, ied_pvr
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_pvr_images(0:num_pvr)
!
      type(PVR_control_params), intent(in) :: pvr_param(num_pvr)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint) :: i_pvr, ist_img, num_img
!
!
      do i_pvr = ist_pvr, ied_pvr
        ist_img = istack_pvr_images(i_pvr-1)
        num_img = istack_pvr_images(i_pvr  ) - ist_img
        call set_output_rot_sequence_image(istep_pvr, -1,               &
     &      pvr_rgb(ist_img+1)%id_pvr_file_type,                        &
     &      pvr_rgb(ist_img+1)%pvr_prefix, num_img,                     &
     &      pvr_param(i_pvr)%stereo_def%n_column_row_view,              &
     &      pvr_rgb(ist_img+1))
      end do
!
      end subroutine output_quilt_PVR_images
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine output_PVR_images                                      &
     &         (istep_pvr, num_pvr, ist_pvr, ied_pvr,                   &
     &          num_pvr_images, istack_pvr_images, pvr_rgb)
!
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      integer(kind = kint), intent(in) :: num_pvr, num_pvr_images
      integer(kind = kint), intent(in) :: ist_pvr, ied_pvr
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_pvr_images(0:num_pvr)
!
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_pvr_images)
!
      integer(kind = kint) :: i_pvr, i_img, ist_img, num_img
!
!
      do i_pvr = ist_pvr, ied_pvr
        ist_img = istack_pvr_images(i_pvr-1)
        num_img = istack_pvr_images(i_pvr  ) - ist_img
        do i_img = 1, num_img
          call sel_write_pvr_image_file(istep_pvr, -1,                  &
     &                                  pvr_rgb(i_img+ist_img))
        end do
      end do
!
      end subroutine output_PVR_images
!
!  ---------------------------------------------------------------------
!
      subroutine output_rotation_PVR_images(istep_pvr,                  &
     &                                      num_frame, pvr_rgb)
!
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr, num_frame
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_frame)
!
      integer(kind = kint) :: i_rot
!
!
      do i_rot = 1, num_frame
        call sel_write_pvr_image_file(istep_pvr, i_rot, pvr_rgb(i_rot))
      end do
!
      end subroutine output_rotation_PVR_images
!
!  ---------------------------------------------------------------------
!
      end module write_multi_PVR_image
