!>@file  t_control_params_stereo_pvr.f90
!!       module t_control_params_stereo_pvr
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Structures for parameteres for stereo volume rendering
!!
!!@verbatim
!!      real(kind = kreal) function each_eye_from_middle                &
!!     &                          (i_img, stereo_def)
!!        type(pvr_stereo_parameter), intent(in) :: stereo_def
!!      subroutine set_pvr_stereo_control(pvr_ctl, stereo_def)
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!!        type(pvr_stereo_parameter), intent(inout) :: stereo_def
!!@endverbatim
!
      module t_control_params_stereo_pvr
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
!>  Stereo view parameters
      type pvr_stereo_parameter
!>    Defined flag for stereo view
        logical :: flag_stereo_pvr = .FALSE.
!>    Flag to make quilt images with fixed view
        logical :: flag_quilt =      .FALSE.
!
!>    Number of images
        integer(kind = kint) :: num_views = 0
!>    Number of row and column of image array (horizontal, vertical)
        integer(kind = kint) :: n_column_row_view(2) = 0
!
!>    Flag to defeine eye separation by angle
        logical :: flag_eye_separation_angle =  .FALSE.
!>    Flag to stepping eye position by angle
        logical :: flag_setp_eye_separation_angle =  .FALSE.
!
!>    Focal length for streo view
        real(kind = kreal) :: focalLength = one
!>    Eye separation for streo view
        real(kind = kreal) :: eye_separation = zero
!>    Eye separation angle for streo view
        real(kind = kreal) :: eye_sep_angle = zero
      end type pvr_stereo_parameter
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      real(kind = kreal) function each_eye_from_middle                  &
     &                          (i_img, stereo_def)
!
      integer(kind = kint), intent(in) :: i_img
      type(pvr_stereo_parameter), intent(in) :: stereo_def
!
      real(kind = kreal) :: pi_180, range, rstep, each_eye, separation
!
!
      pi_180 = four * atan(one) / 180.0d0
      rstep = half - dble(i_img-1) / dble(stereo_def%num_views-1)
      if(stereo_def%flag_setp_eye_separation_angle) then
        if(stereo_def%flag_eye_separation_angle) then
          each_eye = stereo_def%focalLength                             &
     &              * tan(pi_180 * rstep * stereo_def%eye_sep_angle)
        else
          range = two * atan(half*stereo_def%eye_separation             &
     &                       / stereo_def%focalLength)
          each_eye = stereo_def%focalLength * tan(rstep * range)
        end if
      else
        if(stereo_def%flag_eye_separation_angle) then
          separation = stereo_def%focalLength                           &
     &                * tan(half * pi_180 * stereo_def%eye_sep_angle)
        else
          separation = stereo_def%eye_separation
        end if
        each_eye = separation * rstep
      end if
      each_eye_from_middle = each_eye
!
      end function each_eye_from_middle
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_stereo_control(pvr_ctl, stereo_def)
!
      use t_control_data_4_pvr
      use set_area_4_viz
      use skip_comment_f
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      type(pvr_stereo_parameter), intent(inout) :: stereo_def
!
!
      stereo_def%num_views = 0
      stereo_def%flag_stereo_pvr = .FALSE.
      stereo_def%flag_quilt =      .FALSE.
      if(yes_flag(pvr_ctl%streo_ctl%charavalue)) then
        stereo_def%flag_stereo_pvr = .TRUE.
        stereo_def%num_views = 2
      else if(yes_flag(pvr_ctl%quilt_ctl%charavalue)) then
        stereo_def%flag_quilt =      .TRUE.
      end if
!
      call set_pvr_quilt_num_control(pvr_ctl%quilt_c, stereo_def)
!
      end subroutine set_pvr_stereo_control
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_quilt_num_control(quilt_c, stereo_def)
!
      use t_control_data_quilt_image
!
      type(quilt_image_ctl), intent(in) :: quilt_c
      type(pvr_stereo_parameter), intent(inout) :: stereo_def
!
!
      if(stereo_def%flag_quilt) then
        if(quilt_c%i_quilt_image .eq. 0) then
          stereo_def%flag_quilt =      .FALSE.
        else  if(quilt_c%num_column_row_ctl%iflag .gt. 0) then
          stereo_def%n_column_row_view(1:2)                             &
     &        =    quilt_c%num_column_row_ctl%intvalue(1:2)
          stereo_def%num_views = stereo_def%n_column_row_view(1)        &
     &                          * stereo_def%n_column_row_view(2)
        else  if(quilt_c%num_row_column_ctl%iflag .gt. 0) then
          stereo_def%n_column_row_view(1)                               &
     &        =    quilt_c%num_row_column_ctl%intvalue(2)
          stereo_def%n_column_row_view(2)                               &
     &        =    quilt_c%num_row_column_ctl%intvalue(1)
          stereo_def%num_views = stereo_def%n_column_row_view(1)        &
     &                          * stereo_def%n_column_row_view(2)
        end if
      end if
!
!
      end subroutine set_pvr_quilt_num_control
!
!  ---------------------------------------------------------------------
!
      end module t_control_params_stereo_pvr
