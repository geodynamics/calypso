!>@file  set_pvr_modelview_matrix.f90
!!       module set_pvr_modelview_matrix
!!
!!@author H. Matsui
!!@date   Programmed in May. 2009
!
!> @brief Evaluate model view matirx
!!
!!@verbatim
!!      subroutine s_set_pvr_modelview_matrix(mat, view_param)
!!        type(modeview_ctl), intent(in) :: mat
!!        type(pvr_view_parameter), intent(inout) :: view_param
!!      subroutine copy_stereo_perspective_matrix(streo, stereo_def)
!!        type(streo_view_ctl), intent(in) :: streo
!!        type(pvr_stereo_parameter), intent(inout) :: stereo_def
!!@endverbatim
!
      module set_pvr_modelview_matrix
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_error_IDs
      use t_ctl_data_4_view_transfer
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_control_params_stereo_pvr
!
      implicit none
!
      private :: copy_pvr_modelview_matrix, set_viewpoint_vector_ctl
      private :: copy_pvr_perspective_matrix, copy_pvr_image_size
      private :: set_view_rotation_vect_ctl, set_view_scale_factor_ctl
      private :: set_viewpnt_in_viewer_ctl
!
      private :: set_4direction_flag, set_3direction_flag
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_pvr_modelview_matrix(mat, view_param)
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
!
!
      call copy_pvr_image_size(mat%pixel, view_param)
      call copy_pvr_perspective_matrix(mat%proj, view_param)
!
      if (mat%modelview_mat_ctl%num .gt. 0) then
        call copy_pvr_modelview_matrix(mat, view_param)
      else
        call set_viewpoint_vector_ctl(mat, view_param)
      end if
!
      if(mat%view_rotation_deg_ctl%iflag .gt. 0                         &
     &    .and. mat%view_rot_vec_ctl%num .ge. 3) then
        call set_view_rotation_vect_ctl(mat, view_param)
      end if
!
      if(mat%scale_factor_ctl%iflag .gt. 0) then
        view_param%scale_factor_pvr(1:3)                                &
     &            = mat%scale_factor_ctl%realvalue
        view_param%iflag_scale_fact = 1
      else if(mat%scale_vector_ctl%num .ge. 3) then
        call set_view_scale_factor_ctl(mat, view_param)
      end if
!
      if(mat%viewpt_in_viewer_ctl%num .ge. 3) then
        call set_viewpnt_in_viewer_ctl(mat, view_param)
      end if
!
      end subroutine s_set_pvr_modelview_matrix
!
! -----------------------------------------------------------------------
!
      subroutine copy_pvr_image_size(pixel, view_param)
!
      use t_ctl_data_4_screen_pixel
!
      type(screen_pixel_ctl), intent(in) :: pixel
      type(pvr_view_parameter), intent(inout) :: view_param
!
!
      if(pixel%num_xpixel_ctl%iflag .gt. 0) then
        view_param%n_pvr_pixel(1) = pixel%num_xpixel_ctl%intvalue
      else
        view_param%n_pvr_pixel(1) = 640
      end if
!
      if(pixel%num_ypixel_ctl%iflag .gt. 0) then
        view_param%n_pvr_pixel(2) = pixel%num_ypixel_ctl%intvalue
      else
        view_param%n_pvr_pixel(2) = 480
      end if
!
      end subroutine copy_pvr_image_size
!
! -----------------------------------------------------------------------
!
      subroutine copy_pvr_perspective_matrix(proj, view_param)
!
      use t_ctl_data_4_projection
      use t_ctl_data_4_streo_view
!
      type(projection_ctl), intent(in) :: proj
      type(pvr_view_parameter), intent(inout) :: view_param
!
!
      if (proj%perspective_angle_ctl%iflag .gt. 0) then
        view_param%perspective_angle                                    &
     &          = proj%perspective_angle_ctl%realvalue
      else
        view_param%perspective_angle = 10.0d0
      end if
!
      if (proj%perspective_xy_ratio_ctl%iflag .gt. 0) then
        view_param%perspective_xy_ratio                                 &
     &          = proj%perspective_xy_ratio_ctl%realvalue
      else
        view_param%perspective_xy_ratio                                 &
     &          = dble(view_param%n_pvr_pixel(1))                       &
     &           / dble(view_param%n_pvr_pixel(2))
      end if
!
      if (proj%perspective_near_ctl%iflag .gt. 0) then
        view_param%perspective_near                                     &
     &          = proj%perspective_near_ctl%realvalue
      else
        view_param%perspective_near = 1.0d-3
      end if
!
      if (proj%perspective_far_ctl%iflag .gt. 0) then
        view_param%perspective_far                                      &
     &          = proj%perspective_far_ctl%realvalue
      else
        view_param%perspective_far = 1.0d3
      end if
!
      view_param%iflag_perspective                                      &
     &      = proj%perspective_angle_ctl%iflag
!
      end subroutine copy_pvr_perspective_matrix
!
! -----------------------------------------------------------------------
!
      subroutine copy_stereo_perspective_matrix(streo, stereo_def)
!
      use t_ctl_data_4_projection
      use t_ctl_data_4_streo_view
!
      type(streo_view_ctl), intent(in) :: streo
      type(pvr_stereo_parameter), intent(inout) :: stereo_def
!
!
      if(streo%i_stereo_view .eq. 0) then
        if(stereo_def%flag_quilt) then
          stereo_def%flag_quilt = .FALSE.
          if(my_rank.eq.0) then
            write(*,*) 'Stereo view paramters are missing.'
            write(*,*) 'Turn off Quilt view.'
          end if
        end if
      end if
!
      if(streo%focalpoint_ctl%iflag .gt. 0) then
        stereo_def%focalLength = streo%focalpoint_ctl%realvalue
      else
        stereo_def%focalLength = 1.0d+1
      end if
!
      stereo_def%flag_eye_separation_angle = .FALSE.
      if(streo%eye_sep_angle_ctl%iflag .gt. 0) then
        stereo_def%flag_eye_separation_angle = .TRUE.
        stereo_def%eye_sep_angle = streo%eye_sep_angle_ctl%realvalue
      else if(streo%eye_separation_ctl%iflag .gt. 0) then
        stereo_def%eye_separation = streo%eye_separation_ctl%realvalue
      else
        stereo_def%eye_separation = 1.0d-1
      end if
!
      stereo_def%flag_setp_eye_separation_angle = .FALSE.
      if(streo%step_eye_sep_angle_ctl%iflag .gt. 0                      &
     &   .and. yes_flag(streo%step_eye_sep_angle_ctl%charavalue)) then
        stereo_def%flag_setp_eye_separation_angle = .TRUE.
      end if
!
      end subroutine copy_stereo_perspective_matrix
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_pvr_modelview_matrix(mat, view_param)
!
      use skip_comment_f
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
      integer(kind = kint) :: i, nd1, nd2
!
!
      if(mat%modelview_mat_ctl%num .ne. 16) then
        write(e_message,'(a)')                                          &
     &     'Modelview  Matrix should be 16 components'
        call calypso_MPI_abort(ierr_PVR, e_message)
      end if
!
      do i = 1, mat%modelview_mat_ctl%num
        nd1 = set_4direction_flag(mat%modelview_mat_ctl%c1_tbl(i))
        nd2 = set_4direction_flag(mat%modelview_mat_ctl%c2_tbl(i))
!
        if(nd1*nd2 .gt. 0) then
          view_param%modelview(nd2,nd1) = mat%modelview_mat_ctl%vect(i)
        end if
      end do
!
      view_param%iflag_modelview_mat = 1
!
      end subroutine copy_pvr_modelview_matrix
!
! -----------------------------------------------------------------------
!
      subroutine set_viewpoint_vector_ctl(mat, view_param)
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
!
      integer(kind = kint) :: i, nd
!
!
!      if(mat%lookpoint_ctl%num .ne. 3) then
!        write(e_message,'(a)')                                         &
!     &     'Lookatpoint vector should be 3 components'
!        call calypso_MPI_abort(ierr_PVR, e_message)
!      end if
!      if(mat%viewpoint_ctl%num .ne. 3) then
!        write(e_message,'(a)')                                         &
!     &     'Viewpoint vector should be 3 components'
!        call calypso_MPI_abort(ierr_PVR, e_message)
!      end if
!      if(mat%up_dir_ctl%num .ne. 3) then
!        write(e_message,'(a)')                                         &
!     &     'Up-direction vector should be 3 components'
!        call calypso_MPI_abort(ierr_PVR, e_message)
!      end if
!
      do i = 1, mat%lookpoint_ctl%num
        nd = set_3direction_flag(mat%lookpoint_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        view_param%lookat_vec(nd) = mat%lookpoint_ctl%vect(i)
      end do
      if(mat%lookpoint_ctl%num .ge. 3) then
        view_param%iflag_lookpoint = 1
      end if
!
      do i = 1, mat%viewpoint_ctl%num
        nd = set_3direction_flag(mat%viewpoint_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        view_param%viewpoint(nd) = mat%viewpoint_ctl%vect(i)
      end do
      if(mat%viewpoint_ctl%num .ge. 3) then
        view_param%iflag_viewpoint = 1
      end if
!
      do i = 1, mat%up_dir_ctl%num
        nd = set_3direction_flag(mat%up_dir_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
         view_param%up_direction_vec(nd) = mat%up_dir_ctl%vect(i)
      end do
      if(mat%up_dir_ctl%num .ge. 3) then
        view_param%iflag_updir = 1
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'iflag_lookpoint_vec', view_param%iflag_lookpoint
        write(*,*) 'lookat_vec', view_param%lookat_vec(1:3)
        write(*,*) 'iflag_viewpoint_vec', view_param%iflag_viewpoint
        write(*,*) 'viewpoint_vec', view_param%viewpoint(1:3)
        write(*,*) 'iflag_updir_vec', view_param%iflag_updir
        write(*,*) 'up_direction_vec',                                  &
     &            view_param%up_direction_vec(1:3)
      end if
!
      end subroutine set_viewpoint_vector_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_view_rotation_vect_ctl(mat, view_param)
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
!
      integer(kind = kint) :: i, nd
!
!
      if(mat%view_rot_vec_ctl%num .ne. 3) then
        write(e_message,'(a)')                                          &
     &     'Rotaion of viewpoint vector should be 3 components'
        call calypso_MPI_abort(ierr_PVR, e_message)
      end if
!
      if (mat%view_rotation_deg_ctl%iflag .gt. 0) then
        view_param%rotation_pvr(1)                                      &
     &     = mat%view_rotation_deg_ctl%realvalue
      else
        view_param%rotation_pvr(1) = 0.0d0
      end if
!
      do i = 1, mat%view_rot_vec_ctl%num
        nd = set_3direction_flag(mat%view_rot_vec_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        view_param%rotation_pvr(nd+1) = mat%view_rot_vec_ctl%vect(i)
      end do
      if(mat%view_rot_vec_ctl%num.ge.3                                  &
     &     .and. mat%view_rotation_deg_ctl%iflag .gt. 0) then
        view_param%iflag_rotation = 1
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'rotation_vect', view_param%rotation_pvr(2:4)
        write(*,*) 'rotation_angle', view_param%rotation_pvr(1)
      end if
!
      end subroutine set_view_rotation_vect_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_view_scale_factor_ctl(mat, view_param)
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
      integer(kind = kint) :: i, nd
!
!
      if(mat%scale_vector_ctl%num .ne. 3) then
        write(e_message,'(a)')                                          &
     &     'Scale factor vector should be 3 components'
        call calypso_MPI_abort(ierr_PVR, e_message)
      end if
!
      do i = 1, mat%scale_vector_ctl%num
        nd = set_3direction_flag(mat%scale_vector_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        view_param%scale_factor_pvr(nd) = mat%scale_vector_ctl%vect(i)
      end do
      if (mat%scale_vector_ctl%num .ge. 3) then
        view_param%iflag_scale_fact = 1
      end if
!
      end subroutine set_view_scale_factor_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_viewpnt_in_viewer_ctl(mat, view_param)
!
      use skip_comment_f
!
      type(modeview_ctl), intent(in) :: mat
      type(pvr_view_parameter), intent(inout) :: view_param
!
      integer(kind = kint) :: i, nd
!
!
      if(mat%viewpt_in_viewer_ctl%num .ne. 3) then
        write(e_message,'(a)')                                          &
     &     'Viewpoint in viewer should be 3 components'
        call calypso_MPI_abort(ierr_PVR, e_message)
      end if
!
      do i = 1, mat%viewpt_in_viewer_ctl%num
        nd = set_3direction_flag(mat%viewpt_in_viewer_ctl%c_tbl(i))
        if(nd .eq. 0) cycle
        view_param%viewpt_in_viewer_pvr(nd)                             &
     &      = mat%viewpt_in_viewer_ctl%vect(i)
      end do
      if (mat%viewpt_in_viewer_ctl%num .ge. 3)  then
        view_param%iflag_viewpt_in_view = 1
      end if
!
      view_param%lookat_vec(1:2) = view_param%lookat_vec(1:2)           &
     &                         - view_param%viewpt_in_viewer_pvr(1:2)
!
      end subroutine set_viewpnt_in_viewer_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer function set_4direction_flag(dir_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: dir_ctl
!
!
      if     (cmp_no_case(dir_ctl,'x') .or. dir_ctl.eq.'1') then
        set_4direction_flag = 1
      else if(cmp_no_case(dir_ctl,'y') .or. dir_ctl.eq.'2') then
        set_4direction_flag = 2
      else if(cmp_no_case(dir_ctl,'z') .or. dir_ctl.eq.'3') then
        set_4direction_flag = 3
      else if(cmp_no_case(dir_ctl,'w') .or. dir_ctl.eq.'4') then
        set_4direction_flag = 4
      else
        set_4direction_flag = 0
      end if
!
      end function set_4direction_flag
!
! -----------------------------------------------------------------------
!
      integer function set_3direction_flag(dir_ctl)
!
      character(len = kchara), intent(in) :: dir_ctl
!
!
      set_3direction_flag = set_4direction_flag(dir_ctl)
      if(set_4direction_flag(dir_ctl) .eq. 4) set_3direction_flag = 0
!
      end function set_3direction_flag
!
! -----------------------------------------------------------------------
!
      end module set_pvr_modelview_matrix
