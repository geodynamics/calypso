!>@file  cal_pvr_modelview_mat.f90
!!       module cal_pvr_modelview_mat
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Get model view matrix for PVR
!!
!!@verbatim
!!      subroutine cal_pvr_modelview_matrix(i_stereo, i_rot,            &
!!     &           outline, movie_def, stereo_def, view_param,          &
!!     &           viewpoint_vec, modelview_mat)
!!        type(pvr_domain_outline), intent(in) :: outline
!!        type(pvr_movie_parameter), intent(in) :: movie_def
!!        type(pvr_view_parameter), intent(in) :: view_param
!!        real(kind = kreal), intent(inout) :: modelview_mat(4,4)
!!        real(kind = kreal), intent(inout) :: viewpoint_vec(3)
!!@endverbatim
!
      module cal_pvr_modelview_mat
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_control_params_stereo_pvr
!
      implicit none
!
      integer(kind = kint), parameter :: ione_stack(0:1) = (/0,1/)
      private :: ione_stack
!
      private :: cal_modelview_mat_by_views
      private :: update_rot_mat_from_viewpts
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_pvr_modelview_matrix(i_stereo, i_rot,              &
     &           outline, movie_def, stereo_def, view_param,            &
     &           viewpoint_vec, modelview_mat)
!
      use t_surf_grp_4_pvr_domain
      use cal_inverse_small_matrix
      use cal_matrix_vector_smp
!
      integer(kind = kint), intent(in) :: i_stereo, i_rot
      type(pvr_domain_outline), intent(in) :: outline
      type(pvr_movie_parameter), intent(in) :: movie_def
      type(pvr_stereo_parameter), intent(in) :: stereo_def
      type(pvr_view_parameter), intent(in) :: view_param
!
      real(kind = kreal), intent(inout) :: modelview_mat(4,4)
      real(kind = kreal), intent(inout) :: viewpoint_vec(3)
!
      integer(kind = kint) :: i, ierr2
      real(kind = kreal) ::  vec_tmp(4)
      real(kind = kreal) ::  posi_zero(4) = (/zero,zero,zero,one/)
!>    Inverse of modelview matrix
      real(kind = kreal) :: modelview_inv(4,4)
!
!
      call cal_modelview_mat_by_views(i_stereo, i_rot,                  &
     &    outline, movie_def, stereo_def, view_param, modelview_mat)
!
      call cal_inverse_44_matrix(modelview_mat,                         &
     &                           modelview_inv, ierr2)
      call cal_mat44_vec3_on_node(ione, ione, ione_stack,               &
     &    modelview_inv, posi_zero(1), vec_tmp(1))
      viewpoint_vec(1:3) = vec_tmp(1:3)
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'modelview'
        do i = 1, 4
          write(*,'(1p4e16.7)') modelview_mat(i,1:4)
        end do
!
        write(*,*) 'modelview_inv'
        do i = 1, 4
          write(*,'(1p4e16.7)') modelview_inv(i,1:4)
        end do
!
        write(*,*) 'lookat_vec', view_param%lookat_vec(1:3)
        write(*,*) 'scale_factor_pvr',                                  &
     &            view_param%scale_factor_pvr(1:3)
        write(*,*) 'viewpoint_vec', viewpoint_vec(1:3)
        write(*,*) 'viewpt_in_view',                                    &
     &            view_param%viewpt_in_viewer_pvr(1:3)
      end if
!
      end subroutine cal_pvr_modelview_matrix
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_modelview_mat_by_views(i_stereo, i_rot,            &
     &          outline, movie_def, stereo_def, view_param,             &
     &          modelview_mat)
!
      use t_surf_grp_4_pvr_domain
      use transform_mat_operations
      use cal_matrix_vector_smp
!
      integer(kind = kint), intent(in) :: i_stereo, i_rot
      type(pvr_domain_outline), intent(in) :: outline
      type(pvr_movie_parameter), intent(in) :: movie_def
      type(pvr_stereo_parameter), intent(in) :: stereo_def
      type(pvr_view_parameter), intent(in) :: view_param
!
      real(kind = kreal), intent(inout) :: modelview_mat(4,4)
!
      real(kind = kreal) :: rotation_mat(4,4), mat_tmp(4,4)
      real(kind = kreal) :: rotation_axis(3), rev_lookat(3)
      real(kind = kreal) :: rev_eye(3), streo_eye(3), scale(3)
      real(kind = kreal) :: angle_deg
!
!
      if(view_param%iflag_modelview_mat .gt. 0) then
        modelview_mat(1:4,1:4) = view_param%modelview(1:4,1:4)
        return
      end if
!
      if(view_param%iflag_lookpoint .eq. 0) then
        rev_lookat(1:3) = - outline%center_g(1:3)
      else
        rev_lookat(1:3) = - view_param%lookat_vec(1:3)
      end if
!
      if(view_param%iflag_scale_fact .eq. 0) then
        scale(1) = 1.0d0 / outline%rmax_g
        scale(2) = 1.0d0 / outline%rmax_g
        scale(3) = 1.0d0 / outline%rmax_g
      else
        scale(1:3) = view_param%scale_factor_pvr(1:3)
      end if
!
!
      call Kemo_Unit(modelview_mat)
      call Kemo_Translate(modelview_mat, rev_lookat)
!
!    Change scale
      call Kemo_Scale(modelview_mat, scale)
!
!   Rotate by Movie
      if(movie_def%iflag_movie_mode .eq. I_ROTATE_MOVIE                 &
     &    .and. i_rot .gt. 0) then
        rotation_axis(1:3) =       zero
        rotation_axis(movie_def%id_rot_axis) = one
        angle_deg = movie_def%angle_range(1)                            &
     &      + (movie_def%angle_range(2) - movie_def%angle_range(1))     &
     &       * dble(i_rot-1) / dble(movie_def%num_frame)
        call Kemo_Rotate(modelview_mat, angle_deg, rotation_axis(1))
      end if
!
!   Rotate for viewpoint
      if(view_param%iflag_rotation .gt. 0) then
        call Kemo_Rotate(modelview_mat,                                 &
     &      view_param%rotation_pvr(1), view_param%rotation_pvr(2:4))
      else
        mat_tmp(1:4,1:4) = modelview_mat(1:4,1:4)
        call update_rot_mat_from_viewpts(view_param, rotation_mat)
        call cal_matmat44(modelview_mat,                                &
     &                    rotation_mat(1,1), mat_tmp(1,1))
      end if
!
!   Shift for stereo view
      if(stereo_def%flag_stereo_pvr .or. stereo_def%flag_quilt) then
        streo_eye(1) =  each_eye_from_middle(i_stereo, stereo_def)
        streo_eye(2:3) = zero
        call Kemo_Translate(modelview_mat, streo_eye)
      end if
!
!       Shift by viewpoint
      if(view_param%iflag_viewpt_in_view .eq. 0) then
        call cal_mat44_vec3_on_node(ione, ione, ione_stack,             &
     &    modelview_mat, view_param%viewpoint, rev_eye)
      else
        rev_eye(1:3) = - view_param%viewpt_in_viewer_pvr(1:3)
      end if
      call Kemo_Translate(modelview_mat, rev_eye)
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'viewpt_in_view',                                    &
     &             view_param%viewpt_in_viewer_pvr(1:3)
      end if
!
      end subroutine cal_modelview_mat_by_views
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine update_rot_mat_from_viewpts(view_param, rotation_mat)
!
      use mag_of_field_smp
      use cal_products_smp
      use transform_mat_operations
!
      type(pvr_view_parameter), intent(in) :: view_param
      real(kind = kreal), intent(inout) :: rotation_mat(4,4)
!
      integer(kind = kint) :: i
      real(kind = kreal) :: viewing_dir(3), u(3), v(3), size(1)
      real(kind = kreal) :: look_norm(3), view_norm(3), up_norm(3)
!
!
      viewing_dir(1:3) = view_param%lookat_vec(1:3)                     &
     &                  - view_param%viewpoint(1:3)
!$omp parallel
      call cal_vector_magnitude(ione, ione, ione_stack,                 &
     &    viewing_dir(1), size(1) )
!$omp end parallel
      viewing_dir(1:3) = viewing_dir(1:3) / size(1)
!
!$omp parallel
      call cal_vector_magnitude(ione, ione, ione_stack,                 &
     &    viewing_dir(1), size(1) )
!$omp end parallel
      look_norm(1:3) = view_param%lookat_vec(1:3) / size(1)
!
!$omp parallel
      call cal_vector_magnitude(ione, ione, ione_stack,                 &
     &    view_param%viewpoint, size(1) )
!$omp end parallel
      view_norm(1:3) = view_param%viewpoint(1:3) / size(1)
!
!$omp parallel
      call cal_vector_magnitude(ione, ione, ione_stack,                 &
     &    view_param%up_direction_vec(1), size(1) )
!$omp end parallel
      up_norm(1:3) = view_param%up_direction_vec(1:3) / size(1)
!
!    /* find the direction of axis U */
      call cal_cross_prod_no_coef_smp                                   &
     &   (ione, up_norm(1), viewing_dir(1), u(1))
!$omp parallel
      call cal_vector_magnitude(ione, ione, ione_stack,                 &
     &    u(1), size(1) )
!$omp end parallel
      u(1:3) = u(1:3) / size(1)
!
!    /*find the direction of axix V */
      call cal_cross_prod_no_coef_smp(ione, viewing_dir(1), u(1), v(1))
!$omp parallel
      call cal_vector_magnitude(ione, ione, ione_stack,                 &
     &    v(1), size(1) )
!$omp end parallel
      v(1:3) = v(1:3) / size(1)
!
      do i = 1, 3
        rotation_mat(1,i) = u(i)
        rotation_mat(2,i) = v(i)
        rotation_mat(3,i) = viewing_dir(i)
        rotation_mat(4,i) = zero
      end do
      rotation_mat(1:3,4) = zero
      rotation_mat(4,4) = one
!
      end subroutine update_rot_mat_from_viewpts
!
! -----------------------------------------------------------------------
!
      end module cal_pvr_modelview_mat
