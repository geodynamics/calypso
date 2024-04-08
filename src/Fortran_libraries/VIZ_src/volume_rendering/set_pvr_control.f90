!>@file   set_pvr_control.f90
!!@brief  module set_pvr_control
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine bcast_pvr_controls                                   &
!!     &         (num_pvr_ctl, pvr_ctl, cflag_update)
!!        integer(kind = kint), intent(in) :: num_pvr_ctl
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl(num_pvr_ctl)
!!      subroutine s_set_pvr_controls(group, nod_fld,                   &
!!     &                              pvr_ctl_type, pvr_param)
!!        integer(kind = kint), intent(in) :: num_pvr
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!
!!      subroutine flush_each_pvr_control(pvr_param)
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!@endverbatim
!
      module set_pvr_control
!
      use m_precision
      use calypso_mpi
!
      use t_control_data_4_pvr
!
      implicit none
!
      private :: init_multi_view_parameters
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_controls                                     &
     &         (num_pvr_ctl, pvr_ctl, cflag_update)
!
      use ctl_file_pvr_modelview_IO
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: num_pvr_ctl
!
      character(len=kchara), intent(inout) :: cflag_update
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl(num_pvr_ctl)
!
      integer(kind = kint) :: i_pvr
!
!
      if(pvr_ctl(1)%updated_ctl%iflag .gt. 0) then
        cflag_update = pvr_ctl(1)%updated_ctl%charavalue
      end if
!
      do i_pvr = 1, num_pvr_ctl
        call bcast_vr_psf_ctl(pvr_ctl(i_pvr))
      end do
!
      end subroutine bcast_pvr_controls
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_pvr_controls(group, nod_fld,                     &
     &                              pvr_ctl_type, pvr_param)
!
      use t_group_data
      use t_phys_data
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
      use t_control_data_pvr_sections
      use set_control_each_pvr
      use set_field_comp_for_viz
      use set_pvr_modelview_matrix
      use set_control_pvr_movie
!
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type
!
      type(PVR_control_params), intent(inout) :: pvr_param
!
      integer(kind = kint) :: icheck_ncomp(1)
!
!
      call s_set_control_pvr_movie(pvr_ctl_type%movie,                  &
     &                             pvr_param%movie_def)
!
      call check_pvr_field_control(pvr_ctl_type,                        &
     &                             nod_fld%num_phys, nod_fld%phys_name)
!
      call set_control_field_4_pvr                                      &
     &   (pvr_ctl_type%pvr_field_ctl, pvr_ctl_type%pvr_comp_ctl,        &
     &    nod_fld%num_phys, nod_fld%phys_name,                          &
     &    pvr_param%field_def, icheck_ncomp)
      if (icheck_ncomp(1) .gt. 1)                                       &
     &   call calypso_MPI_abort(ierr_PVR, 'set scalar for rendering')
!
      if(iflag_debug .gt. 0) write(*,*) 'set_control_pvr'
      call set_control_pvr(pvr_ctl_type, group%ele_grp, group%surf_grp, &
     &    pvr_param%area_def, pvr_param%draw_param,                     &
     &    pvr_param%color, pvr_param%colorbar)
!
!   set parameters for stereo views
      call set_pvr_stereo_control(pvr_ctl_type, pvr_param%stereo_def)
      call set_pvr_mul_view_params(pvr_ctl_type%mat,                    &
     &    pvr_ctl_type%quilt_c, pvr_ctl_type%movie, pvr_param)
!
      end subroutine s_set_pvr_controls
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_mul_view_params(mat_base, quilt_c, movie_ctl,  &
     &                                   pvr_param)
!
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
      use t_control_data_pvr_sections
      use set_pvr_modelview_matrix
!
      type(modeview_ctl), intent(in) :: mat_base
      type(quilt_image_ctl), intent(in) :: quilt_c
      type(pvr_movie_ctl), intent(in) :: movie_ctl
!
      type(PVR_control_params), intent(inout) :: pvr_param
!
      integer(kind = kint) :: num_views
!
!
      if(quilt_c%mul_qmats_c%num_modelviews_c .gt. 0) then
        num_views = quilt_c%mul_qmats_c%num_modelviews_c
        if(num_views .lt.  pvr_param%stereo_def%num_views) then
          write(e_message,*) 'The number of view paramter should be',   &
     &                 ' more than number of quilt image. (Stop)'
          call calypso_mpi_abort(1,e_message)
       else
         call init_multi_view_parameters(num_views,                     &
     &       quilt_c%mul_qmats_c, pvr_param)
         end if
       else if(movie_ctl%mul_mmats_c%num_modelviews_c .gt. 0) then
         num_views = movie_ctl%mul_mmats_c%num_modelviews_c
         if(num_views .lt. pvr_param%movie_def%num_frame) then
          write(e_message,*) 'The number of view paramter should be',   &
     &                     ' more than number of movie image. (Stop)'
          call calypso_mpi_abort(1,e_message)
        else
          pvr_param%flag_mulview_movie = .TRUE.
          call init_multi_view_parameters(num_views,                    &
     &        movie_ctl%mul_mmats_c, pvr_param)
        end if
      else
        call alloc_multi_view_parameters(ione, pvr_param)
        call copy_stereo_perspective_matrix                             &
     &     (mat_base%streo, pvr_param%stereo_def)
        call s_set_pvr_modelview_matrix(mat_base,                       &
     &      pvr_param%multi_view(1))
      end if
!
      end subroutine set_pvr_mul_view_params
!
!  ---------------------------------------------------------------------
!
      subroutine init_multi_view_parameters(num_views, mul_mmats_c,     &
     &                                      pvr_param)
!
      use t_rendering_vr_image
      use set_pvr_modelview_matrix
!
      integer(kind = kint), intent(in) :: num_views
      type(multi_modelview_ctl), intent(in) :: mul_mmats_c
!
      type(PVR_control_params), intent(inout) :: pvr_param
!
      integer(kind = kint) :: i
!
!
      call alloc_multi_view_parameters(num_views, pvr_param)
      do i = 1, pvr_param%num_multi_views
        call s_set_pvr_modelview_matrix                                 &
     &     (mul_mmats_c%matrices(i), pvr_param%multi_view(i))
      end do
!
      end subroutine init_multi_view_parameters
!
!  ---------------------------------------------------------------------
!
      subroutine flush_each_pvr_control(pvr_param)
!
      use t_pvr_colormap_parameter
      use t_rendering_vr_image
      use t_geometries_in_pvr_screen
!
      type(PVR_control_params), intent(inout) :: pvr_param
!
!
      if(pvr_param%draw_param%num_sections .gt. 0) then
        call dealloc_pvr_sections(pvr_param%draw_param)
      end if
!
      if(pvr_param%draw_param%num_isosurf .gt. 0) then
        call dealloc_pvr_isosurfaces(pvr_param%draw_param)
      end if
!
      call dealloc_pvr_element_group(pvr_param%area_def)
      call dealloc_pvr_color_parameteres(pvr_param%color)
!
      end subroutine flush_each_pvr_control
!
!  ---------------------------------------------------------------------

      end module set_pvr_control
