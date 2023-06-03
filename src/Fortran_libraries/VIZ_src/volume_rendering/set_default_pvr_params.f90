!set_default_pvr_params.f90
!      module set_default_pvr_params
!
!        programmed by H.Matsui on May. 2009
!
!!      subroutine check_pvr_parameters                                 &
!!     &         (outline, num_views, multi_view, color_param)
!!      subroutine set_default_pvr_data_params(outline, color_param)
!!        integer(kind = kint), intent(in) :: num_views
!!        type(pvr_domain_outline), intent(in) :: outline
!!        type(pvr_view_parameter), intent(inout)                       &
!!     &                           :: multi_view(num_views)
!!        type(pvr_colormap_parameter), intent(inout) :: color_param
!
      module set_default_pvr_params
!
      use m_precision
!
      use m_constants
      use t_control_params_4_pvr
      use t_pvr_colormap_parameter
!
      implicit none
!
      private :: set_default_viewpoint_pvr
      private :: set_default_lookatpoint_pvr
      private :: set_default_up_dir_pvr, set_default_light_pvr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_pvr_parameters                                   &
     &         (outline, num_views, multi_view, color_param)
!
      use t_surf_grp_4_pvr_domain
      use t_geometries_in_pvr_screen
!
      integer(kind = kint), intent(in) :: num_views
      type(pvr_domain_outline), intent(in) :: outline
      type(pvr_view_parameter), intent(inout) :: multi_view(num_views)
      type(pvr_colormap_parameter), intent(inout) :: color_param
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_views
        if(multi_view(i)%iflag_viewpoint .eq. 0) then
          call set_default_viewpoint_pvr(outline%center_g,              &
     &        outline%xx_minmax_g, multi_view(i)%viewpoint)
        end if
!
        if(multi_view(i)%iflag_lookpoint .eq. 0) then
          call set_default_lookatpoint_pvr                              &
     &       (outline%center_g, multi_view(i)%lookat_vec)
        end if
!
        if(multi_view(i)%iflag_updir .eq. 0) then
          call set_default_up_dir_pvr(multi_view(i)%up_direction_vec)
        end if
      end do
!
      if(color_param%iflag_pvr_lights .eq. 0) then
        call set_default_light_pvr                                      &
     &     (outline%center_g, outline%xx_minmax_g, color_param)
      end if
!
      end subroutine check_pvr_parameters
!
! -----------------------------------------------------------------------
!
      subroutine set_default_viewpoint_pvr                              &
     &         (center_g, xx_minmax_g, viewpoint_vec)
!
      real(kind = kreal), intent(in) :: center_g(3)
      real(kind = kreal), intent(in) :: xx_minmax_g(2,3)
      real(kind = kreal), intent(inout) :: viewpoint_vec(3)
!
!
      viewpoint_vec(1) = center_g(1)
      viewpoint_vec(2) = xx_minmax_g(2,2) + 1.5d0 * ( xx_minmax_g(2,2)  &
     &                                              - xx_minmax_g(1,2))
      viewpoint_vec(3) = xx_minmax_g(2,3) + 1.5d0 * ( xx_minmax_g(2,3)  &
     &                                              - xx_minmax_g(1,3))
!
      end subroutine set_default_viewpoint_pvr
!
! -----------------------------------------------------------------------
!
      subroutine set_default_lookatpoint_pvr(center_g, lookat_vec)
!
      real(kind = kreal), intent(in) :: center_g(3)
      real(kind = kreal), intent(inout) :: lookat_vec(3)
!
!
      lookat_vec(1:3) = center_g(1:3)
!
      end subroutine set_default_lookatpoint_pvr
!
! -----------------------------------------------------------------------
!
      subroutine set_default_up_dir_pvr(up_direction_vec)
!
      real(kind = kreal), intent(inout) :: up_direction_vec(3)
!
!
      up_direction_vec(1:3) = zero
      up_direction_vec(2) =   one
!
      end subroutine set_default_up_dir_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_default_light_pvr                                  &
     &         (center_g, xx_minmax_g, color_param)
!
      real(kind = kreal), intent(in) :: xx_minmax_g(2,3)
      real(kind = kreal), intent(in) :: center_g(3)
      type(pvr_colormap_parameter), intent(inout) :: color_param
!
!
      color_param%num_pvr_lights = 1
      color_param%xyz_pvr_lights(1,1) = center_g(1)
      color_param%xyz_pvr_lights(2,1) = xx_minmax_g(2,2)                &
     &                        + 0.1d0 * ( xx_minmax_g(2,2)              &
     &                                  - xx_minmax_g(1,2) )
      color_param%xyz_pvr_lights(3,1) = xx_minmax_g(2,3)                &
     &                        + 2.0d0 * ( xx_minmax_g(2,3)              &
     &                                  - xx_minmax_g(1,3) )
!
      end subroutine set_default_light_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_default_pvr_data_params(outline, color_param)
!
      use t_surf_grp_4_pvr_domain
      use set_color_4_pvr
!
      type(pvr_domain_outline), intent(in) :: outline
      type(pvr_colormap_parameter), intent(inout) :: color_param
!
!
      if(color_param%id_pvr_color(2) .eq. iflag_automatic) then
        color_param%pvr_datamap_param(1,1) = outline%d_minmax_pvr(1)
        color_param%pvr_datamap_param(1,2) = outline%d_minmax_pvr(2)
        color_param%pvr_datamap_param(2,1) = zero
        color_param%pvr_datamap_param(2,2) = one
      end if
!
      end subroutine set_default_pvr_data_params
!
! -----------------------------------------------------------------------
!
      end module set_default_pvr_params
