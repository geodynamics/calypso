!>@file  set_rgba_4_each_pixel.f90
!!       module set_rgba_4_each_pixel
!!
!!@author H. Matsui
!!@date   Programmed in July. 2006
!
!> @brief Structures for parameteres for volume rendering
!!
!!@verbatim
!!      subroutine s_set_rgba_4_each_pixel(viewpoint_vec,               &
!!     &          x4in_model, x4out_model, c_data, grad,                &
!!     &          color_param, rgba_pixel, rgba_now)
!!        real(kind = kreal), intent(in) :: viewpoint_vec(3)
!!        real(kind = kreal), intent(in) :: c_data, grad(3)
!!        real(kind = kreal), intent(in) :: x4in_model(4), x4out_model(4)
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        real(kind = kreal), intent(inout) :: rgba_pixel(4), rgba_now(4)
!!      subroutine color_plane_with_light                               &
!!     &         (viewpoint_vec, xout_model, c_data, grad,              &
!!     &          opa_current, color_param, rgba_pixel, rgba_now)
!!        real(kind = kreal), intent(in) :: viewpoint_vec(3)
!!        real(kind = kreal), intent(in) :: c_data, grad(3)
!!        real(kind = kreal), intent(in) :: x4out_model(4)
!!        real(kind = kreal), intent(in) :: opa_current
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        real(kind = kreal), intent(inout) :: rgba_pixel(4), rgba_now(4)
!!      subroutine black_plane_with_light                               &
!!     &         (viewpoint_vec, x4out_model, grad,                     &
!!     &          opa_current, color_param, rgba_pixel, rgba_now)
!!        real(kind = kreal), intent(in) :: viewpoint_vec(3)
!!        real(kind = kreal), intent(in) :: grad(3)
!!        real(kind = kreal), intent(in) :: x4out_model(4)
!!        real(kind = kreal), intent(in) :: opa_current
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        real(kind = kreal), intent(inout) :: rgba_pixel(4), rgba_now(4)
!!      subroutine plane_rendering_with_light                           &
!!     &         (viewpoint_vec, x4_model, surf_normal,                 &
!!     &          opa_current, color_param, rgba_pixel, rgba_now)
!!        real(kind = kreal), intent(in) :: viewpoint_vec(3)
!!        real(kind = kreal), intent(in) :: x4_model(4)
!!        real(kind = kreal), intent(in) :: surf_normal(3)
!!        real(kind = kreal), intent(in) :: opa_current
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        real(kind = kreal), intent(inout) :: rgba_pixel(4), rgba_now(4)
!!      subroutine surface_rendering_with_light                         &
!!     &         (viewpoint_vec, x4_model, surf_normal, color_surf,     &
!!     &          opa_current, color_param, rgba_pixel, rgba_now)
!!        real(kind = kreal), intent(in) :: viewpoint_vec(3)
!!        real(kind = kreal), intent(in) :: x4_model(4)
!!        real(kind = kreal), intent(in) :: surf_normal(3)
!!        real(kind = kreal), intent(in) :: color_surf(3)
!!        real(kind = kreal), intent(in) :: opa_current
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        real(kind = kreal), intent(inout) :: rgba_pixel(4), rgba_now(4)
!!@endverbatim
!
      module set_rgba_4_each_pixel
!
      use m_precision
      use m_constants
      use t_pvr_colormap_parameter
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_rgba_4_each_pixel(viewpoint_vec,                 &
     &          x4in_model, x4out_model, c_data, grad,                  &
     &          color_param, rgba_pixel, rgba_now)
!
      use set_color_4_pvr
      use phong_reflection
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: c_data, grad(3)
      real(kind = kreal), intent(in) :: x4in_model(4), x4out_model(4)
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba_pixel(4), rgba_now(4)
!
      integer(kind = kint) :: num_of_features
      real(kind = kreal) :: color(3)
      real(kind = kreal) :: anb_opacity, opa_current, ray_length
!
!
      ray_length = sqrt((x4out_model(1)-x4in_model(1))**2               &
    &                 + (x4out_model(2)-x4in_model(2))**2               &
    &                 + (x4out_model(3)-x4in_model(3))**2)
!
      num_of_features = color_param%num_opacity_pnt
      anb_opacity = color_param%pvr_opacity_param(1,num_of_features)
!
      call compute_opacity(color_param%id_pvr_color(3), anb_opacity,    &
     &    num_of_features, color_param%pvr_opacity_param,               &
     &    c_data, opa_current)
!
      call value_to_rgb(color_param%id_pvr_color(2),                    &
     &    color_param%id_pvr_color(1), color_param%num_pvr_datamap_pnt, &
     &    color_param%pvr_datamap_param, c_data, color)
!
!
      call cal_phong_reflection(viewpoint_vec,                          &
     &    color_param%num_pvr_lights, color_param%xyz_pvr_lights,       &
     &    grad, color_param%pvr_lighting_real,                          &
     &    x4in_model(1), x4out_model(1), color, rgba_now(1))
!
!      rgba_now(4) = 1.0d0                                              &
!     &        - (1.0d0 - opa_current)**(ray_length)
      rgba_now(4) = -ray_length * LOG(1.0d0 - opa_current)
!      rgba_now(4) = ray_length * opa_current
      rgba_now(1:3) = rgba_now(1:3) * rgba_now(4)
      if(rgba_now(4) .gt. one) rgba_now(4) = one
      if(rgba_now(4) .lt. zero) rgba_now(4) = zero
!
      call composite_alpha_blending(rgba_now, rgba_pixel)
!
      end subroutine s_set_rgba_4_each_pixel
!
! ----------------------------------------------------------------------
!
      subroutine color_plane_with_light                                 &
     &         (viewpoint_vec, x4out_model, c_data, grad,               &
     &          opa_current, color_param, rgba_pixel, rgba_now)
!
      use set_color_4_pvr
      use phong_reflection
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: c_data, grad(3)
      real(kind = kreal), intent(in) :: x4out_model(4)
      real(kind = kreal), intent(in) :: opa_current
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba_pixel(4), rgba_now(4)
!
      real(kind = kreal) :: color(3)
!
!
      call value_to_rgb(color_param%id_pvr_color(2),                    &
     &    color_param%id_pvr_color(1), color_param%num_pvr_datamap_pnt, &
     &    color_param%pvr_datamap_param, c_data, color)
!
!
      call cal_phong_reflection(viewpoint_vec,                          &
     &    color_param%num_pvr_lights, color_param%xyz_pvr_lights,       &
     &    grad, color_param%pvr_lighting_real,                          &
     &    x4out_model(1), x4out_model(1), color, rgba_now(1))
!
      rgba_now(1:3) = rgba_now(1:3) * opa_current
      rgba_now(4) =   opa_current
!
      call composite_alpha_blending(rgba_now, rgba_pixel)
!
      end subroutine color_plane_with_light
!
! ----------------------------------------------------------------------
!
      subroutine black_plane_with_light                                 &
     &         (viewpoint_vec, x4out_model, grad,                       &
     &          opa_current, color_param, rgba_pixel, rgba_now)
!
      use set_color_4_pvr
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: grad(3)
      real(kind = kreal), intent(in) :: x4out_model(4)
      real(kind = kreal), intent(in) :: opa_current
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba_pixel(4), rgba_now(4)
!
      real(kind = kreal), parameter :: black(3) = (/0.0, 0.0, 0.0/)
!
      call surface_rendering_with_light                                 &
     &   (viewpoint_vec, x4out_model, grad, black,                      &
     &    opa_current, color_param, rgba_pixel, rgba_now)
!
      end subroutine black_plane_with_light
!
! ----------------------------------------------------------------------
!
      subroutine plane_rendering_with_light                             &
     &         (viewpoint_vec, x4_model, surf_normal,                   &
     &          opa_current, color_param, rgba_pixel, rgba_now)
!
      use set_color_4_pvr
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: x4_model(4)
      real(kind = kreal), intent(in) :: surf_normal(3)
      real(kind = kreal), intent(in) :: opa_current
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba_pixel(4), rgba_now(4)
!
      real(kind = kreal), parameter :: color(3) = (/0.2, 0.2, 0.2/)
!
      call surface_rendering_with_light                                 &
     &   (viewpoint_vec, x4_model, surf_normal, color,                  &
     &    opa_current, color_param, rgba_pixel, rgba_now)
!
      end subroutine plane_rendering_with_light
!
! ----------------------------------------------------------------------
!
      subroutine surface_rendering_with_light                           &
     &         (viewpoint_vec, x4_model, surf_normal, color_surf,       &
     &          opa_current, color_param, rgba_pixel, rgba_now)
!
      use set_color_4_pvr
      use phong_reflection
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: x4_model(4)
      real(kind = kreal), intent(in) :: surf_normal(3)
      real(kind = kreal), intent(in) :: color_surf(3)
      real(kind = kreal), intent(in) :: opa_current
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba_pixel(4), rgba_now(4)
!
!
      call cal_phong_reflection(viewpoint_vec,                          &
     &    color_param%num_pvr_lights, color_param%xyz_pvr_lights,       &
     &    surf_normal, color_param%pvr_lighting_real,                   &
     &    x4_model(1), x4_model(1), color_surf, rgba_now(1))
!
      rgba_now(1:3) = rgba_now(1:3) * opa_current
      rgba_now(4) =   opa_current
!
      call composite_alpha_blending(rgba_now, rgba_pixel)
!
      end subroutine surface_rendering_with_light
!
! ----------------------------------------------------------------------
!
      end module set_rgba_4_each_pixel
