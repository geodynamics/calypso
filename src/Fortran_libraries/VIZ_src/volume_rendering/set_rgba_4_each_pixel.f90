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
!!     &          color_param, rgba_pixel)
!!      subroutine color_plane_with_light                               &
!!     &         (viewpoint_vec, xout_model, c_data, grad,              &
!!     &          opa_current, color_param, rgba_pixel)
!!      subroutine black_plane_with_light(viewpoint_vec, xout_model,    &
!!     &          grad, opa_current, color_param, rgba_pixel)
!!      subroutine plane_rendering_with_light(viewpoint_vec, x4_model,  &
!!     &          surf_normal, opa_current, color_param, rgba_pixel)
!!      subroutine surface_rendering_with_light                         &
!!     &         (viewpoint_vec, x4_model, surf_normal, color_surf,     &
!!     &          opa_current, color_param, rgba_pixel)
!!
!!      subroutine compute_opacity(transfer_function_style, opa_value,  &
!!     &          num_of_features, fea_point, value, opacity_local)
!!
!!      subroutine phong_reflection(view_point_d,                       &
!!     &          num_of_lights, light_point, norm_v, k_ads,            &
!!     &          in_point, out_point, color, rgb)
!!
!!      subroutine composite_alpha_blending(rgba_src, rgba_tgt)
!!      subroutine alpha_blending(rgba_src, rgba_tgt)
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
      real(kind = kreal), parameter :: EPSILON = 1.0d-9
      private :: EPSILON
!
      character(len = kchara), parameter                                &
     &                        :: hd_intensity =   'intense_chenge'
      character(len = kchara), parameter                                &
     &                        :: hd_pointlinear = 'point_linear'
      integer(kind = kint), parameter :: iflag_anbient =     1
      integer(kind = kint), parameter :: iflag_intense =     2
      integer(kind = kint), parameter :: iflag_pointlinear = 5
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_rgba_4_each_pixel(viewpoint_vec,                 &
     &          x4in_model, x4out_model, c_data, grad,                  &
     &          color_param, rgba_pixel)
!
      use set_color_4_pvr
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: c_data, grad(3)
      real(kind = kreal), intent(in) :: x4in_model(4), x4out_model(4)
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba_pixel(4)
!
      integer(kind = kint) :: num_of_features
      real(kind = kreal) :: color(3)
      real(kind = kreal) :: anb_opacity, opa_current, ray_length
      real(kind = kreal), allocatable :: rgb(:)
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
      allocate(rgb(4))
      call phong_reflection(viewpoint_vec,                              &
     &    color_param%num_pvr_lights, color_param%xyz_pvr_lights,       &
     &    grad, color_param%pvr_lighting_real,                          &
     &    x4in_model(1), x4out_model(1), color, rgb(1))
!
!      rgb(4) = 1.0d0                                                   &
!     &        - (1.0d0 - opa_current)**(ray_length)
      rgb(4) = -ray_length * LOG(1.0d0 - opa_current)
!      rgb(4) = ray_length * opa_current
      rgb(1:3) = rgb(1:3) * rgb(4)
      if(rgb(4) .gt. one) rgb(4) = one
      if(rgb(4) .lt. zero) rgb(4) = zero
!
      call composite_alpha_blending(rgb, rgba_pixel)
      deallocate(rgb)
!
      end subroutine s_set_rgba_4_each_pixel
!
! ----------------------------------------------------------------------
!
      subroutine color_plane_with_light                                 &
     &         (viewpoint_vec, x4out_model, c_data, grad,               &
     &          opa_current, color_param, rgba_pixel)
!
      use set_color_4_pvr
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: c_data, grad(3)
      real(kind = kreal), intent(in) :: x4out_model(4)
      real(kind = kreal), intent(in) :: opa_current
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba_pixel(4)
!
      real(kind = kreal) :: color(3)
      real(kind = kreal), allocatable :: rgb(:)
!
!
      call value_to_rgb(color_param%id_pvr_color(2),                    &
     &    color_param%id_pvr_color(1), color_param%num_pvr_datamap_pnt, &
     &    color_param%pvr_datamap_param, c_data, color)
!
!
      allocate(rgb(4))
      call phong_reflection(viewpoint_vec,                              &
     &    color_param%num_pvr_lights, color_param%xyz_pvr_lights,       &
     &    grad, color_param%pvr_lighting_real,                          &
     &    x4out_model(1), x4out_model(1), color, rgb(1))
!
      rgb(1:3) = rgb(1:3) * opa_current
      rgb(4) =   opa_current
!
      call composite_alpha_blending(rgb, rgba_pixel)
      deallocate(rgb)
!
      end subroutine color_plane_with_light
!
! ----------------------------------------------------------------------
!
      subroutine black_plane_with_light(viewpoint_vec, x4out_model,     &
     &          grad, opa_current, color_param, rgba_pixel)
!
      use set_color_4_pvr
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: grad(3)
      real(kind = kreal), intent(in) :: x4out_model(4)
      real(kind = kreal), intent(in) :: opa_current
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba_pixel(4)
!
      real(kind = kreal), parameter :: black(3) = (/0.0, 0.0, 0.0/)
!
      call surface_rendering_with_light(viewpoint_vec, x4out_model,     &
     &    grad, black, opa_current, color_param, rgba_pixel)
!
      end subroutine black_plane_with_light
!
! ----------------------------------------------------------------------
!
      subroutine plane_rendering_with_light(viewpoint_vec, x4_model,    &
     &          surf_normal, opa_current, color_param, rgba_pixel)
!
      use set_color_4_pvr
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: x4_model(4)
      real(kind = kreal), intent(in) :: surf_normal(3)
      real(kind = kreal), intent(in) :: opa_current
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba_pixel(4)
!
      real(kind = kreal), parameter :: color(3) = (/0.2, 0.2, 0.2/)
!
      call surface_rendering_with_light(viewpoint_vec, x4_model,        &
     &    surf_normal, color, opa_current, color_param, rgba_pixel)
!
      end subroutine plane_rendering_with_light
!
! ----------------------------------------------------------------------
!
      subroutine surface_rendering_with_light                           &
     &         (viewpoint_vec, x4_model, surf_normal, color_surf,       &
     &          opa_current, color_param, rgba_pixel)
!
      use set_color_4_pvr
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: x4_model(4)
      real(kind = kreal), intent(in) :: surf_normal(3)
      real(kind = kreal), intent(in) :: color_surf(3)
      real(kind = kreal), intent(in) :: opa_current
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba_pixel(4)
!
      real(kind = kreal), allocatable :: rgb(:)
!
!
      allocate(rgb(4))
!
      call phong_reflection(viewpoint_vec,                              &
     &    color_param%num_pvr_lights, color_param%xyz_pvr_lights,       &
     &    surf_normal, color_param%pvr_lighting_real,                   &
     &    x4_model(1), x4_model(1), color_surf, rgb(1))
!
      rgb(1:3) = rgb(1:3) * opa_current
      rgb(4) =   opa_current
!
      call composite_alpha_blending(rgb, rgba_pixel)
      deallocate(rgb)
!
      end subroutine surface_rendering_with_light
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine compute_opacity(transfer_function_style, opa_value,    &
     &          num_of_features, fea_point, value, opacity_local)
!
      integer(kind = kint), intent(in) :: transfer_function_style
      real(kind = kreal), intent(in) :: opa_value
      integer(kind = kint), intent(in) :: num_of_features
      real(kind = kreal), intent(in) :: fea_point(3,num_of_features)
      real(kind = kreal), intent(in) :: value
!
      real(kind = kreal), intent(out) :: opacity_local
!
      integer(kind = kint) :: i, min_type
      real(kind = kreal) ::  mint, t
!
!
      mint = 1.0d-17
      min_type = 1
      opacity_local = zero
      if     (transfer_function_style .eq. iflag_anbient) then
        opacity_local = opa_value
      else if(transfer_function_style .eq. iflag_pointlinear) then
        opacity_local = opa_value
        do i = 1, num_of_features-1
          if(value .le. fea_point(1,1)) then
            opacity_local = fea_point(3,1)
            exit
          else if(value .ge. fea_point(1,num_of_features)) then
            opacity_local = fea_point(3,num_of_features)
            exit
          else if(value.ge.fea_point(1,i)                               &
     &         .and. value.le.fea_point(1,i+1)) then
            opacity_local = fea_point(3,i)                              &
     &                     + (fea_point(3,i+1) - fea_point(3,i))        &
     &                     * (value - fea_point(1,i))                   &
     &                      / (fea_point(1,i+1) - fea_point(1,i))
            exit
          end if
        end do
      end if
!
      end subroutine compute_opacity
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine phong_reflection(view_point_d,                         &
     &          num_of_lights, light_point, norm_v, k_ads,              &
     &          in_point4, out_point4, color, rgb)
!
      real(kind = kreal), intent(in) :: view_point_d(3)
      real(kind = kreal), intent(in) :: k_ads(3)
      real(kind = kreal), intent(in) :: norm_v(3)
      integer(kind = kint), intent(in) :: num_of_lights
      real(kind = kreal), intent(in) :: light_point(3,num_of_lights)
!
      real(kind = kreal), intent(in) :: in_point4(4)
      real(kind = kreal), intent(in) :: out_point4(4)
!
      real(kind = kreal), intent(in) :: color(3)
!
      real(kind = kreal), intent(inout)  :: rgb(3)
!
      integer(kind = kint) :: j
      real(kind = kreal) :: cosalpha, costheta
      real(kind = kreal) :: lp_norm, vp_norm, hp_norm, norm
      real(kind = kreal) :: inprodLN, inprodVN, inprodHN
      real(kind = kreal) :: coef
      real(kind = kreal), allocatable :: lp(:), vp(:), hp(:), vo(:)
      real(kind = kreal) :: x4_mid(4)
!
      allocate(vo(3))
      allocate(lp(3), vp(3), hp(3))
!
      rgb(1:3) = zero
      x4_mid(1:4) = half*(out_point4(1:4) +  in_point4(1:4))
      vo(1:3) = view_point_d(1:3) - norm_v(1:3)
!
      do j = 1, num_of_lights
        lp(1:3) = light_point(1:3,j) - x4_mid(1:3)
        vp(1:3) = - x4_mid(1:3)
        hp(1:3) = (lp(1:3) + vp(1:3)) / two
!
        lp_norm = sqrt( lp(1)*lp(1) + lp(2)*lp(2) + lp(3)*lp(3) )
        vp_norm = sqrt( vp(1)*vp(1) + vp(2)*vp(2) + vp(3)*vp(3) )
        hp_norm = sqrt( hp(1)*hp(1) + hp(2)*hp(2) + hp(3)*hp(3) )
        norm =    sqrt( vo(1)*vo(1) + vo(2)*vo(2) + vo(3)*vo(3) )
!
        if(abs(lp_norm) .gt. EPSILON) lp(1:3) = lp(1:3) / lp_norm
        if(abs(vp_norm) .gt. EPSILON) vp(1:3) = vp(1:3) / vp_norm
        if(abs(hp_norm) .gt. EPSILON) hp(1:3) = hp(1:3) / hp_norm
        if(abs(norm) .gt.    EPSILON) vo(1:3) = vo(1:3) / norm
!
        inprodLN = vo(1)*lp(1) + vo(2)*lp(2) + vo(3)*lp(3)
        inprodVN = vo(1)*vp(1) + vo(2)*vp(2) + vo(3)*vp(3)
        inprodHN = vo(1)*hp(1) + vo(2)*hp(2) + vo(3)*hp(3)
!
        cosalpha = inprodLN
        costheta = inprodLN*inprodVN                                    &
     &            - sqrt(one-inprodLN*inprodLN)                         &
     &             *sqrt(one-inprodVN*inprodVN)
        cosalpha = abs(cosalpha)
!
        if(cosalpha .gt. zero) then
          coef = k_ads(1) + k_ads(2)*cosalpha + k_ads(3)*costheta**6
        else
          coef = k_ads(1)
        end if
        rgb(1:3) = rgb(1:3) + color(1:3) * coef
      end do
      deallocate(vo, lp, vp, hp)
!
      end subroutine phong_reflection
!
! ----------------------------------------------------------------------
!
      subroutine composite_alpha_blending(rgba_src, rgba_tgt)
!
      real(kind = kreal), intent(in) :: rgba_src(4)
      real(kind = kreal), intent(inout) :: rgba_tgt(4)
!
!   This us is backward casting!!
!
      rgba_tgt(4) = rgba_src(4) + rgba_tgt(4) * (one - rgba_src(4))
      rgba_tgt(1:3) =  rgba_src(1:3)                                    &
     &               + rgba_tgt(1:3) * (one - rgba_src(4))
!
      end subroutine composite_alpha_blending
!
! ----------------------------------------------------------------------
!
      subroutine alpha_blending(rgba_src, rgba_tgt)
!
      real(kind = kreal), intent(in) :: rgba_src(4)
      real(kind = kreal), intent(inout) :: rgba_tgt(4)
!
      real(kind = kreal) :: rgba_bck(4), a_rgba
!
!
      rgba_bck(1:4) = rgba_tgt(1:4)
!
      rgba_tgt(4) = rgba_src(4) + rgba_bck(4) * (one - rgba_src(4))
      if(rgba_tgt(4) .eq. zero) then
        rgba_tgt(1:3) = zero
      else
        a_rgba = one / rgba_tgt(4)
        rgba_tgt(1:3) =  rgba_src(1:3) * (rgba_src(4)*a_rgba)           &
     &                 + rgba_bck(1:3) * (rgba_bck(4)*a_rgba)           &
     &                  * (one - rgba_src(4))
      end if
!
      end subroutine alpha_blending
!
! ----------------------------------------------------------------------
!
      end module set_rgba_4_each_pixel
