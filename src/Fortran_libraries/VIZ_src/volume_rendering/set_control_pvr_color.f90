!>@file   set_control_pvr_color.f90
!!@brief  module set_control_pvr_color
!!
!!@author H. Matsui
!!@date Programmed in  May. 2006
!
!> @brief Load control parameters from input data
!!
!!@verbatim
!!      subroutine set_control_pvr_lighting(light, color_param)
!!      subroutine set_control_pvr_colormap(color, color_param)
!!      subroutine set_control_pvr_colorbar(cbar_ctl, cbar_param)
!!        type(pvr_light_ctl), intent(in) :: light
!!        type(pvr_colormap_ctl), intent(in) :: color
!!        type(pvr_colormap_parameter), intent(inout) :: color_param
!!        type(pvr_colorbar_ctl), intent(in) :: cbar_ctl
!!@endverbatim
!
      module set_control_pvr_color
!
      use m_precision
!
      use m_constants
      use m_error_IDs
      use calypso_mpi
!
      use t_pvr_colormap_parameter
      use skip_comment_f
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_pvr_lighting(light, color_param)
!
      use t_ctl_data_pvr_light
      use set_color_4_pvr
      use set_rgba_4_each_pixel
!
      type(pvr_light_ctl), intent(in) :: light
!
      type(pvr_colormap_parameter), intent(inout) :: color_param
!
      integer(kind = kint) :: i, icou
      real(kind = kreal) :: r, t, p
!
!
      if(light%ambient_coef_ctl%iflag .gt. 0) then
        color_param%pvr_lighting_real(1)                                &
     &      = light%ambient_coef_ctl%realvalue
      else
        color_param%pvr_lighting_real(1) = 0.5
      end if
!
      if(light%diffuse_coef_ctl%iflag .gt. 0) then
        color_param%pvr_lighting_real(2)                                &
     &      = light%diffuse_coef_ctl%realvalue
      else
        color_param%pvr_lighting_real(2) = 0.7
      end if
!
      if(light%specular_coef_ctl%iflag .gt. 0) then
        color_param%pvr_lighting_real(3)                                &
     &      = light%specular_coef_ctl%realvalue
      else
        color_param%pvr_lighting_real(3) = 0.8
      end if
!
!
      color_param%num_pvr_lights = 0
      if(light%light_position_ctl%num .gt. 0) then
        color_param%num_pvr_lights = color_param%num_pvr_lights         &
     &                              + light%light_position_ctl%num
      end if
!
      if(light%light_sph_posi_ctl%num .gt. 0) then
        color_param%num_pvr_lights = color_param%num_pvr_lights         &
     &                              + light%light_sph_posi_ctl%num
      end if
      if(color_param%num_pvr_lights .eq. 0)                             &
     &                             color_param%num_pvr_lights = 1
!
      call alloc_light_posi_in_view(color_param)
!
      i = 0
      if(light%light_position_ctl%num .gt. 0) then
        do icou = 1, light%light_position_ctl%num
          i = i + 1
          color_param%xyz_pvr_lights(1,i)                               &
     &          = light%light_position_ctl%vec1(icou)
          color_param%xyz_pvr_lights(2,i)                               &
     &          = light%light_position_ctl%vec2(icou)
          color_param%xyz_pvr_lights(3,i)                               &
     &          = light%light_position_ctl%vec3(icou)
        end do
      end if
      if(light%light_sph_posi_ctl%num .gt. 0) then
        do icou = 1, light%light_sph_posi_ctl%num
          i = i + 1
          r = light%light_sph_posi_ctl%vec1(icou)
          t = light%light_sph_posi_ctl%vec2(icou) * atan(one) / 45.0
          p = light%light_sph_posi_ctl%vec3(icou) * atan(one) / 45.0
          color_param%xyz_pvr_lights(1,i) = r * sin(t) * cos(p)
          color_param%xyz_pvr_lights(2,i) = r * sin(t) * sin(p)
          color_param%xyz_pvr_lights(3,i) = r * cos(t)
        end do
      end if

      if(i .gt. 0) then
        color_param%iflag_pvr_lights = 1
      else
        color_param%xyz_pvr_lights(1,1) = one
        color_param%xyz_pvr_lights(2,1) = one
        color_param%xyz_pvr_lights(3,1) = one
      end if
!
      end subroutine set_control_pvr_lighting
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_control_pvr_colormap(color, color_param)
!
      use t_ctl_data_pvr_colormap
      use set_color_4_pvr
      use set_rgba_4_each_pixel
!
      type(pvr_colormap_ctl), intent(in) :: color
!
      type(pvr_colormap_parameter), intent(inout) :: color_param
!
      integer(kind = kint) :: i, ist
      character(len = kchara) :: tmpchara
!
!
!    set colormap setting
!
      color_param%id_pvr_color(1) = iflag_rainbow
      if( color%colormap_mode_ctl%iflag .gt. 0) then
        tmpchara = color%colormap_mode_ctl%charavalue
        if     (cmp_no_case(tmpchara, hd_rainbow)) then
          color_param%id_pvr_color(1) = iflag_rainbow
        else if(cmp_no_case(tmpchara, hd_radblue)) then
          color_param%id_pvr_color(1) = iflag_redblue
        else if(cmp_no_case(tmpchara, hd_grayscale)) then
          color_param%id_pvr_color(1) = iflag_grayscale
        else if(cmp_no_case(tmpchara, hd_sym_gray)) then
          color_param%id_pvr_color(1) = iflag_sym_gray
        else if(cmp_no_case(tmpchara, hd_orangecyan)) then
          color_param%id_pvr_color(1) = iflag_orangecyan
        else if(cmp_no_case(tmpchara, hd_moltenmetal)) then
          color_param%id_pvr_color(1) = iflag_moltenmetal
        else if(cmp_no_case(tmpchara, hd_spacecolor)) then
          color_param%id_pvr_color(1) = iflag_spacecolor
        end if
      end if
!
      color_param%bg_rgba_real(1:4) = 0.0d0
      if(color%background_color_ctl%iflag .gt. 0) then
        color_param%bg_rgba_real(1:3)                                   &
     &        = color%background_color_ctl%realvalue(1:3)
        color_param%bg_rgba_real(4) = 1.0d0
      end if
!
!
      color_param%id_pvr_color(2) = iflag_automatic
      color_param%num_pvr_datamap_pnt = 2
      if( color%data_mapping_ctl%iflag .gt. 0) then
        tmpchara = color%data_mapping_ctl%charavalue
        if      (cmp_no_case(tmpchara, hd_nonlinear)                    &
     &      .or. cmp_no_case(tmpchara, hd_colorlist)) then
          if(color%colortbl_ctl%num .gt. 0) then
            color_param%id_pvr_color(2) = iflag_colorlist
            color_param%num_pvr_datamap_pnt = color%colortbl_ctl%num
          end if
        else if (cmp_no_case(tmpchara, hd_linear)                       &
     &      .or. cmp_no_case(tmpchara, hd_minmax)) then
          if(      color%range_min_ctl%iflag .gt. 0                     &
     &       .and. color%range_max_ctl%iflag .gt. 0) then
            color_param%id_pvr_color(2) = iflag_minmax
          end if
        end if
      end if
!
!
      call alloc_pvr_color_parameteres(color_param)
!
      if (color_param%id_pvr_color(2) .eq. iflag_minmax) then
        color_param%pvr_datamap_param(1,1)                              &
     &        = color%range_min_ctl%realvalue
        color_param%pvr_datamap_param(1,2)                              &
     &        = color%range_max_ctl%realvalue
        color_param%pvr_datamap_param(2,1) = zero
        color_param%pvr_datamap_param(2,2) = one
!
      else if(color_param%id_pvr_color(2) .eq. iflag_colorlist) then
        do i = 1, color_param%num_pvr_datamap_pnt
          color_param%pvr_datamap_param(1,i)                            &
     &       = color%colortbl_ctl%vec1(i)
          color_param%pvr_datamap_param(2,i)                            &
     &       = color%colortbl_ctl%vec2(i)
        end do
!
      else
        color_param%pvr_datamap_param(1,1) = zero
        color_param%pvr_datamap_param(1,2) = zero
        color_param%pvr_datamap_param(2,1) = zero
        color_param%pvr_datamap_param(2,2) = one
      end if
!
!
!
      color_param%id_pvr_color(3) = iflag_anbient
      color_param%num_opacity_pnt = 0
      if( color%opacity_style_ctl%iflag .gt. 0) then
        tmpchara = color%opacity_style_ctl%charavalue
!        if     (cmp_no_case(tmpchara, hd_intensity) then
!          color_param%id_pvr_color(3) = iflag_intense
!        end if
        if(cmp_no_case(tmpchara, hd_pointlinear)) then
          if( color%linear_opacity_ctl%num .gt. 0) then
            color_param%id_pvr_color(3) = iflag_pointlinear
            color_param%num_opacity_pnt = color%linear_opacity_ctl%num
          end if
        end if
      end if
!
      call alloc_pvr_opacity_list(color_param)
!
      if(color_param%id_pvr_color(3) .eq. iflag_pointlinear) then
        do i = 1, color_param%num_opacity_pnt
          color_param%pvr_opacity_param(1,i)                            &
     &                  = color%linear_opacity_ctl%vec1(i)
          color_param%pvr_opacity_param(2,i)                            &
     &                   = color%linear_opacity_ctl%vec1(i)
          color_param%pvr_opacity_param(3,i)                            &
     &                   = color%linear_opacity_ctl%vec2(i)
          color_param%pvr_max_opacity                                   &
     &       = max(color_param%pvr_max_opacity,                         &
     &             color_param%pvr_opacity_param(3,i))
        end do
      end if
!
      ist = color_param%num_opacity_pnt + 1
      color_param%pvr_opacity_param(1,ist) = zero
      color_param%pvr_opacity_param(2,ist) = one
      if( color%fix_opacity_ctl%iflag .gt. 0) then
        color_param%pvr_opacity_param(3,ist)                            &
     &       = color%fix_opacity_ctl%realvalue
      else
        color_param%pvr_opacity_param(3,ist) = 0.001
      end if
      color_param%pvr_max_opacity                                       &
     &       = max(color_param%pvr_max_opacity,                         &
     &             color_param%pvr_opacity_param(3,ist))
!
      end subroutine set_control_pvr_colormap
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_control_pvr_colorbar(cbar_ctl, cbar_param)
!
      use t_ctl_data_pvr_colorbar
      use t_pvr_colormap_parameter
!
      type(pvr_colorbar_ctl), intent(in) :: cbar_ctl
      type(pvr_colorbar_parameter), intent(inout) :: cbar_param
!
      character(len = kchara) :: tmpchara
!
!    set axis label setting
      cbar_param%flag_pvr_axis = .FALSE.
      if( cbar_ctl%axis_switch_ctl%iflag .gt. 0) then
        tmpchara = cbar_ctl%axis_switch_ctl%charavalue
        cbar_param%flag_pvr_axis = cmp_no_case(tmpchara, 'on')
      end if
!
!    set time label setting
      cbar_param%flag_draw_time = .FALSE.
      if( cbar_ctl%time_switch_ctl%iflag .gt. 0) then
        tmpchara = cbar_ctl%time_switch_ctl%charavalue
        cbar_param%flag_draw_time = cmp_no_case(tmpchara, 'on')
      end if
!
!    set mapgrid setting
      cbar_param%flag_draw_mapgrid = .FALSE.
      if( cbar_ctl%mapgrid_switch_ctl%iflag .gt. 0) then
        tmpchara = cbar_ctl%mapgrid_switch_ctl%charavalue
        cbar_param%flag_draw_mapgrid = cmp_no_case(tmpchara, 'on')
      end if
!
!    set colorbar setting
!
      cbar_param%flag_pvr_colorbar = .FALSE.
      if( cbar_ctl%colorbar_switch_ctl%iflag .gt. 0) then
        tmpchara = cbar_ctl%colorbar_switch_ctl%charavalue
        cbar_param%flag_pvr_colorbar                                    &
     &       = cmp_no_case(tmpchara, 'on')                              &
     &   .or. cmp_no_case(tmpchara, 'data')                             &
     &   .or. cmp_no_case(tmpchara, 'equi_data')
      end if
!
      if(cbar_param%flag_pvr_colorbar) then
        cbar_param%flag_pvr_cbar_bottom = .FALSE.
        if( cbar_ctl%colorbar_position_ctl%iflag .gt. 0) then
          tmpchara = cbar_ctl%colorbar_position_ctl%charavalue
          if  (cmp_no_case(tmpchara, 'bottom')) then
            cbar_param%flag_pvr_cbar_bottom = .TRUE.
          end if
        end if

        if( cbar_ctl%colorbar_scale_ctl%iflag .gt. 0) then
          tmpchara = cbar_ctl%colorbar_scale_ctl%charavalue
          if  (cmp_no_case(tmpchara, 'on')) then
            cbar_param%iflag_pvr_cbar_nums = 1
!
            if (cbar_ctl%font_size_ctl%iflag .gt. 0) then
              cbar_param%iscale_font = cbar_ctl%font_size_ctl%intvalue
            else
              cbar_param%iscale_font = 1
            end if
!
            if (cbar_ctl%ngrid_cbar_ctl%iflag .gt. 0) then
              cbar_param%ntick_pvr_colorbar                             &
     &                      = cbar_ctl%ngrid_cbar_ctl%intvalue + 2
            else
              cbar_param%ntick_pvr_colorbar = 3
            end if
!
            if (cbar_ctl%zeromarker_flag_ctl%iflag .gt. 0) then
              tmpchara = cbar_ctl%zeromarker_flag_ctl%charavalue
              if  (cmp_no_case(tmpchara, 'on')) then
                cbar_param%iflag_pvr_zero_mark = 1
              else
                cbar_param%iflag_pvr_zero_mark = 0
              end if
            else
              cbar_param%iflag_pvr_zero_mark = 0
            end if
!
          end if
        end if
!
        if( cbar_ctl%cbar_range_ctl%iflag .gt. 0) then
          cbar_param%cbar_range(1:2)                                    &
     &                = cbar_ctl%cbar_range_ctl%realvalue(1:2)
        end if
      end if
!
      end subroutine set_control_pvr_colorbar
!
!  ---------------------------------------------------------------------
!
      end module set_control_pvr_color
