!>@file   t_map_rendering_data.f90
!!@brief  module t_map_rendering_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine alloc_scalar_on_map(num_pixel, map_data)
!!      subroutine dealloc_scalar_on_map(map_data)
!!        integer(kind = kint), intent(in) :: num_pixel
!!        type(map_rendering_data), intent(inout) :: map_data
!!
!!      subroutine set_ctl_map_rendering_param                          &
!!     &         (proj_type_c, proj_c, map_define_ctl, map_data)
!!        type(read_character_item), intent(in) :: proj_type_c
!!        type(projection_ctl), intent(in) :: proj_c
!!        type(pvr_section_ctl), intent(in) :: map_define_ctl
!!        type(map_rendering_data), intent(inout) :: map_data
!!      subroutine init_map_rendering_data                              &
!!     &         (view_param, pvr_rgb, map_data)
!!        type(pvr_view_parameter), intent(in):: view_param
!!        type(pvr_image_type), intent(in) :: pvr_rgb
!!        type(map_rendering_data), intent(inout) :: map_data
!!      subroutine cal_map_rendering_data(time_d, psf_nod, psf_ele,     &
!!     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!!        type(time_data), intent(in) :: time_d
!!        type(pvr_view_parameter), intent(in):: view_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!        type(phys_data), intent(in) :: psf_phys
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        type(map_rendering_data), intent(inout) :: map_data
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
      module t_map_rendering_data
!
      use calypso_mpi
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_data
      use t_control_params_4_pvr
      use t_pvr_colormap_parameter
!
      implicit  none
!
      real(kind= kreal), parameter, private :: xframe = 2.4
      real(kind= kreal), parameter, private :: yframe = 1.8
!
      character(len = kchara), parameter :: label_xy_plane = 'xy_plane'
      character(len = kchara), parameter :: label_xz_plane = 'xz_plane'
      character(len = kchara), parameter :: label_yz_plane = 'yz_plane'
      character(len = kchara), parameter :: label_aitoff = 'aitoff'
      integer(kind = kint), parameter :: iflag_xy_plane = 1
      integer(kind = kint), parameter :: iflag_xz_plane = 2
      integer(kind = kint), parameter :: iflag_yz_plane = 3
      integer(kind = kint), parameter :: iflag_aitoff = 101
!
      character(len = kchara), parameter :: label_colored = 'color'
      character(len = kchara), parameter :: label_black =   'black'
      character(len = kchara), parameter :: label_white =   'white'
      integer(kind = kint), parameter :: iflag_colored = 1
      integer(kind = kint), parameter :: iflag_black =   2
      integer(kind = kint), parameter :: iflag_white =   3
!
      type map_rendering_data
        logical :: fill_flag = .TRUE.
        logical :: flag_zeroline = .FALSE.
        integer(kind = kint) :: num_line = 0
!
        integer(kind = kint) :: iflag_2d_projection_mode = 0
        integer(kind = kint) :: iflag_isoline_color = 0
!
        real(kind= kreal) :: xmin_frame = -xframe
        real(kind= kreal) :: xmax_frame =  xframe
        real(kind= kreal) :: ymin_frame = -yframe
        real(kind= kreal) :: ymax_frame =  yframe
!
        logical :: flag_tangent_cylinder = .FALSE.
        real(kind = kreal) :: tangent_cylinder_radius(2)
        real(kind = kreal) :: tangent_cylinder_theta(2)                 &
     &                           = (/(20.0d0/13.0d0), (7.0d0/13.0d0)/)
!>    Color of tangent cylinder
        real(kind = kreal) :: tangent_cylinder_rgba(4)                  &
     &                           = (/zero,zero,zero,one/)
!
        real(kind = kreal), allocatable :: d_map(:)
      end type map_rendering_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_scalar_on_map(num_pixel, map_data)
!
      integer(kind = kint), intent(in) :: num_pixel
      type(map_rendering_data), intent(inout) :: map_data
!
      allocate(map_data%d_map(num_pixel))
!
      if(num_pixel .le. 0) return
!$omp parallel workshare
      map_data%d_map(num_pixel) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_scalar_on_map
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_scalar_on_map(map_data)
!
      type(map_rendering_data), intent(inout) :: map_data
!
      deallocate(map_data%d_map)
!
      end subroutine dealloc_scalar_on_map
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_map_rendering_param                            &
     &         (proj_type_c, proj_c, map_define_ctl, map_data)
!
      use t_control_array_character
      use t_ctl_data_pvr_section
      use t_ctl_data_4_projection
      use skip_comment_f
!
      type(read_character_item), intent(in) :: proj_type_c
      type(projection_ctl), intent(in) :: proj_c
      type(pvr_section_ctl), intent(in) :: map_define_ctl
      type(map_rendering_data), intent(inout) :: map_data
!
      character(len = kchara) :: tmpchara
      real(kind = kreal) :: pi
!
!
      if(proj_type_c%iflag .gt. 0) then
        tmpchara = proj_type_c%charavalue
        if(cmp_no_case(tmpchara, label_xy_plane)) then
          map_data%iflag_2d_projection_mode = iflag_xy_plane
        else if(cmp_no_case(tmpchara, label_xz_plane)) then
          map_data%iflag_2d_projection_mode = iflag_xz_plane
        else if(cmp_no_case(tmpchara, label_yz_plane)) then
          map_data%iflag_2d_projection_mode = iflag_yz_plane
        else if(cmp_no_case(tmpchara, label_aitoff)) then
          map_data%iflag_2d_projection_mode = iflag_aitoff
        end if
      end if
!
      map_data%xmin_frame = -xframe
      map_data%xmax_frame =  xframe
      if(proj_c%horizontal_range_ctl%iflag .gt. 0) then
        map_data%xmin_frame = proj_c%horizontal_range_ctl%realvalue(1)
        map_data%xmax_frame = proj_c%horizontal_range_ctl%realvalue(2)
      end if
!
      map_data%ymin_frame = -yframe
      map_data%ymax_frame =  yframe
      if(proj_c%vertical_range_ctl%iflag .gt. 0) then
        map_data%ymin_frame = proj_c%vertical_range_ctl%realvalue(1)
        map_data%ymax_frame = proj_c%vertical_range_ctl%realvalue(2)
      end if
!
!
      map_data%fill_flag = .TRUE.
      if(map_define_ctl%opacity_ctl%iflag .gt. 0                        &
     &  .and. map_define_ctl%opacity_ctl%realvalue .eq. 0.0d0) then
        map_data%fill_flag = .FALSE.
      end if
!
      map_data%flag_zeroline = .FALSE.
      if(map_define_ctl%zeroline_switch_ctl%iflag .gt. 0) then
        map_data%flag_zeroline                                          &
     &   = yes_flag(map_define_ctl%zeroline_switch_ctl%charavalue)
      end if
!
      map_data%num_line = 0
      if(map_define_ctl%isoline_switch_ctl%iflag .gt. 0) then
        if(yes_flag(map_define_ctl%isoline_switch_ctl%charavalue)) then
          if(map_define_ctl%isoline_number_ctl%iflag .gt. 0) then
            map_data%num_line                                           &
     &          = map_define_ctl%isoline_number_ctl%intvalue
          end if
        end if
      end if
!
      map_data%iflag_isoline_color = iflag_black
      if(map_data%num_line .gt. 0) then
        if(map_define_ctl%isoline_color_mode%iflag .gt. 0) then
          tmpchara = map_define_ctl%isoline_color_mode%charavalue
          if(cmp_no_case(tmpchara, label_colored)) then
            map_data%iflag_isoline_color = iflag_colored
          else if(cmp_no_case(tmpchara, label_white)) then
            map_data%iflag_isoline_color = iflag_white
          else if(cmp_no_case(tmpchara, label_black)) then
            map_data%iflag_isoline_color = iflag_black
          end if
        end if
      end if
!
!
      if(map_define_ctl%zeroline_switch_ctl%iflag .gt. 0) then
        map_data%flag_zeroline                                          &
     &   = yes_flag(map_define_ctl%zeroline_switch_ctl%charavalue)
      end if
!
      if(map_define_ctl%tan_cyl_switch_ctl%iflag.gt.0) then
        map_data%flag_tangent_cylinder                                  &
     &   = yes_flag(map_define_ctl%tan_cyl_switch_ctl%charavalue)
      end if
!
      if( (map_define_ctl%tangent_cylinder_inner_ctl%iflag              &
     &  * map_define_ctl%tangent_cylinder_outer_ctl%iflag) .gt. 0) then
        map_data%tangent_cylinder_radius(1)                             &
     &   = map_define_ctl%tangent_cylinder_outer_ctl%realvalue
        map_data%tangent_cylinder_radius(2)                             &
     &   = map_define_ctl%tangent_cylinder_inner_ctl%realvalue
      end if
!
      pi = four*atan(one)
      map_data%tangent_cylinder_theta(1)                                &
     &          = asin(map_data%tangent_cylinder_radius(2)              &
     &               / map_data%tangent_cylinder_radius(1))
      map_data%tangent_cylinder_theta(2)                                &
     &          = pi - map_data%tangent_cylinder_theta(1)
!
      end subroutine set_ctl_map_rendering_param
!
!  ---------------------------------------------------------------------
!
      subroutine init_map_rendering_data                                &
     &         (view_param, pvr_rgb, map_data)
!
      use t_psf_patch_data
      use t_pvr_image_array
!
      type(pvr_view_parameter), intent(in):: view_param
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(map_rendering_data), intent(inout) :: map_data
!
      real(kind = kreal) :: x_tmp, y_tmp, width(2)
      real(kind = kreal) :: aspect
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!
!
      aspect =  view_param%perspective_xy_ratio
!
      map_data%xmin_frame = -xframe
      map_data%xmax_frame =  xframe
      map_data%ymin_frame = -yframe
      map_data%ymax_frame =  yframe
!
      width(1) = map_data%xmax_frame - map_data%xmin_frame
      width(2) = map_data%ymax_frame - map_data%ymin_frame
!
      y_tmp = width(1) * dble(view_param%n_pvr_pixel(2))                &
     &                 / dble(view_param%n_pvr_pixel(1))
      x_tmp = width(2) * dble(view_param%n_pvr_pixel(1))                &
     &                 / dble(view_param%n_pvr_pixel(2))
!
      if(x_tmp .gt. width(1)) then
        map_data%xmin_frame = map_data%xmin_frame * x_tmp / width(1)
        map_data%xmax_frame = map_data%xmax_frame * x_tmp / width(1)
      end if
      if(y_tmp .gt. width(2)) then
        map_data%ymin_frame = map_data%ymin_frame * y_tmp / width(2)
        map_data%ymax_frame = map_data%ymax_frame * y_tmp / width(2)
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'Orthogonal parameter for rendering '
        write(*,*) 'horizontal range:',                                 &
     &             map_data%xmin_frame, map_data%xmax_frame
        write(*,*) 'vertical range:',                                   &
     &             map_data%ymin_frame, map_data%ymax_frame
      end if
!
      end subroutine init_map_rendering_data
!
!  ---------------------------------------------------------------------
!
      subroutine cal_map_rendering_data(time_d, psf_nod, psf_ele,       &
     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!
      use t_psf_patch_data
      use t_time_data
      use t_file_IO_parameter
      use t_map_patch_from_1patch
      use t_pvr_image_array
!
      use set_ucd_data_to_type
      use ucd_IO_select
!
      use draw_aitoff_map
      use draw_lines_on_map
      use draw_pvr_colorbar
      use draw_pixels_on_map
      use set_map_values_for_grids
!
      type(time_data), intent(in) :: time_d
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(phys_data), intent(in) :: psf_phys
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      type(map_rendering_data), intent(inout) :: map_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      type(map_patches_for_1patch) :: map_e1
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!
      call alloc_map_patch_from_1patch(map_e1)
      call set_scalar_on_map_image(psf_nod, psf_ele, psf_phys,          &
     &    map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    pvr_rgb%num_pixel_xy, map_data%d_map, pvr_rgb%rgba_real_gl,   &
     &    map_e1)
      call dealloc_map_patch_from_1patch(map_e1)
!
      if(map_data%fill_flag) then
        call map_value_to_rgb                                           &
     &     (color_param, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),  &
     &      map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
!
      if(map_data%flag_zeroline) then
        call draw_zeroline                                              &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
!
      if(map_data%num_line .gt. 0) then
        call draw_isolines                                              &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), color_param,  &
     &      map_data%num_line, map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
!
      call map_value_to_colatitude                                      &
     &   (map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), map_data%d_map)
      call draw_latitude_grid                                           &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), map_data%d_map, &
     &    pvr_rgb%rgba_real_gl)
      if(map_data%flag_tangent_cylinder) then
        call draw_map_tangent_cyl_grid                                  &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    map_data%tangent_cylinder_theta, map_data%d_map,              &
     &    pvr_rgb%rgba_real_gl)
      end if
!
      call map_value_to_longitude                                       &
     &   (map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), map_data%d_map)
      call draw_longitude_grid                                          &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), map_data%d_map, &
     &    pvr_rgb%rgba_real_gl)
      call draw_mapflame(pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),  &
     &                   map_data%d_map, pvr_rgb%rgba_real_gl)
!
      call fill_background                                              &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    color_param%bg_rgba_real, pvr_rgb%rgba_real_gl)
!
      if(cbar_param%flag_pvr_colorbar) then
        call set_pvr_colorbar(pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels, &
     &      color_param, cbar_param, pvr_rgb%rgba_real_gl(1,1))
      end if
!
      if(cbar_param%flag_draw_time) then
        call set_pvr_timelabel                                          &
     &     (time_d%time, pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,      &
     &      cbar_param, pvr_rgb%rgba_real_gl(1,1))
      end if
!
      end subroutine cal_map_rendering_data
!
!  ---------------------------------------------------------------------
!
      end module t_map_rendering_data
