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
!!      subroutine set_ctl_map_rendering_param                          &
!!     &         (proj_type_c, proj_c, map_define_ctl, map_data)
!!        type(read_character_item), intent(in) :: proj_type_c
!!        type(projection_ctl), intent(in) :: proj_c
!!        type(map_section_ctl), intent(in) :: map_define_ctl
!!        type(map_rendering_data), intent(inout) :: map_data
!!      subroutine init_map_rendering_data                              &
!!     &         (view_param, pvr_rgb, map_data)
!!        type(pvr_view_parameter), intent(in):: view_param
!!        type(pvr_image_type), intent(in) :: pvr_rgb
!!        type(map_rendering_data), intent(inout) :: map_data
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
        logical :: flag_fixed_isoline_range = .FALSE.
        real(kind= kreal) :: dmin_isoline
        real(kind= kreal) :: dmax_isoline
!
        real(kind = kreal) :: width_isoline = 1.5d0
        real(kind = kreal) :: width_grid =    1.0d0
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
      end type map_rendering_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_map_rendering_param                            &
     &         (proj_type_c, proj_c, map_define_ctl, map_data)
!
      use t_control_array_character
      use t_ctl_data_map_section
      use t_ctl_data_4_projection
      use skip_comment_f
!
      type(read_character_item), intent(in) :: proj_type_c
      type(projection_ctl), intent(in) :: proj_c
      type(map_section_ctl), intent(in) :: map_define_ctl
      type(map_rendering_data), intent(inout) :: map_data
!
      character(len = kchara) :: tmpchara
      real(kind = kreal) :: pi
!
!
      map_data%iflag_2d_projection_mode = iflag_aitoff
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
      map_data%flag_zeroline = .FALSE.
      if(map_define_ctl%zeroline_switch_ctl%iflag .gt. 0) then
        map_data%flag_zeroline                                          &
     &   = yes_flag(map_define_ctl%zeroline_switch_ctl%charavalue)
      end if
!
      map_data%num_line = 0
      if(map_define_ctl%isoline_number_ctl%iflag .gt. 0) then
        map_data%num_line = map_define_ctl%isoline_number_ctl%intvalue
      end if
!
      map_data%dmin_isoline = zero
      map_data%dmax_isoline = zero
      if(map_define_ctl%isoline_range_ctl%iflag .gt. 0) then
        map_data%flag_fixed_isoline_range = .TRUE.
        map_data%dmin_isoline                                           &
     &        = map_define_ctl%isoline_range_ctl%realvalue(1)
        map_data%dmax_isoline                                           &
     &        = map_define_ctl%isoline_range_ctl%realvalue(2)
      end if
!
      map_data%width_isoline = 1.5d0
      if(map_define_ctl%isoline_width_ctl%iflag .gt. 0) then
        map_data%width_isoline                                          &
     &        = map_define_ctl%isoline_width_ctl%realvalue
      end if
!
      map_data%width_grid = 1.0d0
      if(map_define_ctl%grid_width_ctl%iflag .gt. 0) then
        map_data%width_grid                                             &
     &        = map_define_ctl%grid_width_ctl%realvalue
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
      end module t_map_rendering_data
