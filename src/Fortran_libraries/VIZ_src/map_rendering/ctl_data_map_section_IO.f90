!>@file   ctl_data_map_section_IO.f90
!!@brief  module ctl_data_map_section_IO
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine init_map_section_ctl_label(hd_block, map_sect_ctl)
!!      subroutine read_map_section_ctl                                 &
!!     &         (id_control, hd_block, icou, map_sect_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(map_section_ctl), intent(inout) :: map_sect_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_map_section_ctl                                &
!!     &         (id_control, hd_block, map_sect_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(map_section_ctl), intent(inout) :: map_sect_ctl
!!        integer(kind = kint), intent(inout) :: level
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin section_ctl
!!    file surface_define     ctl_psf_eq
!!    begin surface_define
!!      ...
!!    end surface_define
!!
!!    zeroline_switch_ctl           On
!!    isoline_color_mode      color, white, or black
!!    isoline_number_ctl            20
!!    isoline_range_ctl          -0.5   0.5
!!    isoline_width_ctl             1.5
!!    grid_width_ctl                1.0
!!
!!    tangent_cylinder_switch_ctl   On
!!    inner_radius_ctl              0.53846
!!    outer_radius_ctl              1.53846
!!  end section_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_data_map_section_IO
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_data_4_psf_def
      use t_control_array_real
      use t_control_array_real2
      use t_control_array_character
      use t_ctl_data_map_section
      use skip_comment_f
!
      implicit  none
!
!   Labels
      character(len=kchara), parameter, private                         &
     &                  :: hd_surface_define =  'surface_define'
!
      character(len=kchara), parameter, private                         &
     &                  :: hd_pvr_sec_zeroline = 'zeroline_switch_ctl'
      character(len=kchara), parameter, private                         &
     &                  :: hd_pvr_isoline_color = 'isoline_color_mode'
      character(len=kchara), parameter, private                         &
     &                  :: hd_isoline_number =    'isoline_number_ctl'
      character(len=kchara), parameter, private                         &
     &                  :: hd_isoline_range =     'isoline_range_ctl'
      character(len=kchara), parameter, private                         &
     &                  :: hd_isoline_width =     'isoline_width_ctl'
      character(len=kchara), parameter, private                         &
     &                  :: hd_grid_width =        'grid_width_ctl'
!
      character(len=kchara), parameter, private                         &
     &        :: hd_tangent_cylinder = 'tangent_cylinder_switch_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_tcyl_inner = 'inner_radius_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_tcyl_outer = 'outer_radius_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_map_section_ctl                                   &
     &         (id_control, hd_block, icou, map_sect_ctl, c_buf)
!
      use ctl_file_section_def_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, icou
      character(len=kchara), intent(in) :: hd_block
      type(map_section_ctl), intent(inout) :: map_sect_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(map_sect_ctl%i_map_sect_ctl .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        if(check_file_flag(c_buf, hd_surface_define)                    &
     &        .or. check_begin_flag(c_buf, hd_surface_define)) then
          call write_multi_ctl_file_message                             &
     &       (hd_block, icou, c_buf%level)
          call sel_read_ctl_pvr_section_def(id_control,                 &
     &        hd_surface_define, map_sect_ctl%fname_sect_ctl,           &
     &        map_sect_ctl%psf_def_c, c_buf)
        end if
!
        call read_chara_ctl_type(c_buf, hd_pvr_sec_zeroline,            &
     &      map_sect_ctl%zeroline_switch_ctl)
        call read_chara_ctl_type(c_buf, hd_pvr_isoline_color,           &
     &      map_sect_ctl%isoline_color_mode)
        call read_integer_ctl_type(c_buf, hd_isoline_number,            &
     &      map_sect_ctl%isoline_number_ctl)
        call read_real2_ctl_type(c_buf, hd_isoline_range,               &
     &      map_sect_ctl%isoline_range_ctl)
        call read_real_ctl_type(c_buf, hd_isoline_width,                &
     &      map_sect_ctl%isoline_width_ctl)
        call read_real_ctl_type(c_buf, hd_grid_width,                   &
     &      map_sect_ctl%grid_width_ctl)
!
        call read_chara_ctl_type(c_buf, hd_tangent_cylinder,            &
     &      map_sect_ctl%tan_cyl_switch_ctl)
        call read_real_ctl_type(c_buf, hd_tcyl_inner,                   &
     &      map_sect_ctl%tangent_cylinder_inner_ctl)
        call read_real_ctl_type(c_buf, hd_tcyl_outer,                   &
     &      map_sect_ctl%tangent_cylinder_outer_ctl)
      end do
      map_sect_ctl%i_map_sect_ctl = 1
!
      end subroutine read_map_section_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_map_section_ctl                                  &
     &         (id_control, hd_block, map_sect_ctl, level)
!
      use ctl_file_section_def_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(map_section_ctl), intent(in) :: map_sect_ctl
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(map_sect_ctl%i_map_sect_ctl .le. 0) return
      maxlen = len_trim(hd_pvr_sec_zeroline)
      maxlen = max(maxlen,len_trim(hd_pvr_isoline_color))
      maxlen = max(maxlen,len_trim(hd_isoline_number))
      maxlen = max(maxlen,len_trim(hd_isoline_range))
      maxlen = max(maxlen,len_trim(hd_isoline_width))
      maxlen = max(maxlen,len_trim(hd_grid_width))
      maxlen = max(maxlen,len_trim(hd_tangent_cylinder))
      maxlen = max(maxlen,len_trim(hd_tcyl_inner))
      maxlen = max(maxlen,len_trim(hd_tcyl_outer))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call sel_write_ctl_pvr_section_def(id_control, hd_surface_define, &
     &    map_sect_ctl%fname_sect_ctl, map_sect_ctl%psf_def_c, level)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    map_sect_ctl%zeroline_switch_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    map_sect_ctl%isoline_color_mode)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    map_sect_ctl%isoline_number_ctl)
      call write_real2_ctl_type(id_control, level, maxlen,              &
     &    map_sect_ctl%isoline_range_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    map_sect_ctl%isoline_width_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    map_sect_ctl%grid_width_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    map_sect_ctl%tan_cyl_switch_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    map_sect_ctl%tangent_cylinder_inner_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    map_sect_ctl%tangent_cylinder_outer_ctl)
      level = write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_map_section_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine init_map_section_ctl_label(hd_block, map_sect_ctl)
!
      use ctl_data_section_def_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(map_section_ctl), intent(inout) :: map_sect_ctl
!
      map_sect_ctl%block_name = hd_block
      call init_psf_def_ctl_stract                                      &
     &   (hd_surface_define, map_sect_ctl%psf_def_c)
!
        call init_chara_ctl_item_label(hd_pvr_sec_zeroline,             &
     &      map_sect_ctl%zeroline_switch_ctl)
        call init_chara_ctl_item_label(hd_pvr_isoline_color,            &
     &      map_sect_ctl%isoline_color_mode)
        call init_int_ctl_item_label(hd_isoline_number,                 &
     &      map_sect_ctl%isoline_number_ctl)
        call init_real2_ctl_item_label(hd_isoline_range,                &
     &      map_sect_ctl%isoline_range_ctl)
        call init_real_ctl_item_label(hd_isoline_width,                 &
     &      map_sect_ctl%isoline_width_ctl)
        call init_real_ctl_item_label(hd_grid_width,                    &
     &      map_sect_ctl%grid_width_ctl)
!
        call init_chara_ctl_item_label(hd_tangent_cylinder,             &
     &      map_sect_ctl%tan_cyl_switch_ctl)
        call init_real_ctl_item_label(hd_tcyl_inner,                    &
     &      map_sect_ctl%tangent_cylinder_inner_ctl)
        call init_real_ctl_item_label(hd_tcyl_outer,                    &
     &      map_sect_ctl%tangent_cylinder_outer_ctl)
!
      end subroutine init_map_section_ctl_label
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_map_section_IO
