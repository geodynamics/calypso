!>@file   t_ctl_data_map_section.f90
!!@brief  module t_ctl_data_map_section
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine dup_map_section_ctl(org_map_sect_c, new_map_sect_c)
!!        type(map_section_ctl), intent(in) :: org_map_sect_c
!!        type(map_section_ctl), intent(inout) :: new_map_sect_c
!!      subroutine dealloc_map_section_ctl(map_sect_ctl)
!!        type(map_section_ctl), intent(inout) :: map_sect_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  array section_ctl
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
!!  end array section_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_map_section
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_data_4_psf_def
      use t_control_array_real
      use t_control_array_real2
      use t_control_array_integer
      use t_control_array_character
      use t_control_array_chara2real
      use skip_comment_f
!
      implicit  none
!
      type map_section_ctl
!>        Block name
        character(len=kchara) :: block_name = 'surface_define'
!
!>        File name of control file to define surface
        character(len = kchara) :: fname_sect_ctl = 'NO_FILE'
!>        Structure to define surface
        type(psf_define_ctl) :: psf_def_c
!
!>        Structure of zero line switch
        type(read_character_item) :: zeroline_switch_ctl
!>        Structure of isoline color mode
        type(read_character_item) :: isoline_color_mode
!>        Structure of number of isoline
        type(read_integer_item) :: isoline_number_ctl
!>        Structure of range of isoline
        type(read_real2_item) :: isoline_range_ctl
!>        Structure to isoline width
        type(read_real_item) :: isoline_width_ctl
!>        Structure to grid width
        type(read_real_item) :: grid_width_ctl
!
!>        Structure of tangent cylinder line switch
        type(read_character_item) :: tan_cyl_switch_ctl
!>        Structure to define outer bounday radius for tangent cylinder
        type(read_real_item) :: tangent_cylinder_inner_ctl
!>        Structure to define inner bounday radius for tangent cylinder
        type(read_real_item) :: tangent_cylinder_outer_ctl
!
        integer(kind = kint) :: i_map_sect_ctl = 0
      end type map_section_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dup_map_section_ctl(org_map_sect_c, new_map_sect_c)
!
      type(map_section_ctl), intent(in) :: org_map_sect_c
      type(map_section_ctl), intent(inout) :: new_map_sect_c
!
!
      new_map_sect_c%block_name =     org_map_sect_c%block_name
      new_map_sect_c%i_map_sect_ctl = org_map_sect_c%i_map_sect_ctl
      new_map_sect_c%fname_sect_ctl = org_map_sect_c%fname_sect_ctl
      call dup_control_4_psf_def                                        &
     &   (org_map_sect_c%psf_def_c, new_map_sect_c%psf_def_c)
!
      call copy_chara_ctl(org_map_sect_c%zeroline_switch_ctl,           &
     &                   new_map_sect_c%zeroline_switch_ctl)
      call copy_chara_ctl(org_map_sect_c%isoline_color_mode,            &
     &                   new_map_sect_c%isoline_color_mode)
      call copy_integer_ctl(org_map_sect_c%isoline_number_ctl,          &
     &                      new_map_sect_c%isoline_number_ctl)
      call copy_real2_ctl(org_map_sect_c%isoline_range_ctl,             &
     &                    new_map_sect_c%isoline_range_ctl)
      call copy_real_ctl(org_map_sect_c%isoline_width_ctl,              &
     &                   new_map_sect_c%isoline_width_ctl)
      call copy_real_ctl(org_map_sect_c%grid_width_ctl,                 &
     &                   new_map_sect_c%grid_width_ctl)
!
      call copy_chara_ctl(org_map_sect_c%tan_cyl_switch_ctl,            &
     &                   new_map_sect_c%tan_cyl_switch_ctl)
      call copy_real_ctl(org_map_sect_c%tangent_cylinder_inner_ctl,     &
     &                   new_map_sect_c%tangent_cylinder_inner_ctl)
      call copy_real_ctl(org_map_sect_c%tangent_cylinder_outer_ctl,     &
     &                   new_map_sect_c%tangent_cylinder_outer_ctl)
!
      end subroutine dup_map_section_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_map_section_ctl(map_sect_ctl)
!
      type(map_section_ctl), intent(inout) :: map_sect_ctl
!
!
      call dealloc_cont_dat_4_psf_def(map_sect_ctl%psf_def_c)
!
      map_sect_ctl%zeroline_switch_ctl%iflag = 0
      map_sect_ctl%isoline_color_mode%iflag = 0
      map_sect_ctl%isoline_number_ctl%iflag = 0
      map_sect_ctl%isoline_range_ctl%iflag = 0
      map_sect_ctl%isoline_width_ctl%iflag = 0
      map_sect_ctl%grid_width_ctl%iflag = 0
!
      map_sect_ctl%tan_cyl_switch_ctl%iflag =          0
      map_sect_ctl%tangent_cylinder_inner_ctl%iflag =  0
      map_sect_ctl%tangent_cylinder_outer_ctl%iflag =  0
!
      map_sect_ctl%i_map_sect_ctl =    0
!
      end subroutine dealloc_map_section_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_map_section
