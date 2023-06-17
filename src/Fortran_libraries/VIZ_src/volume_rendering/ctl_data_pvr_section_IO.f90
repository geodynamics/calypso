!>@file   ctl_data_pvr_section_IO.f90
!!@brief  module ctl_data_pvr_section_IO
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine read_pvr_section_ctl                                 &
!!     &         (id_control, hd_block, pvr_sect_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_section_ctl), intent(inout) :: pvr_sect_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_pvr_section_ctl                                &
!!     &         (id_control, hd_block, pvr_sect_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_section_ctl), intent(inout) :: pvr_sect_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      integer(kind = kint) function num_label_pvr_section()
!!      subroutine set_label_pvr_section(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin section_ctl
!!    file surface_define     ctl_psf_eq
!!    begin surface_define
!!      ...
!!    end surface_define
!!
!!    opacity_ctl           0.9
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
      module ctl_data_pvr_section_IO
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
      use t_ctl_data_pvr_section
      use skip_comment_f
!
      implicit  none
!
!   Labels
      integer(kind = kint), parameter, private                          &
     &                   :: n_label_pvr_section =  11
!
      character(len=kchara), parameter, private                         &
     &                  :: hd_surface_define =  'surface_define'
      character(len=kchara), parameter, private                         &
     &                  :: hd_pvr_opacity =   'opacity_ctl'
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
      subroutine read_pvr_section_ctl                                   &
     &         (id_control, hd_block, pvr_sect_ctl, c_buf)
!
      use ctl_file_section_def_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_section_ctl), intent(inout) :: pvr_sect_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pvr_sect_ctl%i_pvr_sect_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call sel_read_ctl_pvr_section_def                               &
     &     (id_control, hd_surface_define, pvr_sect_ctl%fname_sect_ctl, &
     &      pvr_sect_ctl%psf_def_c, c_buf)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_pvr_opacity, pvr_sect_ctl%opacity_ctl)
        call read_chara_ctl_type(c_buf, hd_pvr_sec_zeroline,            &
     &      pvr_sect_ctl%zeroline_switch_ctl)
        call read_chara_ctl_type(c_buf, hd_pvr_isoline_color,           &
     &      pvr_sect_ctl%isoline_color_mode)
        call read_integer_ctl_type(c_buf, hd_isoline_number,            &
     &      pvr_sect_ctl%isoline_number_ctl)
        call read_real2_ctl_type(c_buf, hd_isoline_range,               &
     &      pvr_sect_ctl%isoline_range_ctl)
        call read_real_ctl_type(c_buf, hd_isoline_width,                &
     &      pvr_sect_ctl%isoline_width_ctl)
        call read_real_ctl_type(c_buf, hd_grid_width,                   &
     &      pvr_sect_ctl%grid_width_ctl)
!
        call read_chara_ctl_type(c_buf, hd_tangent_cylinder,            &
     &      pvr_sect_ctl%tan_cyl_switch_ctl)
        call read_real_ctl_type(c_buf, hd_tcyl_inner,                   &
     &      pvr_sect_ctl%tangent_cylinder_inner_ctl)
        call read_real_ctl_type(c_buf, hd_tcyl_outer,                   &
     &      pvr_sect_ctl%tangent_cylinder_outer_ctl)
      end do
      pvr_sect_ctl%i_pvr_sect_ctl = 1
!
      end subroutine read_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_pvr_section_ctl                                  &
     &         (id_control, hd_block, pvr_sect_ctl, level)
!
      use ctl_file_section_def_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_section_ctl), intent(in) :: pvr_sect_ctl
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(pvr_sect_ctl%i_pvr_sect_ctl .le. 0) return
      maxlen = len_trim(hd_pvr_opacity)
      maxlen = max(maxlen,len_trim(hd_pvr_sec_zeroline))
      maxlen = max(maxlen,len_trim(hd_pvr_isoline_color))
      maxlen = max(maxlen,len_trim(hd_isoline_number))
      maxlen = max(maxlen,len_trim(hd_isoline_range))
      maxlen = max(maxlen,len_trim(hd_isoline_width))
      maxlen = max(maxlen,len_trim(hd_grid_width))
      maxlen = max(maxlen,len_trim(hd_tangent_cylinder))
      maxlen = max(maxlen,len_trim(hd_tcyl_inner))
      maxlen = max(maxlen,len_trim(hd_tcyl_outer))
!
      write(id_control,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call sel_write_ctl_pvr_section_def(id_control, hd_surface_define, &
     &    pvr_sect_ctl%fname_sect_ctl, pvr_sect_ctl%psf_def_c, level)
!
      write(id_control,'(a1)') '!'
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_pvr_opacity, pvr_sect_ctl%opacity_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_pvr_sec_zeroline, pvr_sect_ctl%zeroline_switch_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_pvr_isoline_color, pvr_sect_ctl%isoline_color_mode)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_isoline_number, pvr_sect_ctl%isoline_number_ctl)
      call write_real2_ctl_type(id_control, level, maxlen,              &
     &    hd_isoline_range, pvr_sect_ctl%isoline_range_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_isoline_width, pvr_sect_ctl%isoline_width_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_grid_width, pvr_sect_ctl%grid_width_ctl)
!
      write(id_control,'(a1)') '!'
      call write_chara_ctl_type                                         &
     &   (id_control, level, maxlen, hd_tangent_cylinder,               &
     &    pvr_sect_ctl%tan_cyl_switch_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_tcyl_inner, pvr_sect_ctl%tangent_cylinder_inner_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_tcyl_outer, pvr_sect_ctl%tangent_cylinder_outer_ctl)
      level = write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_section()
      num_label_pvr_section = n_label_pvr_section
      return
      end function num_label_pvr_section
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_section(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_section)
!
!
      call set_control_labels(hd_surface_define,   names( 1))
      call set_control_labels(hd_pvr_opacity,      names( 2))
!
      call set_control_labels(hd_pvr_sec_zeroline,  names( 3))
      call set_control_labels(hd_pvr_isoline_color, names( 4))
      call set_control_labels(hd_isoline_number,    names( 5))
      call set_control_labels(hd_isoline_range,    names( 6))
      call set_control_labels(hd_isoline_width,    names( 7))
      call set_control_labels(hd_grid_width,       names( 8))
!
      call set_control_labels(hd_tangent_cylinder, names( 9))
      call set_control_labels(hd_tcyl_inner,       names(10))
      call set_control_labels(hd_tcyl_outer,       names(11))
!
      end subroutine set_label_pvr_section
!
! ----------------------------------------------------------------------
!
      end module ctl_data_pvr_section_IO
