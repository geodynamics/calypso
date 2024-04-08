!>@file   t_ctl_data_pvr_section.f90
!!@brief  module t_ctl_data_pvr_section
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine dup_pvr_section_ctl(org_pvr_sect_c, new_pvr_sect_c)
!!        type(pvr_section_ctl), intent(in) :: org_pvr_sect_c
!!        type(pvr_section_ctl), intent(inout) :: new_pvr_sect_c
!!      subroutine dealloc_pvr_section_ctl(pvr_sect_ctl)
!!        type(pvr_section_ctl), intent(inout) :: pvr_sect_ctl
!!
!!      subroutine init_pvr_section_ctl_label(hd_block, pvr_sect_ctl)
!!      subroutine read_pvr_section_ctl                                 &
!!     &         (id_control, hd_block, icou, pvr_sect_ctl, c_buf)
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  array section_ctl
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
!!  end array section_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_pvr_section
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
      type pvr_section_ctl
!>        Block name
        character(len=kchara) :: block_name = 'surface_define'
!
!>        File name of control file to define surface
        character(len = kchara) :: fname_sect_ctl = 'NO_FILE'
!>        Structure to define surface
        type(psf_define_ctl) :: psf_def_c
!>        Structure to define opacity of surface
        type(read_real_item) :: opacity_ctl
!>        Structure of zero line switch
        type(read_character_item) :: zeroline_switch_ctl
        integer(kind = kint) :: i_pvr_sect_ctl = 0
      end type pvr_section_ctl
!
      character(len=kchara), parameter, private                         &
     &                  :: hd_surface_define =  'surface_define'
      character(len=kchara), parameter, private                         &
     &                  :: hd_pvr_opacity =   'opacity_ctl'
!
      character(len=kchara), parameter, private                         &
     &                  :: hd_pvr_sec_zeroline = 'zeroline_switch_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dup_pvr_section_ctl(org_pvr_sect_c, new_pvr_sect_c)
!
      type(pvr_section_ctl), intent(in) :: org_pvr_sect_c
      type(pvr_section_ctl), intent(inout) :: new_pvr_sect_c
!
!
      new_pvr_sect_c%block_name =     org_pvr_sect_c%block_name
      new_pvr_sect_c%i_pvr_sect_ctl = org_pvr_sect_c%i_pvr_sect_ctl
      new_pvr_sect_c%fname_sect_ctl = org_pvr_sect_c%fname_sect_ctl
      call dup_control_4_psf_def                                        &
     &   (org_pvr_sect_c%psf_def_c, new_pvr_sect_c%psf_def_c)
!
      call copy_real_ctl(org_pvr_sect_c%opacity_ctl,                    &
     &                   new_pvr_sect_c%opacity_ctl)
      call copy_chara_ctl(org_pvr_sect_c%zeroline_switch_ctl,           &
     &                   new_pvr_sect_c%zeroline_switch_ctl)
!
      end subroutine dup_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_section_ctl(pvr_sect_ctl)
!
      type(pvr_section_ctl), intent(inout) :: pvr_sect_ctl
!
!
      call dealloc_cont_dat_4_psf_def(pvr_sect_ctl%psf_def_c)
      pvr_sect_ctl%opacity_ctl%iflag = 0
      pvr_sect_ctl%zeroline_switch_ctl%iflag = 0
!
      pvr_sect_ctl%i_pvr_sect_ctl =    0
!
      end subroutine dealloc_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_section_ctl                                   &
     &         (id_control, hd_block, icou, pvr_sect_ctl, c_buf)
!
      use ctl_file_section_def_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, icou
      character(len=kchara), intent(in) :: hd_block
      type(pvr_section_ctl), intent(inout) :: pvr_sect_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(pvr_sect_ctl%i_pvr_sect_ctl .gt. 0) return
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
     &        hd_surface_define, pvr_sect_ctl%fname_sect_ctl,           &
     &        pvr_sect_ctl%psf_def_c, c_buf)
        end if
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_pvr_opacity, pvr_sect_ctl%opacity_ctl)
        call read_chara_ctl_type(c_buf, hd_pvr_sec_zeroline,            &
     &      pvr_sect_ctl%zeroline_switch_ctl)
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
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call sel_write_ctl_pvr_section_def(id_control, hd_surface_define, &
     &    pvr_sect_ctl%fname_sect_ctl, pvr_sect_ctl%psf_def_c, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    pvr_sect_ctl%opacity_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pvr_sect_ctl%zeroline_switch_ctl)
      level = write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine init_pvr_section_ctl_label(hd_block, pvr_sect_ctl)
!
      use ctl_data_section_def_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(pvr_section_ctl), intent(inout) :: pvr_sect_ctl
!
      pvr_sect_ctl%block_name = hd_block
      call init_psf_def_ctl_stract                                      &
     &   (hd_surface_define, pvr_sect_ctl%psf_def_c)
!
        call init_real_ctl_item_label                                   &
     &     (hd_pvr_opacity, pvr_sect_ctl%opacity_ctl)
        call init_chara_ctl_item_label(hd_pvr_sec_zeroline,             &
     &      pvr_sect_ctl%zeroline_switch_ctl)
!
      end subroutine init_pvr_section_ctl_label
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_pvr_section
