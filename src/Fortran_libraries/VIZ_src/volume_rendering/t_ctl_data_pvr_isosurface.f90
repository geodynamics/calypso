!>@file   t_ctl_data_pvr_isosurface.f90
!!@brief  module t_ctl_data_pvr_isosurface
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief isosurface control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine init_pvr_isosurface_ctl_label(hd_block, pvr_iso_ctl)
!!      subroutine read_pvr_isosurface_ctl                              &
!!     &         (id_control, hd_block, pvr_iso_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_pvr_isosurface_ctl                             &
!!     &         (id_control, hd_block, pvr_iso_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_isosurf_ctl), intent(in) :: pvr_iso_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dup_pvr_isosurface_ctl(org_pvr_iso_c, new_pvr_iso_c)
!!        type(pvr_isosurf_ctl), intent(in) :: org_pvr_iso_c
!!        type(pvr_isosurf_ctl), intent(inout) :: new_pvr_iso_c
!!      subroutine reset_pvr_isosurface_ctl(pvr_iso_ctl)
!!        type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin isosurface_ctl
!!      isosurf_value       0.3
!!      opacity_ctl         0.9
!!      surface_direction   normal
!!    end isosurface_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_pvr_isosurface
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_isosurf_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'isosurface_ctl'
!
        type(read_character_item) :: isosurf_type_ctl
        type(read_real_item) :: iso_value_ctl
        type(read_real_item) :: opacity_ctl
        integer(kind = kint) :: i_pvr_isosurf_ctl = 0
      end type pvr_isosurf_ctl
!
!     3rd level for isosurface
!
      character(len=kchara) :: hd_isosurf_value = 'isosurf_value'
      character(len=kchara) :: hd_pvr_opacity =   'opacity_ctl'
      character(len=kchara) :: hd_iso_direction = 'surface_direction'
!
      private :: hd_isosurf_value, hd_pvr_opacity, hd_iso_direction
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_isosurface_ctl                                &
     &         (id_control, hd_block, pvr_iso_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_iso_direction,               &
     &      pvr_iso_ctl%isosurf_type_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_isosurf_value, pvr_iso_ctl%iso_value_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_pvr_opacity, pvr_iso_ctl%opacity_ctl)
      end do
      pvr_iso_ctl%i_pvr_isosurf_ctl = 1
!
      end subroutine read_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_pvr_isosurface_ctl                               &
     &         (id_control, hd_block, pvr_iso_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_isosurf_ctl), intent(in) :: pvr_iso_ctl
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(pvr_iso_ctl%i_pvr_isosurf_ctl .le. 0) return
!
      maxlen = len_trim(hd_iso_direction)
      maxlen = max(maxlen, len_trim(hd_isosurf_value))
      maxlen = max(maxlen, len_trim(hd_pvr_opacity))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    pvr_iso_ctl%isosurf_type_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    pvr_iso_ctl%iso_value_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    pvr_iso_ctl%opacity_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine init_pvr_isosurface_ctl_label(hd_block, pvr_iso_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!
!
      pvr_iso_ctl%block_name = hd_block
        call init_chara_ctl_item_label(hd_iso_direction,                &
     &      pvr_iso_ctl%isosurf_type_ctl)
        call init_real_ctl_item_label                                   &
     &     (hd_isosurf_value, pvr_iso_ctl%iso_value_ctl)
        call init_real_ctl_item_label                                   &
     &     (hd_pvr_opacity, pvr_iso_ctl%opacity_ctl)
!
      end subroutine init_pvr_isosurface_ctl_label
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dup_pvr_isosurface_ctl(org_pvr_iso_c, new_pvr_iso_c)
!
      type(pvr_isosurf_ctl), intent(in) :: org_pvr_iso_c
      type(pvr_isosurf_ctl), intent(inout) :: new_pvr_iso_c
!
!
      call copy_chara_ctl(org_pvr_iso_c%isosurf_type_ctl,               &
     &                    new_pvr_iso_c%isosurf_type_ctl)
      call copy_real_ctl(org_pvr_iso_c%iso_value_ctl,                   &
     &                   new_pvr_iso_c%iso_value_ctl)
      call copy_real_ctl(org_pvr_iso_c%opacity_ctl,                     &
     &                   new_pvr_iso_c%opacity_ctl)
!
      end subroutine dup_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_isosurface_ctl(pvr_iso_ctl)
!
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!
!
      pvr_iso_ctl%isosurf_type_ctl%iflag =  0
      pvr_iso_ctl%iso_value_ctl%iflag =     0
      pvr_iso_ctl%opacity_ctl%iflag =       0
!
      pvr_iso_ctl%i_pvr_isosurf_ctl =       0
!
      end subroutine reset_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_pvr_isosurface
