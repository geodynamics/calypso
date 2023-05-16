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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  array section_ctl
!!    file section_ctl     ctl_psf_eq
!!    begin section_ctl
!!      ...
!!    end section_ctl
!!
!!    opacity_ctl       0.9
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
      use t_control_array_character
      use t_control_array_chara2real
      use skip_comment_f
!
      implicit  none
!
      type pvr_section_ctl
        character(len = kchara) :: fname_sect_ctl
        type(psf_define_ctl) :: psf_def_c
        type(read_real_item) :: opacity_ctl
!
        integer(kind = kint) :: i_pvr_sect_ctl = 0
      end type pvr_section_ctl
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
      new_pvr_sect_c%i_pvr_sect_ctl = org_pvr_sect_c%i_pvr_sect_ctl
      new_pvr_sect_c%fname_sect_ctl = org_pvr_sect_c%fname_sect_ctl
      call dup_control_4_psf_def                                        &
     &   (org_pvr_sect_c%psf_def_c, new_pvr_sect_c%psf_def_c)
!
      call copy_real_ctl(org_pvr_sect_c%opacity_ctl,                    &
     &                   new_pvr_sect_c%opacity_ctl)
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
      pvr_sect_ctl%i_pvr_sect_ctl =    0
!
      end subroutine dealloc_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_pvr_section
