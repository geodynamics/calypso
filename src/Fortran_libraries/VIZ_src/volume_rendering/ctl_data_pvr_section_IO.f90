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
!!      subroutine sel_read_ctl_pvr_section_def(id_control, hd_block,   &
!!     &          fname_sect_ctl, psf_def_c, c_buf)
!!      subroutine read_ctl_file_pvr_section_def                        &
!!     &         (id_control, fname_sect_ctl, psf_def_c)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len = kchara), intent(inout) :: fname_sect_ctl
!!        type(psf_define_ctl), intent(inout) :: psf_def_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine sel_write_ctl_pvr_section_def(id_control, hd_block,  &
!!     &          fname_sect_ctl, psf_def_c, level)
!!      subroutine write_ctl_file_pvr_section_def                       &
!!     &         (id_control, fname_sect_ctl, psf_def_c)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len = kchara), intent(in) :: fname_sect_ctl
!!        type(psf_define_ctl), intent(in) :: psf_def_c
!!        integer(kind = kint), intent(inout) :: level
!!
!!      integer(kind = kint) function num_label_pvr_section()
!!      subroutine set_label_pvr_section(names)
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
      module ctl_data_pvr_section_IO
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_data_4_psf_def
      use t_control_array_real
      use t_control_array_character
      use t_control_array_chara2real
      use t_ctl_data_pvr_section
      use skip_comment_f
!
      implicit  none
!
!   Labels
      integer(kind = kint), parameter :: n_label_pvr_section =   2
!
      character(len=kchara), parameter                                  &
     &                  :: hd_surface_define =  'surface_define'
      character(len=kchara), parameter                                  &
     &                  :: hd_pvr_opacity =   'opacity_ctl'
!
      private :: hd_pvr_opacity, hd_surface_define, n_label_pvr_section
!
      private :: sel_read_ctl_pvr_section_def
      private :: read_ctl_file_pvr_section_def
      private :: sel_write_ctl_pvr_section_def
      private :: write_ctl_file_pvr_section_def
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
      use ctl_data_section_def_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_section_ctl), intent(inout) :: pvr_sect_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      pvr_sect_ctl%psf_def_c%i_surface_define = 0
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
      use ctl_data_section_def_IO
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
      level = write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_read_ctl_pvr_section_def(id_control, hd_block,     &
     &          fname_sect_ctl, psf_def_c, c_buf)
!
      use ctl_data_section_def_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(inout) :: fname_sect_ctl
      type(psf_define_ctl), intent(inout) :: psf_def_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_block)) then
        write(*,'(a)', ADVANCE='NO') ' is read from file... '
        fname_sect_ctl = third_word(c_buf)
        call read_ctl_file_pvr_section_def(id_control+2,                &
     &      fname_sect_ctl, hd_block, psf_def_c)
      else if(check_begin_flag(c_buf, hd_block)) then
        write(*,*) ' is included'
        fname_sect_ctl = 'NO_FILE'
        call read_section_def_control(id_control, hd_block,             &
     &                                psf_def_c, c_buf)
      end if
!
      end subroutine sel_read_ctl_pvr_section_def
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_file_pvr_section_def                          &
     &         (id_control, fname_sect_ctl, hd_block, psf_def_c)
!
      use ctl_data_section_def_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: fname_sect_ctl
      character(len=kchara), intent(in) :: hd_block
      type(psf_define_ctl), intent(inout) :: psf_def_c
!
      type(buffer_for_control) :: c_buf1
!
!
      write(*,*) trim(fname_sect_ctl), ' for surface definition'
      open(id_control, file = fname_sect_ctl, status='old')
!
      do
        call load_one_line_from_control(id_control, c_buf1)
        if(check_end_flag(c_buf1, hd_block)) exit
        call read_section_def_control(id_control, hd_block,             &
     &                                psf_def_c, c_buf1)
      end do
!
      close(id_control)
!
      end subroutine read_ctl_file_pvr_section_def
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_write_ctl_pvr_section_def(id_control, hd_block,    &
     &          fname_sect_ctl, psf_def_c, level)
!
      use ctl_data_section_def_IO
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(in) :: fname_sect_ctl
      type(psf_define_ctl), intent(in) :: psf_def_c
      integer(kind = kint), intent(inout) :: level
!
!
      if(cmp_no_case(fname_sect_ctl,'NO_FILE')) then
        call write_section_def_control(id_control, hd_block,            &
     &                                 psf_def_c, level)
      else
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, fname_sect_ctl)
        call write_ctl_file_pvr_section_def                             &
     &     (id_control+2, fname_sect_ctl, hd_block, psf_def_c)
      end if
!
      end subroutine sel_write_ctl_pvr_section_def
!
!  ---------------------------------------------------------------------
!
      subroutine write_ctl_file_pvr_section_def                         &
     &         (id_control, fname_sect_ctl, hd_block, psf_def_c)
!
      use ctl_data_section_def_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: fname_sect_ctl
      character(len=kchara), intent(in) :: hd_block
      type(psf_define_ctl), intent(in) :: psf_def_c
!
      integer(kind = kint) :: level
!
!
      write(*,*) trim(fname_sect_ctl), ' for surface definition'
      level = 0
      open(id_control, file = fname_sect_ctl)
      call write_section_def_control(id_control, hd_block,              &
     &                               psf_def_c, level)
      close(id_control)
!
      end subroutine write_ctl_file_pvr_section_def
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
      call set_control_labels(hd_surface_define, names( 1))
      call set_control_labels(hd_pvr_opacity,    names( 2))
!
      end subroutine set_label_pvr_section
!
! ----------------------------------------------------------------------
!
      end module ctl_data_pvr_section_IO
