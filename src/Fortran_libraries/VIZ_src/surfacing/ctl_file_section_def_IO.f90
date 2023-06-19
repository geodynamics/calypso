!>@file   ctl_file_section_def_IO.f90
!!@brief  module ctl_file_section_def_IO
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    file section_ctl     ctl_psf_eq
!!    begin section_ctl
!!      ...
!!    end section_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_file_section_def_IO
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
      use skip_comment_f
!
      implicit  none
!
      private :: read_ctl_file_pvr_section_def
      private :: write_ctl_file_pvr_section_def
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
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
        fname_sect_ctl = third_word(c_buf)
!
        write(*,'(2a)') ' is read from ... ', trim(fname_sect_ctl)
        call read_ctl_file_pvr_section_def(id_control+2,                &
     &      fname_sect_ctl, hd_block, psf_def_c)
        if(psf_def_c%i_surface_define .ne. 1)                           &
     &                         c_buf%iend = psf_def_c%i_surface_define
      else if(check_begin_flag(c_buf, hd_block)) then
        fname_sect_ctl = 'NO_FILE'
!
        write(*,'(a)') ' is included.'
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
      c_buf1%level = 0
      open(id_control, file = fname_sect_ctl, status='old')
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf1)
        if(c_buf1%iend .gt. 0) exit
        if(check_end_flag(c_buf1, hd_block)) exit
!
        call read_section_def_control(id_control, hd_block,             &
     &                                psf_def_c, c_buf1)
        if(psf_def_c%i_surface_define .gt. 0) exit
      end do
!
      close(id_control)
      if(c_buf1%iend .gt. 0) psf_def_c%i_surface_define = c_buf1%iend
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
      else if(id_control .eq. id_monitor) then
        write(*,'(4a)') '!  ', trim(hd_block),                          &
     &         ' should be written to file ... ', trim(fname_sect_ctl)
        call write_section_def_control(id_control, hd_block,            &
     &                                 psf_def_c, level)
      else
        write(*,'(3a)', ADVANCE='NO')  trim(hd_block),                  &
     &         ' is written to file ... ', trim(fname_sect_ctl)
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
      write(*,*) trim(fname_sect_ctl)
      level = 0
      open(id_control, file = fname_sect_ctl)
      call write_section_def_control(id_control, hd_block,              &
     &                               psf_def_c, level)
      close(id_control)
!
      end subroutine write_ctl_file_pvr_section_def
!
!  ---------------------------------------------------------------------
!
      end module ctl_file_section_def_IO
