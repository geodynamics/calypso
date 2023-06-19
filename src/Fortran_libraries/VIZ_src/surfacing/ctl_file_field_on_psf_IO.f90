!>@file   ctl_file_field_on_psf_IO.f90
!!@brief  module ctl_file_field_on_psf_IO
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine sel_read_ctl_field_on_psf_file(id_control, hd_block, &
!!     &          file_name, fld_on_psf_c, c_buf)
!!      subroutine read_ctl_field_on_psf_file(id_control, file_name,    &
!!     &                                      hd_block, fld_on_psf_c)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len = kchara), intent(inout) :: file_name
!!        type(section_controls), intent(inout) :: psf_ctls
!!        type(field_on_psf_ctl), intent(inout) :: fld_on_psf_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine sel_write_ctl_field_on_psf_file(id_control, hd_block,&
!!     &          file_name, fld_on_psf_c, level)
!!      subroutine write_ctl_field_on_psf_file(id_control, file_name,   &
!!     &                                    hd_block, fld_on_psf_c)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: file_name
!!        character(len=kchara), intent(in) :: hd_block
!!        type(section_controls), intent(in) :: psf_ctls
!!        type(field_on_psf_ctl), intent(in) :: fld_on_psf_c
!!        integer(kind = kint), intent(inout) :: level
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    file   field_on_isosurf   'ctl_iso_temp_value'
!!    begin field_on_isosurf
!!      result_type      constant
!!      result_value     0.7
!!      array output_field   2
!!        output_field    velocity         vector   end
!!        output_field    magnetic_field   radial   end
!!      end array output_field
!!    end field_on_isosurf
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_file_field_on_psf_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_fld_on_psf
      use t_control_data_sections
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_ctl_field_on_psf_file(id_control, hd_block,   &
     &          file_name, fld_on_psf_c, c_buf)
!
      use t_read_control_elements
!
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(inout) :: file_name
      type(field_on_psf_ctl), intent(inout) :: fld_on_psf_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_block)) then
        file_name = third_word(c_buf)
!
        call write_one_ctl_file_message                                 &
     &     (hd_block, c_buf%level, file_name)
        call read_ctl_field_on_psf_file((id_control+2), file_name,      &
     &                               hd_block, fld_on_psf_c, c_buf)
      else if(check_begin_flag(c_buf, hd_block)) then
        file_name = 'NO_FILE'
!
        call write_included_message(hd_block, c_buf%level)
        call read_fld_on_psf_control(id_control, hd_block,              &
     &                               fld_on_psf_c, c_buf)
      end if
!
      end subroutine sel_read_ctl_field_on_psf_file
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_field_on_psf_file(id_control, file_name,      &
     &          hd_block, fld_on_psf_c, c_buf)
!
      use t_read_control_elements
!
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(field_on_psf_ctl), intent(inout) :: fld_on_psf_c
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      write(*,'(a)') trim(file_name)
      open(id_control, file=file_name, status='old')
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_fld_on_psf_control(id_control, hd_block,              &
     &      fld_on_psf_c, c_buf)
        if(fld_on_psf_c%i_iso_result .gt. 0) exit
      end do
      close(id_control)
!
      c_buf%level = c_buf%level - 1
!
      end subroutine read_ctl_field_on_psf_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_write_ctl_field_on_psf_file(id_control, hd_block,  &
     &          file_name, fld_on_psf_c, level)
!
      use t_read_control_elements
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(field_on_psf_ctl), intent(in) :: fld_on_psf_c
      integer(kind = kint), intent(inout) :: level
!
!
      if(cmp_no_case(file_name, 'NO_FILE')) then
        call write_fld_on_psf_control(id_control, hd_block,             &
     &                              fld_on_psf_c, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(4a)') '!  ', trim(hd_block),                          &
     &              ' should be written to file ... ', trim(file_name)
        call write_fld_on_psf_control(id_control, hd_block,             &
     &                              fld_on_psf_c, level)
      else
        write(*,'(3a)', ADVANCE='NO') trim(hd_block),                   &
     &              ' is written to file ... ', trim(file_name)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, file_name)
        call write_ctl_field_on_psf_file((id_control+2), file_name,     &
     &                                hd_block, fld_on_psf_c)
      end if
!
      end subroutine sel_write_ctl_field_on_psf_file
!
!   --------------------------------------------------------------------
!
      subroutine write_ctl_field_on_psf_file(id_control, file_name,     &
     &                                    hd_block, fld_on_psf_c)
!
      use t_read_control_elements
      use t_control_data_4_fld_on_psf
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(field_on_psf_ctl), intent(in) :: fld_on_psf_c
!
      integer(kind = kint) :: level
!
!
      level = 0
      open(id_control, file=file_name)
      call write_fld_on_psf_control(id_control, hd_block,               &
     &                            fld_on_psf_c, level)
      close(id_control)
!
      end subroutine write_ctl_field_on_psf_file
!
!  ---------------------------------------------------------------------
!
      end module ctl_file_field_on_psf_IO
