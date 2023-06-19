!>@file   ctl_file_fieldlines_IO.f90
!!@brief  module ctl_file_fieldlines_IO
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine read_files_4_fline_ctl                               &
!!     &         (id_control, hd_block, fline_ctls, c_buf)
!!      subroutine sel_read_fline_control(id_control, hd_block,         &
!!     &          file_name, fline_ctl_struct, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len = kchara), intent(inout) :: file_name
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fline_ctl), intent(inout)  :: fline_ctl_struct
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine write_files_4_fline_ctl(id_control, hd_block,        &
!!     &                                   fline_ctls, level)
!!      subroutine sel_write_fline_control(id_control, hd_block,        &
!!     &          file_name, fline_ctl_struct, level)
!!      subroutine write_fline_control_file(id_control, file_name,      &
!!     &                                    hd_block, fline_ctl_struct)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: file_name
!!        character(len=kchara), intent(in) :: hd_block
!!        type(fieldline_controls), intent(in) :: fline_ctls
!!        type(fline_ctl), intent(in)  :: fline_ctl_struct
!!        integer(kind = kint), intent(inout) :: level
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  fieldline
!!      file  fieldline  'ctl_fline_magne'
!!    end array fieldline
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_file_fieldlines_IO
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use t_ctl_data_field_line
      use t_control_data_flines
!
      implicit  none
!
      private :: read_fline_control_file
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_files_4_fline_ctl                                 &
     &         (id_control, hd_block, fline_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
      use ctl_data_field_line_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(fieldline_controls), intent(inout) :: fline_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(fline_ctls%fline_ctl_struct)) return
      fline_ctls%num_fline_ctl = 0
      call alloc_fline_ctl_struct(fline_ctls)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_file_flag(c_buf, hd_block)                             &
     &        .or. check_begin_flag(c_buf, hd_block)) then
          call append_new_fline_control(fline_ctls)
!
          call write_multi_ctl_file_message                             &
     &       (hd_block, fline_ctls%num_fline_ctl, c_buf%level)
          call sel_read_fline_control(id_control, hd_block,             &
     &        fline_ctls%fname_fline_ctl(fline_ctls%num_fline_ctl),     &
     &        fline_ctls%fline_ctl_struct(fline_ctls%num_fline_ctl),    &
     &        c_buf)
        end if
      end do
!
      end subroutine read_files_4_fline_ctl
!
!   --------------------------------------------------------------------
!
      subroutine sel_read_fline_control(id_control, hd_block,           &
     &          file_name, fline_ctl_struct, c_buf)
!
      use ctl_data_field_line_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(inout) :: file_name
      type(fline_ctl), intent(inout)  :: fline_ctl_struct
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_block)) then
        file_name = third_word(c_buf)
!
        write(*,'(3a,i4,a)', ADVANCE='NO') 'is read from '
        call read_fline_control_file((id_control+2), file_name,         &
     &                               hd_block, fline_ctl_struct, c_buf)
      else if(check_begin_flag(c_buf, hd_block)) then
        file_name = 'NO_FILE'
!
        write(*,'(a)') ' is included'
        call s_read_field_line_ctl(id_control, hd_block,                &
     &                             fline_ctl_struct, c_buf)
      end if
!
      end subroutine sel_read_fline_control
!
!  ---------------------------------------------------------------------
!
      subroutine read_fline_control_file(id_control, file_name,         &
     &          hd_block, fline_ctl_struct, c_buf)
!
      use ctl_data_field_line_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(fline_ctl), intent(inout)  :: fline_ctl_struct
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      write(*,*) 'Control file: ', trim(file_name)
      call reset_fline_control_flags(fline_ctl_struct)
      open(id_control, file=file_name, status='old')
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call s_read_field_line_ctl(id_control, hd_block,                &
     &      fline_ctl_struct, c_buf)
        if(fline_ctl_struct%i_vr_fline_ctl .gt. 0) exit
      end do
      close(id_control)
!
      c_buf%level = c_buf%level - 1
!
      end subroutine read_fline_control_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_files_4_fline_ctl(id_control, hd_block,          &
     &                                   fline_ctls, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(fieldline_controls), intent(in) :: fline_ctls
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
      if(fline_ctls%num_fline_ctl .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level, hd_block)
      do i = 1, fline_ctls%num_fline_ctl
        write(*,'(3a,i4,a)', ADVANCE='NO') '!  ', trim(hd_block),       &
     &        ' No. ', fline_ctls%num_fline_ctl
        call sel_write_fline_control                                    &
     &     (id_control, hd_block, fline_ctls%fname_fline_ctl(i),        &
     &      fline_ctls%fline_ctl_struct(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_files_4_fline_ctl
!
!   --------------------------------------------------------------------
!
      subroutine sel_write_fline_control(id_control, hd_block,          &
     &          file_name, fline_ctl_struct, level)
!
      use ctl_data_field_line_IO
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(fline_ctl), intent(in)  :: fline_ctl_struct
      integer(kind = kint), intent(inout) :: level
!
!
      if(cmp_no_case(file_name, 'NO_FILE')) then
        write(*,'(a)') ' is included.'
        call write_field_line_ctl(id_control, hd_block,                 &
     &                            fline_ctl_struct, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(2a)') 'shuld be written to ', trim(file_name)
        call write_field_line_ctl(id_control, hd_block,                 &
     &                            fline_ctl_struct, level)
      else
        write(*,'(2a)') 'is written to ', trim(file_name)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, file_name)
        call write_fline_control_file((id_control+2), file_name,        &
     &                                 hd_block, fline_ctl_struct)
      end if
!
      end subroutine sel_write_fline_control
!
!  ---------------------------------------------------------------------
!
      subroutine write_fline_control_file(id_control, file_name,        &
     &                                    hd_block, fline_ctl_struct)
!
      use ctl_data_field_line_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(fline_ctl), intent(in)  :: fline_ctl_struct
!
      integer(kind = kint) :: level
!
!
      level = 0
      open(id_control, file=file_name)
      call write_field_line_ctl(id_control, hd_block,                   &
     &                          fline_ctl_struct, level)
      close(id_control)
!
      end subroutine write_fline_control_file
!
!  ---------------------------------------------------------------------
!
      end module ctl_file_fieldlines_IO
