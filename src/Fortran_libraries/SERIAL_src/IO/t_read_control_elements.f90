!>@file   t_read_control_elements.f90
!!@brief  module t_read_control_elements
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2005
!
!>@brief  Subroutines to read control data
!!
!!@verbatim
!!      subroutine load_one_line_from_control(id_control, c_buf)
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      character(len = kchara) function first_word(c_buf)
!!      character(len = kchara) function second_word(c_buf)
!!      character(len = kchara) function third_word(c_buf)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!
!!      logical function check_begin_flag(c_buf, label)
!!      logical function check_file_flag(c_buf, label)
!!      logical function check_end_flag(c_buf, label)
!!      logical function check_array_flag(c_buf, label)
!!      logical function check_end_array_flag(c_buf, label)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!
!!      subroutine monitor_read_control_label(c_buf)
!!      subroutine monitor_read_control_buffer(c_buf)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!@endverbatim
!!
      module t_read_control_elements
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>   Label to start a control block
       character(len=kchara), parameter  :: hd_begin = 'Begin'
!>   Label to end a control block
       character(len=kchara), parameter  :: hd_end = 'End'
!>   Label for an array control block
       character(len=kchara), parameter  :: hd_array = 'Array'
!>   Label for an array control block
       character(len=kchara), parameter  :: hd_file = 'File'
!
      type buffer_for_control
!>     character for read label
         character(len = kchara) :: header_chara
!>     temporal character for reading line
         character(len = 255) :: ctl_buffer
      end type buffer_for_control
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine load_one_line_from_control(id_control, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      call skip_comment(c_buf%ctl_buffer, id_control)
      c_buf%header_chara = first_word(c_buf)
!
      end subroutine load_one_line_from_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      character(len = kchara) function first_word(c_buf)
!
      type(buffer_for_control), intent(in)  :: c_buf
!
!
      read(c_buf%ctl_buffer,*,err=99) first_word
      return
!
  99  continue
      first_word = ''
!
      end function first_word
!
!   --------------------------------------------------------------------
!
      character(len = kchara) function second_word(c_buf)
!
      type(buffer_for_control), intent(in)  :: c_buf
!
      character(len=kchara)  :: tmpchara
!
      read(c_buf%ctl_buffer,*,err=99) tmpchara, second_word
      return
!
  99  continue
      second_word = ''
!
      end function second_word
!
!   --------------------------------------------------------------------
!
      character(len = kchara) function third_word(c_buf)
!
      type(buffer_for_control), intent(in)  :: c_buf
!
      character(len=kchara)  :: tmpchara
!
      read(c_buf%ctl_buffer,*,err=99) tmpchara, tmpchara, third_word
      return
!
  99  continue
      third_word = ''
!
      end function third_word
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      logical function check_begin_flag(c_buf, label)
!
      use skip_comment_f
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
!
!
      check_begin_flag = .FALSE.
      if(cmp_no_case(first_word(c_buf), hd_begin)) then
        check_begin_flag = cmp_no_case(second_word(c_buf), label)
      end if
!
      end function check_begin_flag
!
!   --------------------------------------------------------------------
!
      logical function check_file_flag(c_buf, label)
!
      use skip_comment_f
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
!
!
      check_file_flag = .FALSE.
      if(cmp_no_case(first_word(c_buf), hd_file)) then
        check_file_flag = cmp_no_case(second_word(c_buf), label)
      end if
!
      end function check_file_flag
!
!   --------------------------------------------------------------------
!
      logical function check_end_flag(c_buf, label)
!
      use skip_comment_f
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
!
!
      check_end_flag = .FALSE.
      if(cmp_no_case(first_word(c_buf), hd_end)) then
        check_end_flag = cmp_no_case(second_word(c_buf), label)
      end if
!
      end function check_end_flag
!
!   --------------------------------------------------------------------
!
      logical function check_array_flag(c_buf, label)
!
      use skip_comment_f
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
!
      character(len=kchara)  :: tmpchara
      integer(kind = kint) :: ntmp = -1
!
!
      check_array_flag = .FALSE.
      if(cmp_no_case(first_word(c_buf), hd_array) .eqv. .FALSE.) return
      if(cmp_no_case(second_word(c_buf), label) .eqv. .FALSE.) return
      read(c_buf%ctl_buffer,*,err=99,end=99) tmpchara, tmpchara, ntmp
      if(ntmp .eq. 0) return
!
  99  continue
      check_array_flag = cmp_no_case(second_word(c_buf), label)
!
      end function check_array_flag
!
!   --------------------------------------------------------------------
!
      logical function check_end_array_flag(c_buf, label)
!
      use skip_comment_f
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
!
!
      check_end_array_flag = .FALSE.
      if(cmp_no_case(first_word(c_buf), hd_end)) then
        if(cmp_no_case(second_word(c_buf), hd_array)) then
          check_end_array_flag = cmp_no_case(third_word(c_buf), label)
        end if
      end if
!
      end function check_end_array_flag
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine monitor_read_control_label(c_buf)
!
      type(buffer_for_control), intent(in)  :: c_buf
!
!
      write(*,*) 'read header: ', trim(c_buf%header_chara)
!
      end subroutine monitor_read_control_label
!
!   --------------------------------------------------------------------
!
      subroutine monitor_read_control_buffer(c_buf)
!
      type(buffer_for_control), intent(in)  :: c_buf
!
!
      write(*,*) 'Buffer: ', trim(c_buf%ctl_buffer)
!
      end subroutine monitor_read_control_buffer
!
!   --------------------------------------------------------------------
!
      end module t_read_control_elements
