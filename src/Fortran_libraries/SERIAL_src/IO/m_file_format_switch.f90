!>@file   m_file_format_switch.f90
!!@brief  module m_file_format_switch
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  integer flags for Data file's format
!!
!!@verbatim
!!      integer(kind= kint) function choose_file_format(file_fmt)
!!      integer(kind= kint) function choose_para_file_format(file_fmt)
!!        type(read_character_item), intent(in) :: file_fmt
!!      subroutine choose_file_format_array(num, files_fmt, id_files_fmt)
!!        type(ctl_array_chara), intent(in) :: files_fmt
!!
!!        File format name
!!          'ascii':   text (formatted) data
!!          'gzip':    gziopped text (formatted) data
!!          'binary':  binary data
!!
!!          'merged_ascii': text (formatted) data in merged file 
!!          'merged_gzip':  gziopped text (formatted) data in merged file
!!          'merged_binary': binary data in merged file
!!@endverbatim
!!
!!@n @param  file_fmt       Structure for File format control
!!@n @param  file_fmt_ctl   File format name
!!@n @param  i_file_fmt     Check flag if file format is read
!!@n @param  id_file_fmt    File format flag (Output)
!
      module m_file_format_switch
!
      use m_precision
      use m_constants
      use m_file_format_labels
      use t_multi_flag_labels
!
      implicit none
!
!
!>      Integer flag for no file
       integer(kind = kint), parameter :: id_no_file =      -1000
!>      Integer flag for no file
       integer(kind = kint), parameter :: id_missing_zlib = -9000
!
!>      Integer flag for ascii data format
      integer(kind = kint), parameter :: id_ascii_file_fmt =    0
!>      Integer flag for binary data format
      integer(kind = kint), parameter :: id_binary_file_fmt =   1
!>      Integer flag for origianl gzipped binary data format
      integer(kind = kint), parameter :: id_gzip_bin_file_fmt = 2
!>      Integer flag for origianl gzipped ascii data format
      integer(kind = kint), parameter :: id_gzip_txt_file_fmt = 3
!
!>      Integer flag for distributed data
      integer(kind = kint), parameter :: iflag_para =      0
!>      Integer flag for merged data
      integer(kind = kint), parameter :: iflag_single =  100
!
!>      Integer flag for Rayleigh data
      integer(kind = kint), parameter :: id_Rayleigh =   105
!>      Integer flag for Rayleigh Ver.0.9x data
      integer(kind = kint), parameter :: id_Rayleigh99 = 195
!
      private :: set_file_format, set_parallel_file_format
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      integer(kind= kint) function choose_file_format(file_fmt)
!
      use t_control_array_character
!
      type(read_character_item), intent(in) :: file_fmt
!
!
      choose_file_format                                                &
     &   = set_file_format(file_fmt%charavalue, file_fmt%iflag)
!
      end function choose_file_format
!
!------------------------------------------------------------------
!
      integer(kind= kint) function choose_para_file_format(file_fmt)
!
      use t_control_array_character
!
      type(read_character_item), intent(in) :: file_fmt
!
!
      choose_para_file_format                                           &
     &  = set_parallel_file_format(file_fmt%charavalue, file_fmt%iflag)
!
      end function choose_para_file_format
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine choose_file_format_array(num, files_fmt, id_files_fmt)
!
      use t_control_array_character
!
      integer(kind= kint), intent(in) :: num
      type(ctl_array_chara), intent(in) :: files_fmt
      integer(kind= kint), intent(inout) :: id_files_fmt(num)
!
      integer(kind = kint) :: i
!
!
      call init_file_format_flags()
!
      do i = 1, num
        id_files_fmt(i)                                                 &
     &    =  set_file_format(files_fmt%c_tbl(i), files_fmt%icou)
      end do
!
      call dealloc_file_format_flags
!
      end subroutine choose_file_format_array
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      integer(kind= kint) function set_file_format                      &
     &                           (file_fmt_ctl, i_file_fmt)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: file_fmt_ctl
      integer(kind= kint), intent(in) ::   i_file_fmt
!
!
      set_file_format = id_ascii_file_fmt
      if (i_file_fmt .gt. 0) then
        if     (check_mul_flags(file_fmt_ctl, binary_flags)) then
           set_file_format = id_binary_file_fmt
        else if(check_mul_flags(file_fmt_ctl, ascii_flags)) then
           set_file_format = id_ascii_file_fmt
        else if(check_mul_flags(file_fmt_ctl, gzip_ascii_flags)) then
           set_file_format = id_gzip_txt_file_fmt
        else if(check_mul_flags(file_fmt_ctl, gzip_bin_flags)) then
           set_file_format = id_gzip_bin_file_fmt
!
        else if(check_mul_flags(file_fmt_ctl, no_file_flags)) then
           set_file_format = id_no_file
        end if
      end if
!
      end function set_file_format
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      integer(kind= kint) function set_parallel_file_format             &
     &                           (file_fmt_ctl, i_file_fmt)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: file_fmt_ctl
      integer(kind= kint), intent(in) ::   i_file_fmt
      integer(kind= kint) :: id_file_fmt
!
!
      call init_file_format_flags()
!
      id_file_fmt = id_ascii_file_fmt
      if (i_file_fmt .gt. 0) then
        if     (check_mul_flags(file_fmt_ctl, mgd_binary_flags)) then
           id_file_fmt = id_binary_file_fmt + iflag_single
        else if(check_mul_flags(file_fmt_ctl, mgd_ascii_flags)) then
           id_file_fmt = id_ascii_file_fmt + iflag_single
        else if(check_mul_flags(file_fmt_ctl, mgd_gzip_flags)) then
           id_file_fmt = id_gzip_txt_file_fmt + iflag_single
        else if(check_mul_flags(file_fmt_ctl, mgd_gzip_bin_flags)) then
           id_file_fmt = id_gzip_bin_file_fmt + iflag_single
        else if(cmp_no_case(file_fmt_ctl, hd_rayleigh99)) then
           id_file_fmt = id_Rayleigh99
        else if(cmp_no_case(file_fmt_ctl, hd_rayleigh)) then
           id_file_fmt = id_Rayleigh
        else
          id_file_fmt = set_file_format(file_fmt_ctl, i_file_fmt)
        end if
      end if
      set_parallel_file_format = id_file_fmt
!
      call dealloc_file_format_flags
!
      end function set_parallel_file_format
!
!------------------------------------------------------------------
!
      end module m_file_format_switch
