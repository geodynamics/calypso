!>@file   m_file_format_switch.f90
!!@brief  module m_file_format_switch
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  integer flags for Data file's format
!!
!!@verbatim
!!      subroutine choose_file_format(file_fmt, id_file_fmt)
!!      subroutine choose_para_file_format(file_fmt, id_file_fmt)
!!        type(read_integer_item), intent(in) :: file_fmt
!!      subroutine choose_file_format_array(num, files_fmt, id_files_fmt)
!!        type(ctl_array_chara), intent(in) :: files_fmt
!!      subroutine set_file_format(file_fmt_ctl, i_file_fmt,            &
!!                id_file_fmt)
!!      subroutine set_parallel_file_format(file_fmt_ctl, i_file_fmt,   &
!!     &          id_file_fmt)
!!
!!        File format name
!!          'ascii':   text (formatted) data
!!          'gzip':    gziopped text (formatted) data
!!          'binary':  binary (unformatted) data
!!
!!          'merged_ascii': text (formatted) data in merged file 
!!          'merged_gzip':  gziopped text (formatted) data in merged file
!!          'merged_binary': binary (unformatted) data in merged file
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
!
      implicit none
!
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
      private :: set_file_format
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine choose_file_format(file_fmt, id_file_fmt)
!
      use t_control_elements
!
      type(read_character_item), intent(in) :: file_fmt
      integer(kind= kint), intent(inout) :: id_file_fmt
!
!
      call set_file_format(file_fmt%charavalue, file_fmt%iflag,         &
     &    id_file_fmt)
!
      end subroutine choose_file_format
!
!------------------------------------------------------------------
!
      subroutine choose_para_file_format(file_fmt, id_file_fmt)
!
      use t_control_elements
!
      type(read_character_item), intent(in) :: file_fmt
      integer(kind= kint), intent(inout) :: id_file_fmt
!
!
      call set_parallel_file_format(file_fmt%charavalue,                &
     &    file_fmt%iflag, id_file_fmt)
!
      end subroutine choose_para_file_format
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine choose_file_format_array(num, files_fmt, id_files_fmt)
!
      use t_read_control_arrays
!
      integer(kind= kint), intent(in) :: num
      type(ctl_array_chara), intent(in) :: files_fmt
      integer(kind= kint), intent(inout) :: id_files_fmt(num)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num
        call set_file_format(files_fmt%c_tbl(i), files_fmt%icou,        &
     &      id_files_fmt(i))
      end do
!
      end subroutine choose_file_format_array
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_file_format(file_fmt_ctl, i_file_fmt,              &
     &          id_file_fmt)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: file_fmt_ctl
      integer(kind= kint), intent(in) ::   i_file_fmt
      integer(kind= kint), intent(inout) :: id_file_fmt
!
!
      id_file_fmt = id_ascii_file_fmt
      if (i_file_fmt .gt. 0) then
        if     (cmp_no_case(file_fmt_ctl, 'binary')                     &
     &     .or. cmp_no_case(file_fmt_ctl, 'bin')        ) then
           id_file_fmt = id_binary_file_fmt
        else if(cmp_no_case(file_fmt_ctl, 'gzip_ascii')                 &
     &     .or. cmp_no_case(file_fmt_ctl, 'gz_ascii')                   &
     &     .or. cmp_no_case(file_fmt_ctl, 'ascii_gzip')                 &
     &     .or. cmp_no_case(file_fmt_ctl, 'ascii_gz')                   &
     &     .or. cmp_no_case(file_fmt_ctl, 'gzip_text')                  &
     &     .or. cmp_no_case(file_fmt_ctl, 'gz_text')                    &
     &     .or. cmp_no_case(file_fmt_ctl, 'text_gzip')                  &
     &     .or. cmp_no_case(file_fmt_ctl, 'text_gz')                    &
     &     .or. cmp_no_case(file_fmt_ctl, 'gzip')                       &
     &     .or. cmp_no_case(file_fmt_ctl, 'gz')         ) then
           id_file_fmt = id_gzip_txt_file_fmt
        else if(cmp_no_case(file_fmt_ctl, 'gzip_binary')                &
     &     .or. cmp_no_case(file_fmt_ctl, 'gz_binary')                  &
     &     .or. cmp_no_case(file_fmt_ctl, 'binary_gzip')                &
     &     .or. cmp_no_case(file_fmt_ctl, 'binary_gz')                  &
     &     .or. cmp_no_case(file_fmt_ctl, 'gzip_bin')                   &
     &     .or. cmp_no_case(file_fmt_ctl, 'gz_bin')                     &
     &     .or. cmp_no_case(file_fmt_ctl, 'bin_gzip')                   &
     &     .or. cmp_no_case(file_fmt_ctl, 'bin_gz')    ) then
           id_file_fmt = id_gzip_bin_file_fmt
        end if
      end if
!
      end subroutine set_file_format
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_parallel_file_format(file_fmt_ctl, i_file_fmt,     &
     &          id_file_fmt)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: file_fmt_ctl
      integer(kind= kint), intent(in) ::   i_file_fmt
      integer(kind= kint), intent(inout) :: id_file_fmt
!
!
      id_file_fmt = id_ascii_file_fmt
      if (i_file_fmt .gt. 0) then
        if     (cmp_no_case(file_fmt_ctl, 'merged_binary')              &
     &     .or. cmp_no_case(file_fmt_ctl, 'merged_bin')                 &
     &     .or. cmp_no_case(file_fmt_ctl, 'binary_merged')              &
     &     .or. cmp_no_case(file_fmt_ctl, 'bin_merged')        ) then
           id_file_fmt = id_binary_file_fmt + iflag_single
        else if(cmp_no_case(file_fmt_ctl, 'merged_ascii')               &
     &     .or. cmp_no_case(file_fmt_ctl, 'ascii_merged')               &
     &     .or. cmp_no_case(file_fmt_ctl, 'merged') ) then
           id_file_fmt = id_ascii_file_fmt + iflag_single
        else if(cmp_no_case(file_fmt_ctl, 'merged_gzip_ascii')          &
     &     .or. cmp_no_case(file_fmt_ctl, 'merged_gz_ascii')            &
     &     .or. cmp_no_case(file_fmt_ctl, 'merged_ascii_gzip')          &
     &     .or. cmp_no_case(file_fmt_ctl, 'merged_ascii_gz')            &
     &     .or. cmp_no_case(file_fmt_ctl, 'merged_gzip')                &
     &     .or. cmp_no_case(file_fmt_ctl, 'merged_gz')                  &
     &     .or. cmp_no_case(file_fmt_ctl, 'gzip_merged')                &
     &     .or. cmp_no_case(file_fmt_ctl, 'gz_merged')) then
           id_file_fmt = id_gzip_txt_file_fmt + iflag_single
        else if(cmp_no_case(file_fmt_ctl, 'merged_gzip_binary')         &
     &     .or. cmp_no_case(file_fmt_ctl, 'merged_gz_binary')           &
     &     .or. cmp_no_case(file_fmt_ctl, 'merged_binary_gzip')         &
     &     .or. cmp_no_case(file_fmt_ctl, 'merged_binary_gz')           &
     &     .or. cmp_no_case(file_fmt_ctl, 'merged_bin_gzip')            &
     &     .or. cmp_no_case(file_fmt_ctl, 'merged_bin_gz')              &
     &     .or. cmp_no_case(file_fmt_ctl, 'gzip_merged_bin')            &
     &     .or. cmp_no_case(file_fmt_ctl, 'gz_merged_bin')) then
           id_file_fmt = id_gzip_bin_file_fmt + iflag_single
        else
          call set_file_format(file_fmt_ctl, i_file_fmt, id_file_fmt)
        end if
      end if
!
      end subroutine set_parallel_file_format
!
!------------------------------------------------------------------
!
      end module m_file_format_switch
