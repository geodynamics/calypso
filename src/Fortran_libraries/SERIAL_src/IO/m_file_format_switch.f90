!>@file   m_file_format_switch.f90
!!@brief  module m_file_format_switch
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  integer flags for file format
!!
!!@verbatim
!!      subroutine choose_file_format(file_fmt_ctl, i_file_fmt,         &
!!                id_file_fmt)
!!
!!        File format name
!!          'ascii':            text (formatted) data
!!          'gzip':             gziopped text (formatted) data
!!          'binary' or 'bin':  binary (unformatted) data
!!
!!@endverbatim
!!
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
      integer(kind = kint), parameter :: id_ascii_file_fmt =    0
      integer(kind = kint), parameter :: id_binary_file_fmt =   1
      integer(kind = kint), parameter :: id_gzip_bin_file_fmt = 2
      integer(kind = kint), parameter :: id_gzip_txt_file_fmt = 3
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine choose_file_format(file_fmt_ctl, i_file_fmt,           &
     &          id_file_fmt)
!
      character(len=kchara), intent(in) :: file_fmt_ctl
      integer(kind= kint), intent(in) ::   i_file_fmt
      integer(kind= kint), intent(inout) :: id_file_fmt
!
!
      id_file_fmt = id_ascii_file_fmt
      if (i_file_fmt .gt. 0) then
        if   (file_fmt_ctl.eq.'binary' .or. file_fmt_ctl.eq.'bin'       &
     &   .or. file_fmt_ctl.eq.'Binary' .or. file_fmt_ctl.eq.'Bin'       &
     &   .or. file_fmt_ctl.eq.'BINARY' .or. file_fmt_ctl.eq.'BIN') then
           id_file_fmt = id_binary_file_fmt
        else if(file_fmt_ctl.eq.'gzip' .or. file_fmt_ctl.eq.'gzip_text' &
     &     .or. file_fmt_ctl.eq.'Gzip' .or. file_fmt_ctl.eq.'Gzip_text' &
     &     .or. file_fmt_ctl.eq.'GZIP' .or. file_fmt_ctl.eq.'GZIP_TEXT' &
     &    ) then
           id_file_fmt = id_gzip_txt_file_fmt
!        else if(file_fmt_ctl.eq.'gzip' .or. file_fmt_ctl.eq.'gzip_bin' &
!     &     .or. file_fmt_ctl.eq.'Gzip' .or. file_fmt_ctl.eq.'Gzip_bin' &
!     &     .or. file_fmt_ctl.eq.'GZIP' .or. file_fmt_ctl.eq.'GZIP_bin' &
!     &    ) then
!           id_file_fmt = id_gzip_bin_file_fmt
        end if
      end if
!
      end subroutine choose_file_format
!
!------------------------------------------------------------------
!
      end module m_file_format_switch
