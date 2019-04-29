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
!!        type(read_integer_item), intent(in) :: file_fmt
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
!>      Integer flag for Rayleigh data
      integer(kind = kint), parameter :: id_rayleigh = 105
!
!>      ascii flag
      character(len = kchara), parameter :: hd_ascii1 = "ascii"
!
!>      bindary flag
      character(len = kchara), parameter :: hd_bin1 = "binary"
!>      bindary flag
      character(len = kchara), parameter :: hd_bin2 = "bin"
!
!>      gzip flag
      character(len = kchara), parameter :: hd_gzip1 =  "gz"
!>      gzip flag
      character(len = kchara), parameter :: hd_gzip2 =  "gzip"
!>      gzip flag
      character(len = kchara), parameter :: hd_gzip3 =  "gzip_ascii"
!>      gzip flag
      character(len = kchara), parameter :: hd_gzip4 =  "gz_ascii"
!>      gzip flag
      character(len = kchara), parameter :: hd_gzip5 =  "ascii_gzip"
!>      gzip flag
      character(len = kchara), parameter :: hd_gzip6 =  "ascii_gz"
!>      gzip flag
      character(len = kchara), parameter :: hd_gzip7 =  "gzip_text"
!>      gzip flag
      character(len = kchara), parameter :: hd_gzip8 =  "gz_text"
!>      gzip flag
      character(len = kchara), parameter :: hd_gzip9 =  "text_gzip"
!>      gzip flag
      character(len = kchara), parameter :: hd_gzip10 = "text_gz"
!
!>      gzipped binary flag
      character(len = kchara), parameter :: hd_bin_gz1 =  "bin_gz"
!>      gzipped binary flag
      character(len = kchara), parameter :: hd_bin_gz2 =  "bin_gzip"
!>      gzipped binary flag
      character(len = kchara), parameter :: hd_bin_gz3 =  "gz_bin"
!>      gzipped binary flag
      character(len = kchara), parameter :: hd_bin_gz4 =  "gzip_bin"
!>      gzipped binary flag
      character(len = kchara), parameter :: hd_bin_gz5 =  "binary_gz"
!>      gzipped binary flag
      character(len = kchara), parameter :: hd_bin_gz6 =  "binary_gzip"
!>      gzipped binary flag
      character(len = kchara), parameter :: hd_bin_gz7 =  "gz_binary"
!>      gzipped binary flag
      character(len = kchara), parameter :: hd_bin_gz8 =  "gzip_binary"
!
!>      merged bindary flag
      character(len = kchara), parameter :: hd_merge1 = "merged"
!>      merged bindary flag
      character(len = kchara), parameter :: hd_merge2 = "merged_ascii"
!>      merged bindary flag
      character(len = kchara), parameter :: hd_merge3 = "ascii_merged"
!>      merged bindary flag
      character(len = kchara), parameter :: hd_merge4 = "single"
!>      merged bindary flag
      character(len = kchara), parameter :: hd_merge5 = "single_ascii"
!>      merged bindary flag
      character(len = kchara), parameter :: hd_merge6 = "ascii_single"
!
!>      merged bindary flag
      character(len = kchara), parameter                                &
     &                        :: hd_merge_bin1 = "merged_binary"
!>      merged bindary flag
      character(len = kchara), parameter                                &
     &                        :: hd_merge_bin2 = "merged_bin"
!>      merged bindary flag
      character(len = kchara), parameter                                &
     &                        :: hd_merge_bin3 = "binary_merged"
!>      merged bindary flag
      character(len = kchara), parameter                                &
     &                        :: hd_merge_bin4 = "bin_merged"
!>      merged bindary flag
      character(len = kchara), parameter                                &
     &                        :: hd_merge_bin5 = "single_binary"
!>      merged bindary flag
      character(len = kchara), parameter                                &
     &                        :: hd_merge_bin6 = "single_bin"
!>      merged bindary flag
      character(len = kchara), parameter                                &
     &                        :: hd_merge_bin7 = "single_merged"
!>      merged bindary flag
      character(len = kchara), parameter                                &
     &                        :: hd_merge_bin8 = "bin_single"
!
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz1 = "merged_gz"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz2 = "merged_gzip"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz3 = "merged_ascii_gz"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz4 = "merged_ascii_gzip"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz5 = "merged_gz_ascii"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz6 = "merged_gzip_ascii"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz7 = "gzip_merged"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz8 = "gz_merged"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz9 = "merged_gz"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz10 = "single_gzip"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz11 = "single_ascii_gz"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz12 = "single_ascii_gzip"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz13 = "single_gz_ascii"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz14 = "single_gzip_ascii"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz15 = "gzip_single"
!>      merged gzip file flag
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz16 = "gz_single"
!
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz1 = "merged_bin_gz"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz2 = "merged_bin_gzip"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz3 = "merged_binary_gz"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz4 = "merged_binary_gzip"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz5 = "merged_gz_binary"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz6 = "merged_gzip_binary"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz7 = "gzip_merged_bin"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz8 = "gz_merged_bin"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz9 = "single_bin_gz"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz10 = "single_bin_gzip"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz11 = "single_binary_gz"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz12 = "single_binary_gzip"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz13 = "single_gz_binary"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz14 = "single_gzip_binary"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz15 = "gzip_single_bin"
!>      merged binary gzip file flag
      character(len = kchara), parameter                                &
     &                     :: hd_merged_bin_gz16 = "gz_single_bin"
!
!>      Rayleigh data flag
      character(len = kchara), parameter :: hd_rayleigh = "Rayleigh"
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
      use t_control_elements
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
      use t_control_elements
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
        id_files_fmt(i)                                                 &
     &    =  set_file_format(files_fmt%c_tbl(i), files_fmt%icou)
      end do
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
      integer(kind= kint) :: id_file_fmt
!
!
      id_file_fmt = id_ascii_file_fmt
      if (i_file_fmt .gt. 0) then
        if     (cmp_no_case(file_fmt_ctl, hd_bin1)                     &
     &     .or. cmp_no_case(file_fmt_ctl, hd_bin2)    ) then
           id_file_fmt = id_binary_file_fmt
        else if(cmp_no_case(file_fmt_ctl, hd_gzip1)                     &
     &     .or. cmp_no_case(file_fmt_ctl, hd_gzip2)                     &
     &     .or. cmp_no_case(file_fmt_ctl, hd_gzip3)                     &
     &     .or. cmp_no_case(file_fmt_ctl, hd_gzip4)                     &
     &     .or. cmp_no_case(file_fmt_ctl, hd_gzip5)                     &
     &     .or. cmp_no_case(file_fmt_ctl, hd_gzip6)                     &
     &     .or. cmp_no_case(file_fmt_ctl, hd_gzip7)                     &
     &     .or. cmp_no_case(file_fmt_ctl, hd_gzip8)                     &
     &     .or. cmp_no_case(file_fmt_ctl, hd_gzip9)                     &
     &     .or. cmp_no_case(file_fmt_ctl, hd_gzip10) ) then
           id_file_fmt = id_gzip_txt_file_fmt
        else if(cmp_no_case(file_fmt_ctl, hd_bin_gz1)                   &
     &     .or. cmp_no_case(file_fmt_ctl, hd_bin_gz2)                   &
     &     .or. cmp_no_case(file_fmt_ctl, hd_bin_gz3)                   &
     &     .or. cmp_no_case(file_fmt_ctl, hd_bin_gz4)                   &
     &     .or. cmp_no_case(file_fmt_ctl, hd_bin_gz5)                   &
     &     .or. cmp_no_case(file_fmt_ctl, hd_bin_gz6)                   &
     &     .or. cmp_no_case(file_fmt_ctl, hd_bin_gz7)                   &
     &     .or. cmp_no_case(file_fmt_ctl, hd_bin_gz8) ) then
           id_file_fmt = id_gzip_bin_file_fmt
        end if
      end if
      set_file_format = id_file_fmt
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
      id_file_fmt = id_ascii_file_fmt
      if (i_file_fmt .gt. 0) then
        if     (cmp_no_case(file_fmt_ctl, hd_merge_bin1)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merge_bin2)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merge_bin3)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merge_bin4)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merge_bin5)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merge_bin6)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merge_bin7)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merge_bin8) ) then
           id_file_fmt = id_binary_file_fmt + iflag_single
        else if(cmp_no_case(file_fmt_ctl, hd_merge1)                    &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merge2)                    &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merge3)                    &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merge4)                    &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merge5)                    &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merge6) ) then
           id_file_fmt = id_ascii_file_fmt + iflag_single
        else if(cmp_no_case(file_fmt_ctl, hd_merged_gz1)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz2)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz3)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz4)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz5)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz6)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz7)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz8)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz9)                &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz10)               &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz11)               &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz12)               &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz13)               &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz14)               &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz15)               &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_gz16) ) then
           id_file_fmt = id_gzip_txt_file_fmt + iflag_single
        else if(cmp_no_case(file_fmt_ctl, hd_merged_bin_gz1)            &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz2)            &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz3)            &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz4)            &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz5)            &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz6)            &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz7)            &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz8)            &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz9)            &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz10)           &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz11)           &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz12)           &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz13)           &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz14)           &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz15)           &
     &     .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz16) ) then
           id_file_fmt = id_gzip_bin_file_fmt + iflag_single
        else if(cmp_no_case(file_fmt_ctl, hd_rayleigh)) then
           id_file_fmt = id_rayleigh
        else
          id_file_fmt = set_file_format(file_fmt_ctl, i_file_fmt)
        end if
      end if
      set_parallel_file_format = id_file_fmt
!
      end function set_parallel_file_format
!
!------------------------------------------------------------------
!
      end module m_file_format_switch
