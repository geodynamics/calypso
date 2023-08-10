!>@file   m_file_format_labels.f90
!!@brief  module m_file_format_labels
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  integer flags for Data file's format
!!
!!@verbatim
!!      subroutine init_file_format_flags()
!!      subroutine dealloc_file_format_flags()
!!        File format name
!!          'ascii':        text (formatted) data
!!          'gzip':         gziopped text (formatted) data
!!          'binary':       binary data
!!          'binary_gz':    gziopped binary data
!!
!!          'merged_ascii': text (formatted) data in merged file 
!!          'merged_binary': binary data in merged file
!!          'merged_gz':    gziopped text (formatted) data in merged file
!!          'merged_bin_gz': gziopped binary data in merged file
!!
!!      subroutine set_label_file_fmt(array_c)
!!        type(ctl_array_chara), intent(inout) :: array_c
!!      subroutine cvt_default_field_format_flag(file_fmt_ctl)
!!@endverbatim
!
      module m_file_format_labels
!
      use m_precision
      use m_constants
      use t_multi_flag_labels
!
      implicit none
!
!
!>      flag parts for distributed ascii data
      character(len = kchara), parameter :: hd_ascii =  'ascii'
!>      flag parts for distributed binary data
      character(len = kchara), parameter :: hd_binary = 'binary'
!>      flag parts for distributed compressed ascii data
      character(len = kchara), parameter :: hd_gzip =   'gzip'
!>      flag parts for distributed compressed binary data
      character(len = kchara), parameter :: hd_bin_gz = 'binary_gz'
!>      flag parts for distributed compressed binary data
      character(len = kchara), parameter :: hd_none =   'none'
!
!>      flag parts for merged ascii data
      character(len = kchara), parameter                                &
     &                        :: hd_merged =  'merged_ascii'
!>      flag parts for merged binary data
      character(len = kchara), parameter                                &
     &                        :: hd_merged_bin = 'merged_binary'
!>      flag parts for merged compressed ascii data
      character(len = kchara), parameter                                &
     &                        :: hd_merged_gz =  'merged_gz'
!>      flag parts for merged compressed binary data
      character(len = kchara), parameter                                &
     &                        :: hd_merged_bin_gz = 'merged_bin_gz'
!
!>      Rayleigh data flag
      character(len = kchara), parameter :: hd_rayleigh = "Rayleigh"
!>      Rayleigh Ver. 0.9 data flag
      character(len = kchara), parameter                                &
     &                        :: hd_rayleigh99 = "Rayleigh_09"
!
!
!>      flag parts for ascii data
      character(len = kchara), parameter :: ascii_names(2)              &
     &                        = (/'ascii ', 'text  '/)
!>      flag parts for binary data
      character(len = kchara), parameter :: bin_names(2)                &
     &                        = (/'binary ', 'bin    '/)
!>      flag parts for compressed data
      character(len = kchara), parameter :: gzip_names(2)               &
     &                        = (/'gzip ', 'gz   '/)
!>      flag parts for MPI-IO usage
      character(len = kchara), parameter :: merged_names(2)             &
     &                        = (/'merged ', 'single '/)
!>      flag parts for MPI-IO usage
      character(len = kchara), parameter :: none_names(2)               &
     &                        = (/'None    ', 'No_file '/)
!!
!>     Character lables for compressed ASCII
!!        'binary', 'bin' 
      type(multi_flag_labels), save :: binary_flags
!>     Character lables for ASCII
!!        'ascii', 'text' 
      type(multi_flag_labels), save :: ascii_flags
!>     Character lables for UCD
!!        'gzip', 'gz' 
      type(multi_flag_labels), save :: gzip_flags
!>     Character lables for merged data by MPI-IO
!!       'merged', 'single' 
      type(multi_flag_labels), save :: merged_flags
!>     Character lables for No file
!!        'None', 'No_file' 
      type(multi_flag_labels), save :: no_file_flags
!
!>     Character lables for compressed ASCII
!!        'ascii_gzip', 'ascii_gz', 'text_gzip', 'text_gz', 
!!        'gzip_ascii', 'gzip_text', 'gz_ascii', 'gz_text', 
!!        'gzip', 'gz'
      type(multi_flag_labels), save :: gzip_ascii_flags
!>     Character lables for compressed ASCII
!!        'binary_gzip', 'binary_gz', 'bin_gzip', 'bin_gz', 
!!        'gzip_binary', 'gzip_bin', 'gz_binary', 'gz_bin' 
      type(multi_flag_labels), save :: gzip_bin_flags
!
!>     Character lables for merged ASCII
!!        'merged_ascii', 'merged_text', 'single_ascii', 'single_text',
!!        'ascii_merged', 'ascii_single', 'text_merged', 'text_single',
!!        'merged', 'single' 
      type(multi_flag_labels), save :: mgd_ascii_flags
!>     Character lables for merged binary
!!        'merged_binary', 'merged_bin', 'single_binary', 'single_bin',
!!        'binary_merged', 'binary_single', 'bin_merged', 'bin_single' 
      type(multi_flag_labels), save :: mgd_binary_flags
!
!>     Character lables for merged compressed ASCII
!!        'merged_ascii_gzip', 'merged_ascii_gz',   'merged_text_gzip',
!!        'merged_text_gz',    'merged_gzip_ascii', 'merged_gzip_text',
!!        'merged_gz_ascii',   'merged_gz_text',    'merged_gzip',
!!        'merged_gz',         'single_ascii_gzip', 'single_ascii_gz',
!!        'single_text_gzip',  'single_text_gz',    'single_gzip_ascii',
!!        'single_gzip_text',  'single_gz_ascii',   'single_gz_text',
!!        'single_gzip',       'single_gz',         'ascii_gzip_merged',
!!        'ascii_gzip_single', 'ascii_gz_merged',   'ascii_gz_single',
!!        'text_gzip_merged',  'text_gzip_single',  'text_gz_merged',
!!        'text_gz_single',    'gzip_ascii_merged', 'gzip_ascii_single',
!!        'gzip_text_merged',  'gzip_text_single',  'gz_ascii_merged',
!!        'gz_ascii_single',   'gz_text_merged',    'gz_text_single',
!!        'gzip_merged', 'gzip_single', 'gz_merged', 'gz_single' 
      type(multi_flag_labels), save :: mgd_gzip_flags
!
!>     Character lables for compressesd merged binary
!!        'merged_binary_gzip', 'merged_binary_gz',   'merged_bin_gzip',
!!        'merged_bin_gz',      'merged_gzip_binary', 'merged_gzip_bin',
!!        'merged_gz_binary',   'merged_gz_bin',   'single_binary_gzip',
!!        'single_binary_gz',   'single_bin_gzip', 'single_bin_gz',
!!        'single_gzip_binary', 'single_gzip_bin', 'single_gz_binary',
!!        'single_gz_bin', 'binary_gzip_merged',   'binary_gzip_single',
!!        'binary_gz_merged',   'binary_gz_single',   'bin_gzip_merged',
!!        'bin_gzip_single',    'bin_gz_merged',      'bin_gz_single',
!!        'gzip_binary_merged', 'gzip_binary_single', 'gzip_bin_merged',
!!        'gzip_bin_single',    'gz_binary_merged',   'gz_binary_single',
!!        'gz_bin_merged',      'gz_bin_single' 
      type(multi_flag_labels), save :: mgd_gzip_bin_flags
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine init_file_format_flags()
!
      integer(kind = kint) :: icou
!
      call init_multi_flags_by_labels(itwo, ascii_names, ascii_flags)
      call init_multi_flags_by_labels(itwo, bin_names, binary_flags)
      call init_multi_flags_by_labels(itwo, gzip_names, gzip_flags)
      call init_multi_flags_by_labels(itwo, merged_names, merged_flags)
!
      call init_multi_flags_by_labels(itwo, none_names, no_file_flags)
!
      call init_from_two_kinds_flags                                    &
     &   (ascii_flags, gzip_flags, gzip_ascii_flags, icou)
      call append_multi_flag_labels                                     &
     &   (gzip_flags, gzip_ascii_flags)
!
      call init_from_two_kinds_flags                                    &
     &   (binary_flags, gzip_flags, gzip_bin_flags, icou)
!
!
      call init_from_two_kinds_flags                                    &
     &   (merged_flags, ascii_flags, mgd_ascii_flags, icou)
      call append_multi_flag_labels(merged_flags, mgd_ascii_flags)
!
      call init_from_two_kinds_flags                                    &
     &   (merged_flags, gzip_ascii_flags, mgd_gzip_flags, icou)
!
      call init_from_two_kinds_flags                                    &
     &   (merged_flags, binary_flags, mgd_binary_flags, icou)
      call init_from_two_kinds_flags                                    &
     &   (merged_flags, gzip_bin_flags, mgd_gzip_bin_flags, icou)
!
      end subroutine init_file_format_flags
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_file_format_flags()
!
      call dealloc_multi_flags(ascii_flags)
      call dealloc_multi_flags(binary_flags)
      call dealloc_multi_flags(gzip_flags)
      call dealloc_multi_flags(merged_flags)
!
      call dealloc_multi_flags(no_file_flags)
!
      call dealloc_multi_flags(gzip_ascii_flags)
      call dealloc_multi_flags(gzip_bin_flags)
!
      call dealloc_multi_flags(mgd_ascii_flags)
      call dealloc_multi_flags(mgd_binary_flags)
!
      call dealloc_multi_flags(mgd_gzip_flags)
      call dealloc_multi_flags(mgd_gzip_bin_flags)
!
      end subroutine dealloc_file_format_flags
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_label_file_fmt(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(hd_ascii, array_c)
      call append_c_to_ctl_array(hd_binary, array_c)
      call append_c_to_ctl_array(hd_gzip, array_c)
      call append_c_to_ctl_array(hd_bin_gz, array_c)

      call append_c_to_ctl_array(hd_merged, array_c)
      call append_c_to_ctl_array(hd_merged_bin, array_c)
      call append_c_to_ctl_array(hd_merged_gz, array_c)
      call append_c_to_ctl_array(hd_merged_bin_gz, array_c)
!
      call append_c_to_ctl_array(hd_rayleigh, array_c)
      call append_c_to_ctl_array(hd_rayleigh99, array_c)
!
      end subroutine set_label_file_fmt
!
! ----------------------------------------------------------------------
!
      subroutine cvt_default_field_format_flag(file_fmt_ctl)
!
      use skip_comment_f
!
      character(len=kchara), intent(inout) :: file_fmt_ctl
!
!
      call init_file_format_flags()
!
      if     (check_mul_flags(file_fmt_ctl, mgd_binary_flags)) then
         file_fmt_ctl = trim(hd_merged_bin) // char(0)
      else if(check_mul_flags(file_fmt_ctl, mgd_ascii_flags)) then
         file_fmt_ctl = trim(hd_merged) // char(0)
      else if(check_mul_flags(file_fmt_ctl, mgd_gzip_flags)) then
         file_fmt_ctl = trim(hd_merged_gz) // char(0)
      else if(check_mul_flags(file_fmt_ctl, mgd_gzip_bin_flags)) then
         file_fmt_ctl = trim(hd_merged_bin_gz) // char(0)
      else if(check_mul_flags(file_fmt_ctl, binary_flags)) then
         file_fmt_ctl = trim(hd_bin_gz) // char(0)
      else if(check_mul_flags(file_fmt_ctl, ascii_flags)) then
         file_fmt_ctl = trim(hd_ascii) // char(0)
      else if(check_mul_flags(file_fmt_ctl, gzip_ascii_flags)) then
         file_fmt_ctl = trim(hd_gzip) // char(0)
      else if(check_mul_flags(file_fmt_ctl, gzip_bin_flags)) then
         file_fmt_ctl = trim(hd_bin_gz) // char(0)
      else if(cmp_no_case(file_fmt_ctl, hd_rayleigh99)) then
         file_fmt_ctl = trim(hd_rayleigh99) // char(0)
      else if(cmp_no_case(file_fmt_ctl, hd_rayleigh)) then
         file_fmt_ctl = trim(hd_rayleigh) // char(0)
      else if(check_mul_flags(file_fmt_ctl, no_file_flags)) then
         file_fmt_ctl = trim(hd_none) // char(0)
      else
         file_fmt_ctl = trim(hd_ascii) // char(0)
      end if
!
      call dealloc_file_format_flags
!
      end subroutine cvt_default_field_format_flag
!
!------------------------------------------------------------------
!
      end module m_file_format_labels
