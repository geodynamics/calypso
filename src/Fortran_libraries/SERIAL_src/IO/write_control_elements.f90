!>@file   write_control_elements.f90
!!@brief  module write_control_elements
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2005
!
!>@brief  Subroutines to write control data
!!
!!@verbatim
!!      integer(kind = kint) function write_begin_flag_for_ctl          &
!!     &                                         (id_file, level, label)
!!      integer(kind = kint) function write_end_flag_for_ctl            &
!!     &                                         (id_file, level, label)
!!      integer(kind = kint) function write_array_flag_for_ctl          &
!!     &                                         (id_file, level, label)
!!      integer(kind = kint) function write_end_array_flag_for_ctl      &
!!     &                                         (id_file, level, label)
!!
!!      subroutine write_real_ctl_item                                  &
!!     &         (id_file, level, maxlen, label, real_data)
!!      subroutine write_integer_ctl_item                               &
!!     &          (id_file, level, maxlen, label, int_data)
!!      subroutine write_character_ctl_item                             &
!!     &         (id_file, level, maxlen, label, chara_data)
!!
!!      subroutine write_real2_ctl_item                                 &
!!     &         (id_file, level, maxlen, label, real1, real2)
!!      subroutine write_real3_ctl_item                                 &
!!     &         (id_file, level, maxlen, label, real1, real2, real3)
!!       subroutine write_int_real_ctl_item                             &
!!     &          (id_file, level, maxlen, label, int_data, real_data)
!!      subroutine write_integer2_ctl_item                              &
!!     &         (id_file, level, maxlen, label, int1, int2)
!!      subroutine write_integer3_ctl_item                              &
!!     &         (id_file, level, maxlen, label, int1, int2, int3)
!!      subroutine write_chara_real_ctl_item                            &
!!     &          (id_file, level, maxlen, label, chara1, real_data)
!!       subroutine write_chara_real2_ctl_item(id_file, level, maxlen,  &
!!     &           label, chara_data, real1, real2)
!!      subroutine write_chara2_real_ctl_item(id_file, level, maxlen,   &
!!     &           label, chara1, chara2, real_data)
!!      subroutine write_chara_int_ctl_item                             &
!!     &          (id_file, level, maxlen, label, chara_data, int_data)
!!      subroutine write_i2_r_ctl_item                                  &
!!     &          (id_file, level, maxlen, label, int1, int2, real_data)
!!      subroutine write_i2_r2_ctl_item(id_file, level, maxlen,         &
!!     &           label, int1, int2, real1, real2)
!!      subroutine write_i_c_r_ctl_item(id_file, level, maxlen,         &
!!     &          label, int_data, chara_data, real_data)
!!      subroutine write_character2_ctl_item                            &
!!     &         (id_file, level, maxlen, label, chara1, chara2)
!!      subroutine write_character3_ctl_item                            &
!!     &         (id_file, level, maxlen, label, chara1, chara2, chara3)
!!      subroutine write_chara_int3_ctl_item(id_file, level,maxlen,     &
!!     &          label, chara_data, ivec1, ivec2, ivec3)
!!
!!      subroutine write_file_name_for_ctl_line                         &
!!     &         (id_file, level, label, fname)
!!      subroutine write_file_names_from_ctl_line                       &
!!     &         (id_file, level, label, num, fname)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer (kind=kint), intent(in) :: num
!!        character(len=kchara), intent(in) :: label
!!        character(len=kchara), intent(in) :: fname(num)
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine write_multi_ctl_file_message(label, num, level)
!!      subroutine write_one_ctl_file_message(label, level, file_name)
!!      subroutine write_included_message(label, level)
!!        integer (kind=kint), intent(in) :: num, level
!!        character(len=kchara), intent(in) :: label, file_name
!!@endverbatim
!!
!!@n @param  ctl_name   label for control block
!!@n @param  label      label for control items
!!@n @param  iflag_end  integer flag for reading block
!!@n @param  iflag_dat  integer flag for reading block
!!@n @param  num_array  size of array block
!!@n @param  num        number of blocks already read
!!@n @param  icou       counter for reading array
!!
!!@n @param real_data     read real data
!!@n @param int_data      read integre data
!!@n @param chara_data    read character data
!!
!!@n @param ivect         integer array data
!!@n @param int1          integer array data
!!@n @param int2          integer array data
!!@n @param vect          real array data
!!@n @param vec1          real array data
!!@n @param vec2          real array data
!!@n @param vec3          real array data
!!@n @param c_tbl         character array data
!!@n @param c1_tbl         character array data
!!@n @param c2_tbl         character array data
!!@n @param c3_tbl         character array data
!
      module write_control_elements
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>   file ID to output on screen
       integer(kind = kint), parameter :: id_monitor = 6
!
!>   Label to start a control block
       character(len=kchara), parameter  :: hd_begin = 'begin'
!>   Label to end a control block
       character(len=kchara), parameter  :: hd_end = 'end'
!>   Label for an array control block
       character(len=kchara), parameter  :: hd_array = 'array'
!>   Label for an array control block
       character(len=kchara), parameter  :: hd_file = 'file'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function write_begin_flag_for_ctl            &
     &                                         (id_file, level, label)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, hd_begin)
      write(id_file,'(a)') trim(label)
!
      write_begin_flag_for_ctl = level + 1
!
      end function write_begin_flag_for_ctl
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function write_end_flag_for_ctl              &
     &                                         (id_file, level, label)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
!
      integer(kind = kint) :: level_new
!
      level_new = max(level-1,0)
!
      call write_space_4_parse(id_file, level_new)
      call write_ctl_chara_cont(id_file, hd_end)
      call write_ctl_chara_lf(id_file, label)
!
      write_end_flag_for_ctl = level_new
!
      end function write_end_flag_for_ctl
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function write_array_flag_for_ctl            &
     &                                     (id_file, level, label)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, hd_array)
      call write_ctl_chara_lf(id_file, label)
!
      write_array_flag_for_ctl = level + 1
!
      end function write_array_flag_for_ctl
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function write_end_array_flag_for_ctl        &
     &                                         (id_file, level, label)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
!
      integer(kind = kint) :: level_new
!
      level_new = max(level-1,0)
!
      call write_space_4_parse(id_file, level_new)
      call write_ctl_chara_cont(id_file, hd_end)
      call write_ctl_chara_cont(id_file, hd_array)
      call write_ctl_chara_lf(id_file, label)
!
      write_end_array_flag_for_ctl = level_new
!
      end function write_end_array_flag_for_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_real_ctl_item                                    &
     &         (id_file, level, maxlen, label, real_data)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      real(kind = kreal), intent(in) :: real_data
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      write(id_file,'(1pE25.15e3)') real_data
!
      end subroutine write_real_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_integer_ctl_item                                 &
     &         (id_file, level, maxlen, label, int_data)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(in) :: int_data
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      write(id_file,'(i16)')  int_data
!
      end subroutine write_integer_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_character_ctl_item                               &
     &         (id_file, level, maxlen, label, chara_data)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara_data
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      call write_ctl_chara_lf(id_file, chara_data)
!
      end subroutine write_character_ctl_item
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_real2_ctl_item                                   &
     &         (id_file, level, maxlen, label, real1, real2)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      real(kind = kreal), intent(in) :: real1, real2
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      write(id_file,'(1p2E25.15e3)') real1, real2
!
       end subroutine write_real2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_real3_ctl_item                                   &
     &         (id_file, level, maxlen, label, real1, real2, real3)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      real(kind = kreal), intent(in) :: real1, real2, real3
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      write(id_file,'(1p3E25.15e3)') real1, real2, real3
!
      end subroutine write_real3_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_int_real_ctl_item                                &
     &         (id_file, level, maxlen, label, int_data, real_data)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level, maxlen
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(in) :: int_data
      real(kind = kreal), intent(in) :: real_data
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      write(id_file,'(i16,1pE25.15e3)') int_data, real_data
!
      end subroutine write_int_real_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_integer2_ctl_item                                &
     &         (id_file, level, maxlen, label, int1, int2)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(in) :: int1, int2
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      write(id_file,'(2i16)') int1, int2
!
      end subroutine write_integer2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_integer3_ctl_item                                &
     &         (id_file, level, maxlen, label, int1, int2, int3)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(in) :: int1, int2, int3
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      write(id_file,'(3i16)') int1, int2, int3
!
      end subroutine write_integer3_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_chara_real_ctl_item                             &
     &          (id_file, level, maxlen, label, chara1, real_data)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen(0:1)
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara1
      real(kind = kreal), intent(in) :: real_data
!
      integer(kind = kint) :: nspace0, nspace1
!
      nspace0 = maxlen(0) - len_trim(label)
      nspace1 = maxlen(1) - len_trim(chara1) - 2 * iflag_divide(chara1)
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      call write_ctl_chara_cont(id_file, chara1)
      call write_spaces(id_file, nspace1)
      write(id_file,'(a2,1pE25.15e3)')  '  ', real_data
!
      end subroutine write_chara_real_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_chara_real2_ctl_item(id_file, level, maxlen,    &
     &           label, chara_data, real1, real2)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level, maxlen
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara_data
      real(kind = kreal), intent(in) :: real1, real2
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      call write_ctl_chara_cont(id_file, chara_data)
      write(id_file,'(a4,1p2E25.15e3)') '    ', real1, real2
!
      end subroutine write_chara_real2_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_chara2_real_ctl_item(id_file, level, maxlen,    &
     &           label, chara1, chara2, real_data)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara1, chara2
      real(kind = kreal), intent(in) :: real_data
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      call write_ctl_chara_cont(id_file, chara1)
      call write_ctl_chara_cont(id_file, chara2)
      write(id_file,'(1pE25.15e3)')  real_data
!
      end subroutine write_chara2_real_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_chara_int_ctl_item                              &
     &          (id_file, level, maxlen, label, chara_data, int_data)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara_data
      integer (kind=kint), intent(in) :: int_data
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      call write_ctl_chara_cont(id_file, chara_data)
      write(id_file,'(i16)')  int_data
!
      end subroutine write_chara_int_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_i2_r_ctl_item                                   &
     &          (id_file, level, maxlen, label, int1, int2, real_data)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level, maxlen
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: int1, int2
      real(kind = kreal), intent(in) :: real_data
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      write(id_file,'(2i16,1pE25.15e3)') int1, int2, real_data
!
      end subroutine write_i2_r_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_i2_r2_ctl_item(id_file, level, maxlen,          &
     &           label, int1, int2, real1, real2)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level, maxlen
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: int1, int2
      real(kind = kreal), intent(in) :: real1, real2
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      write(id_file,'(2i16,1p2E25.15e3)') int1, int2, real1, real2
!
      end subroutine write_i2_r2_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_i_c_r_ctl_item(id_file, level, maxlen,          &
     &           label, int_data, chara_data, real_data)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level, maxlen
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: int_data
      character(len=kchara), intent(in) :: chara_data
      real(kind = kreal), intent(in) :: real_data
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      write(id_file,'(i16,a2,a,a2,1pE25.15e3)')                         &
     &       int_data, '  ', trim(chara_data), '  ', real_data
!
      end subroutine write_i_c_r_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_character2_ctl_item                              &
     &         (id_file, level, maxlen, label, chara1, chara2)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen(0:1)
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara1, chara2
!
      integer(kind = kint) :: nspace0, nspace1
!
      nspace0 = maxlen(0) - len_trim(label)
      nspace1 = maxlen(1) - len_trim(chara1) - 2 * iflag_divide(chara1)
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      call write_ctl_chara_cont(id_file, chara1)
      call write_spaces(id_file, nspace1)
      call write_ctl_chara_lf(id_file, chara2)
!
      end subroutine write_character2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_character3_ctl_item                              &
     &         (id_file, level, maxlen, label, chara1, chara2, chara3)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen(0:2)
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara1, chara2, chara3
!
      integer(kind = kint) :: nspace0, nspace1, nspace2
!
      nspace0 = maxlen(0) - len_trim(label)
      nspace1 = maxlen(1) - len_trim(chara1) - 2 * iflag_divide(chara1)
      nspace2 = maxlen(2) - len_trim(chara2) - 2 * iflag_divide(chara2)
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      call write_ctl_chara_cont(id_file, chara1)
      call write_spaces(id_file, nspace1)
      call write_ctl_chara_cont(id_file, chara2)
      call write_spaces(id_file, nspace2)
      call write_ctl_chara_lf(id_file, chara3)
!
      end subroutine write_character3_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_chara_int3_ctl_item(id_file, level, maxlen,     &
     &           label, chara_data, ivec1, ivec2, ivec3)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level, maxlen
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara_data
      integer (kind=kint), intent(in) :: ivec1, ivec2, ivec3
!
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label) + 4
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_spaces(id_file, nspace0)
      call write_ctl_chara_cont(id_file, chara_data)
      write(id_file,'(4a,3i16)')  '    ', ivec1, ivec2, ivec3
!
      end subroutine write_chara_int3_ctl_item
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_file_name_for_ctl_line                           &
     &         (id_file, level, label, fname)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: fname
!
!
      call write_space_4_parse(id_file, level)
      write(id_file,'(2a,a2,a)')                                        &
     &              'file  ', trim(label), '  ', trim(fname)
!
       end subroutine write_file_name_for_ctl_line
!
!   --------------------------------------------------------------------
!
      subroutine write_file_names_from_ctl_line                         &
     &         (id_file, label, num, fname, level)
!
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_file
      integer (kind=kint), intent(in) :: num
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: fname(num)
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      level = write_array_flag_for_ctl(id_file, level, label)
      do i = 1, num
        call write_file_name_for_ctl_line                               &
     &     (id_file, level, label, fname(i))
      end do
      level = write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_file_names_from_ctl_line
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_multi_ctl_file_message(label, num, level)
      use write_control_items
      integer (kind=kint), intent(in) :: num, level
      character(len=kchara), intent(in) :: label
!
      write(id_monitor,'(a)',ADVANCE='NO') '! '
      call write_space_4_parse(id_monitor, level)
      write(id_monitor,'(2a)',ADVANCE='NO') 'Control for ', trim(label)
      if(num .le. 0) return
!
      write(id_monitor,'(a,i4)', ADVANCE='NO') ' No. ',  num
!
      end subroutine write_multi_ctl_file_message
!
!   --------------------------------------------------------------------
!
      subroutine write_one_ctl_file_message(label, level, file_name)
      use write_control_items
      integer (kind=kint), intent(in) :: level
      character(len=kchara), intent(in) :: label, file_name
!
        write(id_monitor,'(a)',ADVANCE='NO') '! '
        call write_space_4_parse(id_monitor, level)
        write(id_monitor,'(4a)') 'Control for ', trim(label),           &
     &                   ' is read from file... ', trim(file_name)
!
      end subroutine write_one_ctl_file_message
!
!   --------------------------------------------------------------------
!
      subroutine write_included_message(label, level)
      use write_control_items
      integer (kind=kint), intent(in) :: level
      character(len=kchara), intent(in) :: label
!
        write(id_monitor,'(a)',ADVANCE='NO') '! '
        call write_space_4_parse(id_monitor, level)
        write(id_monitor,'(3a)') 'Control for ', trim(label),           &
     &                           ' is included.'
!
      end subroutine write_included_message
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      end module write_control_elements
