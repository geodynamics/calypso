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
!!     &                            (id_file, level, label)
!!      integer(kind = kint) function write_end_flag_for_ctl            &
!!     &                            (id_file, level, label)
!!      subroutine write_array_flag_for_ctl(id_file, level, label, num)
!!      subroutine write_end_array_flag_for_ctl(id_file, level, label)
!!
!!      subroutine write_real_ctl_item                                  &
!!     &         (id_file, level, maxlen, label, real_data)
!!      subroutine write_integer_ctl_item                               &
!!     &          (id_file, level, maxlen, label, int_data)
!!      subroutine write_character_ctl_item                             &
!!     &         (id_file, level, maxlen, label, chara_data)
!!
!!      subroutine write_real2_ctl_item                                 &
!!     &         (id_file, level, label, real1, real2)
!!      subroutine write_real3_ctl_item                                 &
!!     &         (id_file, level, label, real1, real2, real3)
!!       subroutine write_int_real_ctl_item                             &
!!     &          (id_file, level, label, int_data, real_data)
!!      subroutine write_integer2_ctl_item                              &
!!     &         (id_file, level, label, int1, int2)
!!      subroutine write_integer3_ctl_item                              &
!!     &         (id_file, level, label, int1, int2, int3)
!!       subroutine write_chara_real_ctl_item                           &
!!     &          (id_file, level, label, chara_data, real_data)
!!       subroutine write_chara_real2_ctl_item                          &
!!     &          (id_file, level, label, chara_data, real1, real2)
!!       subroutine write_chara2_real_ctl_item                          &
!!     &          (id_file, level, label, chara1, chara2, real_data)
!!       subroutine write_chara_int_ctl_item                            &
!!     &          (id_file, level, label, chara_data, int_data)
!!       subroutine write_i2_r_ctl_item                                 &
!!     &          (id_file, level, label, int1, int2, real_data)
!!       subroutine write_i2_r2_ctl_item                                &
!!     &          (id_file, level, label, int1, int2, real1, real2)
!!       subroutine write_i_c_r_ctl_item(id_file, level, label,         &
!!     &           int_data, chara_data, real_data)
!!      subroutine write_character2_ctl_item                            &
!!     &         (id_file, level, label, chara1, chara2)
!!      subroutine write_character3_ctl_item                            &
!!     &         (id_file, level, label, maxlen, chara1, chara2, chara3)
!!
!!      subroutine write_file_name_for_ctl_line                         &
!!     &         (id_file, level, label, fname)
!!      subroutine write_file_names_from_ctl_line                       &
!!     &         (id_file, level, label, num, fname)
!!
!!      subroutine write_control_array_int_list                         &
!!     &         (id_file, level, label, num, ivect)
!!      subroutine write_control_array_int_r_list                       &
!!     &         (id_file, level, label, num, ivect, vect)
!!      subroutine write_control_array_vect_list                        &
!!     &         (id_file, level, label, num, c_tbl, vect)
!!      subroutine write_control_array_int_v_list                       &
!!     &         (id_file, level, label, num, c_tbl, ivect)
!!      subroutine write_control_array_real_list                        &
!!     &         (id_file, level, label, num, vec1)
!!      subroutine write_control_array_real2_list                       &
!!     &         (id_file, level, label, num, vec1, vec2)
!!      subroutine write_control_array_real3_list                       &
!!     &         (id_file, level, label, num, vec1, vec2, vec3)
!!      subroutine write_control_array_chara_list                       &
!!     &         (id_file, level, label, num, c_tbl)
!!      subroutine write_control_array_chara2_list                      &
!!     &         (id_file, level, label, num, c1_tbl, c2_tbl)
!!      subroutine write_control_array_chara3_list                      &
!!     &         (id_file, level, label, num, c1_tbl, c2_tbl, c3_tbl)
!!      subroutine write_control_array_c_r2_list                        &
!!     &         (id_file, level, label, num, c_tbl,  vec1, vec2)
!!      subroutine write_control_array_c2_r_list                        &
!!     &         (id_file, level, label, num, c1_tbl, c2_tbl, vect)
!!      subroutine write_control_array_i_c_r_list                       &
!!     &         (id_file, level, label, num, ivect, c_tbl, vect)
!!      subroutine write_control_array_int2_list                        &
!!     &         (id_file, level, label, num, int1, int2)
!!      subroutine write_control_array_i2_r_list                        &
!!     &         (id_file, level, label, num, int1, int2, vect)
!!      subroutine write_control_array_i2_r2_list                       &
!!     &         (id_file, level, label, num, int1, int2, vec1, vec2)
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
      use skip_comment_f
!
      implicit none
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
     &                            (id_file, level, label)
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
     &                            (id_file, level, label)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
!
!
      if(level .le. 0) then
        write_end_flag_for_ctl = level
      else
        write_end_flag_for_ctl = level - 1
      end if
!
      call write_space_4_parse(id_file, (level-1))
      call write_ctl_chara_cont(id_file, hd_end)
      call write_ctl_chara_lf(id_file, label)
!
      end function write_end_flag_for_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_array_flag_for_ctl(id_file, level, label, num)
!
      integer(kind = kint), intent(in) :: id_file, level
      integer (kind=kint), intent(in) :: num
      character(len=kchara), intent(in) :: label
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, hd_array)
      call write_ctl_chara_cont(id_file, label)
      write(id_file,'(i6)') num
!
      end subroutine write_array_flag_for_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_end_array_flag_for_ctl(id_file, level, label)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, hd_end)
      call write_ctl_chara_cont(id_file, hd_array)
      call write_ctl_chara_lf(id_file, label)
!
      end subroutine write_end_array_flag_for_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_real_ctl_item                                    &
     &         (id_file, level, maxlen, label, real_data)
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      real(kind = kreal), intent(in) :: real_data
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label)
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
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(in) :: int_data
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label)
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
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara_data
!
      integer(kind = kint) :: nspace0
!
      nspace0 = maxlen - len_trim(label)
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
     &         (id_file, level, label, real1, real2)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      real(kind = kreal), intent(in) :: real1, real2
!
!
      call write_space_4_parse(id_file, level)
      write(id_file,'(a,a2,1p2E25.15e3)')                               &
     &            trim(label), '  ', real1, real2
!
       end subroutine write_real2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_real3_ctl_item                                   &
     &         (id_file, level, label, real1, real2, real3)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      real(kind = kreal), intent(in) :: real1, real2, real3
!
!
      call write_space_4_parse(id_file, level)
      write(id_file,'(a,a2,1p3E25.15e3)')                               &
     &            trim(label), '  ', real1, real2, real3
!
      end subroutine write_real3_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_int_real_ctl_item                                &
     &         (id_file, level, label, int_data, real_data)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(in) :: int_data
      real(kind = kreal), intent(in) :: real_data
!
!
      call write_space_4_parse(id_file, level)
      write(id_file,'(a,a2,i16,1pE25.15e3)')                            &
     &                       trim(label), '  ', int_data, real_data
!
      end subroutine write_int_real_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_integer2_ctl_item                                &
     &         (id_file, level, label, int1, int2)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(in) :: int1, int2
!
!
      call write_space_4_parse(id_file, level)
      write(id_file,'(a,a2,2i16)') trim(label), '  ', int1, int2
!
      end subroutine write_integer2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_integer3_ctl_item                                &
     &         (id_file, level, label, int1, int2, int3)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(in) :: int1, int2, int3
!
!
      call write_space_4_parse(id_file, level)
      write(id_file,'(a,a2,3i16)') trim(label), '  ', int1, int2, int3
!
      end subroutine write_integer3_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_chara_real_ctl_item                             &
     &          (id_file, level, label, chara_data, real_data)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara_data
      real(kind = kreal), intent(in) :: real_data
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_ctl_chara_cont(id_file, chara_data)
      write(id_file,'(1pE25.15e3)')  real_data
!
      end subroutine write_chara_real_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_chara_real2_ctl_item                            &
     &          (id_file, level, label, chara_data, real1, real2)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara_data
      real(kind = kreal), intent(in) :: real1, real2
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_ctl_chara_cont(id_file, chara_data)
      write(id_file,'(1p2E25.15e3)')  real1, real2
!
      end subroutine write_chara_real2_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_chara2_real_ctl_item                            &
     &          (id_file, level, label, chara1, chara2, real_data)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara1, chara2
      real(kind = kreal), intent(in) :: real_data
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_ctl_chara_cont(id_file, chara1)
      call write_ctl_chara_cont(id_file, chara2)
      write(id_file,'(1pE25.15e3)')  real_data
!
      end subroutine write_chara2_real_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_chara_int_ctl_item                              &
     &          (id_file, level, label, chara_data, int_data)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara_data
      integer (kind=kint), intent(in) :: int_data
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_ctl_chara_cont(id_file, chara_data)
      write(id_file,'(i16)')  int_data
!
      end subroutine write_chara_int_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_i2_r_ctl_item                                   &
     &          (id_file, level, label, int1, int2, real_data)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: int1, int2
      real(kind = kreal), intent(in) :: real_data
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      write(id_file,'(2i16,1pE25.15e3)') int1, int2, real_data
!
      end subroutine write_i2_r_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_i2_r2_ctl_item                                  &
     &          (id_file, level, label, int1, int2, real1, real2)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: int1, int2
      real(kind = kreal), intent(in) :: real1, real2
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      write(id_file,'(2i16,1p2E25.15e3)') int1, int2, real1, real2
!
      end subroutine write_i2_r2_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine write_i_c_r_ctl_item(id_file, level, label,           &
     &           int_data, chara_data, real_data)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: int_data
      character(len=kchara), intent(in) :: chara_data
      real(kind = kreal), intent(in) :: real_data
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      write(id_file,'(i16,a2,a,a2,1pE25.15e3)')                         &
     &       int_data, '  ', trim(chara_data), '  ', real_data
!
      end subroutine write_i_c_r_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_character2_ctl_item                              &
     &         (id_file, level, label, chara1, chara2)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: chara1, chara2
!
!
      call write_space_4_parse(id_file, level)
      call write_ctl_chara_cont(id_file, label)
      call write_ctl_chara_cont(id_file, chara1)
      call write_ctl_chara_lf(id_file, chara2)
!
      end subroutine write_character2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine write_character3_ctl_item                              &
     &         (id_file, level, label, maxlen, chara1, chara2, chara3)
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
!   --------------------------------------------------------------------
!
      subroutine write_file_name_for_ctl_line                           &
     &         (id_file, level, label, fname)
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
     &         (id_file, level, label, num, fname)
!
      integer(kind = kint), intent(in) :: id_file, level
      integer (kind=kint), intent(in) :: num
      character(len=kchara), intent(in) :: label
      character(len=kchara), intent(in) :: fname(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_file_name_for_ctl_line                               &
     &     (id_file, (level+1), label, fname(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_file_names_from_ctl_line
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_control_array_int_list                           &
     &         (id_file, level, label, num, ivect)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer (kind=kint), intent(in) :: ivect(num)
!
      integer(kind = kint) :: i, length
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        length = len_trim(label)
        call write_integer_ctl_item                                     &
     &     (id_file, (level+1), length, label, ivect(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_int_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_int_r_list                         &
     &         (id_file, level, label, num, ivect, vect)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer (kind=kint), intent(in) :: ivect(num)
      real (kind=kreal), intent(in) :: vect(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_int_real_ctl_item                                    &
     &     (id_file, (level+1), label, ivect(i), vect(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_int_r_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_vect_list                          &
     &         (id_file, level, label, num, c_tbl, vect)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: c_tbl(num)
      real (kind=kreal), intent(in) :: vect(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_chara_real_ctl_item                                  &
     &     (id_file, (level+1), label, c_tbl(i), vect(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_vect_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_int_v_list                         &
     &         (id_file, level, label, num, c_tbl, ivect)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: c_tbl(num)
      integer (kind=kint), intent(in) :: ivect(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_chara_int_ctl_item                                   &
     &     (id_file, (level+1), label, c_tbl(i), ivect(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_int_v_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_real_list                          &
     &         (id_file, level, label, num, vec1)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      real (kind=kreal), intent(in) :: vec1(num)
!
      integer(kind = kint) :: i, length
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        length = len_trim(label)
        call write_real_ctl_item                                        &
     &     (id_file, (level+1), length, label, vec1(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_real_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_real2_list                         &
     &         (id_file, level, label, num, vec1, vec2)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      real (kind=kreal), intent(in) :: vec1(num), vec2(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_real2_ctl_item                                       &
     &     (id_file, (level+1), label, vec1(i), vec2(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_real2_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_real3_list                         &
     &         (id_file, level, label, num, vec1, vec2, vec3)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      real (kind=kreal), intent(in) :: vec1(num), vec2(num)
      real (kind=kreal), intent(in) :: vec3(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_real3_ctl_item                                       &
     &     (id_file, (level+1), label, vec1(i), vec2(i), vec3(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_real3_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_chara_list                         &
     &         (id_file, level, label, num, c_tbl)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: c_tbl(num)
!
      integer(kind = kint) :: i, length
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        length = len_trim(label)
        call write_character_ctl_item                                   &
     &     (id_file, (level+1), length, label, c_tbl(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_chara_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_chara2_list                        &
     &         (id_file, level, label, num, c1_tbl, c2_tbl)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: c1_tbl(num), c2_tbl(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_character2_ctl_item                                  &
     &     (id_file, (level+1), label, c1_tbl(i), c2_tbl(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_chara2_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_chara3_list                        &
     &         (id_file, level, label, num, c1_tbl, c2_tbl, c3_tbl)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: c1_tbl(num), c2_tbl(num)
      character(len=kchara), intent(in) :: c3_tbl(num)
!
      integer(kind = kint) :: maxlen(0:2)
      integer(kind = kint) :: i
!
      maxlen(0) = len_trim(label)
      maxlen(1) = max_len_of_charaarray(num, c1_tbl)
      maxlen(2) = max_len_of_charaarray(num, c2_tbl)
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_character3_ctl_item(id_file, (level+1),              &
     &      label, maxlen, c1_tbl(i), c2_tbl(i), c3_tbl(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_chara3_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c_r2_list                          &
     &         (id_file, level, label, num, c_tbl,  vec1, vec2)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: c_tbl(num)
      real (kind=kreal), intent(in) :: vec1(num), vec2(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_chara_real2_ctl_item(id_file, (level+1),             &
     &      label, c_tbl(i), vec1(i), vec2(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_c_r2_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c2_r_list                          &
     &         (id_file, level, label, num, c1_tbl, c2_tbl, vect)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: c1_tbl(num), c2_tbl(num)
      real (kind=kreal), intent(in) :: vect(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_chara2_real_ctl_item(id_file, (level+1),             &
     &      label, c1_tbl(i), c2_tbl(i), vect(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_c2_r_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i_c_r_list                         &
     &         (id_file, level, label, num, ivect, c_tbl, vect)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: c_tbl(num)
      integer(kind = kint), intent(in) :: ivect(num)
      real (kind=kreal), intent(in) :: vect(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_i_c_r_ctl_item(id_file, (level+1),                   &
     &      label, ivect(i), c_tbl(i), vect(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_i_c_r_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_int2_list                          &
     &         (id_file, level, label, num, int1, int2)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int1(num), int2(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_integer2_ctl_item                                    &
     &     (id_file, (level+1), label, int1(i), int2(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_int2_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i2_r_list                          &
     &         (id_file, level, label, num, int1, int2, vect)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int1(num), int2(num)
      real (kind=kreal), intent(in) :: vect(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_i2_r_ctl_item                                        &
     &     (id_file, (level+1), label, int1(i), int2(i), vect(i))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_i2_r_list
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i2_r2_list                         &
     &         (id_file, level, label, num, int1, int2, vec1, vec2)
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int1(num), int2(num)
      real (kind=kreal), intent(in) :: vec1(num), vec2(num)
!
      integer(kind = kint) :: i
!
!
      call write_array_flag_for_ctl(id_file, level, label, num)
      do i = 1, num
        call write_i2_r2_ctl_item (id_file, (level+1), label,           &
     &     int1(i), int2(i), vec1(num), vec2(num))
      end do
      call write_end_array_flag_for_ctl(id_file, level, label)
!
      end subroutine write_control_array_i2_r2_list
!
!   --------------------------------------------------------------------
!
      end module write_control_elements
