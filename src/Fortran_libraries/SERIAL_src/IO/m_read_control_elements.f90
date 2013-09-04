!>@file   m_read_control_elements.f90
!!@brief  module m_read_control_elements
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2005
!
!>@brief  Subroutines to read control data
!!
!!@verbatim
!!      integer function right_begin_flag(ctl_name)
!!      integer function right_file_flag(ctl_name)
!!
!!      subroutine find_control_end_flag(ctl_name, iflag_end)
!!      subroutine find_control_array_flag(ctl_name, num_array)
!!      subroutine find_control_end_array_flag(ctl_name, num, iflag_end)
!!
!!      subroutine read_real_ctl_item(label, iflag_dat, real_data)
!!      subroutine read_integer_ctl_item(label, iflag_dat, int_data)
!!      subroutine read_character_ctl_item(label, iflag_dat,            &
!!     &           chara_data)
!!
!!      subroutine read_real2_ctl_item(label, iflag_dat, real_data)
!!      subroutine read_real3_ctl_item(label, iflag_dat, real_data)
!!      subroutine read_integer3_ctl_item(label, iflag_dat, int_data)
!!      subroutine read_character3_ctl_item(label, iflag_dat,           &
!!     &           chara_data)
!!
!!      subroutine read_file_names_from_ctl_line(num, icou, fname)
!!
!!      subroutine read_control_array_int_list(label, num, icou, ivect)
!!      subroutine read_control_array_int_r_list(label, num, icou,      &
!!     &          ivect, vect)
!!      subroutine read_control_array_vect_list(label, num, icou,       &
!!     &          c_tbl, vect)
!!      subroutine read_control_array_int_v_list(label, num, icou,      &
!!     &          c_tbl, ivect)
!!      subroutine read_control_array_real_list(label, num, icou,       &
!!     &          vec1)
!!      subroutine read_control_array_real2_list(label, num, icou,      &
!!     &          vec1, vec2)
!!      subroutine read_control_array_real3_list(label, num, icou,      &
!!     &          vec1, vec2, vec3)
!!      subroutine read_control_array_chara_list(label, num, icou, c_tbl)
!!      subroutine read_control_array_chara2_list(label, num, icou,     &
!!     &          c1_tbl, c2_tbl)
!!      subroutine read_control_array_chara3_list(label, num, icou,     &
!!     &          c1_tbl, c2_tbl, c3_tbl)
!!      subroutine read_control_array_char_r_list(label, num, icou,     &
!!     &          c_tbl, vec1)
!!      subroutine read_control_array_c_r2_list(label, num, icou, c_tbl,&
!!     &          vec1, vec2)
!!      subroutine read_control_array_c2_r_list(label, num, icou,       &
!!     &          c1_tbl, c2_tbl, vect)
!!      subroutine read_control_array_i_c_r_list(label, num, icou,      &
!!     &          ivect, c_tbl, vect)
!!      subroutine read_control_array_int2_list(label, num, icou, int1, &
!!     &          int2)
!!      subroutine read_control_array_i2_r_list(label, num, icou, int1, &
!!     &          int2, vect)
!!      subroutine read_control_array_i2_r2_list(label, num, icou, int1,&
!!     &          int2, vec1, vec2)
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
      module m_read_control_elements
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>   control file id
      integer (kind=kint) :: ctl_file_code = 11
!
!>   character for read label
       character(len=kchara), private  :: header_chara
!
!>   temporal character for reading line
       character(len=255), private :: character_4_read
!
      private :: read_integers_from_line, read_int_reals_from_line
      private :: read_vector_from_line, read_int_vect_from_line
      private :: read_control_array_real,  read_control_array_chara
      private :: read_control_array_real2, read_control_array_chara2
      private :: read_control_array_real3, read_control_array_chara3
      private :: read_control_array_chara_real
      private :: read_control_array_chara_real2
      private :: read_control_array_chara2_real
      private :: read_control_array_int_ch_re
      private :: read_control_array_int2, read_control_array_int2_real
      private :: read_control_array_int2_real2
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine load_ctl_label_and_line
!
      use skip_comment_f
!
!
      call skip_comment(character_4_read, ctl_file_code)
      read(character_4_read,*) header_chara
!
      end subroutine load_ctl_label_and_line
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer function right_begin_flag(ctl_name)
!
      character(len=kchara), intent(in) :: ctl_name
      character(len=kchara)  :: tmpchara, item_name
!
!
      right_begin_flag = 0
      if (   header_chara.eq.'begin' .or. header_chara.eq.'Begin'       &
     &  .or. header_chara.eq.'BEGIN') then
        read(character_4_read,*) tmpchara, item_name
        if(item_name .eq. ctl_name) right_begin_flag = 1
      end if
!
!      write(*,*) 'header_chara: ', trim(header_chara)
!      write(*,*) right_begin_flag, trim(ctl_name)
!
      end function right_begin_flag
!
!   --------------------------------------------------------------------
!
      integer function right_file_flag(ctl_name)
!
      character(len=kchara), intent(in) :: ctl_name
      character(len=kchara)  :: tmpchara, item_name
!
!
      right_file_flag = 0
      if (   header_chara.eq.'file' .or. header_chara.eq.'File'         &
     &  .or. header_chara.eq.'FILE') then
        read(character_4_read,*) tmpchara, item_name
        if(item_name .eq. ctl_name) right_file_flag = 1
      end if
!
      end function right_file_flag
!
!   --------------------------------------------------------------------
!
      subroutine find_control_end_flag(ctl_name, iflag_end)
!
      character(len=kchara), intent(in) :: ctl_name
      integer(kind = kint), intent(inout) :: iflag_end
!
      character(len=kchara)  :: tmpchara, item_name
!
!
      if(    header_chara.eq.'end' .or. header_chara.eq.'End'           &
     &  .or. header_chara.eq.'END') then
        read(character_4_read,*) tmpchara, item_name
        if(item_name .eq. ctl_name) iflag_end = 1
      end if 
!
      end subroutine find_control_end_flag
!
!   --------------------------------------------------------------------
!
      subroutine find_control_array_flag(ctl_name, num_array)
!
      character(len=kchara), intent(in) :: ctl_name
      integer(kind = kint), intent(inout) :: num_array
!
      character(len=kchara)  :: tmpchara, item_name
!
!
!      write(*,*) 'header_chara: ', num_array, trim(header_chara)
      if(num_array .gt. 0) return
!
      if(    header_chara.eq.'array' .or. header_chara.eq.'Array'       &
     &  .or. header_chara.eq.'ARRAY') then
        read(character_4_read,*) tmpchara, item_name
!        write(*,*) trim(item_name),': ', trim(ctl_name)
        if(item_name .ne. ctl_name) return
!
        read(character_4_read,*) tmpchara, item_name, num_array
!        write(*,*) 'num_array: ', num_array
     end if
!
      end subroutine find_control_array_flag
!
!   --------------------------------------------------------------------
!
      subroutine find_control_end_array_flag(ctl_name, num, iflag_end)
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: ctl_name
      integer(kind = kint), intent(inout) :: iflag_end
!
      character(len=kchara)  :: tmpchara, array_name, item_name
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(    header_chara.eq.'end' .or. header_chara.eq.'End'           &
     &  .or. header_chara.eq.'END') then
        read(character_4_read,*) tmpchara, array_name
        if(    array_name.eq.'array' .or. array_name.eq.'Array'         &
     &    .or. array_name.eq.'ARRAY') then
          read(character_4_read,*) tmpchara, array_name, item_name
          if(item_name .eq. ctl_name) iflag = 1
        end if
      end if
!
      if(iflag .gt. 0) then
        if(iflag_end .lt. num) then
           write(*,*) 'number of array is not enough!'
           stop
        end if
      else
        if(iflag_end .ge. num) then
          write(*,*) ' array should be finished!!'
          iflag_end = num-1
        end if
      end if
!
      end subroutine find_control_end_array_flag
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_real_ctl_item(label, iflag_dat, real_data)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      real(kind = kreal), intent(inout) :: real_data
!
!
      if(iflag_dat.gt.0 .or. header_chara.ne.label) return
!
      read(character_4_read,*) header_chara, real_data
      if (iflag_debug .gt. 0)  write(*,*) trim(header_chara), real_data
      iflag_dat = 1
!
      end subroutine read_real_ctl_item
!
!   --------------------------------------------------------------------
!
       subroutine read_integer_ctl_item(label, iflag_dat, int_data)
!
       character(len=kchara), intent(in) :: label
       integer (kind=kint), intent(inout) :: iflag_dat
       integer (kind=kint), intent(inout) :: int_data
!
!
      if(iflag_dat.gt.0 .or. header_chara.ne.label) return
!
      read(character_4_read,*) header_chara, int_data
      if (iflag_debug .gt. 0)  write(*,*) trim(header_chara), int_data
      iflag_dat = 1
!
       end subroutine read_integer_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_character_ctl_item(label, iflag_dat,              &
     &           chara_data)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara_data
!
!
      if(iflag_dat.gt.0 .or. header_chara.ne.label) return
!
      read(character_4_read,*) header_chara, chara_data
      if (iflag_debug .gt. 0)  write(*,*) trim(header_chara), ': ',     &
     &                                    trim(chara_data)
      iflag_dat = 1
!
       end subroutine read_character_ctl_item
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_real2_ctl_item(label, iflag_dat, real_data)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      real(kind = kreal), intent(inout) :: real_data(2)
!
!
      if(iflag_dat.gt.0 .or. header_chara.ne.label) return
!
      read(character_4_read,*) header_chara, real_data(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a2,1p3e16.7)')               &
     &            trim(header_chara), ': ', real_data(1:2)
      iflag_dat = 1
!
       end subroutine read_real2_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_real3_ctl_item(label, iflag_dat, real_data)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      real(kind = kreal), intent(inout) :: real_data(3)
!
!
      if(iflag_dat.gt.0 .or. header_chara.ne.label) return
!
      read(character_4_read,*) header_chara, real_data(1:3)
      if (iflag_debug .gt. 0)  write(*,'(a,a2,1p3e16.7)')               &
     &            trim(header_chara), ': ', real_data(1:3)
      iflag_dat = 1
!
      end subroutine read_real3_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_integer3_ctl_item(label, iflag_dat, int_data)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      integer (kind=kint), intent(inout) :: int_data(3)
!
!
      if(iflag_dat.gt.0 .or. header_chara.ne.label) return
!
      read(character_4_read,*) header_chara, int_data(1:3)
      if (iflag_debug .gt. 0)  write(*,'(a,a2,3i6)')                    &
     &            trim(header_chara), ': ', int_data(1:3)
      iflag_dat = 1
!
      end subroutine read_integer3_ctl_item
!
!   --------------------------------------------------------------------
!
      subroutine read_character3_ctl_item(label, iflag_dat,             &
     &           chara_data)
!
      character(len=kchara), intent(in) :: label
      integer (kind=kint), intent(inout) :: iflag_dat
      character(len=kchara), intent(inout) :: chara_data(3)
!
!
      if(iflag_dat.gt.0 .or. header_chara.ne.label) return
!
      read(character_4_read,*) header_chara, chara_data(1:3)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(header_chara), ' 1: ', chara_data(1)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(header_chara), ' 2: ', chara_data(2)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(header_chara), ' 3: ', chara_data(3)
      iflag_dat = 1
!
       end subroutine read_character3_ctl_item
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
       subroutine read_file_names_from_ctl_line(num, icou, fname)
!
       integer (kind=kint), intent(in) :: num
       integer (kind=kint), intent(inout) :: icou
       character(len=kchara), intent(inout) :: fname(num)
!
       character(len=kchara) :: tmpchara
!
!
       if(icou .ge. num) return
       icou = icou + 1
       read(character_4_read,*) header_chara, tmpchara, fname(icou)
!
       end subroutine read_file_names_from_ctl_line
!
!   --------------------------------------------------------------------
!
      subroutine read_integers_from_line(num, icou, ivect)
!
      integer (kind=kint), intent(in) :: num
      integer (kind=kint), intent(inout) :: icou
      integer (kind=kint), intent(inout) :: ivect(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, ivect(icou)
!
      end subroutine read_integers_from_line
!
!   --------------------------------------------------------------------
!
      subroutine read_int_reals_from_line(num, icou, ivect, vect)
!
      integer (kind=kint), intent(in) :: num
      integer (kind=kint), intent(inout) :: icou
      integer (kind=kint), intent(inout) :: ivect(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, ivect(icou), vect(icou)
!
      end subroutine read_int_reals_from_line
!
!   --------------------------------------------------------------------
!
      subroutine read_vector_from_line(num, icou, c_tbl, vect)
!
      integer (kind=kint), intent(in) :: num
      integer (kind=kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, c_tbl(icou), vect(icou)
!
      end subroutine read_vector_from_line
!
!   --------------------------------------------------------------------
!
      subroutine read_int_vect_from_line(num, icou, c_tbl, ivect)
!
      integer (kind=kint), intent(in) :: num
      integer (kind=kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      integer (kind=kint), intent(inout) :: ivect(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, c_tbl(icou), ivect(icou)
!
      end subroutine read_int_vect_from_line
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_real(num, icou, vec1)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      real (kind=kreal), intent(inout) :: vec1(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, vec1(icou)
!
      end subroutine read_control_array_real
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_real2(num, icou, vec1, vec2)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      real (kind=kreal), intent(inout) :: vec1(num), vec2(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, vec1(icou), vec2(icou)
!
      end subroutine read_control_array_real2
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_real3(num, icou, vec1, vec2, vec3)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      real (kind=kreal), intent(inout) :: vec1(num), vec2(num)
      real (kind=kreal), intent(inout) :: vec3(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara,                            &
     &                          vec1(icou), vec2(icou), vec3(icou)
!
      end subroutine read_control_array_real3
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_chara(num, icou, c_tbl)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, c_tbl(icou)
!
      end subroutine read_control_array_chara
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_chara2(num, icou, chara1, chara2)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: chara1(num), chara2(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara,                            &
     &                           chara1(icou), chara2(icou)
!
      end subroutine read_control_array_chara2
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_chara3(num, icou, chara1, chara2,   &
     &          chara3)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: chara1(num), chara2(num)
      character(len=kchara), intent(inout) :: chara3(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, chara1(icou),              &
     &                           chara2(icou), chara3(icou)
!
      end subroutine read_control_array_chara3
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_chara_real(num, icou, c_tbl, vec1)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      real (kind=kreal), intent(inout) :: vec1(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, c_tbl(icou), vec1(icou)
!
      end subroutine read_control_array_chara_real
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_chara_real2(num, icou, c_tbl,       &
     &          vec1, vec2)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      real (kind=kreal), intent(inout) :: vec1(num), vec2(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, c_tbl(icou),               &
     &                           vec1(icou), vec2(icou)
!
      end subroutine read_control_array_chara_real2
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_chara2_real(num, icou,              &
     &          chara1, chara2, vect)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: chara1(num), chara2(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara,                            &
     &                         chara1(icou), chara2(icou), vect(icou)
!
      end subroutine read_control_array_chara2_real
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_int_ch_re(num, icou,                &
     &          ivect, c_tbl, vect)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      integer (kind = kint), intent(inout) :: ivect(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara,                            &
     &                         ivect(icou), c_tbl(icou), vect(icou)
!
      end subroutine read_control_array_int_ch_re
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_int2(num, icou, int1, int2)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: int1(num), int2(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, int1(icou), int2(icou)
!
      end subroutine read_control_array_int2
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_int2_real(num, icou, int1, int2,    &
     &          vect)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: int1(num), int2(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, int1(icou), int2(icou),    &
     &                          vect(icou)
!
       end subroutine read_control_array_int2_real
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_int2_real2(num, icou, int1, int2,   &
     &        vec1, vec2)
!
      integer (kind = kint), intent(in) :: num
      integer (kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: int1(num), int2(num)
      real (kind=kreal), intent(inout) :: vec1(num), vec2(num)
!
!
      if(icou .ge. num) return
      icou = icou + 1
      read(character_4_read,*) header_chara, int1(icou), int2(icou),    &
     &                          vec1(icou), vec2(icou)
!
       end subroutine read_control_array_int2_real2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_int_list(label, num, icou, ivect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      integer (kind=kint), intent(inout) :: ivect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_integers_from_line(num, icou, ivect)
        end if
      end do
!
      end subroutine read_control_array_int_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_int_r_list(label, num, icou,        &
     &          ivect, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      integer (kind=kint), intent(inout) :: ivect(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_int_reals_from_line(num, icou, ivect, vect)
        end if
      end do
!
      end subroutine read_control_array_int_r_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_vect_list(label, num, icou,         &
     &          c_tbl, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_vector_from_line(num, icou, c_tbl, vect)
        end if
      end do
!
      end subroutine read_control_array_vect_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_int_v_list(label, num, icou,        &
     &          c_tbl, ivect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      integer (kind=kint), intent(inout) :: ivect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_int_vect_from_line(num, icou, c_tbl, ivect)
        end if
      end do
!
      end subroutine read_control_array_int_v_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_real_list(label, num, icou,        &
     &          vec1)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      real (kind=kreal), intent(inout) :: vec1(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_real(num, icou, vec1)
        end if
      end do
!
      end subroutine read_control_array_real_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_real2_list(label, num, icou,        &
     &          vec1, vec2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      real (kind=kreal), intent(inout) :: vec1(num), vec2(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_real2(num, icou, vec1, vec2)
        end if
      end do
!
      end subroutine read_control_array_real2_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_real3_list(label, num, icou,        &
     &          vec1, vec2, vec3)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      real (kind=kreal), intent(inout) :: vec1(num), vec2(num)
      real (kind=kreal), intent(inout) :: vec3(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_real3(num, icou, vec1, vec2, vec3)
        end if
      end do
!
      end subroutine read_control_array_real3_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_chara_list(label, num, icou, c_tbl)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_chara(num, icou, c_tbl)
        end if
      end do
!
      end subroutine read_control_array_chara_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_chara2_list(label, num, icou,       &
     &          c1_tbl, c2_tbl)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c1_tbl(num), c2_tbl(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_chara2(num, icou, c1_tbl, c2_tbl)
        end if
      end do
!
      end subroutine read_control_array_chara2_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_chara3_list(label, num, icou,       &
     &          c1_tbl, c2_tbl, c3_tbl)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c1_tbl(num), c2_tbl(num)
      character(len=kchara), intent(inout) :: c3_tbl(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_chara3(num, icou, c1_tbl, c2_tbl,     &
     &        c3_tbl)
        end if
      end do
!
      end subroutine read_control_array_chara3_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_char_r_list(label, num, icou,       &
     &          c_tbl, vec1)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      real (kind=kreal), intent(inout) :: vec1(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_chara_real(num, icou, c_tbl, vec1)
        end if
      end do
!
      end subroutine read_control_array_char_r_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c_r2_list(label, num, icou, c_tbl,  &
     &          vec1, vec2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      real (kind=kreal), intent(inout) :: vec1(num), vec2(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_chara_real2(num, icou, c_tbl,         &
     &          vec1, vec2)
        end if
      end do
!
      end subroutine read_control_array_c_r2_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c2_r_list(label, num, icou,         &
     &          c1_tbl, c2_tbl, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c1_tbl(num), c2_tbl(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_chara2_real(num, icou,                &
     &        c1_tbl, c2_tbl, vect)
        end if
      end do
!
      end subroutine read_control_array_c2_r_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i_c_r_list(label, num, icou,        &
     &          ivect, c_tbl, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      character(len=kchara), intent(inout) :: c_tbl(num)
      integer(kind = kint), intent(inout) :: ivect(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_int_ch_re(num, icou,                  &
     &        ivect, c_tbl, vect)
        end if
      end do
!
      end subroutine read_control_array_i_c_r_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_int2_list(label, num, icou, int1,   &
     &          int2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: int1(num), int2(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_int2(num, icou, int1, int2)
        end if
      end do
!
      end subroutine read_control_array_int2_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i2_r_list(label, num, icou, int1,   &
     &          int2, vect)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: int1(num), int2(num)
      real (kind=kreal), intent(inout) :: vect(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_int2_real(num, icou, int1, int2,      &
     &        vect)
        end if
      end do
!
      end subroutine read_control_array_i2_r_list
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i2_r2_list(label, num, icou, int1,  &
     &          int2, vec1, vec2)
!
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: int1(num), int2(num)
      real (kind=kreal), intent(inout) :: vec1(num), vec2(num)
!
      if (icou .gt. 0) return
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(label, num, icou)
        if(icou .eq. num) exit
!
        if(header_chara.eq.label) then
          call read_control_array_int2_real2(num, icou, int1, int2,     &
     &        vec1, vec2)
        end if
      end do
!
      end subroutine read_control_array_i2_r2_list
!
!   --------------------------------------------------------------------
!
      end module m_read_control_elements
