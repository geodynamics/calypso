!>@file   t_control_array_character3.f90
!!        module t_control_array_character3
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Structure of control array input with 3 words
!!
!!@verbatim
!!      subroutine init_chara3_ctl_item_label(label, chara3_item)
!!      subroutine read_character3_ctl_type(c_buf, label, chara3_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara3_item), intent(inout) :: chara3_item
!!      subroutine write_character3_ctl_type                            &
!!     &         (id_file, level, chara3_item)
!!        type(read_chara3_item), intent(in) :: chara3_item
!!      subroutine copy_character3_ctl(org_c3, new_c3)
!!        type(read_chara3_item), intent(in) :: org_c3
!!        type(read_chara3_item), intent(inout) :: new_c3
!!
!!      subroutine alloc_control_array_c3(array_c3)
!!      subroutine dealloc_control_array_c3(array_c3)
!!      subroutine init_c3_ctl_array_label(label, array_c3)
!!      subroutine read_control_array_c3                                &
!!     &         (id_control, label, array_c3, c_buf)
!!        type(ctl_array_c3), intent(inout) :: array_c3
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_control_array_c3                               &
!!     &         (id_control, level, array_c3)
!!        type(ctl_array_c3), intent(in) :: array_c3
!!
!!      subroutine append_control_array_c3(read_c3, array_c3)
!!        type(read_chara3_item), intent(inout) ::    read_c3
!!        type(ctl_array_c3), intent(inout) :: array_c3
!!      subroutine copy_control_array_c3(num_copy, org_c3, tgt_c3)
!!        type(ctl_array_c3), intent(in) ::    org_c3
!!        type(ctl_array_c3), intent(inout) :: tgt_c3
!!      subroutine append_control_item_c3(read_c3, array_c3)
!!        type(read_chara3_item), intent(in) ::    read_c3
!!        type(ctl_array_c3), intent(inout) :: array_c3
!!@endverbatim
!!
!!
      module t_control_array_character3
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with three characters
      type read_chara3_item
!>        Item name
        character(len=kchara) :: item_name = 'char_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue(3)
      end type read_chara3_item
!
!>  Structure for three charactors control array 
      type ctl_array_c3
!>        Item name
        character(len=kchara) :: array_name = 'char_array'
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st character
        character(len=kchara), allocatable :: c1_tbl(:)
!>     array for 2nd character
        character(len=kchara), allocatable :: c2_tbl(:)
!>     array for 3rd character
        character(len=kchara), allocatable :: c3_tbl(:)
      end type ctl_array_c3
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine init_chara3_ctl_item_label(label, chara3_item)
      character(len=kchara), intent(in) :: label
      type(read_chara3_item), intent(inout) :: chara3_item
!
      chara3_item%item_name = trim(label)
      end subroutine init_chara3_ctl_item_label
!
! ----------------------------------------------------------------------
!
      subroutine read_character3_ctl_type(c_buf, label, chara3_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara3_item), intent(inout) :: chara3_item
!
       character(len=kchara) :: tmpchara
!
!
      if(chara3_item%iflag.gt.0) return
      chara3_item%item_name = trim(label)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara3_item%charavalue(1:3)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &      trim(c_buf%header_chara), ' 1: ', chara3_item%charavalue(1)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &      trim(c_buf%header_chara), ' 2: ', chara3_item%charavalue(2)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &      trim(c_buf%header_chara), ' 3: ', chara3_item%charavalue(3)
      chara3_item%iflag = 1
!
      end subroutine read_character3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_character3_ctl_type                              &
     &         (id_file, level, chara3_item)
!
      use write_control_items
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      type(read_chara3_item), intent(in) :: chara3_item
!
      integer(kind = kint) :: i
      integer(kind = kint) :: maxlen(0:2)
!
!
      if(chara3_item%iflag .eq. 0) return
!
      maxlen(0) = len_trim(chara3_item%item_name)
      do i = 1, 2
        maxlen(i) = len_trim(chara3_item%charavalue(i))                 &
     &           + iflag_divide(chara3_item%charavalue(i))
      end do
      call write_character3_ctl_item(id_file, level, maxlen,            &
     &    chara3_item%item_name, chara3_item%charavalue(1),             &
     &    chara3_item%charavalue(2), chara3_item%charavalue(3))
!
       end subroutine write_character3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_character3_ctl(org_c3, new_c3)
!
      type(read_chara3_item), intent(in) :: org_c3
      type(read_chara3_item), intent(inout) :: new_c3
!
!
      new_c3%item_name =       org_c3%item_name
      new_c3%iflag =           org_c3%iflag
      new_c3%charavalue(1:3) = org_c3%charavalue(1:3)
!
       end subroutine copy_character3_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c3(array_c3)
!
      type(ctl_array_c3), intent(inout) :: array_c3
!
!
      allocate( array_c3%c1_tbl(array_c3%num) )
      allocate( array_c3%c2_tbl(array_c3%num) )
      allocate( array_c3%c3_tbl(array_c3%num) )
!
      end subroutine alloc_control_array_c3
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c3(array_c3)
!
      type(ctl_array_c3), intent(inout) :: array_c3
!
!
      if(allocated(array_c3%c1_tbl) .eqv. .FALSE.) return
      deallocate(array_c3%c1_tbl, array_c3%c2_tbl, array_c3%c3_tbl)
      array_c3%num = 0
!
      end subroutine dealloc_control_array_c3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine init_c3_ctl_array_label(label, array_c3)
      character(len=kchara), intent(in) :: label
      type(ctl_array_c3), intent(inout) :: array_c3
!
      array_c3%array_name = trim(label)
      end subroutine init_c3_ctl_array_label
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c3                                  &
     &         (id_control, label, array_c3, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_c3), intent(inout) :: array_c3
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_chara3_item) :: read_c3
!
!
      if(array_c3%icou .gt. 0) return
      array_c3%array_name = trim(label)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!
      read_c3%iflag = 0
      array_c3%num =  0
      call alloc_control_array_c3(array_c3)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_character3_ctl_type(c_buf, label, read_c3)
          call append_control_array_c3(read_c3, array_c3)
        end if
      end do
!
      end subroutine read_control_array_c3
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c3                                 &
     &         (id_control, level, array_c3)
!
      use write_control_items
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_c3), intent(in) :: array_c3
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen(0:2)
      integer(kind = kint) :: i
!
!
      if(array_c3%num .le. 0) return
!
      maxlen(0) = len_trim(array_c3%array_name)
      maxlen(1) = max_len_of_charaarray(array_c3%num, array_c3%c1_tbl)
      maxlen(2) = max_len_of_charaarray(array_c3%num, array_c3%c2_tbl)
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_c3%array_name)
      do i = 1, array_c3%num
        call write_character3_ctl_item                                  &
     &     (id_control, level, maxlen, array_c3%array_name,             &
     &      array_c3%c1_tbl(i), array_c3%c2_tbl(i), array_c3%c3_tbl(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_c3%array_name)
!
      end subroutine write_control_array_c3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c3(read_c3, array_c3)
!
      type(read_chara3_item), intent(inout) ::    read_c3
      type(ctl_array_c3), intent(inout) :: array_c3
!
      type(ctl_array_c3) :: tmp_c3
!
!
      tmp_c3%num = array_c3%num
      call alloc_control_array_c3(tmp_c3)
      call copy_control_array_c3(tmp_c3%num, array_c3, tmp_c3)
      call dealloc_control_array_c3(array_c3)
!
      array_c3%num = tmp_c3%num + 1
      call alloc_control_array_c3(array_c3)
      call copy_control_array_c3(tmp_c3%num, tmp_c3, array_c3)
      call append_control_item_c3(read_c3, array_c3)
      read_c3%iflag = 0
!
      call dealloc_control_array_c3(tmp_c3)
!
      end subroutine append_control_array_c3
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c3(num_copy, org_c3, tgt_c3)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_c3), intent(in) ::    org_c3
      type(ctl_array_c3), intent(inout) :: tgt_c3
!
!
      tgt_c3%array_name = org_c3%array_name
      tgt_c3%icou =       org_c3%icou
!
      if(num_copy .le. 0) return
      tgt_c3%c1_tbl(1:num_copy) = org_c3%c1_tbl(1:num_copy)
      tgt_c3%c2_tbl(1:num_copy) = org_c3%c2_tbl(1:num_copy)
      tgt_c3%c3_tbl(1:num_copy) = org_c3%c3_tbl(1:num_copy)
!
      end subroutine copy_control_array_c3
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c3(read_c3, array_c3)
!
      type(read_chara3_item), intent(in) ::    read_c3
      type(ctl_array_c3), intent(inout) :: array_c3
!
!
      array_c3%icou = array_c3%icou + read_c3%iflag
      array_c3%c1_tbl(array_c3%num) = read_c3%charavalue(1)
      array_c3%c2_tbl(array_c3%num) = read_c3%charavalue(2)
      array_c3%c3_tbl(array_c3%num) = read_c3%charavalue(3)
!
      end subroutine append_control_item_c3
!
! -----------------------------------------------------------------------
!
      end module t_control_array_character3
