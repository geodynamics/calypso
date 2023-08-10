!>@file   t_control_array_character2.f90
!!        module t_control_array_character2
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read char-char control arrays
!!
!!@verbatim
!!      subroutine read_character2_ctl_type(c_buf, label, chara2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara2_item), intent(inout) :: chara2_item
!!      subroutine write_character2_ctl_type                            &
!!     &         (id_file, level, chara2_item)
!!        type(read_chara2_item), intent(in) :: chara2_item
!!      subroutine copy_character2_ctl(org_c2, new_c2)
!!        type(read_chara2_item), intent(in) :: org_c2
!!        type(read_chara2_item), intent(inout) :: new_c2
!!
!!      subroutine alloc_control_array_c2(array_c2)
!!      subroutine dealloc_control_array_c2(array_c2)
!!      subroutine init_chara2_ctl_array_label(label, array_c2)
!!      subroutine read_control_array_c2                                &
!!     &         (id_control, label, array_c2, c_buf)
!!        type(ctl_array_c2), intent(inout) :: array_c2
!!        type(buffer_for_control), intent(in)  :: c_buf
!!      subroutine write_control_array_c2                               &
!!     &         (id_control, level, array_c2)
!!        type(ctl_array_c2), intent(in) :: array_c2
!!
!!      subroutine append_control_array_c2(read_c2, array_c2)
!!        type(read_chara2_item), intent(inout) ::    read_c2
!!        type(ctl_array_c2), intent(inout) :: array_c2
!!      subroutine dup_control_array_c2(org_c2, tgt_c2)
!!      subroutine copy_control_array_c2(num_copy, org_c2, tgt_c2)
!!        type(ctl_array_c2), intent(in) ::    org_c2
!!        type(ctl_array_c2), intent(inout) :: tgt_c2
!!      subroutine append_control_item_c2(read_c2, array_c2)
!!        type(read_chara2_item), intent(in) ::    read_c2
!!        type(ctl_array_c2), intent(inout) :: array_c2
!!@endverbatim
!!
!!
      module t_control_array_character2
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with three characters
      type read_chara2_item
!>        Item name
        character(len=kchara) :: item_name = 'char_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue(2)
      end type read_chara2_item
!
!>  Structure for three charactors control array 
      type ctl_array_c2
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
      end type ctl_array_c2
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_character2_ctl_type(c_buf, label, chara2_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara2_item), intent(inout) :: chara2_item
!
       character(len=kchara) :: tmpchara
!
!
      if(chara2_item%iflag.gt.0) return
      chara2_item%item_name = trim(label)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara2_item%charavalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &      trim(c_buf%header_chara), ' 1: ', chara2_item%charavalue(1)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &      trim(c_buf%header_chara), ' 2: ', chara2_item%charavalue(2)
      chara2_item%iflag = 1
!
      end subroutine read_character2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_character2_ctl_type                              &
     &         (id_file, level, chara2_item)
!
      use m_constants
      use write_control_items
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      type(read_chara2_item), intent(in) :: chara2_item
!
      integer(kind = kint) :: maxlen(0:1)
!
      maxlen(0) = len_trim(chara2_item%item_name)
      maxlen(1)                                                         &
     &     = max_len_of_charaarray(ione, chara2_item%charavalue(1))
!
      if(chara2_item%iflag .eq. 0) return
      call write_character2_ctl_item                                    &
     &   (id_file, level, maxlen, chara2_item%item_name,                &
     &    chara2_item%charavalue(1), chara2_item%charavalue(2))
!
       end subroutine write_character2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_character2_ctl(org_c2, new_c2)
!
      type(read_chara2_item), intent(in) :: org_c2
      type(read_chara2_item), intent(inout) :: new_c2
!
!
      new_c2%item_name =      org_c2%item_name
      new_c2%iflag =           org_c2%iflag
      new_c2%charavalue(1:2) = org_c2%charavalue(1:2)
!
       end subroutine copy_character2_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c2(array_c2)
!
      type(ctl_array_c2), intent(inout) :: array_c2
!
!
      allocate( array_c2%c1_tbl(array_c2%num) )
      allocate( array_c2%c2_tbl(array_c2%num) )
!
      end subroutine alloc_control_array_c2
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c2(array_c2)
!
      type(ctl_array_c2), intent(inout) :: array_c2
!
!
      if(allocated(array_c2%c1_tbl) .eqv. .FALSE.) return
      deallocate(array_c2%c1_tbl, array_c2%c2_tbl)
      array_c2%num = 0
!
      end subroutine dealloc_control_array_c2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine init_chara2_ctl_array_label(label, array_c2)
      character(len=kchara), intent(in) :: label
      type(ctl_array_c2), intent(inout) :: array_c2
!
      array_c2%array_name = trim(label)
      end subroutine init_chara2_ctl_array_label
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c2                                  &
     &         (id_control, label, array_c2, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_c2), intent(inout) :: array_c2
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_chara2_item) :: read_c2
!
!
      if(array_c2%icou .gt. 0) return
      array_c2%array_name = trim(label)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!
      read_c2%iflag = 0
      array_c2%num =  0
      call alloc_control_array_c2(array_c2)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_character2_ctl_type(c_buf, label, read_c2)
          call append_control_array_c2(read_c2, array_c2)
        end if
      end do
!
      end subroutine read_control_array_c2
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c2                                 &
     &         (id_control, level, array_c2)
!
      use write_control_items
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_c2), intent(in) :: array_c2
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen(0:1)
      integer(kind = kint) :: i
!
!
      if(array_c2%num .le. 0) return
!
      maxlen(0) = len_trim(array_c2%array_name)
      maxlen(1) = max_len_of_charaarray(array_c2%num, array_c2%c1_tbl)
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_c2%array_name)
      do i = 1, array_c2%num
        call write_character2_ctl_item                                  &
     &     (id_control, level, maxlen, array_c2%array_name,             &
     &      array_c2%c1_tbl(i), array_c2%c2_tbl(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_c2%array_name)
!
      end subroutine write_control_array_c2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c2(read_c2, array_c2)
!
      type(read_chara2_item), intent(inout) ::    read_c2
      type(ctl_array_c2), intent(inout) :: array_c2
!
      type(ctl_array_c2) ::    org_c2
!
!
      org_c2%num = array_c2%num
      call alloc_control_array_c2(org_c2)
      call copy_control_array_c2(org_c2%num, array_c2, org_c2)
      call dealloc_control_array_c2(array_c2)
!
      array_c2%num = org_c2%num + 1
      call alloc_control_array_c2(array_c2)
      call copy_control_array_c2(org_c2%num, org_c2, array_c2)
      call append_control_item_c2(read_c2, array_c2)
      read_c2%iflag = 0
!
      call dealloc_control_array_c2(org_c2)
!
      end subroutine append_control_array_c2
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_array_c2(org_c2, tgt_c2)
!
      type(ctl_array_c2), intent(in) ::    org_c2
      type(ctl_array_c2), intent(inout) :: tgt_c2
!
!
      tgt_c2%num = org_c2%num
      call alloc_control_array_c2(tgt_c2)
      call copy_control_array_c2(org_c2%num, org_c2, tgt_c2)
!
      end subroutine dup_control_array_c2
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c2(num_copy, org_c2, tgt_c2)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_c2), intent(in) ::    org_c2
      type(ctl_array_c2), intent(inout) :: tgt_c2
!
!
      tgt_c2%array_name = org_c2%array_name
      tgt_c2%icou =       org_c2%icou
!
      if(num_copy .le. 0) return
      tgt_c2%c1_tbl(1:num_copy) = org_c2%c1_tbl(1:num_copy)
      tgt_c2%c2_tbl(1:num_copy) = org_c2%c2_tbl(1:num_copy)
!
      end subroutine copy_control_array_c2
!
! -----------------------------------------------------------------------

      subroutine append_control_item_c2(read_c2, array_c2)
!
      type(read_chara2_item), intent(in) ::    read_c2
      type(ctl_array_c2), intent(inout) :: array_c2
!
!
      array_c2%icou = array_c2%icou + read_c2%iflag
      array_c2%c1_tbl(array_c2%num) = read_c2%charavalue(1)
      array_c2%c2_tbl(array_c2%num) = read_c2%charavalue(2)
!
      end subroutine append_control_item_c2
!
! -----------------------------------------------------------------------
!
      end module t_control_array_character2
