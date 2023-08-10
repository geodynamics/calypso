!>@file   t_control_array_character.f90
!!        module t_control_array_character
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read character control arrays
!!
!!@verbatim
!!      subroutine init_chara_ctl_item_label(label, chara_item)
!!      subroutine read_chara_ctl_type(c_buf, label, chara_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_character_item), intent(inout) :: chara_item
!!      subroutine write_chara_ctl_type                                 &
!!     &         (id_file, level, maxlen, chara_item)
!!        type(read_character_item), intent(in) :: chara_item
!!      subroutine copy_chara_ctl(org_c1, new_c1)
!!        type(read_character_item), intent(in) :: org_c1
!!        type(read_character_item), intent(inout) :: new_c1
!!      logical function cmp_read_chara_item(c_item1, c_item2)
!!        type(read_character_item), intent(in) :: c_item1, c_item2
!!
!!      subroutine alloc_control_array_chara(array_chara)
!!      subroutine dealloc_control_array_chara(array_chara)
!!      subroutine init_chara_ctl_array_label(label, array_chara)
!!      subroutine read_control_array_c1                                &
!!     &         (id_control, label, array_chara, c_buf)
!!        type(ctl_array_chara), intent(inout) :: array_chara
!!        type(buffer_for_control), intent(in)  :: c_buf
!!      subroutine write_control_array_c1                               &
!!     &         (id_control, level, array_chara)
!!        type(ctl_array_chara), intent(in) :: array_chara
!!      logical function cmp_control_array_c1(c_array1, c_array2)
!!        type(ctl_array_chara), intent(in) :: c_array1, c_array2
!!
!!      subroutine append_control_array_c1(read_c1, array_c1)
!!        type(read_character_item), intent(inout) ::    read_c1
!!        type(ctl_array_chara), intent(inout) :: array_c1
!!      subroutine dup_control_array_c1(org_c1, tgt_c1)
!!      subroutine copy_control_array_c1(num_copy, org_c1, tgt_c1)
!!        type(ctl_array_chara), intent(in) ::    org_c1
!!        type(ctl_array_chara), intent(inout) :: tgt_c1
!!      subroutine append_control_item_c1(read_c1, array_c1)
!!        type(read_character_item), intent(in) ::    read_c1
!!        type(ctl_array_chara), intent(inout) :: array_c1
!!
!!      subroutine append_c_to_ctl_array(chara_in, array_c1)
!!        character(len = kchara), intent(in) ::  chara_in
!!        type(ctl_array_chara), intent(inout) :: array_c1
!!@endverbatim
!!
!!
      module t_control_array_character
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control character item
      type read_character_item
!>        Item name
        character(len=kchara) :: item_name = 'Chara_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character item
        character(len=kchara) :: charavalue
      end type read_character_item
!
!>  Structure for character control array 
      type ctl_array_chara
!>        Item name
        character(len=kchara) :: array_name = 'Chara_item'
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st character
        character(len=kchara), allocatable :: c_tbl(:)
      end type ctl_array_chara
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_chara_ctl_item_label(label, chara_item)
      character(len=kchara), intent(in) :: label
      type(read_character_item), intent(inout) :: chara_item
!
      chara_item%item_name = trim(label)
      end subroutine init_chara_ctl_item_label
!
! ----------------------------------------------------------------------
!
      subroutine read_chara_ctl_type(c_buf, label, chara_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_character_item), intent(inout) :: chara_item
!
      character(len=kchara) :: tmpchara
!
!
      if(chara_item%iflag .gt. 0) return
      call init_chara_ctl_item_label(label, chara_item)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara_item%charavalue
      if (iflag_debug .gt. 0)  write(*,*) trim(c_buf%header_chara),     &
     &                       ': ', trim(chara_item%charavalue)
      chara_item%iflag = 1
!
      end subroutine read_chara_ctl_type
!
! ----------------------------------------------------------------------
!
      subroutine write_chara_ctl_type                                   &
     &         (id_file, level, maxlen, chara_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      type(read_character_item), intent(in) :: chara_item
!
!
      if(chara_item%iflag .eq. 0) return
      call write_character_ctl_item(id_file, level, maxlen,             &
     &    chara_item%item_name, chara_item%charavalue)
!
      end subroutine write_chara_ctl_type
!
! ----------------------------------------------------------------------
!
      subroutine copy_chara_ctl(org_c1, new_c1)
!
      type(read_character_item), intent(in) :: org_c1
      type(read_character_item), intent(inout) :: new_c1
!
!
      new_c1%item_name = org_c1%item_name
      new_c1%iflag =      org_c1%iflag
      new_c1%charavalue = org_c1%charavalue
!
      end subroutine copy_chara_ctl
!
! ----------------------------------------------------------------------
!
      logical function cmp_read_chara_item(c_item1, c_item2)
!
      use skip_comment_f
!
      type(read_character_item), intent(in) :: c_item1, c_item2
!
      cmp_read_chara_item = .FALSE.
      if(cmp_no_case(trim(c_item1%item_name),                           &
     &               trim(c_item2%item_name)) .eqv. .FALSE.) return
      if(c_item1%iflag .ne. c_item2%iflag) return
      if(c_item1%iflag .gt. 0) then
        if(cmp_no_case(trim(c_item1%charavalue),                        &
     &                 trim(c_item2%charavalue)) .eqv. .FALSE.) return
      end if
!
      cmp_read_chara_item = .TRUE.
!
      end function cmp_read_chara_item
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_control_array_chara(array_chara)
!
      type(ctl_array_chara), intent(inout) :: array_chara
!
!
      allocate( array_chara%c_tbl(array_chara%num) )
!
      end subroutine alloc_control_array_chara
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_control_array_chara(array_chara)
!
      type(ctl_array_chara), intent(inout) :: array_chara
!
!
      if(allocated(array_chara%c_tbl) .eqv. .FALSE.) return
      deallocate(array_chara%c_tbl)
      array_chara%num = 0
!
      end subroutine dealloc_control_array_chara
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_chara_ctl_array_label(label, array_chara)
      character(len=kchara), intent(in) :: label
      type(ctl_array_chara), intent(inout) :: array_chara
!
      array_chara%array_name = trim(label)
      end subroutine init_chara_ctl_array_label
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c1                                  &
     &         (id_control, label, array_chara, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_chara), intent(inout) :: array_chara
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_character_item) :: read_c1
!
!
      if(array_chara%icou .gt. 0) return
      call init_chara_ctl_array_label(label, array_chara)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!
      read_c1%iflag = 0
      array_chara%num =  0
      call alloc_control_array_chara(array_chara)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_chara_ctl_type(c_buf, label, read_c1)
          call append_control_array_c1(read_c1, array_chara)
        end if
      end do
!
      end subroutine read_control_array_c1
!
! ----------------------------------------------------------------------
!
      subroutine write_control_array_c1                                 &
     &         (id_control, level, array_chara)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_chara), intent(in) :: array_chara
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i, length
!
!
      if(array_chara%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_chara%array_name)
      do i = 1, array_chara%num
        length = len_trim(array_chara%array_name)
        call write_character_ctl_item(id_control, level, length,        &
     &      array_chara%array_name, array_chara%c_tbl(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_chara%array_name)
!
      end subroutine write_control_array_c1
!
! ----------------------------------------------------------------------
!
      logical function cmp_control_array_c1(c_array1, c_array2)
!
      use skip_comment_f
!
      type(ctl_array_chara), intent(in) :: c_array1, c_array2
      integer(kind = kint) :: i
!
      cmp_control_array_c1 = .FALSE.
      if(cmp_no_case(trim(c_array1%array_name),                         &
     &               trim(c_array2%array_name)) .eqv. .FALSE.) return
      if(c_array1%num .ne.  c_array2%num) return
      if(c_array1%icou .ne. c_array2%icou) return
      do i = 1, c_array1%num
        if(cmp_no_case(trim(c_array1%c_tbl(i)),                         &
     &                 trim(c_array2%c_tbl(i))) .eqv. .FALSE.) return
      end do
      cmp_control_array_c1 = .TRUE.
!
      end function cmp_control_array_c1
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine append_control_array_c1(read_c1, array_c1)
!
      type(read_character_item), intent(inout) ::    read_c1
      type(ctl_array_chara), intent(inout) :: array_c1
!
      type(ctl_array_chara) ::    org_c1
!
!
      org_c1%num = array_c1%num
      call alloc_control_array_chara(org_c1)
      call copy_control_array_c1(org_c1%num, array_c1, org_c1)
      call dealloc_control_array_chara(array_c1)
!
      array_c1%num = org_c1%num + 1
      call alloc_control_array_chara(array_c1)
      call copy_control_array_c1(org_c1%num, org_c1, array_c1)
      call append_control_item_c1(read_c1, array_c1)
      read_c1%iflag = 0
!
      call dealloc_control_array_chara(org_c1)
!
      end subroutine append_control_array_c1
!
! ----------------------------------------------------------------------
!
      subroutine dup_control_array_c1(org_c1, tgt_c1)
!
      type(ctl_array_chara), intent(in) ::    org_c1
      type(ctl_array_chara), intent(inout) :: tgt_c1
!
!
      tgt_c1%num = org_c1%num
      call alloc_control_array_chara(tgt_c1)
      call copy_control_array_c1(org_c1%num, org_c1, tgt_c1)
!
      end subroutine dup_control_array_c1
!
! ----------------------------------------------------------------------
!
      subroutine copy_control_array_c1(num_copy, org_c1, tgt_c1)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_chara), intent(in) ::    org_c1
      type(ctl_array_chara), intent(inout) :: tgt_c1
!
!
      tgt_c1%array_name = org_c1%array_name
      tgt_c1%icou =       org_c1%icou
!
      if(num_copy .le. 0) return
      tgt_c1%c_tbl(1:num_copy) = org_c1%c_tbl(1:num_copy)
!
      end subroutine copy_control_array_c1
!
! ----------------------------------------------------------------------
!
      subroutine append_control_item_c1(read_c1, array_c1)
!
      type(read_character_item), intent(in) ::    read_c1
      type(ctl_array_chara), intent(inout) :: array_c1
!
!
      array_c1%icou = array_c1%icou + read_c1%iflag
      array_c1%c_tbl(array_c1%num) = read_c1%charavalue
!
      end subroutine append_control_item_c1
!
! ----------------------------------------------------------------------
!
      subroutine append_c_to_ctl_array(chara_in, array_c1)
!
      character(len = kchara), intent(in) ::  chara_in
      type(ctl_array_chara), intent(inout) :: array_c1
      type(read_character_item) :: read_c1
!
      read_c1%item_name = '   '
      read_c1%iflag =     1
      read_c1%charavalue = trim(chara_in)
!
      call append_control_array_c1(read_c1, array_c1)
!
      end subroutine append_c_to_ctl_array
!
! ----------------------------------------------------------------------
!
      end module t_control_array_character
