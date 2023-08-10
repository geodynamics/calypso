!>@file   t_control_array_chara2int.f90
!!        module t_control_array_chara2int
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read char-char-int control arrays
!!
!!@verbatim
!!      subroutine init_chara2int_ctl_item_label(label, c2i_item)
!!      subroutine read_char2int_ctl_type(c_buf, label, c2i_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara2_int_item), intent(inout) :: c2i_item
!!      subroutine write_char2real_ctl_type                             &
!!     &         (id_file, level, maxlen, c2i_item)
!!        type(read_chara2_int_item), intent(in) :: c2i_item
!!      subroutine copy_char2int_ctl(org_c2i, new_c2i)
!!        type(read_chara2_int_item), intent(in) :: org_c2i
!!        type(read_chara2_int_item), intent(inout) :: new_c2i
!!      logical function cmp_read_char2int_item(c2i_item1, c2i_item2)
!!        type(read_chara2_int_item), intent(in) :: c2i_item1
!!        type(read_chara2_int_item), intent(in) :: c2i_item2
!!
!!      subroutine alloc_control_array_c2_i(array_c2i)
!!      subroutine dealloc_control_array_c2_i(array_c2i)
!!      subroutine init_c2_i_ctl_array_label(label, array_c2i)
!!      subroutine read_control_array_c2_i                              &
!!     &         (id_control, label, array_c2i, c_buf)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!        type(buffer_for_control), intent(in)  :: c_buf
!!      subroutine write_control_array_c2_i                             &
!!     &         (id_control, level, array_c2i)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: label
!!        type(ctl_array_c2i), intent(in) :: array_c2i
!!        integer(kind = kint), intent(inout) :: level
!!      logical function cmp_control_array_c2_i(array1_c2i, array2_c2i)
!!        type(ctl_array_c2i), intent(in) :: array1_c2i, array2_c2i
!!
!!      subroutine append_control_array_c2_i(read_c2i, array_c2i)
!!        type(read_chara2_int_item), intent(inout) ::    read_c2i
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!      subroutine dup_control_array_c2_i(org_c2i, tgt_c2i)
!!      subroutine copy_control_array_c2_i(num_copy, org_c2i, tgt_c2i)
!!        type(ctl_array_c2i), intent(in) ::    org_c2i
!!        type(ctl_array_c2i), intent(inout) :: tgt_c2i
!!      subroutine append_control_item_c2_i(read_c2i, array_c2i)
!!        type(read_chara2_int_item), intent(in) ::    read_c2i
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!!      subroutine append_c2i_to_ctl_array                              &
!!     &         (chara1_in, chara2_in, int_in, array_c2i)
!!        character(len = kchara), intent(in) ::  chara1_in, chara2_in
!!        integer(kind = kint), intent(in) ::  int_in
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!@endverbatim
!!
!!
      module t_control_array_chara2int
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with three characters
      type read_chara2_int_item
!>        Item name
        character(len=kchara) :: item_name = 'char_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue(2)
!>        array for read real item
        integer(kind = kint) ::   intvalue
      end type read_chara2_int_item
!
!>  Structure for two charactors and real control array 
      type ctl_array_c2i
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
!>     array for 1st real
        integer(kind = kint), allocatable :: ivec(:)
      end type ctl_array_c2i
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine init_chara2int_ctl_item_label(label, c2i_item)
      character(len=kchara), intent(in) :: label
      type(read_chara2_int_item), intent(inout) :: c2i_item
!
      c2i_item%item_name = trim(label)
      end subroutine init_chara2int_ctl_item_label
!
! ----------------------------------------------------------------------
!
      subroutine read_char2int_ctl_type(c_buf, label, c2i_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara2_int_item), intent(inout) :: c2i_item
!
       character(len=kchara) :: tmpchara
!
!
      if(c2i_item%iflag.gt.0) return
      c2i_item%item_name = trim(label)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, c2i_item%charavalue(1:2),      &
     &                         c2i_item%intvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &         trim(c_buf%header_chara), ' 1: ', c2i_item%charavalue(1)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &         trim(c_buf%header_chara), ' 2: ', c2i_item%charavalue(2)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,i16)')                    &
     &         trim(c_buf%header_chara), ' int: ', c2i_item%intvalue
      c2i_item%iflag = 1
!
      end subroutine read_char2int_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_char2real_ctl_type                               &
     &         (id_file, level, maxlen, c2i_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      type(read_chara2_int_item), intent(in) :: c2i_item
!
!
      if(c2i_item%iflag .eq. 0) return
!
      call write_chara2_int_ctl_item(id_file, level, maxlen,            &
     &    c2i_item%item_name, c2i_item%charavalue(1),                   &
     &    c2i_item%charavalue(2), c2i_item%intvalue)
!
       end subroutine write_char2real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_char2int_ctl(org_c2i, new_c2i)
!
      type(read_chara2_int_item), intent(in) :: org_c2i
      type(read_chara2_int_item), intent(inout) :: new_c2i
!
!
      new_c2i%item_name =       org_c2i%item_name
      new_c2i%iflag =           org_c2i%iflag
      new_c2i%charavalue(1:2) = org_c2i%charavalue(1:2)
      new_c2i%intvalue =        org_c2i%intvalue
!
      end subroutine copy_char2int_ctl
!
!   --------------------------------------------------------------------
!
      logical function cmp_read_char2int_item(c2i_item1, c2i_item2)
!
      use skip_comment_f
!
      type(read_chara2_int_item), intent(in) :: c2i_item1
      type(read_chara2_int_item), intent(in) :: c2i_item2
!
      cmp_read_char2int_item = .FALSE.
      if(cmp_no_case(trim(c2i_item1%item_name),                         &
     &               trim(c2i_item2%item_name)) .eqv. .FALSE.) return
      if(c2i_item1%iflag .ne. c2i_item2%iflag) return
      if(c2i_item1%iflag .gt. 0) then
        if(cmp_no_case(trim(c2i_item1%charavalue(1)),                   &
     &             trim(c2i_item2%charavalue(1))) .eqv. .FALSE.) return
        if(cmp_no_case(trim(c2i_item1%charavalue(2)),                   &
     &             trim(c2i_item2%charavalue(2))) .eqv. .FALSE.) return
        if(c2i_item1%intvalue .ne. c2i_item2%intvalue) return
      end if
!
      cmp_read_char2int_item = .TRUE.
!
      end function cmp_read_char2int_item
!
! ----------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c2_i(array_c2i)
!
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
!
      allocate( array_c2i%c1_tbl(array_c2i%num) )
      allocate( array_c2i%c2_tbl(array_c2i%num) )
      allocate( array_c2i%ivec(array_c2i%num) )
!
      if(array_c2i%num .eq. 0) return
      array_c2i%ivec = 0.0d0
!
      end subroutine alloc_control_array_c2_i
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c2_i(array_c2i)
!
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
!
      if(allocated(array_c2i%c1_tbl) .eqv. .FALSE.) return
      deallocate( array_c2i%c1_tbl, array_c2i%c2_tbl, array_c2i%ivec)
      array_c2i%num = 0
!
      end subroutine dealloc_control_array_c2_i
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine init_c2_i_ctl_array_label(label, array_c2i)
      character(len=kchara), intent(in) :: label
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = trim(label)
      end subroutine init_c2_i_ctl_array_label
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c2_i                                &
     &         (id_control, label, array_c2i, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_c2i), intent(inout) :: array_c2i
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_chara2_int_item) :: read_c2i
!
!
      if(array_c2i%icou .gt. 0) return
      array_c2i%array_name = trim(label)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!
      read_c2i%iflag = 0
      array_c2i%num =  0
      call alloc_control_array_c2_i(array_c2i)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_char2int_ctl_type(c_buf, label, read_c2i)
          call append_control_array_c2_i(read_c2i, array_c2i)
        end if
      end do
!
      end subroutine read_control_array_c2_i
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c2_i                              &
     &         (id_control, level, array_c2i)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_c2i), intent(in) :: array_c2i
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(array_c2i%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_c2i%array_name)
      do i = 1, array_c2i%num
        call write_chara2_int_ctl_item(id_control, level,               &
     &     len_trim(array_c2i%array_name), array_c2i%array_name,        &
     &     array_c2i%c1_tbl(i), array_c2i%c2_tbl(i), array_c2i%ivec(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_c2i%array_name)
!
      end subroutine write_control_array_c2_i
!
!   --------------------------------------------------------------------
!
      logical function cmp_control_array_c2_i(array1_c2i, array2_c2i)
!
      use skip_comment_f
!
      type(ctl_array_c2i), intent(in) :: array1_c2i, array2_c2i
      integer(kind = kint) :: i
!
      cmp_control_array_c2_i = .FALSE.
      if(cmp_no_case(trim(array1_c2i%array_name),                       &
     &               trim(array2_c2i%array_name)) .eqv. .FALSE.) return
      if(array1_c2i%num .ne.  array2_c2i%num) return
      if(array1_c2i%icou .ne. array2_c2i%icou) return
      do i = 1, array1_c2i%num
        if(cmp_no_case(trim(array1_c2i%c1_tbl(i)),                      &
     &                trim(array2_c2i%c1_tbl(i))) .eqv. .FALSE.) return
        if(cmp_no_case(trim(array1_c2i%c2_tbl(i)),                      &
     &                trim(array2_c2i%c2_tbl(i))) .eqv. .FALSE.) return
        if(array1_c2i%ivec(i) .ne. array2_c2i%ivec(i)) return
      end do
      cmp_control_array_c2_i = .TRUE.
!
      end function cmp_control_array_c2_i
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c2_i(read_c2i, array_c2i)
!
      type(read_chara2_int_item), intent(inout) ::    read_c2i
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      type(ctl_array_c2i) ::    org_c2i
!
!
      org_c2i%num = array_c2i%num
      call alloc_control_array_c2_i(org_c2i)
      call copy_control_array_c2_i(org_c2i%num, array_c2i, org_c2i)
      call dealloc_control_array_c2_i(array_c2i)
!
      array_c2i%num = org_c2i%num + 1
      call alloc_control_array_c2_i(array_c2i)
      call copy_control_array_c2_i(org_c2i%num, org_c2i, array_c2i)
      call append_control_item_c2_i(read_c2i, array_c2i)
      read_c2i%iflag = 0
!
      call dealloc_control_array_c2_i(org_c2i)
!
      end subroutine append_control_array_c2_i
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_array_c2_i(org_c2i, tgt_c2i)
!
      type(ctl_array_c2i), intent(in) ::    org_c2i
      type(ctl_array_c2i), intent(inout) :: tgt_c2i
!
!
      tgt_c2i%num = org_c2i%num
      call alloc_control_array_c2_i(tgt_c2i)
      call copy_control_array_c2_i(org_c2i%num, org_c2i, tgt_c2i)
!
      end subroutine dup_control_array_c2_i
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c2_i(num_copy, org_c2i, tgt_c2i)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_c2i), intent(in) ::    org_c2i
      type(ctl_array_c2i), intent(inout) :: tgt_c2i
!
!
      tgt_c2i%array_name = org_c2i%array_name
      tgt_c2i%icou =       org_c2i%icou
!
      if(num_copy .le. 0) return
      tgt_c2i%c1_tbl(1:num_copy) = org_c2i%c1_tbl(1:num_copy)
      tgt_c2i%c2_tbl(1:num_copy) = org_c2i%c2_tbl(1:num_copy)
      tgt_c2i%ivec(1:num_copy) =   org_c2i%ivec(1:num_copy)
!
      end subroutine copy_control_array_c2_i
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c2_i(read_c2i, array_c2i)
!
      type(read_chara2_int_item), intent(in) ::    read_c2i
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
!
      array_c2i%icou = array_c2i%icou + read_c2i%iflag
      array_c2i%c1_tbl(array_c2i%num) = read_c2i%charavalue(1)
      array_c2i%c2_tbl(array_c2i%num) = read_c2i%charavalue(2)
      array_c2i%ivec(array_c2i%num) =   read_c2i%intvalue
!
      end subroutine append_control_item_c2_i
!
! -----------------------------------------------------------------------
!
      subroutine append_c2i_to_ctl_array                                &
     &         (chara1_in, chara2_in, int_in, array_c2i)
!
      character(len = kchara), intent(in) ::  chara1_in, chara2_in
      integer(kind = kint), intent(in) ::  int_in
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      type(read_chara2_int_item) :: read_c2i
!
      read_c2i%item_name = '   '
      read_c2i%iflag =     1
      read_c2i%charavalue(1) = trim(chara1_in)
      read_c2i%charavalue(2) = trim(chara2_in)
      read_c2i%intvalue =      int_in
!
      call append_control_array_c2_i(read_c2i, array_c2i)
!
      end subroutine append_c2i_to_ctl_array
!
! ----------------------------------------------------------------------
!
      end module t_control_array_chara2int
