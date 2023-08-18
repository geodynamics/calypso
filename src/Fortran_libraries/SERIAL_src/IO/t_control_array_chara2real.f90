!>@file   t_control_array_chara2real.f90
!!        module t_control_array_chara2real
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read char-char-real control arrays
!!
!!@verbatim
!!      subroutine init_chara2real_ctl_item_label(label, c2r_item)
!!      subroutine read_char2real_ctl_type(c_buf, label, c2r_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara2_real_item), intent(inout) :: c2r_item
!!      subroutine write_char2real_ctl_type                             &
!!     &         (id_file, level, maxlen, c2r_item)
!!        type(read_chara2_real_item), intent(in) :: c2r_item
!!      subroutine copy_char2real_ctl(org_c2r, new_c2r)
!!        type(read_chara2_real_item), intent(in) :: org_c2r
!!        type(read_chara2_real_item), intent(inout) :: new_c2r
!!      logical function cmp_read_char2real_item(c2r_item1, c2r_item2)
!!        type(read_chara2_real_item), intent(in) :: c2r_item1
!!        type(read_chara2_real_item), intent(in) :: c2r_item2
!!
!!      subroutine alloc_control_array_c2_r(array_c2r)
!!      subroutine dealloc_control_array_c2_r(array_c2r)
!!      subroutine init_c2_r_ctl_array_label(label, array_c2r)
!!      subroutine read_control_array_c2_r                              &
!!     &         (id_control, label, array_c2r, c_buf)
!!        type(ctl_array_c2r), intent(inout) :: array_c2r
!!        type(buffer_for_control), intent(in)  :: c_buf
!!      subroutine write_control_array_c2_r                             &
!!     &         (id_control, level, array_c2r)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: label
!!        type(ctl_array_c2r), intent(in) :: array_c2r
!!        integer(kind = kint), intent(inout) :: level
!!      logical function cmp_control_array_c2_r(array1_c2r, array2_c2r)
!!        type(ctl_array_c2r), intent(in) :: array1_c2r, array2_c2r
!!
!!      subroutine append_control_array_c2_r(read_c2r, array_c2r)
!!        type(read_chara2_real_item), intent(inout) ::    read_c2r
!!        type(ctl_array_c2r), intent(inout) :: array_c2r
!!      subroutine dup_control_array_c2_r(org_c2r, tgt_c2r)
!!      subroutine copy_control_array_c2_r(num_copy, org_c2r, tgt_c2r)
!!        type(ctl_array_c2r), intent(in) ::    org_c2r
!!        type(ctl_array_c2r), intent(inout) :: tgt_c2r
!!      subroutine append_control_item_c2_r(read_c2r, array_c2r)
!!        type(read_chara2_real_item), intent(in) ::    read_c2r
!!        type(ctl_array_c2r), intent(inout) :: array_c2r
!!@endverbatim
!!
!!
      module t_control_array_chara2real
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with three characters
      type read_chara2_real_item
!>        Item name
        character(len=kchara) :: item_name = 'char_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue(2)
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_chara2_real_item
!
!>  Structure for two charactors and real control array 
      type ctl_array_c2r
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
        real(kind = kreal), allocatable :: vect(:)
      end type ctl_array_c2r
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine init_chara2real_ctl_item_label(label, c2r_item)
      character(len=kchara), intent(in) :: label
      type(read_chara2_real_item), intent(inout) :: c2r_item
!
      c2r_item%item_name = trim(label)
      end subroutine init_chara2real_ctl_item_label
!
! ----------------------------------------------------------------------
!
      subroutine read_char2real_ctl_type(c_buf, label, c2r_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara2_real_item), intent(inout) :: c2r_item
!
       character(len=kchara) :: tmpchara
!
!
      if(c2r_item%iflag.gt.0) return
      c2r_item%item_name = trim(label)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, c2r_item%charavalue(1:2),      &
     &                         c2r_item%realvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &         trim(c_buf%header_chara), ' 1: ', c2r_item%charavalue(1)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &         trim(c_buf%header_chara), ' 2: ', c2r_item%charavalue(2)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1pe23.15)')               &
     &         trim(c_buf%header_chara), ' real: ', c2r_item%realvalue
      c2r_item%iflag = 1
!
      end subroutine read_char2real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_char2real_ctl_type                               &
     &         (id_file, level, maxlen, c2r_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      type(read_chara2_real_item), intent(in) :: c2r_item
!
!
      if(c2r_item%iflag .eq. 0) return
!
      call write_chara2_real_ctl_item(id_file, level, maxlen,           &
     &    c2r_item%item_name, c2r_item%charavalue(1),                   &
     &    c2r_item%charavalue(2), c2r_item%realvalue)
!
       end subroutine write_char2real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_char2real_ctl(org_c2r, new_c2r)
!
      type(read_chara2_real_item), intent(in) :: org_c2r
      type(read_chara2_real_item), intent(inout) :: new_c2r
!
!
      new_c2r%item_name =        org_c2r%item_name
      new_c2r%iflag =            org_c2r%iflag
      new_c2r%charavalue(1:2) =  org_c2r%charavalue(1:2)
      new_c2r%realvalue =        org_c2r%realvalue
!
      end subroutine copy_char2real_ctl
!
!   --------------------------------------------------------------------
!
      logical function cmp_read_char2real_item(c2r_item1, c2r_item2)
!
      use skip_comment_f
!
      type(read_chara2_real_item), intent(in) :: c2r_item1
      type(read_chara2_real_item), intent(in) :: c2r_item2
!
      cmp_read_char2real_item = .FALSE.
      if(cmp_no_case(trim(c2r_item1%item_name),                         &
     &               trim(c2r_item2%item_name)) .eqv. .FALSE.) return
      if(c2r_item1%iflag .ne. c2r_item2%iflag) return
      if(c2r_item1%iflag .gt. 0) then
        if(cmp_no_case(trim(c2r_item1%charavalue(1)),                   &
     &             trim(c2r_item2%charavalue(1))) .eqv. .FALSE.) return
        if(cmp_no_case(trim(c2r_item1%charavalue(2)),                   &
     &             trim(c2r_item2%charavalue(2))) .eqv. .FALSE.) return
        if(c2r_item1%realvalue .ne. c2r_item2%realvalue) return
      end if
!
      cmp_read_char2real_item = .TRUE.
!
      end function cmp_read_char2real_item
!
! ----------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c2_r(array_c2r)
!
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
!
      allocate( array_c2r%c1_tbl(array_c2r%num) )
      allocate( array_c2r%c2_tbl(array_c2r%num) )
      allocate( array_c2r%vect(array_c2r%num) )
!
      if(array_c2r%num .eq. 0) return
      array_c2r%vect = 0.0d0
!
      end subroutine alloc_control_array_c2_r
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c2_r(array_c2r)
!
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
!
      if(allocated(array_c2r%c1_tbl) .eqv. .FALSE.) return
      deallocate( array_c2r%c1_tbl, array_c2r%c2_tbl, array_c2r%vect)
      array_c2r%num = 0
!
      end subroutine dealloc_control_array_c2_r
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine init_c2_r_ctl_array_label(label, array_c2r)
      character(len=kchara), intent(in) :: label
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
      array_c2r%array_name = trim(label)
      end subroutine init_c2_r_ctl_array_label
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c2_r                                &
     &         (id_control, label, array_c2r, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_c2r), intent(inout) :: array_c2r
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_chara2_real_item) :: read_c2r
!
!
      if(array_c2r%icou .gt. 0) return
      array_c2r%array_name = trim(label)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!
      read_c2r%iflag = 0
      array_c2r%num =  0
      call alloc_control_array_c2_r(array_c2r)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_char2real_ctl_type(c_buf, label, read_c2r)
          call append_control_array_c2_r(read_c2r, array_c2r)
        end if
      end do
!
      end subroutine read_control_array_c2_r
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c2_r                              &
     &         (id_control, level, array_c2r)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_c2r), intent(in) :: array_c2r
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(array_c2r%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_c2r%array_name)
      do i = 1, array_c2r%num
        call write_chara2_real_ctl_item(id_control, level,              &
     &     len_trim(array_c2r%array_name), array_c2r%array_name,        &
     &     array_c2r%c1_tbl(i), array_c2r%c2_tbl(i), array_c2r%vect(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_c2r%array_name)
!
      end subroutine write_control_array_c2_r
!
!   --------------------------------------------------------------------
!
      logical function cmp_control_array_c2_r(array1_c2r, array2_c2r)
!
      use skip_comment_f
!
      type(ctl_array_c2r), intent(in) :: array1_c2r, array2_c2r
      integer(kind = kint) :: i
!
      cmp_control_array_c2_r = .FALSE.
      if(cmp_no_case(trim(array1_c2r%array_name),                       &
     &               trim(array2_c2r%array_name)) .eqv. .FALSE.) return
      if(array1_c2r%num .ne.  array2_c2r%num) return
      if(array1_c2r%icou .ne. array2_c2r%icou) return
      do i = 1, array1_c2r%num
        if(cmp_no_case(trim(array1_c2r%c1_tbl(i)),                      &
     &                trim(array2_c2r%c1_tbl(i))) .eqv. .FALSE.) return
        if(cmp_no_case(trim(array1_c2r%c2_tbl(i)),                      &
     &                trim(array2_c2r%c2_tbl(i))) .eqv. .FALSE.) return
        if(array1_c2r%vect(i) .ne. array2_c2r%vect(i)) return
      end do
      cmp_control_array_c2_r = .TRUE.
!
      end function cmp_control_array_c2_r
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c2_r(read_c2r, array_c2r)
!
      type(read_chara2_real_item), intent(inout) ::    read_c2r
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
      type(ctl_array_c2r) ::    org_c2r
!
!
      org_c2r%num = array_c2r%num
      call alloc_control_array_c2_r(org_c2r)
      call copy_control_array_c2_r(org_c2r%num, array_c2r, org_c2r)
      call dealloc_control_array_c2_r(array_c2r)
!
      array_c2r%num = org_c2r%num + 1
      call alloc_control_array_c2_r(array_c2r)
      call copy_control_array_c2_r(org_c2r%num, org_c2r, array_c2r)
      call append_control_item_c2_r(read_c2r, array_c2r)
      read_c2r%iflag = 0
!
      call dealloc_control_array_c2_r(org_c2r)
!
      end subroutine append_control_array_c2_r
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_array_c2_r(org_c2r, tgt_c2r)
!
      type(ctl_array_c2r), intent(in) ::    org_c2r
      type(ctl_array_c2r), intent(inout) :: tgt_c2r
!
!
      tgt_c2r%num = org_c2r%num
      call alloc_control_array_c2_r(tgt_c2r)
      call copy_control_array_c2_r(org_c2r%num, org_c2r, tgt_c2r)
!
      end subroutine dup_control_array_c2_r
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c2_r(num_copy, org_c2r, tgt_c2r)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_c2r), intent(in) ::    org_c2r
      type(ctl_array_c2r), intent(inout) :: tgt_c2r
!
      tgt_c2r%array_name = org_c2r%array_name
      tgt_c2r%icou =       org_c2r%icou
!
      if(num_copy .le. 0) return
      tgt_c2r%c1_tbl(1:num_copy) = org_c2r%c1_tbl(1:num_copy)
      tgt_c2r%c2_tbl(1:num_copy) = org_c2r%c2_tbl(1:num_copy)
      tgt_c2r%vect(1:num_copy) =   org_c2r%vect(1:num_copy)
!
      end subroutine copy_control_array_c2_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c2_r(read_c2r, array_c2r)
!
      type(read_chara2_real_item), intent(in) ::    read_c2r
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
!
      array_c2r%icou = array_c2r%icou + read_c2r%iflag
      array_c2r%c1_tbl(array_c2r%num) = read_c2r%charavalue(1)
      array_c2r%c2_tbl(array_c2r%num) = read_c2r%charavalue(2)
      array_c2r%vect(array_c2r%num) =   read_c2r%realvalue
!
      end subroutine append_control_item_c2_r
!
! -----------------------------------------------------------------------
!
      end module t_control_array_chara2real
