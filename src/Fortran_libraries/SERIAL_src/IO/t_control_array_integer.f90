!>@file   t_control_array_integer.f90
!!        module t_control_array_integer
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine init_int_ctl_item_label(label, int_item)
!!      subroutine read_integer_ctl_type(c_buf, label, int_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_integer_item), intent(inout) :: int_item
!!      subroutine write_integer_ctl_type                               &
!!     &         (id_file, level, maxlen, int_item)
!!        type(read_integer_item), intent(in) :: int_item
!!      subroutine copy_integer_ctl(org_i1, new_i1)
!!        type(read_integer_item), intent(in) :: org_i1
!!        type(read_integer_item), intent(inout) :: new_i1
!!      logical function cmp_read_integer_item(i_item1, i_item2)
!!        type(read_integer_item), intent(in) :: i_item1, i_item2
!!
!!      subroutine alloc_control_array_int(array_int)
!!      subroutine dealloc_control_array_int(array_int)
!!      subroutine init_int_ctl_array_label(label, array_int)
!!      subroutine read_control_array_i1                                &
!!     &         (id_control, label, array_int, c_buf)
!!        type(ctl_array_int), intent(inout) :: array_int
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_control_array_i1                               &
!!     &         (id_control, level, array_int)
!!        type(ctl_array_int), intent(in) :: array_int
!!      logical function cmp_control_array_c1(i_array1, i_array2)
!!        type(ctl_array_int), intent(in) :: i_array1, i_array2
!!
!!      subroutine append_control_array_int(read_i1, array_i1)
!!        type(read_integer_item), intent(inout) ::    read_i1
!!        type(ctl_array_int), intent(inout) :: array_i1
!!      subroutine copy_control_array_int(num_copy, org_i1, tgt_i1)
!!        type(ctl_array_int), intent(in) ::    org_i1
!!        type(ctl_array_int), intent(inout) :: tgt_i1
!!      subroutine append_control_item_int(read_i1, array_i1)
!!        type(read_integer_item), intent(in) ::    read_i1
!!        type(ctl_array_int), intent(inout) :: array_i1
!!@endverbatim
!!
!!
      module t_control_array_integer
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control integer item
      type read_integer_item
!>        Item name
        character(len=kchara) :: item_name = 'Integer_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer item
        integer(kind = kint) ::  intvalue
      end type read_integer_item
!
!>  Structure for integer control array 
      type ctl_array_int
!>        Item name
        character(len=kchara) :: array_name = 'Integer_array'
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st real
        integer(kind = kint), allocatable :: ivec(:)
      end type ctl_array_int
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine init_int_ctl_item_label(label, int_item)
      character(len=kchara), intent(in) :: label
      type(read_integer_item), intent(inout) :: int_item
!
      int_item%item_name = trim(label)
      end subroutine init_int_ctl_item_label
!
!   --------------------------------------------------------------------
!
      subroutine read_integer_ctl_type(c_buf, label, int_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_integer_item), intent(inout) :: int_item
!
      character(len=kchara) :: tmpchara
!
!
      if(int_item%iflag .gt. 0) return
      call init_int_ctl_item_label(label, int_item)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, int_item%intvalue
      if (iflag_debug .gt. 0)  write(*,*) trim(c_buf%header_chara),     &
     &                        int_item%intvalue
      int_item%iflag = 1
!
      end subroutine read_integer_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_integer_ctl_type                                 &
     &         (id_file, level, maxlen, int_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      type(read_integer_item), intent(in) :: int_item
!
!
      if(int_item%iflag .eq. 0) return
      call write_integer_ctl_item(id_file, level, maxlen,               &
     &                           int_item%item_name, int_item%intvalue)
!
       end subroutine write_integer_ctl_type
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine copy_integer_ctl(org_i1, new_i1)
!
      type(read_integer_item), intent(in) :: org_i1
      type(read_integer_item), intent(inout) :: new_i1
!
!
       new_i1%item_name = org_i1%item_name
       new_i1%iflag =      org_i1%iflag
       new_i1%intvalue =   org_i1%intvalue
!
       end subroutine copy_integer_ctl
!
!   --------------------------------------------------------------------
!
      logical function cmp_read_integer_item(i_item1, i_item2)
!
      use skip_comment_f
!
      type(read_integer_item), intent(in) :: i_item1, i_item2
!
      cmp_read_integer_item = .FALSE.
      if(cmp_no_case(trim(i_item1%item_name),                           &
     &               trim(i_item2%item_name)) .eqv. .FALSE.) return
      if(i_item1%iflag .ne.    i_item2%iflag) return
      if(i_item1%iflag .gt. 0) then
        if(i_item1%intvalue .ne. i_item2%intvalue) return
      end if
!
      cmp_read_integer_item = .TRUE.
!
      end function cmp_read_integer_item
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_control_array_int(array_int)
!
      type(ctl_array_int), intent(inout) :: array_int
!
!
      allocate( array_int%ivec(array_int%num) )
!
      if(array_int%num .eq. 0) return
      array_int%ivec = 0
!
      end subroutine alloc_control_array_int
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_int(array_int)
!
      type(ctl_array_int), intent(inout) :: array_int
!
!
      if(allocated(array_int%ivec) .eqv. .FALSE.) return
      deallocate(array_int%ivec)
      array_int%num = 0
!
      end subroutine dealloc_control_array_int
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine init_int_ctl_array_label(label, array_int)
      character(len=kchara), intent(in) :: label
      type(ctl_array_int), intent(inout) :: array_int
!
      array_int%array_name = trim(label)
      end subroutine init_int_ctl_array_label
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i1                                  &
     &         (id_control, label, array_int, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_int), intent(inout) :: array_int
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_integer_item) :: read_i1
!
!
      if(array_int%icou .gt. 0) return
      call init_int_ctl_array_label(label, array_int)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!
      read_i1%iflag = 0
      array_int%num =  0
      call alloc_control_array_int(array_int)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_integer_ctl_type(c_buf, label, read_i1)
          call append_control_array_int(read_i1, array_int)
        end if
      end do
!
      end subroutine read_control_array_i1
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i1                                 &
     &         (id_control, level, array_int)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_int), intent(in) :: array_int
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i, length
!
!
      if(array_int%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_int%array_name)
      do i = 1, array_int%num
        length = len_trim(array_int%array_name)
        call write_integer_ctl_item(id_control, level, length,          &
     &      array_int%array_name, array_int%ivec(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_int%array_name)
!
      end subroutine write_control_array_i1
!
!   --------------------------------------------------------------------
!
      logical function cmp_control_array_c1(i_array1, i_array2)
!
      use skip_comment_f
!
      type(ctl_array_int), intent(in) :: i_array1, i_array2
      integer(kind = kint) :: i
!
      cmp_control_array_c1 = .FALSE.
      if(cmp_no_case(trim(i_array1%array_name),                         &
     &               trim(i_array2%array_name)) .eqv. .FALSE.) return
      if(i_array1%num .ne.  i_array2%num) return
      if(i_array1%icou .ne. i_array2%icou) return
      do i = 1, i_array1%num
        if(i_array1%ivec(i) .ne. i_array2%ivec(i)) return
      end do
      cmp_control_array_c1 = .TRUE.
!
      end function cmp_control_array_c1
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_int(read_i1, array_i1)
!
      type(read_integer_item), intent(inout) ::    read_i1
      type(ctl_array_int), intent(inout) :: array_i1
!
      type(ctl_array_int) ::    org_i1
!
!
      org_i1%num = array_i1%num
      call alloc_control_array_int(org_i1)
      call copy_control_array_int(org_i1%num, array_i1, org_i1)
      call dealloc_control_array_int(array_i1)
!
      array_i1%num = org_i1%num + 1
      call alloc_control_array_int(array_i1)
      call copy_control_array_int(org_i1%num, org_i1, array_i1)
      call append_control_item_int(read_i1, array_i1)
      read_i1%iflag = 0
!
      call dealloc_control_array_int(org_i1)
!
      end subroutine append_control_array_int
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_int(num_copy, org_i1, tgt_i1)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_int), intent(in) ::    org_i1
      type(ctl_array_int), intent(inout) :: tgt_i1
!
!
      tgt_i1%array_name = org_i1%array_name
      tgt_i1%icou =       org_i1%icou
!
      if(num_copy .le. 0) return
      tgt_i1%ivec(1:num_copy) = org_i1%ivec(1:num_copy)
!
      end subroutine copy_control_array_int
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_int(read_i1, array_i1)
!
      type(read_integer_item), intent(in) ::    read_i1
      type(ctl_array_int), intent(inout) :: array_i1
!
!
      array_i1%icou = array_i1%icou + read_i1%iflag
      array_i1%ivec(array_i1%num) = read_i1%intvalue
!
      end subroutine append_control_item_int
!
! -----------------------------------------------------------------------
!
      end module t_control_array_integer
