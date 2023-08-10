!>@file   t_control_array_integer3.f90
!!        module t_control_array_integer3
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine read_integer3_ctl_type(c_buf, label, int3_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int3_item), intent(inout) :: int3_item
!!      subroutine write_integer3_ctl_type                              &
!!     &         (id_control, level, maxlen, int3_item)
!!        type(read_int3_item), intent(in) :: int3_item
!!      subroutine copy_integer3_ctl(org_i3, new_i3)
!!        type(read_int3_item), intent(in) :: org_i3
!!        type(read_int3_item), intent(inout) :: new_i3
!!
!!      subroutine alloc_control_array_i3(array_i3)
!!      subroutine dealloc_control_array_i3(array_i3)
!!      subroutine read_control_array_i3                                &
!!     &         (id_control, label, array_i3, c_buf)
!!        type(ctl_array_i3), intent(inout) :: array_i3
!!      subroutine write_control_array_i3                               &
!!     &         (id_control, level, array_i3)
!!        type(ctl_array_i3), intent(in) :: array_i3
!!
!!      subroutine append_control_array_i3(read_i3, array_i3)
!!        type(read_int3_item), intent(inout) ::    read_i3
!!        type(ctl_array_i3), intent(inout) :: array_i3
!!      subroutine dup_control_array_i3(org_i3, tgt_i3)
!!        type(ctl_array_i3), intent(in) ::    org_i3
!!        type(ctl_array_i3), intent(inout) :: tgt_i3
!!      subroutine copy_control_array_i3(num_copy, org_i3, tgt_i3)
!!        type(ctl_array_i3), intent(in) ::    org_i3
!!        type(ctl_array_i3), intent(inout) :: tgt_i3
!!      subroutine append_control_item_i3(read_i3, array_i3)
!!        type(read_int3_item), intent(in) ::    read_i3
!!        type(ctl_array_i3), intent(inout) :: array_i3
!!@endverbatim
!!
!!
      module t_control_array_integer3
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
!
      implicit none
!
!>        structure of control item with three integers
      type read_int3_item
!>        Item name
        character(len=kchara) :: item_name = 'integer_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue(3)
      end type read_int3_item
!
!>  Structure for 2 integers control array 
      type ctl_array_i3
!>        Item name
        character(len=kchara) :: array_name = 'integer_array'
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st integer
        integer(kind=kint), allocatable :: int1(:)
!>     array for 2nd integer
        integer(kind=kint), allocatable :: int2(:)
!>     array for 3rd integer
        integer(kind=kint), allocatable :: int3(:)
      end type ctl_array_i3
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_integer3_ctl_type(c_buf, label, int3_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int3_item), intent(inout) :: int3_item
!
       character(len=kchara) :: tmpchara
!
!
!
      if(int3_item%iflag.gt.0) return
      int3_item%item_name = trim(label)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, int3_item%intvalue(1:3)
      if (iflag_debug .gt. 0)  write(*,'(a,a2,3i6)')                    &
     &          trim(c_buf%header_chara), ': ', int3_item%intvalue(1:3)
      int3_item%iflag = 1
!
      end subroutine read_integer3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_integer3_ctl_type                                &
     &         (id_control, level, maxlen, int3_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level, maxlen
      type(read_int3_item), intent(in) :: int3_item
!
      if(int3_item%iflag .eq. 0) return
      call write_integer3_ctl_item(id_control, level, maxlen,           &
     &    int3_item%item_name, int3_item%intvalue(1),                   &
     &    int3_item%intvalue(2), int3_item%intvalue(3))
!
      end subroutine write_integer3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_integer3_ctl(org_i3, new_i3)
!
      type(read_int3_item), intent(in) :: org_i3
      type(read_int3_item), intent(inout) :: new_i3
!
      new_i3%item_name =     org_i3%item_name
      new_i3%iflag =         org_i3%iflag
      new_i3%intvalue(1:3) = org_i3%intvalue(1:3)
!
      end subroutine copy_integer3_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_i3(array_i3)
!
      type(ctl_array_i3), intent(inout) :: array_i3
!
!
      allocate( array_i3%int1(array_i3%num) )
      allocate( array_i3%int2(array_i3%num) )
      allocate( array_i3%int3(array_i3%num) )
!
      if(array_i3%num .eq. 0) return
      array_i3%int1 = 0
      array_i3%int2 = 0
      array_i3%int3 = 0
!
      end subroutine alloc_control_array_i3
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_i3(array_i3)
!
      type(ctl_array_i3), intent(inout) :: array_i3
!
!
      if(allocated(array_i3%int1) .eqv. .FALSE.) return
      deallocate(array_i3%int1, array_i3%int2, array_i3%int3)
      array_i3%num = 0
!
      end subroutine dealloc_control_array_i3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i3                                  &
     &         (id_control, label, array_i3, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_i3), intent(inout) :: array_i3
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_int3_item) :: read_i3
!
!
      if(array_i3%icou .gt. 0) return
      array_i3%array_name = trim(label)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!
      read_i3%iflag = 0
      array_i3%num =  0
      call alloc_control_array_i3(array_i3)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_integer3_ctl_type(c_buf, label, read_i3)
          call append_control_array_i3(read_i3, array_i3)
        end if
      end do
!
      end subroutine read_control_array_i3
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i3                                 &
     &         (id_control, level, array_i3)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_i3), intent(in) :: array_i3
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(array_i3%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_i3%array_name)
      do i = 1, array_i3%num
        call write_integer3_ctl_item(id_control, level,                 &
     &      len_trim(array_i3%array_name), array_i3%array_name,         &
     &      array_i3%int1(i), array_i3%int2(i), array_i3%int3(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_i3%array_name)
!
      end subroutine write_control_array_i3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_i3(read_i3, array_i3)
!
      type(read_int3_item), intent(inout) ::    read_i3
      type(ctl_array_i3), intent(inout) :: array_i3
!
      type(ctl_array_i3) ::    org_i3
!
!
      org_i3%num = array_i3%num
      call alloc_control_array_i3(org_i3)
      call copy_control_array_i3(org_i3%num, array_i3, org_i3)
      call dealloc_control_array_i3(array_i3)
!
      array_i3%num = org_i3%num + 1
      call alloc_control_array_i3(array_i3)
      call copy_control_array_i3(org_i3%num, org_i3, array_i3)
      call append_control_item_i3(read_i3, array_i3)
      read_i3%iflag = 0
!
      call dealloc_control_array_i3(org_i3)
!
      end subroutine append_control_array_i3
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_array_i3(org_i3, tgt_i3)
!
      type(ctl_array_i3), intent(in) ::    org_i3
      type(ctl_array_i3), intent(inout) :: tgt_i3
!
!
      tgt_i3%num = org_i3%num
      call alloc_control_array_i3(tgt_i3)
      call copy_control_array_i3(org_i3%num, org_i3, tgt_i3)
!
      end subroutine dup_control_array_i3
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_i3(num_copy, org_i3, tgt_i3)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_i3), intent(in) ::    org_i3
      type(ctl_array_i3), intent(inout) :: tgt_i3
!
      tgt_i3%array_name = org_i3%array_name
      tgt_i3%icou =       org_i3%icou
!
      if(num_copy .le. 0) return
      tgt_i3%int1(1:num_copy) = org_i3%int1(1:num_copy)
      tgt_i3%int2(1:num_copy) = org_i3%int2(1:num_copy)
      tgt_i3%int3(1:num_copy) = org_i3%int3(1:num_copy)
!
      end subroutine copy_control_array_i3
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_i3(read_i3, array_i3)
!
      type(read_int3_item), intent(in) ::    read_i3
      type(ctl_array_i3), intent(inout) :: array_i3
!
!
      array_i3%icou = array_i3%icou + read_i3%iflag
      array_i3%int1(array_i3%num) = read_i3%intvalue(1)
      array_i3%int2(array_i3%num) = read_i3%intvalue(2)
      array_i3%int3(array_i3%num) = read_i3%intvalue(3)
!
      end subroutine append_control_item_i3
!
! -----------------------------------------------------------------------
!
      end module t_control_array_integer3
