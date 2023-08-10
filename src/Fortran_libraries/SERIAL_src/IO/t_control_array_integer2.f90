!>@file   t_control_array_integer2.f90
!!        module t_control_array_integer2
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine init_integer2_ctl_item_label(label, int2_item)
!!      subroutine read_integer2_ctl_type(c_buf, label, int2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int2_item), intent(inout) :: int2_item
!!      subroutine write_integer2_ctl_type                              &
!!     &         (id_file, level, maxlen, int2_item)
!!        type(read_int2_item), intent(in) :: int2_item
!!      subroutine copy_integer2_ctl(org_i2, new_i2)
!!        type(read_int2_item), intent(in) :: org_i2
!!        type(read_int2_item), intent(inout) :: new_i2
!!
!!      subroutine alloc_control_array_i2(array_i2)
!!      subroutine dealloc_control_array_i2(array_i2)
!!      subroutine init_int2_ctl_array_label(label, array_i2)
!!      subroutine read_control_array_i2                                &
!!     &         (id_control, label, array_i2, c_buf)
!!        type(ctl_array_i2), intent(inout) :: array_i2
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_control_array_i2                               &
!!     &         (id_control, level, array_i2)
!!        type(ctl_array_i2), intent(in) :: array_i2
!!
!!      subroutine append_control_array_i2(read_i2, array_i2)
!!        type(read_int2_item), intent(inout) ::    read_i2
!!        type(ctl_array_i2), intent(inout) :: array_i2
!!      subroutine dup_control_array_i2(org_i2, tgt_i2)
!!        type(ctl_array_i2), intent(in) ::    org_i2
!!        type(ctl_array_i2), intent(inout) :: tgt_i2
!!      subroutine copy_control_array_i2(num_copy, org_i2, tgt_i2)
!!        type(ctl_array_i2), intent(in) ::    org_i2
!!        type(ctl_array_i2), intent(inout) :: tgt_i2
!!      subroutine append_control_item_i2(read_i2, array_i2)
!!        type(read_int2_item), intent(in) ::    read_i2
!!        type(ctl_array_i2), intent(inout) :: array_i2
!!@endverbatim
!!
!!
      module t_control_array_integer2
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control integer item
      type read_int2_item
!>        Item name
        character(len=kchara) :: item_name = 'integer_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer item
        integer(kind = kint) ::  intvalue(2)
      end type read_int2_item
!
!>  Structure for 2 integers control array 
      type ctl_array_i2
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
      end type ctl_array_i2
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine init_integer2_ctl_item_label(label, int2_item)
      character(len=kchara), intent(in) :: label
      type(read_int2_item), intent(inout) :: int2_item
!
      int2_item%item_name = trim(label)
      end subroutine init_integer2_ctl_item_label
!
!   --------------------------------------------------------------------
!
      subroutine read_integer2_ctl_type(c_buf, label, int2_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int2_item), intent(inout) :: int2_item
!
      character(len=kchara) :: tmpchara
!
!
      if(int2_item%iflag.gt.0) return
      int2_item%item_name = trim(label)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, int2_item%intvalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a2,2i6)')                    &
     &          trim(c_buf%header_chara), ': ', int2_item%intvalue(1:2)
      int2_item%iflag = 1
!
      end subroutine read_integer2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_integer2_ctl_type                                &
     &         (id_file, level, maxlen, int2_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      type(read_int2_item), intent(in) :: int2_item
!
!
      if(int2_item%iflag .eq. 0) return
      call write_integer2_ctl_item                                      &
     &   (id_file, level, maxlen, int2_item%item_name,                  &
     &    int2_item%intvalue(1), int2_item%intvalue(2))
!
       end subroutine write_integer2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_integer2_ctl(org_i2, new_i2)
!
      type(read_int2_item), intent(in) :: org_i2
      type(read_int2_item), intent(inout) :: new_i2
!
      new_i2%item_name =     org_i2%item_name
      new_i2%iflag =         org_i2%iflag
      new_i2%intvalue(1:2) = org_i2%intvalue(1:2)
!
      end subroutine copy_integer2_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_i2(array_i2)
!
      type(ctl_array_i2), intent(inout) :: array_i2
!
!
      allocate( array_i2%int1(array_i2%num) )
      allocate( array_i2%int2(array_i2%num) )
!
      if(array_i2%num .eq. 0) return
      array_i2%int1 = 0
      array_i2%int2 = 0
!
      end subroutine alloc_control_array_i2
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_i2(array_i2)
!
      type(ctl_array_i2), intent(inout) :: array_i2
!
!
      if(allocated(array_i2%int1) .eqv. .FALSE.) return
      deallocate(array_i2%int1, array_i2%int2)
      array_i2%num = 0
!
      end subroutine dealloc_control_array_i2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine init_int2_ctl_array_label(label, array_i2)
      character(len=kchara), intent(in) :: label
      type(ctl_array_i2), intent(inout) :: array_i2
!
      array_i2%array_name = trim(label)
      end subroutine init_int2_ctl_array_label
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i2                                  &
     &         (id_control, label, array_i2, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_i2), intent(inout) :: array_i2
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_int2_item) :: read_i2
!
!
      if(array_i2%icou .gt. 0) return
      array_i2%array_name = trim(label)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!
      read_i2%iflag = 0
      array_i2%num =  0
      call alloc_control_array_i2(array_i2)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_integer2_ctl_type(c_buf, label, read_i2)
          call append_control_array_i2(read_i2, array_i2)
        end if
      end do
!
      end subroutine read_control_array_i2
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i2                                 &
     &         (id_control, level, array_i2)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_i2), intent(in) :: array_i2
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(array_i2%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_i2%array_name)
      do i = 1, array_i2%num
        call write_integer2_ctl_item(id_control, level,                 &
     &      len_trim(array_i2%array_name), array_i2%array_name,         &
     &      array_i2%int1(i), array_i2%int2(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_i2%array_name)
!
      end subroutine write_control_array_i2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_i2(read_i2, array_i2)
!
      type(read_int2_item), intent(inout) ::    read_i2
      type(ctl_array_i2), intent(inout) :: array_i2
!
      type(ctl_array_i2) ::    org_i2
!
!
      org_i2%num = array_i2%num
      call alloc_control_array_i2(org_i2)
      call copy_control_array_i2(org_i2%num, array_i2, org_i2)
      call dealloc_control_array_i2(array_i2)
!
      array_i2%num = org_i2%num + 1
      call alloc_control_array_i2(array_i2)
      call copy_control_array_i2(org_i2%num, org_i2, array_i2)
      call append_control_item_i2(read_i2, array_i2)
      read_i2%iflag = 0
!
      call dealloc_control_array_i2(org_i2)
!
      end subroutine append_control_array_i2
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_array_i2(org_i2, tgt_i2)
!
      type(ctl_array_i2), intent(in) ::    org_i2
      type(ctl_array_i2), intent(inout) :: tgt_i2
!
!
      tgt_i2%num = org_i2%num
      call alloc_control_array_i2(tgt_i2)
      call copy_control_array_i2(org_i2%num, org_i2, tgt_i2)
!
      end subroutine dup_control_array_i2
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_i2(num_copy, org_i2, tgt_i2)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_i2), intent(in) ::    org_i2
      type(ctl_array_i2), intent(inout) :: tgt_i2
!
!
      tgt_i2%array_name = org_i2%array_name
      tgt_i2%icou =       org_i2%icou
!
      if(num_copy .le. 0) return
      tgt_i2%int1(1:num_copy) = org_i2%int1(1:num_copy)
      tgt_i2%int2(1:num_copy) = org_i2%int2(1:num_copy)
!
      end subroutine copy_control_array_i2
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_i2(read_i2, array_i2)
!
      type(read_int2_item), intent(in) ::    read_i2
      type(ctl_array_i2), intent(inout) :: array_i2
!
!
      array_i2%icou = array_i2%icou + read_i2%iflag
      array_i2%int1(array_i2%num) = read_i2%intvalue(1)
      array_i2%int2(array_i2%num) = read_i2%intvalue(2)
!
      end subroutine append_control_item_i2
!
! -----------------------------------------------------------------------
!
      end module t_control_array_integer2
