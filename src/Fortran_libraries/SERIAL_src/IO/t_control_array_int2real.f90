!>@file   t_control_array_int2real.f90
!!        module t_control_array_int2real
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read int-int-real control arrays
!!
!!@verbatim
!!      subroutine read_int2real_ctl_type(c_buf, label, i2r_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int2_real_item), intent(inout) :: i2r_item
!!      subroutine write_int2real_ctl_type                              &
!!     &         (id_file, level, maxlen, i2r_item)
!!        type(read_int2_real_item), intent(in) :: i2r_item
!!      subroutine copy_int2real_ctl(org_i2r, new_i2r)
!!        type(read_int2_real_item), intent(in) :: org_i2r
!!        type(read_int2_real_item), intent(inout) :: new_i2r
!!
!!      subroutine alloc_control_array_i2_r(array_i2r)
!!      subroutine dealloc_control_array_i2_r(array_i2r)
!!      subroutine read_control_array_i2_r                              &
!!     &         (id_control, label, array_i2r, c_buf)
!!        type(ctl_array_i2r), intent(inout) :: array_i2r
!!        type(buffer_for_control), intent(in)  :: c_buf
!!      subroutine write_control_array_i2_r                             &
!!     &         (id_control, level, array_i2r)
!!        type(ctl_array_i2r), intent(in) :: array_i2r
!!
!!      subroutine append_control_array_i2_r(read_i2r, array_i2r)
!!        type(read_int2_real_item), intent(inout) ::    read_i2r
!!        type(ctl_array_i2r), intent(inout) :: array_i2r
!!      subroutine copy_control_array_i2_r(num_copy, org_i2r, tgt_i2r)
!!        type(ctl_array_i2r), intent(in) ::    org_i2r
!!        type(ctl_array_i2r), intent(inout) :: tgt_i2r
!!      subroutine append_control_item_i2_r(read_i2r, array_i2r)
!!        type(read_int2_real_item), intent(in) ::    read_i2r
!!        type(ctl_array_i2r), intent(inout) :: array_i2r
!!@endverbatim
!!
!!
      module t_control_array_int2real
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with three characters
      type read_int2_real_item
!>        Item name
        character(len=kchara) :: item_name = 'integer_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue(2)
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_int2_real_item
!
!>  Structure for 1 real and 2 integers control array 
      type ctl_array_i2r
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
!>     array for 1st real
        real(kind = kreal), allocatable :: vect(:)
      end type ctl_array_i2r
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_int2real_ctl_type(c_buf, label, i2r_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int2_real_item), intent(inout) :: i2r_item
!
      character(len=kchara) :: tmpchara
!
!
      if(i2r_item%iflag.gt.0) return
      i2r_item%item_name = trim(label)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, i2r_item%intvalue(1:2),        &
     &                         i2r_item%realvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,2i16)')                   &
     &      trim(c_buf%header_chara), ' int:  ', i2r_item%intvalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1pe23.15)')               &
     &      trim(c_buf%header_chara), ' real: ', i2r_item%realvalue
      i2r_item%iflag = 1
!
      end subroutine read_int2real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_int2real_ctl_type                                &
     &         (id_file, level, maxlen, i2r_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level, maxlen
      type(read_int2_real_item), intent(in) :: i2r_item
!
!
      if(i2r_item%iflag .eq. 0) return
!
      call write_i2_r_ctl_item                                          &
    &    (id_file, level, maxlen, i2r_item%item_name,                   &
     &    i2r_item%intvalue(1),  i2r_item%intvalue(2),                  &
     &    i2r_item%realvalue)
!
       end subroutine write_int2real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_int2real_ctl(org_i2r, new_i2r)
!
      type(read_int2_real_item), intent(in) :: org_i2r
      type(read_int2_real_item), intent(inout) :: new_i2r
!
!
      new_i2r%item_name =     org_i2r%item_name
      new_i2r%iflag =         org_i2r%iflag
      new_i2r%intvalue(1:2) = org_i2r%intvalue(1:2)
      new_i2r%realvalue =     org_i2r%realvalue
!
       end subroutine copy_int2real_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_i2_r(array_i2r)
!
      type(ctl_array_i2r), intent(inout) :: array_i2r
!
!
      allocate( array_i2r%int1(array_i2r%num) )
      allocate( array_i2r%int2(array_i2r%num) )
      allocate( array_i2r%vect(array_i2r%num) )
!
      if(array_i2r%num .eq. 0) return
      array_i2r%int1 =     0
      array_i2r%int2 =     0
      array_i2r%vect = 0.0d0
!
      end subroutine alloc_control_array_i2_r
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_i2_r(array_i2r)
!
      type(ctl_array_i2r), intent(inout) :: array_i2r
!
!
      if(allocated(array_i2r%int1) .eqv. .FALSE.) return
      deallocate( array_i2r%int1, array_i2r%int2, array_i2r%vect)
      array_i2r%num = 0
!
      end subroutine dealloc_control_array_i2_r
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i2_r                                &
     &         (id_control, label, array_i2r, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_i2r), intent(inout) :: array_i2r
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_int2_real_item) :: read_i2r
!
!
      if(array_i2r%icou .gt. 0) return
      array_i2r%array_name = trim(label)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!
      read_i2r%iflag = 0
      array_i2r%num =  0
      call alloc_control_array_i2_r(array_i2r)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_int2real_ctl_type(c_buf, label, read_i2r)
          call append_control_array_i2_r(read_i2r, array_i2r)
        end if
      end do
!
      end subroutine read_control_array_i2_r
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i2_r                               &
     &         (id_control, level, array_i2r)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_i2r), intent(in) :: array_i2r
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(array_i2r%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_i2r%array_name)
      do i = 1, array_i2r%num
        call write_i2_r_ctl_item(id_control, level,                     &
     &      len_trim(array_i2r%array_name), array_i2r%array_name,       &
     &      array_i2r%int1(i), array_i2r%int2(i), array_i2r%vect(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_i2r%array_name)
!
      end subroutine write_control_array_i2_r
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_i2_r(read_i2r, array_i2r)
!
      type(read_int2_real_item), intent(inout) ::    read_i2r
      type(ctl_array_i2r), intent(inout) :: array_i2r
!
      type(ctl_array_i2r) ::    org_i2r
!
!
      org_i2r%num = array_i2r%num
      call alloc_control_array_i2_r(org_i2r)
      call copy_control_array_i2_r(org_i2r%num, array_i2r, org_i2r)
      call dealloc_control_array_i2_r(array_i2r)
!
      array_i2r%num = org_i2r%num + 1
      call alloc_control_array_i2_r(array_i2r)
      call copy_control_array_i2_r(org_i2r%num, org_i2r, array_i2r)
      call append_control_item_i2_r(read_i2r, array_i2r)
      read_i2r%iflag = 0
!
      call dealloc_control_array_i2_r(org_i2r)
!
      end subroutine append_control_array_i2_r
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_i2_r(num_copy, org_i2r, tgt_i2r)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_i2r), intent(in) ::    org_i2r
      type(ctl_array_i2r), intent(inout) :: tgt_i2r
!
!
      tgt_i2r%array_name = org_i2r%array_name
      tgt_i2r%icou =       org_i2r%icou
!
      if(num_copy .le. 0) return
      tgt_i2r%int1(1:num_copy) = org_i2r%int1(1:num_copy)
      tgt_i2r%int2(1:num_copy) = org_i2r%int2(1:num_copy)
      tgt_i2r%vect(1:num_copy) = org_i2r%vect(1:num_copy)
!
      end subroutine copy_control_array_i2_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_i2_r(read_i2r, array_i2r)
!
      type(read_int2_real_item), intent(in) ::    read_i2r
      type(ctl_array_i2r), intent(inout) :: array_i2r
!
!
      array_i2r%icou = array_i2r%icou + read_i2r%iflag
      array_i2r%int1(array_i2r%num) = read_i2r%intvalue(1)
      array_i2r%int2(array_i2r%num) = read_i2r%intvalue(2)
      array_i2r%vect(array_i2r%num) = read_i2r%realvalue
!
      end subroutine append_control_item_i2_r
!
! -----------------------------------------------------------------------
!
      end module t_control_array_int2real
