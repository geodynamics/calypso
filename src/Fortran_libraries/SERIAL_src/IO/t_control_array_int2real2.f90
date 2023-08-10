!>@file   t_control_array_int2real2.f90
!!        module t_control_array_int2real2
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read int-int-real-real control arrays
!!
!!@verbatim
!!      subroutine read_int2real2_ctl_type(c_buf, label, i2r2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int2_real2_item), intent(inout) :: i2r2_item
!!      subroutine write_int2real2_ctl_type                             &
!!     &         (id_file, level, maxlen, i2r2_item)
!!        type(read_int2_real2_item), intent(in) :: i2r2_item
!!      subroutine copy_int2real2_ctl(org_i2r2, new_i2r2)
!!        type(read_int2_real2_item), intent(in) :: org_i2r2
!!        type(read_int2_real2_item), intent(inout) :: new_i2r2
!!
!!      subroutine alloc_control_array_i2_r2(array_i2r2)
!!      subroutine dealloc_control_array_i2_r2(array_i2r2)
!!      subroutine read_control_array_i2_r2                             &
!!     &         (id_control, label, array_i2r2, c_buf)
!!        type(ctl_array_i2r2), intent(inout) :: array_i2r2
!!        type(buffer_for_control), intent(in)  :: c_buf
!!      subroutine write_control_array_i2_r2                            &
!!     &         (id_control, level, array_i2r2)
!!        type(ctl_array_i2r2), intent(in) :: array_i2r2
!!
!!      subroutine append_control_array_i2_r2(read_i2r2, array_i2r2)
!!        type(read_int2_real2_item), intent(inout) ::    read_i2r2
!!        type(ctl_array_i2r2), intent(inout) :: array_i2r2
!!      subroutine copy_control_array_i2_r2                             &
!!     &         (num_copy, org_i2r2, tgt_i2r2)
!!        type(ctl_array_i2r2), intent(in) ::    org_i2r2
!!        type(ctl_array_i2r2), intent(inout) :: tgt_i2r2
!!      subroutine append_control_item_i2_r2(read_i2r2, array_i2r2)
!!        type(read_int2_real2_item), intent(in) ::    read_i2r2
!!        type(ctl_array_i2r2), intent(inout) :: array_i2r2
!!@endverbatim
!!
!!
      module t_control_array_int2real2
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with three characters
      type read_int2_real2_item
!>        Item name
        character(len=kchara) :: item_name = 'chara_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue(2)
!>        array for read real item
        real(kind = kreal) ::    realvalue(2)
      end type read_int2_real2_item
!
!>  Structure for 2 reals and 2 integeres control array 
      type ctl_array_i2r2
!>        Item name
        character(len=kchara) :: array_name = 'chara_array'
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st integer
        integer(kind=kint), allocatable :: int1(:)
!>     array for 2nd integer
        integer(kind=kint), allocatable :: int2(:)
!>     array for 1st real
        real(kind = kreal), allocatable :: vec1(:)
!>     array for 2nd real
        real(kind = kreal), allocatable :: vec2(:)
      end type ctl_array_i2r2
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_int2real2_ctl_type(c_buf, label, i2r2_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int2_real2_item), intent(inout) :: i2r2_item
!
       character(len=kchara) :: tmpchara
!
!
      if(i2r2_item%iflag.gt.0) return
      i2r2_item%item_name = trim(label)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, i2r2_item%intvalue(1:2),       &
     &                         i2r2_item%realvalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,2i16)')                   &
     &    trim(c_buf%header_chara), ' int:  ', i2r2_item%intvalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1p2e23.15)')              &
     &    trim(c_buf%header_chara), ' real: ', i2r2_item%realvalue(1:2)
      i2r2_item%iflag = 1
!
      end subroutine read_int2real2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_int2real2_ctl_type                               &
     &         (id_file, level, maxlen, i2r2_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level, maxlen
      type(read_int2_real2_item), intent(in) :: i2r2_item
!
!
      if(i2r2_item%iflag .eq. 0) return
!
      call write_i2_r2_ctl_item                                         &
     &   (id_file, level, maxlen, i2r2_item%item_name,                  &
     &    i2r2_item%intvalue(1),  i2r2_item%intvalue(2),                &
     &    i2r2_item%realvalue(1), i2r2_item%realvalue(2))
!
       end subroutine write_int2real2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_int2real2_ctl(org_i2r2, new_i2r2)
!
      type(read_int2_real2_item), intent(in) :: org_i2r2
      type(read_int2_real2_item), intent(inout) :: new_i2r2
!
!
      new_i2r2%item_name =      org_i2r2%item_name
      new_i2r2%iflag =          org_i2r2%iflag
      new_i2r2%intvalue(1:2) =  org_i2r2%intvalue(1:2)
      new_i2r2%realvalue(1:2) = org_i2r2%realvalue(1:2)
!
       end subroutine copy_int2real2_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_i2_r2(array_i2r2)
!
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
!
!
      allocate( array_i2r2%int1(array_i2r2%num) )
      allocate( array_i2r2%int2(array_i2r2%num) )
      allocate( array_i2r2%vec1(array_i2r2%num) )
      allocate( array_i2r2%vec2(array_i2r2%num) )
!
      if(array_i2r2%num .eq. 0) return
      array_i2r2%int1 =     0
      array_i2r2%int2 =     0
      array_i2r2%vec1 = 0.0d0
      array_i2r2%vec2 = 0.0d0
!
      end subroutine alloc_control_array_i2_r2
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_i2_r2(array_i2r2)
!
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
!
!
      if(allocated(array_i2r2%int1) .eqv. .FALSE.) return
      deallocate( array_i2r2%int1, array_i2r2%int2)
      deallocate( array_i2r2%vec1, array_i2r2%vec2)
      array_i2r2%num = 0
!
      end subroutine dealloc_control_array_i2_r2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i2_r2                               &
     &         (id_control, label, array_i2r2, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_int2_real2_item) :: read_i2r2
!
!
      if(array_i2r2%icou .gt. 0) return
      array_i2r2%array_name = trim(label)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_i2r2%icou .gt. 0) return
!
      read_i2r2%iflag = 0
      array_i2r2%num =  0
      call alloc_control_array_i2_r2(array_i2r2)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_int2real2_ctl_type(c_buf, label, read_i2r2)
          call append_control_array_i2_r2(read_i2r2, array_i2r2)
        end if
      end do
!
      end subroutine read_control_array_i2_r2
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i2_r2                              &
     &         (id_control, level, array_i2r2)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_i2r2), intent(in) :: array_i2r2
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(array_i2r2%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_i2r2%array_name)
      do i = 1, array_i2r2%num
        call write_i2_r2_ctl_item(id_control, level,                    &
     &      len_trim(array_i2r2%array_name), array_i2r2%array_name,     &
     &      array_i2r2%int1(i), array_i2r2%int2(i),                     &
     &      array_i2r2%vec1(i), array_i2r2%vec2(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_i2r2%array_name)
!
      end subroutine write_control_array_i2_r2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_i2_r2(read_i2r2, array_i2r2)
!
      type(read_int2_real2_item), intent(inout) ::    read_i2r2
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
!
      type(ctl_array_i2r2) ::    org_i2r2
!
!
      org_i2r2%num = array_i2r2%num
      call alloc_control_array_i2_r2(org_i2r2)
      call copy_control_array_i2_r2(org_i2r2%num, array_i2r2, org_i2r2)
      call dealloc_control_array_i2_r2(array_i2r2)
!
      array_i2r2%num = org_i2r2%num + 1
      call alloc_control_array_i2_r2(array_i2r2)
      call copy_control_array_i2_r2(org_i2r2%num, org_i2r2, array_i2r2)
      call append_control_item_i2_r2(read_i2r2, array_i2r2)
      read_i2r2%iflag = 0
!
      call dealloc_control_array_i2_r2(org_i2r2)
!
      end subroutine append_control_array_i2_r2
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_i2_r2                               &
     &         (num_copy, org_i2r2, tgt_i2r2)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_i2r2), intent(in) ::    org_i2r2
      type(ctl_array_i2r2), intent(inout) :: tgt_i2r2
!
!
      tgt_i2r2%array_name = org_i2r2%array_name
      tgt_i2r2%icou =       org_i2r2%icou
!
      if(num_copy .le. 0) return
      tgt_i2r2%int1(1:num_copy) = org_i2r2%int1(1:num_copy)
      tgt_i2r2%int2(1:num_copy) = org_i2r2%int2(1:num_copy)
      tgt_i2r2%vec1(1:num_copy) = org_i2r2%vec1(1:num_copy)
      tgt_i2r2%vec2(1:num_copy) = org_i2r2%vec2(1:num_copy)
!
      end subroutine copy_control_array_i2_r2
!
!   --------------------------------------------------------------------
!
      subroutine append_control_item_i2_r2(read_i2r2, array_i2r2)
!
      type(read_int2_real2_item), intent(in) ::    read_i2r2
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
!
!
      array_i2r2%icou = array_i2r2%icou + read_i2r2%iflag
      array_i2r2%int1(array_i2r2%num) = read_i2r2%intvalue(1)
      array_i2r2%int2(array_i2r2%num) = read_i2r2%intvalue(2)
      array_i2r2%vec1(array_i2r2%num) = read_i2r2%realvalue(1)
      array_i2r2%vec2(array_i2r2%num) = read_i2r2%realvalue(2)
!
      end subroutine append_control_item_i2_r2
!
! -----------------------------------------------------------------------
!
      end module t_control_array_int2real2
