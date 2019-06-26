!>@file   t_control_array_int2real2.f90
!!@brief  module t_control_array_int2real2
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine alloc_control_array_i2_r2(array_i2r2)
!!      subroutine dealloc_control_array_i2_r2(array_i2r2)
!!      subroutine read_control_array_i2_r2                             &
!!     &         (id_control, label, array_i2r2, c_buf)
!!        type(ctl_array_i2r2), intent(inout) :: array_i2r2
!!      subroutine write_control_array_i2_r2                            &
!!     &         (id_control, level, label, array_i2r2)
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
      use t_control_elements
!
      implicit none
!
!>  Structure for 2 reals and 2 integeres control array 
      type ctl_array_i2r2
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
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_i2r2%icou .gt. 0) return
!
      read_i2r2%iflag = 0
      array_i2r2%num =  0
      call alloc_control_array_i2_r2(array_i2r2)
!
      do
        call load_one_line_from_control(id_control, c_buf)
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
     &         (id_control, level, label, array_i2r2)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_i2r2), intent(in) :: array_i2r2
!
      integer(kind = kint) :: i
!
!
      if(array_i2r2%num .le. 0) return
      write(id_control,'(a1)') '!'
!
      call write_array_flag_for_ctl                                     &
     &   (id_control, level, label, array_i2r2%num)
      do i = 1, array_i2r2%num
        call write_i2_r2_ctl_item (id_control, (level+1), label,        &
     &      array_i2r2%int1(i), array_i2r2%int2(i),                     &
     &      array_i2r2%vec1(i), array_i2r2%vec2(i))
      end do
      call write_end_array_flag_for_ctl(id_control, level, label)
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
      if(num_copy .le. 0) return
      tgt_i2r2%icou = org_i2r2%icou
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
