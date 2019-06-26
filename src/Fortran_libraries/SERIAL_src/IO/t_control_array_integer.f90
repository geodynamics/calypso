!>@file   t_control_array_integer.f90
!!@brief  module t_control_array_integer
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine alloc_control_array_int(array_int)
!!      subroutine dealloc_control_array_int(array_int)
!!      subroutine read_control_array_i1                                &
!!     &         (id_control, label, array_int, c_buf)
!!        type(ctl_array_int), intent(inout) :: array_int
!!      subroutine write_control_array_i1                               &
!!     &         (id_control, level, label, array_int)
!!        type(ctl_array_int), intent(in) :: array_int
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
      use t_control_elements
!
      implicit none
!
!>  Structure for integer control array 
      type ctl_array_int
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
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_int%icou .gt. 0) return
!
      read_i1%iflag = 0
      array_int%num =  0
      call alloc_control_array_int(array_int)
!
      do
        call load_one_line_from_control(id_control, c_buf)
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
     &         (id_control, level, label, array_int)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_int), intent(in) :: array_int
!
      integer(kind = kint) :: i, length
!
!
      if(array_int%num .le. 0) return
      write(id_control,'(a1)') '!'
!
      call write_array_flag_for_ctl                                     &
     &   (id_control, level, label, array_int%num)
      do i = 1, array_int%num
        length = len_trim(label)
        call write_integer_ctl_item(id_control, (level+1), length,      &
     &      label, array_int%ivec(i))
      end do
      call write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_i1
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
      if(num_copy .le. 0) return
      tgt_i1%icou = org_i1%icou
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
