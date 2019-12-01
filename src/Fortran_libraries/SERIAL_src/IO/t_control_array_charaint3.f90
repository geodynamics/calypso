!>@file   t_control_array_charaint3.f90
!!@brief  module t_control_array_charaint3
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine alloc_control_array_c_i3(array_ci3)
!!      subroutine dealloc_control_array_c_i3(array_ci3)
!!      subroutine read_control_array_c_i3                              &
!!     &         (id_control, label, array_ci3, c_buf)
!!        type(ctl_array_ci3), intent(inout) :: array_ci3
!!      subroutine write_control_array_c_i3                             &
!!     &         (id_control, level, label, array_ci3)
!!        type(ctl_array_ci3), intent(in) :: array_ci3
!!
!!      subroutine append_control_array_c_i3(read_ci3, array_ci3)
!!        type(read_chara_int3_item), intent(inout) ::    read_ci3
!!        type(ctl_array_ci3), intent(inout) :: array_ci3
!!      subroutine copy_control_array_c_i3(num_copy, org_ci, tgt_ci)
!!        type(ctl_array_ci3), intent(in) ::    org_ci
!!        type(ctl_array_ci3), intent(inout) :: tgt_ci
!!      subroutine append_control_item_c_i3(read_ci3, array_ci3)
!!        type(read_chara_int3_item), intent(in) ::    read_ci3
!!        type(ctl_array_ci3), intent(inout) :: array_ci3
!!@endverbatim
!!
!!
      module t_control_array_charaint3
!
      use m_precision
      use t_control_elements
!
      implicit none
!
!>  Structure for charactor and integer control array 
      type ctl_array_ci3
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st character
        character(len=kchara), allocatable :: c_tbl(:)
!>     array for 1st integer
        integer(kind = kint), allocatable :: ivec1(:)
!>     array for 2nd integer
        integer(kind = kint), allocatable :: ivec2(:)
!>     array for 3rd integer
        integer(kind = kint), allocatable :: ivec3(:)
      end type ctl_array_ci3
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c_i3(array_ci3)
!
      type(ctl_array_ci3), intent(inout) :: array_ci3
!
!
      allocate( array_ci3%c_tbl(array_ci3%num) )
      allocate( array_ci3%ivec1(array_ci3%num) )
      allocate( array_ci3%ivec2(array_ci3%num) )
      allocate( array_ci3%ivec3(array_ci3%num) )
!
      if(array_ci3%num .eq. 0) return
      array_ci3%ivec1 = 0
      array_ci3%ivec2 = 0
      array_ci3%ivec3 = 0
!
      end subroutine alloc_control_array_c_i3
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c_i3(array_ci3)
!
      type(ctl_array_ci3), intent(inout) :: array_ci3
!
!
      if(allocated(array_ci3%c_tbl) .eqv. .FALSE.) return
      deallocate( array_ci3%c_tbl, array_ci3%ivec1)
      deallocate( array_ci3%ivec2, array_ci3%ivec3)
      array_ci3%num = 0
!
      end subroutine dealloc_control_array_c_i3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c_i3                                &
     &         (id_control, label, array_ci3, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_ci3), intent(inout) :: array_ci3
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_chara_int3_item) :: read_ci3
!
!
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_ci3%icou .gt. 0) return
!
      read_ci3%iflag = 0
      array_ci3%num =  0
      call alloc_control_array_c_i3(array_ci3)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_charaint3_ctl_type(c_buf, label, read_ci3)
          call append_control_array_c_i3(read_ci3, array_ci3)
        end if
      end do
!
      end subroutine read_control_array_c_i3
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c_i3                               &
     &         (id_control, level, label, array_ci3)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_ci3), intent(in) :: array_ci3
!
      integer(kind = kint) :: i
!
!
      if(array_ci3%num .le. 0) return
      write(id_control,'(a1)') '!'
!
      call write_array_flag_for_ctl                                     &
     &   (id_control, level, label, array_ci3%num)
      do i = 1, array_ci3%num
        call write_chara_int3_ctl_item(id_control, (level+1), label,    &
     &     array_ci3%c_tbl(i), array_ci3%ivec1(i),                      &
     &     array_ci3%ivec2(i), array_ci3%ivec3(i))
      end do
      call write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_c_i3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c_i3(read_ci3, array_ci3)
!
      type(read_chara_int3_item), intent(inout) ::    read_ci3
      type(ctl_array_ci3), intent(inout) :: array_ci3
!
      type(ctl_array_ci3) ::    org_ci
!
!
      org_ci%num = array_ci3%num
      call alloc_control_array_c_i3(org_ci)
      call copy_control_array_c_i3(org_ci%num, array_ci3, org_ci)
      call dealloc_control_array_c_i3(array_ci3)
!
      array_ci3%num = org_ci%num + 1
      call alloc_control_array_c_i3(array_ci3)
      call copy_control_array_c_i3(org_ci%num, org_ci, array_ci3)
      call append_control_item_c_i3(read_ci3, array_ci3)
      read_ci3%iflag = 0
!
      call dealloc_control_array_c_i3(org_ci)
!
      end subroutine append_control_array_c_i3
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c_i3(num_copy, org_ci, tgt_ci)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_ci3), intent(in) ::    org_ci
      type(ctl_array_ci3), intent(inout) :: tgt_ci
!
!
      if(num_copy .le. 0) return
      tgt_ci%icou = org_ci%icou
      tgt_ci%c_tbl(1:num_copy) = org_ci%c_tbl(1:num_copy)
      tgt_ci%ivec1(1:num_copy) = org_ci%ivec1(1:num_copy)
      tgt_ci%ivec2(1:num_copy) = org_ci%ivec2(1:num_copy)
      tgt_ci%ivec3(1:num_copy) = org_ci%ivec3(1:num_copy)
!
      end subroutine copy_control_array_c_i3
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c_i3(read_ci3, array_ci3)
!
      type(read_chara_int3_item), intent(in) ::    read_ci3
      type(ctl_array_ci3), intent(inout) :: array_ci3
!
!
      array_ci3%icou = array_ci3%icou + read_ci3%iflag
      array_ci3%c_tbl(array_ci3%num) = read_ci3%charavalue
      array_ci3%ivec1(array_ci3%num) = read_ci3%intvalue(1)
      array_ci3%ivec2(array_ci3%num) = read_ci3%intvalue(2)
      array_ci3%ivec3(array_ci3%num) = read_ci3%intvalue(3)
!
      end subroutine append_control_item_c_i3
!
! -----------------------------------------------------------------------
!
      end module t_control_array_charaint3
