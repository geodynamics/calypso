!>@file   t_control_array_character2.f90
!!@brief  module t_control_array_character2
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine alloc_control_array_c2(array_c2)
!!      subroutine dealloc_control_array_c2(array_c2)
!!      subroutine read_control_array_c2                                &
!!     &         (id_control, label, array_c2, c_buf)
!!        type(ctl_array_c2), intent(inout) :: array_c2
!!      subroutine write_control_array_c2                               &
!!     &         (id_control, level, label, array_c2)
!!        type(ctl_array_c2), intent(in) :: array_c2
!!
!!      subroutine append_control_array_c2(read_c2, array_c2)
!!        type(read_chara2_item), intent(inout) ::    read_c2
!!        type(ctl_array_c2), intent(inout) :: array_c2
!!      subroutine dup_control_array_c2(org_c2, tgt_c2)
!!      subroutine copy_control_array_c2(num_copy, org_c2, tgt_c2)
!!        type(ctl_array_c2), intent(in) ::    org_c2
!!        type(ctl_array_c2), intent(inout) :: tgt_c2
!!      subroutine append_control_item_c2(read_c2, array_c2)
!!        type(read_chara2_item), intent(in) ::    read_c2
!!        type(ctl_array_c2), intent(inout) :: array_c2
!!@endverbatim
!!
!!
      module t_control_array_character2
!
      use m_precision
      use t_control_elements
!
      implicit none
!
!>  Structure for three charactors control array 
      type ctl_array_c2
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st character
        character(len=kchara), allocatable :: c1_tbl(:)
!>     array for 2nd character
        character(len=kchara), allocatable :: c2_tbl(:)
      end type ctl_array_c2
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c2(array_c2)
!
      type(ctl_array_c2), intent(inout) :: array_c2
!
!
      allocate( array_c2%c1_tbl(array_c2%num) )
      allocate( array_c2%c2_tbl(array_c2%num) )
!
      end subroutine alloc_control_array_c2
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c2(array_c2)
!
      type(ctl_array_c2), intent(inout) :: array_c2
!
!
      if(allocated(array_c2%c1_tbl) .eqv. .FALSE.) return
      deallocate(array_c2%c1_tbl, array_c2%c2_tbl)
      array_c2%num = 0
!
      end subroutine dealloc_control_array_c2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c2                                  &
     &         (id_control, label, array_c2, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_c2), intent(inout) :: array_c2
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_chara2_item) :: read_c2
!
!
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_c2%icou .gt. 0) return
!
      read_c2%iflag = 0
      array_c2%num =  0
      call alloc_control_array_c2(array_c2)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_character2_ctl_type(c_buf, label, read_c2)
          call append_control_array_c2(read_c2, array_c2)
        end if
      end do
!
      end subroutine read_control_array_c2
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c2                                 &
     &         (id_control, level, label, array_c2)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_c2), intent(in) :: array_c2
!
      integer(kind = kint) :: maxlen(0:1)
      integer(kind = kint) :: i
!
!
      if(array_c2%num .le. 0) return
      write(id_control,'(a1)') '!'
!
      maxlen(0) = len_trim(label)
      maxlen(1) = max_len_of_charaarray(array_c2%num, array_c2%c1_tbl)
!
      call write_array_flag_for_ctl                                     &
     &   (id_control, level, label, array_c2%num)
      do i = 1, array_c2%num
        call write_character2_ctl_item(id_control, (level+1), label,    &
     &      maxlen, array_c2%c1_tbl(i), array_c2%c2_tbl(i))
      end do
      call write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_c2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c2(read_c2, array_c2)
!
      type(read_chara2_item), intent(inout) ::    read_c2
      type(ctl_array_c2), intent(inout) :: array_c2
!
      type(ctl_array_c2) ::    org_c2
!
!
      org_c2%num = array_c2%num
      call alloc_control_array_c2(org_c2)
      call copy_control_array_c2(org_c2%num, array_c2, org_c2)
      call dealloc_control_array_c2(array_c2)
!
      array_c2%num = org_c2%num + 1
      call alloc_control_array_c2(array_c2)
      call copy_control_array_c2(org_c2%num, org_c2, array_c2)
      call append_control_item_c2(read_c2, array_c2)
      read_c2%iflag = 0
!
      call dealloc_control_array_c2(org_c2)
!
      end subroutine append_control_array_c2
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_array_c2(org_c2, tgt_c2)
!
      type(ctl_array_c2), intent(in) ::    org_c2
      type(ctl_array_c2), intent(inout) :: tgt_c2
!
!
      tgt_c2%num = org_c2%num
      call alloc_control_array_c2(tgt_c2)
      call copy_control_array_c2(org_c2%num, org_c2, tgt_c2)
!
      end subroutine dup_control_array_c2
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c2(num_copy, org_c2, tgt_c2)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_c2), intent(in) ::    org_c2
      type(ctl_array_c2), intent(inout) :: tgt_c2
!
!
      if(num_copy .le. 0) return
      tgt_c2%icou = org_c2%icou
      tgt_c2%c1_tbl(1:num_copy) = org_c2%c1_tbl(1:num_copy)
      tgt_c2%c2_tbl(1:num_copy) = org_c2%c2_tbl(1:num_copy)
!
      end subroutine copy_control_array_c2
!
! -----------------------------------------------------------------------

      subroutine append_control_item_c2(read_c2, array_c2)
!
      type(read_chara2_item), intent(in) ::    read_c2
      type(ctl_array_c2), intent(inout) :: array_c2
!
!
      array_c2%icou = array_c2%icou + read_c2%iflag
      array_c2%c1_tbl(array_c2%num) = read_c2%charavalue(1)
      array_c2%c2_tbl(array_c2%num) = read_c2%charavalue(2)
!
      end subroutine append_control_item_c2
!
! -----------------------------------------------------------------------
!
      end module t_control_array_character2
