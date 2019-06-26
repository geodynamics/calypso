!>@file   t_control_array_character3.f90
!!@brief  module t_control_array_character3
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Structure of control array input with 3 words
!!
!!@verbatim
!!      subroutine alloc_control_array_c3(array_c3)
!!      subroutine dealloc_control_array_c3(array_c3)
!!      subroutine read_control_array_c3                                &
!!     &         (id_control, label, array_c3, c_buf)
!!        type(ctl_array_c3), intent(inout) :: array_c3
!!      subroutine write_control_array_c3                               &
!!     &         (id_control, level, label, array_c3)
!!        type(ctl_array_c3), intent(in) :: array_c3
!!
!!      subroutine append_control_array_c3(read_c3, array_c3)
!!        type(read_chara3_item), intent(inout) ::    read_c3
!!        type(ctl_array_c3), intent(inout) :: array_c3
!!      subroutine copy_control_array_c3(num_copy, org_c3, tgt_c3)
!!        type(ctl_array_c3), intent(in) ::    org_c3
!!        type(ctl_array_c3), intent(inout) :: tgt_c3
!!      subroutine append_control_item_c3(read_c3, array_c3)
!!        type(read_chara3_item), intent(in) ::    read_c3
!!        type(ctl_array_c3), intent(inout) :: array_c3
!!@endverbatim
!!
!!
      module t_control_array_character3
!
      use m_precision
      use t_control_elements
!
      implicit none
!
!>  Structure for three charactors control array 
      type ctl_array_c3
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st character
        character(len=kchara), allocatable :: c1_tbl(:)
!>     array for 2nd character
        character(len=kchara), allocatable :: c2_tbl(:)
!>     array for 3rd character
        character(len=kchara), allocatable :: c3_tbl(:)
      end type ctl_array_c3
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c3(array_c3)
!
      type(ctl_array_c3), intent(inout) :: array_c3
!
!
      allocate( array_c3%c1_tbl(array_c3%num) )
      allocate( array_c3%c2_tbl(array_c3%num) )
      allocate( array_c3%c3_tbl(array_c3%num) )
!
      end subroutine alloc_control_array_c3
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c3(array_c3)
!
      type(ctl_array_c3), intent(inout) :: array_c3
!
!
      if(allocated(array_c3%c1_tbl) .eqv. .FALSE.) return
      deallocate(array_c3%c1_tbl, array_c3%c2_tbl, array_c3%c3_tbl)
      array_c3%num = 0
!
      end subroutine dealloc_control_array_c3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c3                                  &
     &         (id_control, label, array_c3, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_c3), intent(inout) :: array_c3
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_chara3_item) :: read_c3
!
!
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_c3%icou .gt. 0) return
!
      read_c3%iflag = 0
      array_c3%num =  0
      call alloc_control_array_c3(array_c3)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_character3_ctl_type(c_buf, label, read_c3)
          call append_control_array_c3(read_c3, array_c3)
        end if
      end do
!
      end subroutine read_control_array_c3
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c3                                 &
     &         (id_control, level, label, array_c3)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_c3), intent(in) :: array_c3
!
      integer(kind = kint) :: maxlen(0:2)
      integer(kind = kint) :: i
!
!
      if(array_c3%num .le. 0) return
      write(id_control,'(a1)') '!'
!
      maxlen(0) = len_trim(label)
      maxlen(1) = max_len_of_charaarray(array_c3%num, array_c3%c1_tbl)
      maxlen(2) = max_len_of_charaarray(array_c3%num, array_c3%c2_tbl)
!
      call write_array_flag_for_ctl                                     &
     &   (id_control, level, label, array_c3%num)
      do i = 1, array_c3%num
        call write_character3_ctl_item                                  &
     &     (id_control, (level+1), label, maxlen,                       &
     &      array_c3%c1_tbl(i), array_c3%c2_tbl(i), array_c3%c3_tbl(i))
      end do
      call write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_c3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c3(read_c3, array_c3)
!
      type(read_chara3_item), intent(inout) ::    read_c3
      type(ctl_array_c3), intent(inout) :: array_c3
!
      type(ctl_array_c3) :: tmp_c3
!
!
      tmp_c3%num = array_c3%num
      call alloc_control_array_c3(tmp_c3)
      call copy_control_array_c3(tmp_c3%num, array_c3, tmp_c3)
      call dealloc_control_array_c3(array_c3)
!
      array_c3%num = tmp_c3%num + 1
      call alloc_control_array_c3(array_c3)
      call copy_control_array_c3(tmp_c3%num, tmp_c3, array_c3)
      call append_control_item_c3(read_c3, array_c3)
      read_c3%iflag = 0
!
      call dealloc_control_array_c3(tmp_c3)
!
      end subroutine append_control_array_c3
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c3(num_copy, org_c3, tgt_c3)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_c3), intent(in) ::    org_c3
      type(ctl_array_c3), intent(inout) :: tgt_c3
!
!
      if(num_copy .le. 0) return
      tgt_c3%icou = org_c3%icou
      tgt_c3%c1_tbl(1:num_copy) = org_c3%c1_tbl(1:num_copy)
      tgt_c3%c2_tbl(1:num_copy) = org_c3%c2_tbl(1:num_copy)
      tgt_c3%c3_tbl(1:num_copy) = org_c3%c3_tbl(1:num_copy)
!
      end subroutine copy_control_array_c3
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c3(read_c3, array_c3)
!
      type(read_chara3_item), intent(in) ::    read_c3
      type(ctl_array_c3), intent(inout) :: array_c3
!
!
      array_c3%icou = array_c3%icou + read_c3%iflag
      array_c3%c1_tbl(array_c3%num) = read_c3%charavalue(1)
      array_c3%c2_tbl(array_c3%num) = read_c3%charavalue(2)
      array_c3%c3_tbl(array_c3%num) = read_c3%charavalue(3)
!
      end subroutine append_control_item_c3
!
! -----------------------------------------------------------------------
!
      end module t_control_array_character3
