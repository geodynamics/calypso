!>@file   t_control_array_character.f90
!!@brief  module t_control_array_character
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine alloc_control_array_chara(array_chara)
!!      subroutine dealloc_control_array_chara(array_chara)
!!      subroutine read_control_array_c1                                &
!!     &         (id_control, label, array_chara, c_buf)
!!        type(ctl_array_chara), intent(inout) :: array_chara
!!      subroutine write_control_array_c1                               &
!!     &         (id_control, level, label, array_chara)
!!        type(ctl_array_chara), intent(in) :: array_chara
!!
!!      subroutine append_control_array_c1(read_c1, array_c1)
!!        type(read_character_item), intent(inout) ::    read_c1
!!        type(ctl_array_chara), intent(inout) :: array_c1
!!      subroutine dup_control_array_c1(org_c1, tgt_c1)
!!      subroutine copy_control_array_c1(num_copy, org_c1, tgt_c1)
!!        type(ctl_array_chara), intent(in) ::    org_c1
!!        type(ctl_array_chara), intent(inout) :: tgt_c1
!!      subroutine append_control_item_c1(read_c1, array_c1)
!!        type(read_character_item), intent(in) ::    read_c1
!!        type(ctl_array_chara), intent(inout) :: array_c1
!!@endverbatim
!!
!!
      module t_control_array_character
!
      use m_precision
      use t_control_elements
!
      implicit none
!
!>  Structure for character control array 
      type ctl_array_chara
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st character
        character(len=kchara), allocatable :: c_tbl(:)
      end type ctl_array_chara
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_chara(array_chara)
!
      type(ctl_array_chara), intent(inout) :: array_chara
!
!
      allocate( array_chara%c_tbl(array_chara%num) )
!
      end subroutine alloc_control_array_chara
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_chara(array_chara)
!
      type(ctl_array_chara), intent(inout) :: array_chara
!
!
      if(allocated(array_chara%c_tbl) .eqv. .FALSE.) return
      deallocate(array_chara%c_tbl)
      array_chara%num = 0
!
      end subroutine dealloc_control_array_chara
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c1                                  &
     &         (id_control, label, array_chara, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_chara), intent(inout) :: array_chara
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_character_item) :: read_c1
!
!
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_chara%icou .gt. 0) return
!
      read_c1%iflag = 0
      array_chara%num =  0
      call alloc_control_array_chara(array_chara)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_chara_ctl_type(c_buf, label, read_c1)
          call append_control_array_c1(read_c1, array_chara)
        end if
      end do
!
      end subroutine read_control_array_c1
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c1                                 &
     &         (id_control, level, label, array_chara)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_chara), intent(in) :: array_chara
!
      integer(kind = kint) :: i, length
!
!
      if(array_chara%num .le. 0) return
      write(id_control,'(a1)') '!'
!
      call write_array_flag_for_ctl                                     &
     &   (id_control, level, label, array_chara%num)
      do i = 1, array_chara%num
        length = len_trim(label)
        call write_character_ctl_item(id_control, (level+1), length,    &
     &      label, array_chara%c_tbl(i))
      end do
      call write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_c1
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c1(read_c1, array_c1)
!
      type(read_character_item), intent(inout) ::    read_c1
      type(ctl_array_chara), intent(inout) :: array_c1
!
      type(ctl_array_chara) ::    org_c1
!
!
      org_c1%num = array_c1%num
      call alloc_control_array_chara(org_c1)
      call copy_control_array_c1(org_c1%num, array_c1, org_c1)
      call dealloc_control_array_chara(array_c1)
!
      array_c1%num = org_c1%num + 1
      call alloc_control_array_chara(array_c1)
      call copy_control_array_c1(org_c1%num, org_c1, array_c1)
      call append_control_item_c1(read_c1, array_c1)
      read_c1%iflag = 0
!
      call dealloc_control_array_chara(org_c1)
!
      end subroutine append_control_array_c1
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_array_c1(org_c1, tgt_c1)
!
      type(ctl_array_chara), intent(in) ::    org_c1
      type(ctl_array_chara), intent(inout) :: tgt_c1
!
!
      tgt_c1%num = org_c1%num
      call alloc_control_array_chara(tgt_c1)
      call copy_control_array_c1(org_c1%num, org_c1, tgt_c1)
!
      end subroutine dup_control_array_c1
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c1(num_copy, org_c1, tgt_c1)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_chara), intent(in) ::    org_c1
      type(ctl_array_chara), intent(inout) :: tgt_c1
!
!
      if(num_copy .le. 0) return
      tgt_c1%icou = org_c1%icou
      tgt_c1%c_tbl(1:num_copy) = org_c1%c_tbl(1:num_copy)
!
      end subroutine copy_control_array_c1
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c1(read_c1, array_c1)
!
      type(read_character_item), intent(in) ::    read_c1
      type(ctl_array_chara), intent(inout) :: array_c1
!
!
      array_c1%icou = array_c1%icou + read_c1%iflag
      array_c1%c_tbl(array_c1%num) = read_c1%charavalue
!
      end subroutine append_control_item_c1
!
! -----------------------------------------------------------------------
!
      end module t_control_array_character
