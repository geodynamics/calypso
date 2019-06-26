!>@file   t_control_array_chara2real.f90
!!@brief  module t_control_array_chara2real
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine alloc_control_array_c2_r(array_c2r)
!!      subroutine dealloc_control_array_c2_r(array_c2r)
!!      subroutine read_control_array_c2_r                              &
!!     &         (id_control, label, array_c2r, c_buf)
!!        type(ctl_array_c2r), intent(inout) :: array_c2r
!!      subroutine write_control_array_c2_r                             &
!!     &         (id_control, level, label, array_c2r)
!!        type(ctl_array_c2r), intent(in) :: array_c2r
!!
!!      subroutine append_control_array_c2_r(read_c2r, array_c2r)
!!        type(read_chara2_real_item), intent(inout) ::    read_c2r
!!        type(ctl_array_c2r), intent(inout) :: array_c2r
!!      subroutine dup_control_array_c2_r(org_c2r, tgt_c2r)
!!      subroutine copy_control_array_c2_r(num_copy, org_c2r, tgt_c2r)
!!        type(ctl_array_c2r), intent(in) ::    org_c2r
!!        type(ctl_array_c2r), intent(inout) :: tgt_c2r
!!      subroutine append_control_item_c2_r(read_c2r, array_c2r)
!!        type(read_chara2_real_item), intent(in) ::    read_c2r
!!        type(ctl_array_c2r), intent(inout) :: array_c2r
!!@endverbatim
!!
!!
      module t_control_array_chara2real
!
      use m_precision
      use t_control_elements
!
      implicit none
!
!>  Structure for two charactors and real control array 
      type ctl_array_c2r
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st character
        character(len=kchara), allocatable :: c1_tbl(:)
!>     array for 2nd character
        character(len=kchara), allocatable :: c2_tbl(:)
!>     array for 1st real
        real(kind = kreal), allocatable :: vect(:)
      end type ctl_array_c2r
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c2_r(array_c2r)
!
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
!
      allocate( array_c2r%c1_tbl(array_c2r%num) )
      allocate( array_c2r%c2_tbl(array_c2r%num) )
      allocate( array_c2r%vect(array_c2r%num) )
!
      if(array_c2r%num .eq. 0) return
      array_c2r%vect = 0.0d0
!
      end subroutine alloc_control_array_c2_r
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c2_r(array_c2r)
!
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
!
      if(allocated(array_c2r%c1_tbl) .eqv. .FALSE.) return
      deallocate( array_c2r%c1_tbl, array_c2r%c2_tbl, array_c2r%vect)
      array_c2r%num = 0
!
      end subroutine dealloc_control_array_c2_r
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c2_r                                &
     &         (id_control, label, array_c2r, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_c2r), intent(inout) :: array_c2r
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_chara2_real_item) :: read_c2r
!
!
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_c2r%icou .gt. 0) return
!
      read_c2r%iflag = 0
      array_c2r%num =  0
      call alloc_control_array_c2_r(array_c2r)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_char2real_ctl_type(c_buf, label, read_c2r)
          call append_control_array_c2_r(read_c2r, array_c2r)
        end if
      end do
!
      end subroutine read_control_array_c2_r
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c2_r                              &
     &         (id_control, level, label, array_c2r)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_c2r), intent(in) :: array_c2r
!
      integer(kind = kint) :: i
!
!
      if(array_c2r%num .le. 0) return
      write(id_control,'(a1)') '!'
!
      call write_array_flag_for_ctl                                     &
     &   (id_control, level, label, array_c2r%num)
      do i = 1, array_c2r%num
        call write_chara2_real_ctl_item(id_control, (level+1), label,   &
     &     array_c2r%c1_tbl(i), array_c2r%c2_tbl(i), array_c2r%vect(i))
      end do
      call write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_c2_r
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c2_r(read_c2r, array_c2r)
!
      type(read_chara2_real_item), intent(inout) ::    read_c2r
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
      type(ctl_array_c2r) ::    org_c2r
!
!
      org_c2r%num = array_c2r%num
      call alloc_control_array_c2_r(org_c2r)
      call copy_control_array_c2_r(org_c2r%num, array_c2r, org_c2r)
      call dealloc_control_array_c2_r(array_c2r)
!
      array_c2r%num = org_c2r%num + 1
      call alloc_control_array_c2_r(array_c2r)
      call copy_control_array_c2_r(org_c2r%num, org_c2r, array_c2r)
      call append_control_item_c2_r(read_c2r, array_c2r)
      read_c2r%iflag = 0
!
      call dealloc_control_array_c2_r(org_c2r)
!
      end subroutine append_control_array_c2_r
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_array_c2_r(org_c2r, tgt_c2r)
!
      type(ctl_array_c2r), intent(in) ::    org_c2r
      type(ctl_array_c2r), intent(inout) :: tgt_c2r
!
!
      tgt_c2r%num = org_c2r%num
      call alloc_control_array_c2_r(tgt_c2r)
      call copy_control_array_c2_r(org_c2r%num, org_c2r, tgt_c2r)
!
      end subroutine dup_control_array_c2_r
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c2_r(num_copy, org_c2r, tgt_c2r)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_c2r), intent(in) ::    org_c2r
      type(ctl_array_c2r), intent(inout) :: tgt_c2r
!
!
      if(num_copy .le. 0) return
      tgt_c2r%icou = org_c2r%icou
      tgt_c2r%c1_tbl(1:num_copy) = org_c2r%c1_tbl(1:num_copy)
      tgt_c2r%c2_tbl(1:num_copy) = org_c2r%c2_tbl(1:num_copy)
      tgt_c2r%vect(1:num_copy) =   org_c2r%vect(1:num_copy)
!
      end subroutine copy_control_array_c2_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c2_r(read_c2r, array_c2r)
!
      type(read_chara2_real_item), intent(in) ::    read_c2r
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
!
      array_c2r%icou = array_c2r%icou + read_c2r%iflag
      array_c2r%c1_tbl(array_c2r%num) = read_c2r%charavalue(1)
      array_c2r%c2_tbl(array_c2r%num) = read_c2r%charavalue(2)
      array_c2r%vect(array_c2r%num) =   read_c2r%realvalue
!
      end subroutine append_control_item_c2_r
!
! -----------------------------------------------------------------------
!
      end module t_control_array_chara2real
