!>@file   t_control_array_charareal.f90
!!@brief  module t_control_array_charareal
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine alloc_control_array_c_r(array_cr)
!!      subroutine dealloc_control_array_c_r(array_cr)
!!      subroutine read_control_array_c_r                               &
!!     &         (id_control, label, array_cr, c_buf)
!!        type(ctl_array_cr), intent(inout) :: array_cr
!!      subroutine write_control_array_c_r                              &
!!     &         (id_control, level, label, array_cr)
!!        type(ctl_array_cr), intent(in) :: array_cr
!!
!!      subroutine append_control_array_c_r(read_cr, array_cr)
!!        type(read_chara_real_item), intent(inout) ::    read_cr
!!        type(ctl_array_cr), intent(inout) :: array_cr
!!      subroutine dup_control_array_c_r(org_cr, tgt_cr)
!!      subroutine copy_control_array_c_r(num_copy, org_cr, tgt_cr)
!!        type(ctl_array_cr), intent(in) ::    org_cr
!!        type(ctl_array_cr), intent(inout) :: tgt_cr
!!      subroutine append_control_item_c_r(read_cr, array_cr)
!!        type(read_chara_real_item), intent(in) ::    read_cr
!!        type(ctl_array_cr), intent(inout) :: array_cr
!!@endverbatim
!!
!!
      module t_control_array_charareal
!
      use m_precision
      use t_control_elements
!
      implicit none
!
!>  Structure for charactor and real control array 
      type ctl_array_cr
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st character
        character(len=kchara), allocatable :: c_tbl(:)
!>     array for 1st real
        real(kind = kreal), allocatable :: vect(:)
      end type ctl_array_cr
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c_r(array_cr)
!
      type(ctl_array_cr), intent(inout) :: array_cr
!
!
      allocate( array_cr%c_tbl(array_cr%num) )
      allocate( array_cr%vect(array_cr%num) )
!
      if(array_cr%num .eq. 0) return
      array_cr%vect = 0.0d0
!
      end subroutine alloc_control_array_c_r
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c_r(array_cr)
!
      type(ctl_array_cr), intent(inout) :: array_cr
!
!
      if(allocated(array_cr%c_tbl) .eqv. .FALSE.) return
      deallocate( array_cr%c_tbl, array_cr%vect)
      array_cr%num = 0
!
      end subroutine dealloc_control_array_c_r
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c_r                                 &
     &         (id_control, label, array_cr, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_cr), intent(inout) :: array_cr
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_chara_real_item) :: read_cr
!
!
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_cr%icou .gt. 0) return
!
      read_cr%iflag = 0
      array_cr%num =  0
      call alloc_control_array_c_r(array_cr)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_charareal_ctl_type(c_buf, label, read_cr)
          call append_control_array_c_r(read_cr, array_cr)
        end if
      end do
!
      end subroutine read_control_array_c_r
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c_r                                &
     &         (id_control, level, label, array_cr)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_cr), intent(in) :: array_cr
!
      integer(kind = kint) :: i
!
!
      if(array_cr%num .le. 0) return
      write(id_control,'(a1)') '!'
!
      call write_array_flag_for_ctl                                     &
     &   (id_control, level, label, array_cr%num)
      do i = 1, array_cr%num
        call write_chara_real_ctl_item(id_control, (level+1), label,    &
     &     array_cr%c_tbl(i), array_cr%vect(i))
      end do
      call write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_c_r
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c_r(read_cr, array_cr)
!
      type(read_chara_real_item), intent(inout) ::    read_cr
      type(ctl_array_cr), intent(inout) :: array_cr
!
      type(ctl_array_cr) ::    org_cr
!
!
      org_cr%num = array_cr%num
      call alloc_control_array_c_r(org_cr)
      call copy_control_array_c_r(org_cr%num, array_cr, org_cr)
      call dealloc_control_array_c_r(array_cr)
!
      array_cr%num = org_cr%num + 1
      call alloc_control_array_c_r(array_cr)
      call copy_control_array_c_r(org_cr%num, org_cr, array_cr)
      call append_control_item_c_r(read_cr, array_cr)
      read_cr%iflag = 0
!
      call dealloc_control_array_c_r(org_cr)
!
      end subroutine append_control_array_c_r
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_array_c_r(org_cr, tgt_cr)
!
      type(ctl_array_cr), intent(in) ::    org_cr
      type(ctl_array_cr), intent(inout) :: tgt_cr
!
!
      tgt_cr%num = org_cr%num
      call alloc_control_array_c_r(tgt_cr)
      call copy_control_array_c_r(org_cr%num, org_cr, tgt_cr)
!
      end subroutine dup_control_array_c_r
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c_r(num_copy, org_cr, tgt_cr)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_cr), intent(in) ::    org_cr
      type(ctl_array_cr), intent(inout) :: tgt_cr
!
!
      if(num_copy .le. 0) return
      tgt_cr%icou = org_cr%icou
      tgt_cr%c_tbl(1:num_copy) = org_cr%c_tbl(1:num_copy)
      tgt_cr%vect(1:num_copy) =  org_cr%vect(1:num_copy)
!
      end subroutine copy_control_array_c_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c_r(read_cr, array_cr)
!
      type(read_chara_real_item), intent(in) ::    read_cr
      type(ctl_array_cr), intent(inout) :: array_cr
!
!
      array_cr%icou = array_cr%icou + read_cr%iflag
      array_cr%c_tbl(array_cr%num) = read_cr%charavalue
      array_cr%vect(array_cr%num) =  read_cr%realvalue
!
      end subroutine append_control_item_c_r
!
! -----------------------------------------------------------------------
!
      end module t_control_array_charareal
