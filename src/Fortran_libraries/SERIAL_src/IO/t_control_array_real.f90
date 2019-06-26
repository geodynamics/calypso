!>@file   t_control_array_real.f90
!!@brief  module t_control_array_real
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine alloc_control_array_real(array_real)
!!      subroutine dealloc_control_array_real(array_real)
!!      subroutine read_control_array_r1                                &
!!     &         (id_control, label, array_real, c_buf)
!!        type(ctl_array_real), intent(inout) :: array_real
!!      subroutine write_control_array_r1                               &
!!     &         (id_control, level, label, array_real)
!!        type(ctl_array_real), intent(in) :: array_real
!!
!!      subroutine append_control_array_real(read_r1, array_r1)
!!        type(read_real_item), intent(inout) ::    read_r1
!!        type(ctl_array_real), intent(inout) :: array_r1
!!      subroutine copy_control_array_real(num_copy, org_r1, tgt_r1)
!!        type(ctl_array_real), intent(in) ::    org_r1
!!        type(ctl_array_real), intent(inout) :: tgt_r1
!!      subroutine append_control_item_real(read_r1, array_r1)
!!        type(read_real_item), intent(in) ::    read_r1
!!        type(ctl_array_real), intent(inout) :: array_r1
!!@endverbatim
!!
!!
      module t_control_array_real
!
      use m_precision
      use t_control_elements
!
      implicit none
!
!>  Structure for real control array 
      type ctl_array_real
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st real
        real(kind = kreal), allocatable :: vect(:)
      end type ctl_array_real
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_real(array_real)
!
      type(ctl_array_real), intent(inout) :: array_real
!
!
      allocate( array_real%vect(array_real%num) )
!
      if(array_real%num .eq. 0) return
      array_real%vect = 0.0d0
!
      end subroutine alloc_control_array_real
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_real(array_real)
!
      type(ctl_array_real), intent(inout) :: array_real
!
!
      if(allocated(array_real%vect) .eqv. .FALSE.) return
      deallocate(array_real%vect)
      array_real%num = 0
!
      end subroutine dealloc_control_array_real
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_r1                                  &
     &         (id_control, label, array_real, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_real), intent(inout) :: array_real
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_real_item) :: read_r1
!
!
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_real%icou .gt. 0) return
!
      read_r1%iflag = 0
      array_real%num =  0
      call alloc_control_array_real(array_real)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_real_ctl_type(c_buf, label, read_r1)
          call append_control_array_real(read_r1, array_real)
        end if
      end do
!
      end subroutine read_control_array_r1
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_r1                                 &
     &         (id_control, level, label, array_real)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_real), intent(in) :: array_real
!
      integer(kind = kint) :: i, length
!
!
      if(array_real%num .le. 0) return
      write(id_control,'(a1)') '!'
!
      call write_array_flag_for_ctl                                     &
     &   (id_control, level, label, array_real%num)
      do i = 1, array_real%num
        length = len_trim(label)
        call write_real_ctl_item(id_control, (level+1), length, label,  &
     &     array_real%vect(i))
      end do
      call write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_r1
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_real(read_r1, array_r1)
!
      type(read_real_item), intent(inout) ::    read_r1
      type(ctl_array_real), intent(inout) :: array_r1
!
      type(ctl_array_real) ::    org_r1
!
!
      org_r1%num = array_r1%num
      call alloc_control_array_real(org_r1)
      call copy_control_array_real(org_r1%num, array_r1, org_r1)
      call dealloc_control_array_real(array_r1)
!
      array_r1%num = org_r1%num + 1
      call alloc_control_array_real(array_r1)
      call copy_control_array_real(org_r1%num, org_r1, array_r1)
      call append_control_item_real(read_r1, array_r1)
      read_r1%iflag = 0
!
      call dealloc_control_array_real(org_r1)
!
      end subroutine append_control_array_real
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_real(num_copy, org_r1, tgt_r1)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_real), intent(in) ::    org_r1
      type(ctl_array_real), intent(inout) :: tgt_r1
!
!
      if(num_copy .le. 0) return
      tgt_r1%icou = org_r1%icou
      tgt_r1%vect(1:num_copy) = org_r1%vect(1:num_copy)
!
      end subroutine copy_control_array_real
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_real(read_r1, array_r1)
!
      type(read_real_item), intent(in) ::    read_r1
      type(ctl_array_real), intent(inout) :: array_r1
!
!
      array_r1%icou = array_r1%icou + read_r1%iflag
      array_r1%vect(array_r1%num) = read_r1%realvalue
!
      end subroutine append_control_item_real
!
! -----------------------------------------------------------------------
!
      end module t_control_array_real
