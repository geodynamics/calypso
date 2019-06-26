!>@file   t_control_array_real3.f90
!!@brief  module t_control_array_real3
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine alloc_control_array_r3(array_r3)
!!      subroutine dealloc_control_array_r3(array_r3)
!!      subroutine read_control_array_r3                                &
!!     &         (id_control, label, array_r3, c_buf)
!!        type(ctl_array_r3), intent(inout) :: array_r3
!!      subroutine write_control_array_r3                               &
!!     &         (id_control, level, label, array_r3)
!!        type(ctl_array_r3), intent(in) :: array_r3
!!
!!      subroutine append_control_array_r3(read_r3, array_r3)
!!        type(read_real3_item), intent(inout) ::    read_r3
!!        type(ctl_array_r3), intent(inout) :: array_r3
!!      subroutine dup_control_array_r3(org_r3, tgt_r3)
!!        type(ctl_array_r3), intent(in) ::    org_r3
!!        type(ctl_array_r3), intent(inout) :: tgt_r3
!!      subroutine copy_control_array_r3(num_copy, org_r3, tgt_r3)
!!        type(ctl_array_r3), intent(in) ::    org_r3
!!        type(ctl_array_r3), intent(inout) :: tgt_r3
!!      subroutine append_control_item_r3(read_r3, array_r3)
!!        type(read_real3_item), intent(in) ::    read_r3
!!        type(ctl_array_r3), intent(inout) :: array_r3
!!@endverbatim
!!
!!
      module t_control_array_real3
!
      use m_precision
      use t_control_elements
!
      implicit none
!
!>  Structure for three reals control array 
      type ctl_array_r3
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st real
        real(kind = kreal), allocatable :: vec1(:)
!>     array for 2nd real
        real(kind = kreal), allocatable :: vec2(:)
!>     array for 3rd real
        real(kind = kreal), allocatable :: vec3(:)
      end type ctl_array_r3
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_r3(array_r3)
!
      type(ctl_array_r3), intent(inout) :: array_r3
!
!
      allocate( array_r3%vec1(array_r3%num) )
      allocate( array_r3%vec2(array_r3%num) )
      allocate( array_r3%vec3(array_r3%num) )
!
      if(array_r3%num .eq. 0) return
      array_r3%vec1 = 0.0d0
      array_r3%vec2 = 0.0d0
      array_r3%vec3 = 0.0d0
!
      end subroutine alloc_control_array_r3
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_r3(array_r3)
!
      type(ctl_array_r3), intent(inout) :: array_r3
!
!
      if(allocated(array_r3%vec1) .eqv. .FALSE.) return
      deallocate(array_r3%vec1, array_r3%vec2, array_r3%vec3)
      array_r3%num = 0
!
      end subroutine dealloc_control_array_r3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_r3                                  &
     &         (id_control, label, array_r3, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_r3), intent(inout) :: array_r3
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_real3_item) :: read_r3
!
!
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_r3%icou .gt. 0) return
!
      read_r3%iflag = 0
      array_r3%num =  0
      call alloc_control_array_r3(array_r3)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_real3_ctl_type(c_buf, label, read_r3)
          call append_control_array_r3(read_r3, array_r3)
        end if
      end do
!
      end subroutine read_control_array_r3
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_r3                                 &
     &         (id_control, level, label, array_r3)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_r3), intent(in) :: array_r3
!
      integer(kind = kint) :: i
!
!
      if(array_r3%num .le. 0) return
      write(id_control,'(a1)') '!'
!
      call write_array_flag_for_ctl                                     &
     &   (id_control, level, label, array_r3%num)
      do i = 1, array_r3%num
        call write_real3_ctl_item(id_control, (level+1), label,         &
     &     array_r3%vec1(i), array_r3%vec2(i), array_r3%vec3(i))
      end do
      call write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_r3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_r3(read_r3, array_r3)
!
      type(read_real3_item), intent(inout) ::    read_r3
      type(ctl_array_r3), intent(inout) :: array_r3
!
      type(ctl_array_r3) ::    org_r3
!
!
      org_r3%num = array_r3%num
      call alloc_control_array_r3(org_r3)
      call copy_control_array_r3(org_r3%num, array_r3, org_r3)
      call dealloc_control_array_r3(array_r3)
!
      array_r3%num = org_r3%num + 1
      call alloc_control_array_r3(array_r3)
      call copy_control_array_r3(org_r3%num, org_r3, array_r3)
      call append_control_item_r3(read_r3, array_r3)
      read_r3%iflag = 0
!
      call dealloc_control_array_r3(org_r3)
!
      end subroutine append_control_array_r3
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_array_r3(org_r3, tgt_r3)
!
      type(ctl_array_r3), intent(in) ::    org_r3
      type(ctl_array_r3), intent(inout) :: tgt_r3
!
!
      tgt_r3%num = org_r3%num
      call alloc_control_array_r3(tgt_r3)
      call copy_control_array_r3(org_r3%num, org_r3, tgt_r3)
!
      end subroutine dup_control_array_r3
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_r3(num_copy, org_r3, tgt_r3)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_r3), intent(in) ::    org_r3
      type(ctl_array_r3), intent(inout) :: tgt_r3
!
!
      if(num_copy .le. 0) return
      tgt_r3%icou = org_r3%icou
      tgt_r3%vec1(1:num_copy) = org_r3%vec1(1:num_copy)
      tgt_r3%vec2(1:num_copy) = org_r3%vec2(1:num_copy)
      tgt_r3%vec3(1:num_copy) = org_r3%vec3(1:num_copy)
!
      end subroutine copy_control_array_r3
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_r3(read_r3, array_r3)
!
      type(read_real3_item), intent(in) ::    read_r3
      type(ctl_array_r3), intent(inout) :: array_r3
!
!
      array_r3%icou = array_r3%icou + read_r3%iflag
      array_r3%vec1(array_r3%num) = read_r3%realvalue(1)
      array_r3%vec2(array_r3%num) = read_r3%realvalue(2)
      array_r3%vec3(array_r3%num) = read_r3%realvalue(3)
!
      end subroutine append_control_item_r3
!
! -----------------------------------------------------------------------
!
      end module t_control_array_real3
