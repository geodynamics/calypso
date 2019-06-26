!>@file   t_control_array_intreal.f90
!!@brief  module t_control_array_intreal
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine alloc_control_array_i_r(array_ir)
!!      subroutine dealloc_control_array_i_r(array_ir)
!!      subroutine read_control_array_i_r                               &
!!     &         (id_control, label, array_ir, c_buf)
!!        type(ctl_array_ir), intent(inout) :: array_ir
!!      subroutine write_control_array_i_r                              &
!!     &         (id_control, level, label, array_ir)
!!        type(ctl_array_ir), intent(in) :: array_ir
!!
!!      subroutine append_control_array_i_r(read_ir, array_ir)
!!        type(read_int_real_item), intent(inout) ::    read_ir
!!        type(ctl_array_ir), intent(inout) :: array_ir
!!      subroutine copy_control_array_i_r(num_copy, org_ir, tgt_ir)
!!        type(ctl_array_ir), intent(in) ::    org_ir
!!        type(ctl_array_ir), intent(inout) :: tgt_ir
!!      subroutine append_control_item_i_r(read_ir, array_ir)
!!        type(read_int_real_item), intent(in) ::    read_ir
!!        type(ctl_array_ir), intent(inout) :: array_ir
!!@endverbatim
!!
!!
      module t_control_array_intreal
!
      use m_precision
      use t_control_elements
!
      implicit none
!
!>  Structure for real and integer control array 
      type ctl_array_ir
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st integer
        integer(kind=kint), allocatable :: ivec(:)
!>     array for 1st real
        real(kind = kreal), allocatable :: vect(:)
      end type ctl_array_ir
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_i_r(array_ir)
!
      type(ctl_array_ir), intent(inout) :: array_ir
!
!
      allocate( array_ir%ivec(array_ir%num) )
      allocate( array_ir%vect(array_ir%num) )
!
      if(array_ir%num .eq. 0) return
      array_ir%ivec =     0
      array_ir%vect = 0.0d0
!
      end subroutine alloc_control_array_i_r
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_i_r(array_ir)
!
      type(ctl_array_ir), intent(inout) :: array_ir
!
!
      if(allocated(array_ir%ivec) .eqv. .FALSE.) return
      deallocate( array_ir%ivec, array_ir%vect)
      array_ir%num = 0
!
      end subroutine dealloc_control_array_i_r
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i_r                                 &
     &         (id_control, label, array_ir, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_ir), intent(inout) :: array_ir
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_int_real_item) :: read_ir
!
!
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_ir%icou .gt. 0) return
!
      read_ir%iflag = 0
      array_ir%num =  0
      call alloc_control_array_i_r(array_ir)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_intreal_ctl_type(c_buf, label, read_ir)
          call append_control_array_i_r(read_ir, array_ir)
        end if
      end do
!
      end subroutine read_control_array_i_r
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i_r                                &
     &         (id_control, level, label, array_ir)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_ir), intent(in) :: array_ir
!
      integer(kind = kint) :: i
!
!
      if(array_ir%num .le. 0) return
      write(id_control,'(a1)') '!'
!
      call write_array_flag_for_ctl                                     &
     &   (id_control, level, label, array_ir%num)
      do i = 1, array_ir%num
        call write_int_real_ctl_item(id_control, (level+1), label,      &
     &      array_ir%ivec(i), array_ir%vect(i))
      end do
      call write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_i_r
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_i_r(read_ir, array_ir)
!
      type(read_int_real_item), intent(inout) ::    read_ir
      type(ctl_array_ir), intent(inout) :: array_ir
!
      type(ctl_array_ir) ::    org_ir
!
!
      org_ir%num = array_ir%num
      call alloc_control_array_i_r(org_ir)
      call copy_control_array_i_r(org_ir%num, array_ir, org_ir)
      call dealloc_control_array_i_r(array_ir)
!
      array_ir%num = org_ir%num + 1
      call alloc_control_array_i_r(array_ir)
      call copy_control_array_i_r(org_ir%num, org_ir, array_ir)
      call append_control_item_i_r(read_ir, array_ir)
      read_ir%iflag = 0
!
      call dealloc_control_array_i_r(org_ir)
!
      end subroutine append_control_array_i_r
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_i_r(num_copy, org_ir, tgt_ir)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_ir), intent(in) ::    org_ir
      type(ctl_array_ir), intent(inout) :: tgt_ir
!
!
      if(num_copy .le. 0) return
      tgt_ir%icou = org_ir%icou
      tgt_ir%ivec(1:num_copy) = org_ir%ivec(1:num_copy)
      tgt_ir%vect(1:num_copy) = org_ir%vect(1:num_copy)
!
      end subroutine copy_control_array_i_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_i_r(read_ir, array_ir)
!
      type(read_int_real_item), intent(in) ::    read_ir
      type(ctl_array_ir), intent(inout) :: array_ir
!
!
      array_ir%icou = array_ir%icou + read_ir%iflag
      array_ir%ivec(array_ir%num) = read_ir%intvalue
      array_ir%vect(array_ir%num) = read_ir%realvalue
!
      end subroutine append_control_item_i_r
!
! -----------------------------------------------------------------------
!
      end module t_control_array_intreal
