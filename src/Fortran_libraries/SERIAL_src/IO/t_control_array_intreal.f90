!>@file   t_control_array_intreal.f90
!!        module t_control_array_intreal
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine read_intreal_ctl_type(c_buf, label, ir_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int_real_item), intent(inout) :: ir_item
!!      subroutine write_intreal_ctl_type                               &
!!     &         (id_file, level, maxlen, label, ir_item)
!!        type(read_int_real_item), intent(in) :: ir_item
!!      subroutine copy_intreal_ctl(org_ir, new_ir)
!!        type(read_int_real_item), intent(in) :: org_ir
!!        type(read_int_real_item), intent(inout) :: new_ir
!!
!!      subroutine alloc_control_array_i_r(array_ir)
!!      subroutine dealloc_control_array_i_r(array_ir)
!!      subroutine read_control_array_i_r                               &
!!     &         (id_control, label, array_ir, c_buf)
!!        type(ctl_array_ir), intent(inout) :: array_ir
!!        type(buffer_for_control), intent(in)  :: c_buf
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
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with three characters
      type read_int_real_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_int_real_item
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
      subroutine read_intreal_ctl_type(c_buf, label, ir_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int_real_item), intent(inout) :: ir_item
!
      character(len=kchara) :: tmpchara
!
!
      if(ir_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, ir_item%intvalue,              &
     &                         ir_item%realvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,i16)')                    &
     &         trim(c_buf%header_chara), ' int:  ', ir_item%intvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1pe23.15)')               &
     &         trim(c_buf%header_chara), ' real: ', ir_item%realvalue
      ir_item%iflag = 1
!
      end subroutine read_intreal_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_intreal_ctl_type                                 &
     &         (id_file, level, maxlen, label, ir_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level, maxlen
      character(len=kchara), intent(in) :: label
      type(read_int_real_item), intent(in) :: ir_item
!
!
      if(ir_item%iflag .eq. 0) return
!
      call write_int_real_ctl_item(id_file, level, maxlen, label,       &
     &    ir_item%intvalue, ir_item%realvalue)
!
       end subroutine write_intreal_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_intreal_ctl(org_ir, new_ir)
!
      type(read_int_real_item), intent(in) :: org_ir
      type(read_int_real_item), intent(inout) :: new_ir
!
!
      new_ir%iflag =     org_ir%iflag
      new_ir%intvalue =  org_ir%intvalue
      new_ir%realvalue = org_ir%realvalue
!
       end subroutine copy_intreal_ctl
!
!   --------------------------------------------------------------------
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
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
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
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_ir), intent(in) :: array_ir
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(array_ir%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level, label)
      do i = 1, array_ir%num
        call write_int_real_ctl_item                                    &
     &     (id_control, level, len_trim(label), label,                  &
     &      array_ir%ivec(i), array_ir%vect(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level, label)
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
