!>@file   t_control_array_intchrreal.f90
!!@brief  module t_control_array_intchrreal
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine alloc_control_array_i_c_r(array_icr)
!!      subroutine dealloc_control_array_i_c_r(array_icr)
!!      subroutine read_control_array_i_c_r                             &
!!     &         (id_control, label, array_icr, c_buf)
!!        type(ctl_array_icr), intent(inout) :: array_icr
!!      subroutine write_control_array_i_c_r                            &
!!     &         (id_control, level, label, array_icr)
!!        type(ctl_array_icr), intent(in) :: array_icr
!!
!!      subroutine append_control_array_i_c_r(read_icr, array_icr)
!!        type(read_int_chara_real_item), intent(inout) ::    read_icr
!!        type(ctl_array_icr), intent(inout) :: array_icr
!!      subroutine copy_control_array_i_c_r(num_copy, org_icr, tgt_icr)
!!        type(ctl_array_icr), intent(in) ::    org_icr
!!        type(ctl_array_icr), intent(inout) :: tgt_icr
!!      subroutine append_control_item_i_c_r(read_icr, array_icr)
!!        type(read_int_chara_real_item), intent(in) ::    read_icr
!!        type(ctl_array_icr), intent(inout) :: array_icr
!!@endverbatim
!!
!!
      module t_control_array_intchrreal
!
      use m_precision
      use t_control_elements
!
      implicit none
!
!>  Structure for integere, charactor, and real control array 
      type ctl_array_icr
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st integer
        integer(kind=kint), allocatable :: ivec(:)
!>     array for character
        character(len=kchara), allocatable :: c_tbl(:)
!>     array for 1st real
        real(kind = kreal), allocatable :: vect(:)
      end type ctl_array_icr
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_i_c_r(array_icr)
!
      type(ctl_array_icr), intent(inout) :: array_icr
!
!
      allocate( array_icr%ivec(array_icr%num) )
      allocate( array_icr%c_tbl(array_icr%num) )
      allocate( array_icr%vect(array_icr%num) )
!
      if(array_icr%num .eq. 0) return
      array_icr%ivec =     0
      array_icr%vect = 0.0d0
!
      end subroutine alloc_control_array_i_c_r
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_i_c_r(array_icr)
!
      type(ctl_array_icr), intent(inout) :: array_icr
!
!
      if(allocated(array_icr%ivec) .eqv. .FALSE.) return
      deallocate( array_icr%ivec, array_icr%c_tbl, array_icr%vect)
      array_icr%num = 0
!
      end subroutine dealloc_control_array_i_c_r
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_i_c_r                               &
     &         (id_control, label, array_icr, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_icr), intent(inout) :: array_icr
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_int_chara_real_item) :: read_icr
!
!
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_icr%icou .gt. 0) return
!
      read_icr%iflag = 0
      array_icr%num =  0
      call alloc_control_array_i_c_r(array_icr)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_intchrreal_ctl_type(c_buf, label, read_icr)
          call append_control_array_i_c_r(read_icr, array_icr)
        end if
      end do
!
      end subroutine read_control_array_i_c_r
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i_c_r                              &
     &         (id_control, level, label, array_icr)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_icr), intent(in) :: array_icr
!
      integer(kind = kint) :: i
!
!
      if(array_icr%num .le. 0) return
      write(id_control,'(a1)') '!'
!
      call write_array_flag_for_ctl                                     &
     &   (id_control, level, label, array_icr%num)
      do i = 1, array_icr%num
        call write_i_c_r_ctl_item(id_control, (level+1), label,         &
     &      array_icr%ivec(i), array_icr%c_tbl(i), array_icr%vect(i))
      end do
      call write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_i_c_r
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_i_c_r(read_icr, array_icr)
!
      type(read_int_chara_real_item), intent(inout) ::    read_icr
      type(ctl_array_icr), intent(inout) :: array_icr
!
      type(ctl_array_icr) ::    org_icr
!
!
      org_icr%num = array_icr%num
      call alloc_control_array_i_c_r(org_icr)
      call copy_control_array_i_c_r(org_icr%num, array_icr, org_icr)
      call dealloc_control_array_i_c_r(array_icr)
!
      array_icr%num = org_icr%num + 1
      call alloc_control_array_i_c_r(array_icr)
      call copy_control_array_i_c_r(org_icr%num, org_icr, array_icr)
      call append_control_item_i_c_r(read_icr, array_icr)
      read_icr%iflag = 0
!
      call dealloc_control_array_i_c_r(org_icr)
!
      end subroutine append_control_array_i_c_r
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_i_c_r(num_copy, org_icr, tgt_icr)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_icr), intent(in) ::    org_icr
      type(ctl_array_icr), intent(inout) :: tgt_icr
!
!
      if(num_copy .le. 0) return
      tgt_icr%icou = org_icr%icou
      tgt_icr%ivec(1:num_copy) =  org_icr%ivec(1:num_copy)
      tgt_icr%c_tbl(1:num_copy) = org_icr%c_tbl(1:num_copy)
      tgt_icr%vect(1:num_copy) =  org_icr%vect(1:num_copy)
!
      end subroutine copy_control_array_i_c_r
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_i_c_r(read_icr, array_icr)
!
      type(read_int_chara_real_item), intent(in) ::    read_icr
      type(ctl_array_icr), intent(inout) :: array_icr
!
!
      array_icr%icou = array_icr%icou + read_icr%iflag
      array_icr%ivec(array_icr%num) =  read_icr%intvalue
      array_icr%c_tbl(array_icr%num) = read_icr%charavalue
      array_icr%vect(array_icr%num) =  read_icr%realvalue
!
      end subroutine append_control_item_i_c_r
!
! -----------------------------------------------------------------------
!
      end module t_control_array_intchrreal
