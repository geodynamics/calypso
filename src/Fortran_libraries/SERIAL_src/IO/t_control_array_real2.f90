!>@file   t_control_array_real2.f90
!!        module t_control_array_real2
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine read_real2_ctl_type(c_buf, label, real2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_real2_item), intent(inout) :: real2_item
!!      subroutine write_real2_ctl_type                                 &
!!     &         (id_file, level, maxlen, label, real2_item)
!!        type(read_real2_item), intent(in) :: real2_item
!!      subroutine copy_real2_ctl(org_r2, new_r2)
!!        type(read_real2_item), intent(inout) :: org_r2
!!        type(read_real2_item), intent(inout) :: new_r2
!!
!!      subroutine alloc_control_array_r2(array_r2)
!!      subroutine dealloc_control_array_r2(array_r2)
!!      subroutine read_control_array_r2                                &
!!     &         (id_control, label, array_r2, c_buf)
!!        type(ctl_array_r2), intent(inout) :: array_r2
!!        type(buffer_for_control), intent(in)  :: c_buf
!!      subroutine write_control_array_r2                               &
!!     &         (id_control, level, label, array_r2)
!!        type(ctl_array_r2), intent(in) :: array_r2
!!
!!      subroutine append_control_array_r2(read_r2, array_r2)
!!        type(read_real2_item), intent(inout) ::    read_r2
!!        type(ctl_array_r2), intent(inout) :: array_r2
!!      subroutine dup_control_array_r2(org_r2, tgt_r2)
!!      subroutine copy_control_array_r2(num_copy, org_r2, tgt_r2)
!!        type(ctl_array_r2), intent(in) ::    org_r2
!!        type(ctl_array_r2), intent(inout) :: tgt_r2
!!      subroutine append_control_item_r2(read_r2, array_r2)
!!        type(read_real2_item), intent(in) ::    read_r2
!!        type(ctl_array_r2), intent(inout) :: array_r2
!!@endverbatim
!!
!!
      module t_control_array_real2
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with two reals
      type read_real2_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read real items
        real(kind = kreal) ::    realvalue(2)
      end type read_real2_item
!
!>  Structure for two reals control array 
      type ctl_array_r2
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st real
        real(kind = kreal), allocatable :: vec1(:)
!>     array for 2nd real
        real(kind = kreal), allocatable :: vec2(:)
      end type ctl_array_r2
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_real2_ctl_type(c_buf, label, real2_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_real2_item), intent(inout) :: real2_item
!
      character(len=kchara) :: tmpchara
!
!
      if(real2_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, real2_item%realvalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a2,1p3e16.7)')               &
     &        trim(c_buf%header_chara), ': ', real2_item%realvalue(1:2)
      real2_item%iflag = 1
!
       end subroutine read_real2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_real2_ctl_type                                   &
     &         (id_file, level, maxlen, label, real2_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      integer(kind = kint), intent(in) :: maxlen
      type(read_real2_item), intent(in) :: real2_item
!
!
      if(real2_item%iflag .eq. 0) return
      call write_real2_ctl_item(id_file, level, maxlen, label,          &
     &    real2_item%realvalue(1), real2_item%realvalue(2))
!
       end subroutine write_real2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_real2_ctl(org_r2, new_r2)
!
      type(read_real2_item), intent(in) :: org_r2
      type(read_real2_item), intent(inout) :: new_r2
!
!
      new_r2%iflag =          org_r2%iflag
      new_r2%realvalue(1:2) = org_r2%realvalue(1:2)
!
       end subroutine copy_real2_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_r2(array_r2)
!
      type(ctl_array_r2), intent(inout) :: array_r2
!
!
      allocate( array_r2%vec1(array_r2%num) )
      allocate( array_r2%vec2(array_r2%num) )
!
      if(array_r2%num .eq. 0) return
      array_r2%vec1 = 0.0d0
      array_r2%vec2 = 0.0d0
!
      end subroutine alloc_control_array_r2
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_r2(array_r2)
!
      type(ctl_array_r2), intent(inout) :: array_r2
!
!
      if(allocated(array_r2%vec1) .eqv. .FALSE.) return
      deallocate(array_r2%vec1, array_r2%vec2)
      array_r2%num = 0
!
      end subroutine dealloc_control_array_r2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_r2                                  &
     &         (id_control, label, array_r2, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_r2), intent(inout) :: array_r2
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_real2_item) :: read_r2
!
!
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_r2%icou .gt. 0) return
!
      read_r2%iflag = 0
      array_r2%num =  0
      call alloc_control_array_r2(array_r2)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_real2_ctl_type(c_buf, label, read_r2)
          call append_control_array_r2(read_r2, array_r2)
        end if
      end do
!
      end subroutine read_control_array_r2
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_r2                                 &
     &         (id_control, level, label, array_r2)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_r2), intent(in) :: array_r2
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(array_r2%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level, label)
      do i = 1, array_r2%num
        call write_real2_ctl_item(id_control, level, len_trim(label),   &
     &      label, array_r2%vec1(i), array_r2%vec2(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_r2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_r2(read_r2, array_r2)
!
      type(read_real2_item), intent(inout) ::    read_r2
      type(ctl_array_r2), intent(inout) :: array_r2
!
      type(ctl_array_r2) ::    org_r2
!
!
      org_r2%num = array_r2%num
      call alloc_control_array_r2(org_r2)
      call copy_control_array_r2(org_r2%num, array_r2, org_r2)
      call dealloc_control_array_r2(array_r2)
!
      array_r2%num = org_r2%num + 1
      call alloc_control_array_r2(array_r2)
      call copy_control_array_r2(org_r2%num, org_r2, array_r2)
      call append_control_item_r2(read_r2, array_r2)
      read_r2%iflag = 0
!
      call dealloc_control_array_r2(org_r2)
!
      end subroutine append_control_array_r2
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_array_r2(org_r2, tgt_r2)
!
      type(ctl_array_r2), intent(in) ::    org_r2
      type(ctl_array_r2), intent(inout) :: tgt_r2
!
!
      tgt_r2%num = org_r2%num
      call alloc_control_array_r2(tgt_r2)
      call copy_control_array_r2(org_r2%num, org_r2, tgt_r2)
!
      end subroutine dup_control_array_r2
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_r2(num_copy, org_r2, tgt_r2)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_r2), intent(in) ::    org_r2
      type(ctl_array_r2), intent(inout) :: tgt_r2
!
!
      if(num_copy .le. 0) return
      tgt_r2%icou = org_r2%icou
      tgt_r2%vec1(1:num_copy) = org_r2%vec1(1:num_copy)
      tgt_r2%vec2(1:num_copy) = org_r2%vec2(1:num_copy)
!
      end subroutine copy_control_array_r2
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_r2(read_r2, array_r2)
!
      type(read_real2_item), intent(in) ::    read_r2
      type(ctl_array_r2), intent(inout) :: array_r2
!
!
      array_r2%icou = array_r2%icou + read_r2%iflag
      array_r2%vec1(array_r2%num) = read_r2%realvalue(1)
      array_r2%vec2(array_r2%num) = read_r2%realvalue(2)
!
      end subroutine append_control_item_r2
!
! -----------------------------------------------------------------------
!
      end module t_control_array_real2
