!>@file   t_control_array_real3.f90
!!        module t_control_array_real3
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine read_real3_ctl_type(c_buf, label, real3_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_real3_item), intent(inout) :: real3_item
!!      subroutine write_real3_ctl_type                                 &
!!     &         (id_file, level, maxlen, label, real3_item)
!!        type(read_real3_item), intent(in) :: real3_item
!!      subroutine copy_real3_ctl(org_r3, new_r3)
!!        type(read_real3_item), intent(in) :: org_r3
!!        type(read_real3_item), intent(inout) :: new_r3
!!
!!      subroutine alloc_control_array_r3(array_r3)
!!      subroutine dealloc_control_array_r3(array_r3)
!!      subroutine read_control_array_r3                                &
!!     &         (id_control, label, array_r3, c_buf)
!!        type(ctl_array_r3), intent(inout) :: array_r3
!!        type(buffer_for_control), intent(in)  :: c_buf
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
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with three reals
      type read_real3_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read real items
        real(kind = kreal) ::    realvalue(3)
      end type read_real3_item
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
      subroutine read_real3_ctl_type(c_buf, label, real3_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_real3_item), intent(inout) :: real3_item
!
       character(len=kchara) :: tmpchara
!
!
      if(real3_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, real3_item%realvalue(1:3)
      if (iflag_debug .gt. 0)  write(*,'(a,a2,1p3e16.7)')               &
     &        trim(c_buf%header_chara), ': ', real3_item%realvalue(1:3)
      real3_item%iflag = 1
!
       end subroutine read_real3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_real3_ctl_type                                   &
     &         (id_file, level, maxlen, label, real3_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      type(read_real3_item), intent(in) :: real3_item
!
!
      if(real3_item%iflag .eq. 0) return
      call write_real3_ctl_item(id_file, level, maxlen, label,          &
     &    real3_item%realvalue(1), real3_item%realvalue(2),             &
     &    real3_item%realvalue(3))
!
       end subroutine write_real3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_real3_ctl(org_r3, new_r3)
!
      type(read_real3_item), intent(in) :: org_r3
      type(read_real3_item), intent(inout) :: new_r3
!
!
      new_r3%iflag =          org_r3%iflag
      new_r3%realvalue(1:3) = org_r3%realvalue(1:3)
!
       end subroutine copy_real3_ctl
!
!   --------------------------------------------------------------------
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
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
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
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_r3), intent(in) :: array_r3
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(array_r3%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level, label)
      do i = 1, array_r3%num
        call write_real3_ctl_item                                       &
     &     (id_control, level, len_trim(label), label,                  &
     &      array_r3%vec1(i), array_r3%vec2(i), array_r3%vec3(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level, label)
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
