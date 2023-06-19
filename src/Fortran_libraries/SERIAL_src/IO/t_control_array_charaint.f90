!>@file   t_control_array_charaint.f90
!!        module t_control_array_charaint
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read char-int control arrays
!!
!!@verbatim
!!      subroutine read_charaint_ctl_type(c_buf, label, ci_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara_int_item), intent(inout) :: ci_item
!!      subroutine write_charaint_ctl_type                              &
!!     &         (id_file, level, maxlen, label, ci_item)
!!        type(read_chara_int_item), intent(in) :: ci_item
!!      subroutine copy_charaint_ctl(org_ci, new_ci)
!!        type(read_chara_int_item), intent(in) :: org_ci
!!        type(read_chara_int_item), intent(inout) :: new_ci
!!
!!      subroutine alloc_control_array_c_i(array_ci)
!!      subroutine dealloc_control_array_c_i(array_ci)
!!      subroutine read_control_array_c_i                               &
!!     &         (id_control, label, array_ci, c_buf)
!!        type(ctl_array_ci), intent(inout) :: array_ci
!!        type(buffer_for_control), intent(in)  :: c_buf
!!      subroutine write_control_array_c_i                              &
!!     &         (id_control, level, label, array_ci)
!!        type(ctl_array_ci), intent(in) :: array_ci
!!
!!      subroutine append_control_array_c_i(read_ci, array_ci)
!!        type(read_chara_int_item), intent(inout) ::    read_ci
!!        type(ctl_array_ci), intent(inout) :: array_ci
!!      subroutine dup_control_array_c_i(org_ci, tgt_ci)
!!      subroutine copy_control_array_c_i(num_copy, org_ci, tgt_ci)
!!        type(ctl_array_ci), intent(in) ::    org_ci
!!        type(ctl_array_ci), intent(inout) :: tgt_ci
!!      subroutine append_control_item_c_i(read_ci, array_ci)
!!        type(read_chara_int_item), intent(in) ::    read_ci
!!        type(ctl_array_ci), intent(inout) :: array_ci
!!@endverbatim
!!
!!
      module t_control_array_charaint
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with three characters
      type read_chara_int_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue
!>        array for read integer items
        integer(kind = kint) ::  intvalue
      end type read_chara_int_item
!
!>  Structure for charactor and integer control array 
      type ctl_array_ci
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st character
        character(len=kchara), allocatable :: c_tbl(:)
!>     array for 1st integer
        integer(kind = kint), allocatable :: ivec(:)
      end type ctl_array_ci
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_charaint_ctl_type(c_buf, label, ci_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara_int_item), intent(inout) :: ci_item
!
       character(len=kchara) :: tmpchara
!
!
      if(ci_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, ci_item%charavalue,            &
     &                        ci_item%intvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,a)')                      &
     &          trim(c_buf%header_chara), ' char: ', ci_item%charavalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,i16)')                    &
     &          trim(c_buf%header_chara), ' int:  ', ci_item%intvalue
      ci_item%iflag = 1
!
      end subroutine read_charaint_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_charaint_ctl_type                                &
     &         (id_file, level, maxlen, label, ci_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      type(read_chara_int_item), intent(in) :: ci_item
!
!
      if(ci_item%iflag .eq. 0) return
!
      call write_chara_int_ctl_item(id_file, level, maxlen, label,      &
     &    ci_item%charavalue, ci_item%intvalue)
!
       end subroutine write_charaint_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_charaint_ctl(org_ci, new_ci)
!
      type(read_chara_int_item), intent(in) :: org_ci
      type(read_chara_int_item), intent(inout) :: new_ci
!
!
      new_ci%iflag =       org_ci%iflag
      new_ci%charavalue =  org_ci%charavalue
      new_ci%intvalue =    org_ci%intvalue
!
       end subroutine copy_charaint_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c_i(array_ci)
!
      type(ctl_array_ci), intent(inout) :: array_ci
!
!
      allocate( array_ci%c_tbl(array_ci%num) )
      allocate( array_ci%ivec(array_ci%num) )
!
      if(array_ci%num .eq. 0) return
      array_ci%ivec = 0
!
      end subroutine alloc_control_array_c_i
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c_i(array_ci)
!
      type(ctl_array_ci), intent(inout) :: array_ci
!
!
      if(allocated(array_ci%c_tbl) .eqv. .FALSE.) return
      deallocate( array_ci%c_tbl, array_ci%ivec)
      array_ci%num = 0
!
      end subroutine dealloc_control_array_c_i
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c_i                                 &
     &         (id_control, label, array_ci, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_ci), intent(inout) :: array_ci
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_chara_int_item) :: read_ci
!
!
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
      if(array_ci%icou .gt. 0) return
!
      read_ci%iflag = 0
      array_ci%num =  0
      call alloc_control_array_c_i(array_ci)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_charaint_ctl_type(c_buf, label, read_ci)
          call append_control_array_c_i(read_ci, array_ci)
        end if
      end do
!
      end subroutine read_control_array_c_i
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c_i                                &
     &         (id_control, level, label, array_ci)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_ci), intent(in) :: array_ci
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(array_ci%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level, label)
      do i = 1, array_ci%num
        call write_chara_int_ctl_item                                   &
     &     (id_control, level, len_trim(label), label,                  &
     &      array_ci%c_tbl(i), array_ci%ivec(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level, label)
!
      end subroutine write_control_array_c_i
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c_i(read_ci, array_ci)
!
      type(read_chara_int_item), intent(inout) ::    read_ci
      type(ctl_array_ci), intent(inout) :: array_ci
!
      type(ctl_array_ci) ::    org_ci
!
!
      org_ci%num = array_ci%num
      call alloc_control_array_c_i(org_ci)
      call copy_control_array_c_i(org_ci%num, array_ci, org_ci)
      call dealloc_control_array_c_i(array_ci)
!
      array_ci%num = org_ci%num + 1
      call alloc_control_array_c_i(array_ci)
      call copy_control_array_c_i(org_ci%num, org_ci, array_ci)
      call append_control_item_c_i(read_ci, array_ci)
      read_ci%iflag = 0
!
      call dealloc_control_array_c_i(org_ci)
!
      end subroutine append_control_array_c_i
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_array_c_i(org_ci, tgt_ci)
!
      type(ctl_array_ci), intent(in) ::    org_ci
      type(ctl_array_ci), intent(inout) :: tgt_ci
!
!
      tgt_ci%num = org_ci%num
      call alloc_control_array_c_i(tgt_ci)
      call copy_control_array_c_i(org_ci%num, org_ci, tgt_ci)
!
      end subroutine dup_control_array_c_i
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c_i(num_copy, org_ci, tgt_ci)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_ci), intent(in) ::    org_ci
      type(ctl_array_ci), intent(inout) :: tgt_ci
!
!
      if(num_copy .le. 0) return
      tgt_ci%icou = org_ci%icou
      tgt_ci%c_tbl(1:num_copy) = org_ci%c_tbl(1:num_copy)
      tgt_ci%ivec(1:num_copy) =  org_ci%ivec(1:num_copy)
!
      end subroutine copy_control_array_c_i
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c_i(read_ci, array_ci)
!
      type(read_chara_int_item), intent(in) ::    read_ci
      type(ctl_array_ci), intent(inout) :: array_ci
!
!
      array_ci%icou = array_ci%icou + read_ci%iflag
      array_ci%c_tbl(array_ci%num) = read_ci%charavalue
      array_ci%ivec(array_ci%num) =  read_ci%intvalue
!
      end subroutine append_control_item_c_i
!
! -----------------------------------------------------------------------
!
      end module t_control_array_charaint
