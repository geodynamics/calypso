!>@file   t_control_array_charareal.f90
!!        module t_control_array_charareal
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read char-real control arrays
!!
!!@verbatim
!!      subroutine read_charareal_ctl_type(c_buf, label, cr_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara_real_item), intent(inout) :: cr_item
!!      subroutine write_charareal_ctl_type                             &
!!     &         (id_file, level, label, cr_item)
!!        type(read_chara_real_item), intent(in) :: cr_item
!!      subroutine copy_charareal_ctl(org_cr, new_cr)
!!        type(read_chara_real_item), intent(in) :: org_cr
!!        type(read_chara_real_item), intent(inout) :: new_cr
!!
!!      subroutine alloc_control_array_c_r(array_cr)
!!      subroutine dealloc_control_array_c_r(array_cr)
!!      subroutine read_control_array_c_r                               &
!!     &         (id_control, label, array_cr, c_buf)
!!        type(ctl_array_cr), intent(inout) :: array_cr
!!        type(buffer_for_control), intent(in)  :: c_buf
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
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with three characters
      type read_chara_real_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_chara_real_item
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
      subroutine read_charareal_ctl_type(c_buf, label, cr_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara_real_item), intent(inout) :: cr_item
!
       character(len=kchara) :: tmpchara
!
!
      if(cr_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, cr_item%charavalue,            &
     &                         cr_item%realvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,a)')                      &
     &         trim(c_buf%header_chara), ' char: ', cr_item%charavalue
      if (iflag_debug .gt. 0)  write(*,'(a,a4,1pe23.15)')               &
     &         trim(c_buf%header_chara), ' vect: ', cr_item%realvalue
      cr_item%iflag = 1
!
      end subroutine read_charareal_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_charareal_ctl_type                               &
     &         (id_file, level, label, cr_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(read_chara_real_item), intent(in) :: cr_item
!
      integer(kind = kint) :: maxlen(0:1)
!
!
      if(cr_item%iflag .eq. 0) return
!
      maxlen(0) = len_trim(label)
      maxlen(1) = len_trim(cr_item%charavalue)
!
      call write_chara_real_ctl_item(id_file, level, maxlen, label,     &
     &    cr_item%charavalue, cr_item%realvalue)
!
       end subroutine write_charareal_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_charareal_ctl(org_cr, new_cr)
!
      type(read_chara_real_item), intent(in) :: org_cr
      type(read_chara_real_item), intent(inout) :: new_cr
!
!
      new_cr%iflag =       org_cr%iflag
      new_cr%charavalue =  org_cr%charavalue
      new_cr%realvalue =   org_cr%realvalue
!
       end subroutine copy_charareal_ctl
!
!   --------------------------------------------------------------------
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
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
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
      use write_control_items
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_cr), intent(in) :: array_cr
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
      integer(kind = kint) :: maxlen(0:1)
!
!
      if(array_cr%num .le. 0) return
!
      maxlen(0) = len_trim(label)
      maxlen(1) = max_len_of_charaarray(array_cr%num, array_cr%c_tbl)
!
      level = write_array_flag_for_ctl(id_control, level, label)
      do i = 1, array_cr%num
        call write_chara_real_ctl_item(id_control, level, maxlen,       &
     &      label, array_cr%c_tbl(i), array_cr%vect(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level, label)
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
