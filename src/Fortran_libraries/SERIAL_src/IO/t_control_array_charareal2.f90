!>@file   t_control_array_charareal2.f90
!!        module t_control_array_charareal2
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read char-real-real control arrays
!!
!!@verbatim
!!      subroutine read_charreal2_ctl_type(c_buf, label, cr2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara_real2_item), intent(inout) :: cr2_item
!!      subroutine write_charreal2_ctl_type                             &
!!     &         (id_file, level, maxlen, cr2_item)
!!        type(read_chara_real2_item), intent(in) :: cr2_item
!!      subroutine copy_charreal2_ctl(org_cr2, new_cr2)
!!        type(read_chara_real2_item), intent(in) :: org_cr2
!!        type(read_chara_real2_item), intent(inout) :: new_cr2
!!
!!      subroutine alloc_control_array_c_r2(array_cr2)
!!      subroutine dealloc_control_array_c_r2(array_cr2)
!!      subroutine read_control_array_c_r2                              &
!!     &         (id_control, label, array_cr2, c_buf)
!!        type(ctl_array_cr2), intent(inout) :: array_cr2
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_control_array_c_r2                             &
!!     &         (id_control, level, array_cr2)
!!        type(ctl_array_cr2), intent(in) :: array_cr2
!!
!!      subroutine append_control_array_c_r2(read_cr2, array_cr2)
!!        type(read_chara_real2_item), intent(inout) ::    read_cr2
!!        type(ctl_array_cr2), intent(inout) :: array_cr2
!!      subroutine copy_control_array_c_r2(num_copy, org_cr2, tgt_cr2)
!!        type(ctl_array_cr2), intent(in) ::    org_cr2
!!        type(ctl_array_cr2), intent(inout) :: tgt_cr2
!!      subroutine append_control_item_c_r2(read_cr2, array_cr2)
!!        type(read_chara_real2_item), intent(in) ::    read_cr2
!!        type(ctl_array_cr2), intent(inout) :: array_cr2
!!@endverbatim
!!
!!
      module t_control_array_charareal2
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with three characters
      type read_chara_real2_item
!>        Item name
        character(len=kchara) :: item_name = 'char_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue
!>        array for read real item
        real(kind = kreal) ::    realvalue(2)
      end type read_chara_real2_item
!
!>  Structure for charactor and two reals control array 
      type ctl_array_cr2
!>        Item name
        character(len=kchara) :: array_name = 'char_array'
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for character
        character(len=kchara), allocatable :: c_tbl(:)
!>     array for 1st real
        real(kind = kreal), allocatable :: vec1(:)
!>     array for 2nd real
        real(kind = kreal), allocatable :: vec2(:)
      end type ctl_array_cr2
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_charreal2_ctl_type(c_buf, label, cr2_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara_real2_item), intent(inout) :: cr2_item
!
       character(len=kchara) :: tmpchara
!
!
      if(cr2_item%iflag.gt.0) return
      cr2_item%item_name = trim(label)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, cr2_item%charavalue,           &
     &                        cr2_item%realvalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf%header_chara), ' cr2_item%charavalue: ',   &
     &            cr2_item%charavalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1p2e23.15)')              &
     &     trim(c_buf%header_chara), ' real: ', cr2_item%realvalue(1:2)
      cr2_item%iflag = 1
!
      end subroutine read_charreal2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_charreal2_ctl_type                               &
     &         (id_file, level, maxlen, cr2_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level, maxlen
      type(read_chara_real2_item), intent(in) :: cr2_item
!
!
      if(cr2_item%iflag .eq. 0) return
!
      call write_chara_real2_ctl_item(id_file, level, maxlen,           &
     &    cr2_item%item_name, cr2_item%charavalue,                      &
     &    cr2_item%realvalue(1), cr2_item%realvalue(2))
!
       end subroutine write_charreal2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_charreal2_ctl(org_cr2, new_cr2)
!
      type(read_chara_real2_item), intent(in) :: org_cr2
      type(read_chara_real2_item), intent(inout) :: new_cr2
!
!
      new_cr2%item_name =      org_cr2%item_name
      new_cr2%iflag =          org_cr2%iflag
      new_cr2%charavalue =     org_cr2%charavalue
      new_cr2%realvalue(1:2) = org_cr2%realvalue(1:2)
!
      end subroutine copy_charreal2_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c_r2(array_cr2)
!
      type(ctl_array_cr2), intent(inout) :: array_cr2
!
!
      allocate( array_cr2%c_tbl(array_cr2%num) )
      allocate( array_cr2%vec1(array_cr2%num) )
      allocate( array_cr2%vec2(array_cr2%num) )
!
      if(array_cr2%num .eq. 0) return
      array_cr2%vec1 = 0.0d0
      array_cr2%vec2 = 0.0d0
!
      end subroutine alloc_control_array_c_r2
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c_r2(array_cr2)
!
      type(ctl_array_cr2), intent(inout) :: array_cr2
!
!
      if(allocated(array_cr2%c_tbl) .eqv. .FALSE.) return
      deallocate(array_cr2%c_tbl, array_cr2%vec1, array_cr2%vec2)
      array_cr2%num = 0
!
      end subroutine dealloc_control_array_c_r2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c_r2                                &
     &         (id_control, label, array_cr2, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_cr2), intent(inout) :: array_cr2
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_chara_real2_item) :: read_cr2
!
!
      if(array_cr2%icou .gt. 0) return
      array_cr2%array_name = trim(label)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!
      read_cr2%iflag = 0
      array_cr2%num =  0
      call alloc_control_array_c_r2(array_cr2)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_charreal2_ctl_type(c_buf, label, read_cr2)
          call append_control_array_c_r2(read_cr2, array_cr2)
        end if
      end do
!
      end subroutine read_control_array_c_r2
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c_r2                              &
     &         (id_control, level, array_cr2)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_cr2), intent(in) :: array_cr2
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(array_cr2%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_cr2%array_name)
      do i = 1, array_cr2%num
        call write_chara_real2_ctl_item(id_control, level,              &
     &     len_trim(array_cr2%array_name), array_cr2%array_name,        &
     &     array_cr2%c_tbl(i), array_cr2%vec1(i), array_cr2%vec2(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_cr2%array_name)
!
      end subroutine write_control_array_c_r2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c_r2(read_cr2, array_cr2)
!
      type(read_chara_real2_item), intent(inout) ::    read_cr2
      type(ctl_array_cr2), intent(inout) :: array_cr2
!
      type(ctl_array_cr2) ::    org_cr2
!
!
      org_cr2%num = array_cr2%num
      call alloc_control_array_c_r2(org_cr2)
      call copy_control_array_c_r2(org_cr2%num, array_cr2, org_cr2)
      call dealloc_control_array_c_r2(array_cr2)
!
      array_cr2%num = org_cr2%num + 1
      call alloc_control_array_c_r2(array_cr2)
      call copy_control_array_c_r2(org_cr2%num, org_cr2, array_cr2)
      call append_control_item_c_r2(read_cr2, array_cr2)
      read_cr2%iflag = 0
!
      call dealloc_control_array_c_r2(org_cr2)
!
      end subroutine append_control_array_c_r2
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c_r2(num_copy, org_cr2, tgt_cr2)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_cr2), intent(in) ::    org_cr2
      type(ctl_array_cr2), intent(inout) :: tgt_cr2
!
!
      tgt_cr2%array_name = org_cr2%array_name
      tgt_cr2%icou =       org_cr2%icou
!
      if(num_copy .le. 0) return
      tgt_cr2%c_tbl(1:num_copy) = org_cr2%c_tbl(1:num_copy)
      tgt_cr2%vec1(1:num_copy) =  org_cr2%vec1(1:num_copy)
      tgt_cr2%vec2(1:num_copy) =  org_cr2%vec2(1:num_copy)
!
      end subroutine copy_control_array_c_r2
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c_r2(read_cr2, array_cr2)
!
      type(read_chara_real2_item), intent(in) ::    read_cr2
      type(ctl_array_cr2), intent(inout) :: array_cr2
!
!
      array_cr2%icou = array_cr2%icou + read_cr2%iflag
      array_cr2%c_tbl(array_cr2%num) = read_cr2%charavalue
      array_cr2%vec1(array_cr2%num) =  read_cr2%realvalue(1)
      array_cr2%vec2(array_cr2%num) =  read_cr2%realvalue(2)
!
      end subroutine append_control_item_c_r2
!
! -----------------------------------------------------------------------
!
      end module t_control_array_charareal2
