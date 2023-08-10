!>@file   t_control_array_charaint3.f90
!!        module t_control_array_charaint3
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read char-int-int-int control arrays
!!
!!@verbatim
!!      subroutine init_charaint3_ctl_item_label(label, ci3_item)
!!      subroutine read_charaint3_ctl_type(c_buf, label, ci3_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara_int3_item), intent(inout) :: ci3_item
!!      subroutine write_charaint3_ctl_type                             &
!!     &         (id_file, level, maxlen, ci3_item)
!!        type(read_chara_int3_item), intent(in) :: ci3_item
!!      subroutine copy_charaint3_ctl(org_ci3, new_ci3)
!!        type(read_chara_int3_item), intent(in) :: org_ci3
!!        type(read_chara_int3_item), intent(inout) :: new_ci3
!!
!!      subroutine alloc_control_array_c_i3(array_ci3)
!!      subroutine dealloc_control_array_c_i3(array_ci3)
!!      subroutine init_c_i3_ctl_array_label(label, array_ci3)
!!      subroutine read_control_array_c_i3                              &
!!     &         (id_control, label, array_ci3, c_buf)
!!        type(ctl_array_ci3), intent(inout) :: array_ci3
!!        type(buffer_for_control), intent(in)  :: c_buf
!!      subroutine write_control_array_c_i3                             &
!!     &         (id_control, level, array_ci3)
!!        type(ctl_array_ci3), intent(in) :: array_ci3
!!
!!      subroutine append_control_array_c_i3(read_ci3, array_ci3)
!!        type(read_chara_int3_item), intent(inout) ::    read_ci3
!!        type(ctl_array_ci3), intent(inout) :: array_ci3
!!      subroutine copy_control_array_c_i3(num_copy, org_ci3, tgt_ci3)
!!        type(ctl_array_ci3), intent(in) ::    org_ci3
!!        type(ctl_array_ci3), intent(inout) :: tgt_ci3
!!      subroutine append_control_item_c_i3(read_ci3, array_ci3)
!!        type(read_chara_int3_item), intent(in) ::    read_ci3
!!        type(ctl_array_ci3), intent(inout) :: array_ci3
!!@endverbatim
!!
!!
      module t_control_array_charaint3
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control item with character and three integers
      type read_chara_int3_item
!>        Item name
        character(len=kchara) :: item_name = 'chara_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue
!>        array for read integer items
        integer(kind = kint) ::  intvalue(3)
      end type read_chara_int3_item
!
!>  Structure for charactor and integer control array 
      type ctl_array_ci3
!>        Item name
        character(len=kchara) :: array_name = 'chara_array'
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st character
        character(len=kchara), allocatable :: c_tbl(:)
!>     array for 1st integer
        integer(kind = kint), allocatable :: ivec1(:)
!>     array for 2nd integer
        integer(kind = kint), allocatable :: ivec2(:)
!>     array for 3rd integer
        integer(kind = kint), allocatable :: ivec3(:)
      end type ctl_array_ci3
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine init_charaint3_ctl_item_label(label, ci3_item)
      character(len=kchara), intent(in) :: label
      type(read_chara_int3_item), intent(inout) :: ci3_item
!
      ci3_item%item_name = trim(label)
      end subroutine init_charaint3_ctl_item_label
!
! ----------------------------------------------------------------------
!
      subroutine read_charaint3_ctl_type(c_buf, label, ci3_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara_int3_item), intent(inout) :: ci3_item
!
       character(len=kchara) :: tmpchara
!
!
      if(ci3_item%iflag.gt.0) return
      ci3_item%item_name = trim(label)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, ci3_item%charavalue,           &
     &                         ci3_item%intvalue(1:3)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,2i16)')                   &
     &       trim(c_buf%header_chara), ' ci3_item%charavalue:  ',       &
     &       trim(ci3_item%charavalue)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1p2e23.15)')              &
     &       trim(c_buf%header_chara), ' int: ', ci3_item%intvalue(1:3)
      ci3_item%iflag = 1
!
      end subroutine read_charaint3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_charaint3_ctl_type                               &
     &         (id_file, level, maxlen, ci3_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level, maxlen
      type(read_chara_int3_item), intent(in) :: ci3_item
!
!
      if(ci3_item%iflag .eq. 0) return
!
      call write_chara_int3_ctl_item                                    &
     &   (id_file, level, maxlen, ci3_item%item_name,                   &
     &    ci3_item%charavalue, ci3_item%intvalue(1),                    &
     &    ci3_item%intvalue(2), ci3_item%intvalue(3))
!
       end subroutine write_charaint3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_charaint3_ctl(org_ci3, new_ci3)
!
      type(read_chara_int3_item), intent(in) :: org_ci3
      type(read_chara_int3_item), intent(inout) :: new_ci3
!
!
      new_ci3%item_name =      new_ci3%item_name
      new_ci3%iflag =          org_ci3%iflag
      new_ci3%charavalue =     org_ci3%charavalue
      new_ci3%intvalue(1:3) =  org_ci3%intvalue(1:3)
!
       end subroutine copy_charaint3_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_c_i3(array_ci3)
!
      type(ctl_array_ci3), intent(inout) :: array_ci3
!
!
      allocate( array_ci3%c_tbl(array_ci3%num) )
      allocate( array_ci3%ivec1(array_ci3%num) )
      allocate( array_ci3%ivec2(array_ci3%num) )
      allocate( array_ci3%ivec3(array_ci3%num) )
!
      if(array_ci3%num .eq. 0) return
      array_ci3%ivec1 = 0
      array_ci3%ivec2 = 0
      array_ci3%ivec3 = 0
!
      end subroutine alloc_control_array_c_i3
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_c_i3(array_ci3)
!
      type(ctl_array_ci3), intent(inout) :: array_ci3
!
!
      if(allocated(array_ci3%c_tbl) .eqv. .FALSE.) return
      deallocate( array_ci3%c_tbl, array_ci3%ivec1)
      deallocate( array_ci3%ivec2, array_ci3%ivec3)
      array_ci3%num = 0
!
      end subroutine dealloc_control_array_c_i3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine init_c_i3_ctl_array_label(label, array_ci3)
      character(len=kchara), intent(in) :: label
      type(ctl_array_ci3), intent(inout) :: array_ci3
!
      array_ci3%array_name = trim(label)
      end subroutine init_c_i3_ctl_array_label
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_c_i3                                &
     &         (id_control, label, array_ci3, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_ci3), intent(inout) :: array_ci3
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_chara_int3_item) :: read_ci3
!
!
      if(array_ci3%icou .gt. 0) return
      array_ci3%array_name = trim(label)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!
      read_ci3%iflag = 0
      array_ci3%num =  0
      call alloc_control_array_c_i3(array_ci3)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_charaint3_ctl_type(c_buf, label, read_ci3)
          call append_control_array_c_i3(read_ci3, array_ci3)
        end if
      end do
!
      end subroutine read_control_array_c_i3
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c_i3                               &
     &         (id_control, level, array_ci3)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_ci3), intent(in) :: array_ci3
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(array_ci3%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_ci3%array_name)
      do i = 1, array_ci3%num
        call write_chara_int3_ctl_item(id_control, level,               &
     &      len_trim(array_ci3%array_name), array_ci3%array_name,       &
     &      array_ci3%c_tbl(i), array_ci3%ivec1(i),                     &
     &      array_ci3%ivec2(i), array_ci3%ivec3(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_ci3%array_name)
!
      end subroutine write_control_array_c_i3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_c_i3(read_ci3, array_ci3)
!
      type(read_chara_int3_item), intent(inout) ::    read_ci3
      type(ctl_array_ci3), intent(inout) :: array_ci3
!
      type(ctl_array_ci3) ::    org_ci
!
!
      org_ci%num = array_ci3%num
      call alloc_control_array_c_i3(org_ci)
      call copy_control_array_c_i3(org_ci%num, array_ci3, org_ci)
      call dealloc_control_array_c_i3(array_ci3)
!
      array_ci3%num = org_ci%num + 1
      call alloc_control_array_c_i3(array_ci3)
      call copy_control_array_c_i3(org_ci%num, org_ci, array_ci3)
      call append_control_item_c_i3(read_ci3, array_ci3)
      read_ci3%iflag = 0
!
      call dealloc_control_array_c_i3(org_ci)
!
      end subroutine append_control_array_c_i3
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_c_i3(num_copy, org_ci3, tgt_ci3)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_ci3), intent(in) ::    org_ci3
      type(ctl_array_ci3), intent(inout) :: tgt_ci3
!
!
      tgt_ci3%array_name = org_ci3%array_name
      tgt_ci3%icou =       org_ci3%icou
!
      if(num_copy .le. 0) return
      tgt_ci3%c_tbl(1:num_copy) = org_ci3%c_tbl(1:num_copy)
      tgt_ci3%ivec1(1:num_copy) = org_ci3%ivec1(1:num_copy)
      tgt_ci3%ivec2(1:num_copy) = org_ci3%ivec2(1:num_copy)
      tgt_ci3%ivec3(1:num_copy) = org_ci3%ivec3(1:num_copy)
!
      end subroutine copy_control_array_c_i3
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_c_i3(read_ci3, array_ci3)
!
      type(read_chara_int3_item), intent(in) ::    read_ci3
      type(ctl_array_ci3), intent(inout) :: array_ci3
!
!
      array_ci3%icou = array_ci3%icou + read_ci3%iflag
      array_ci3%c_tbl(array_ci3%num) = read_ci3%charavalue
      array_ci3%ivec1(array_ci3%num) = read_ci3%intvalue(1)
      array_ci3%ivec2(array_ci3%num) = read_ci3%intvalue(2)
      array_ci3%ivec3(array_ci3%num) = read_ci3%intvalue(3)
!
      end subroutine append_control_item_c_i3
!
! -----------------------------------------------------------------------
!
      end module t_control_array_charaint3
