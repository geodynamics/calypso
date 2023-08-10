!>@file   t_control_array_real.f90
!!        module t_control_array_real
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine init_real_ctl_item_label(label, real_item)
!!      subroutine read_real_ctl_type(c_buf, label, real_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_real_item), intent(inout) :: real_item
!!      subroutine write_real_ctl_type                                  &
!!     &         (id_file, level, maxlen, real_item)
!!        type(read_real_item), intent(in) :: real_item
!!      subroutine copy_real_ctl(org_r1, new_r1)
!!        type(read_real_item), intent(in) :: org_r1
!!        type(read_real_item), intent(inout) :: new_r1
!!      logical function cmp_read_real_item(r_item1, r_item2)
!!        type(read_real_item), intent(in) :: r_item1, r_item2
!!
!!      subroutine alloc_control_array_real(array_real)
!!      subroutine dealloc_control_array_real(array_real)
!!      subroutine init_real_ctl_array_label(label, array_real)
!!      subroutine read_control_array_r1                                &
!!     &         (id_control, label, array_real, c_buf)
!!        type(ctl_array_real), intent(inout) :: array_real
!!        type(buffer_for_control), intent(in)  :: c_buf
!!      subroutine write_control_array_r1                               &
!!     &         (id_control, level, array_real)
!!        type(ctl_array_real), intent(in) :: array_real
!!
!!      subroutine append_control_array_real(read_r1, array_r1)
!!        type(read_real_item), intent(inout) ::    read_r1
!!        type(ctl_array_real), intent(inout) :: array_r1
!!      subroutine copy_control_array_real(num_copy, org_r1, tgt_r1)
!!        type(ctl_array_real), intent(in) ::    org_r1
!!        type(ctl_array_real), intent(inout) :: tgt_r1
!!      subroutine append_control_item_real(read_r1, array_r1)
!!        type(read_real_item), intent(in) ::    read_r1
!!        type(ctl_array_real), intent(inout) :: array_r1
!!@endverbatim
!!
!!
      module t_control_array_real
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!>        structure of control real item
      type read_real_item
!>        Item name
        character(len=kchara) :: item_name = 'Real_item'
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_real_item
!
!>  Structure for real control array 
      type ctl_array_real
!>        Item name
        character(len=kchara) :: array_name = 'Real_array'
!>     number of array items
        integer(kind=kint) :: num = 0
!>     array counter
        integer(kind=kint) :: icou = 0
!>     array for 1st real
        real(kind = kreal), allocatable :: vect(:)
      end type ctl_array_real
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine init_real_ctl_item_label(label, real_item)
      character(len=kchara), intent(in) :: label
      type(read_real_item), intent(inout) :: real_item
!
      real_item%item_name = trim(label)
      end subroutine init_real_ctl_item_label
!
! ----------------------------------------------------------------------
!
      subroutine read_real_ctl_type(c_buf, label, real_item)
!
      use t_read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_real_item), intent(inout) :: real_item
!
      character(len=kchara) :: tmpchara
!
!
      if(real_item%iflag .gt. 0) return
      real_item%item_name = trim(label)
      if(c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, real_item%realvalue
      if (iflag_debug .gt. 0)  write(*,*) trim(c_buf%header_chara),     &
     &                       real_item%realvalue
      real_item%iflag = 1
!
      end subroutine read_real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_real_ctl_type                                    &
     &         (id_file, level, maxlen, real_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      type(read_real_item), intent(in) :: real_item
!
!
      if(real_item%iflag .eq. 0) return
      call write_real_ctl_item(id_file, level, maxlen,                  &
     &                        real_item%item_name, real_item%realvalue)
!
      end subroutine write_real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine copy_real_ctl(org_r1, new_r1)
!
      type(read_real_item), intent(in) :: org_r1
      type(read_real_item), intent(inout) :: new_r1
!
!
      new_r1%item_name = org_r1%item_name
      new_r1%iflag =     org_r1%iflag
      new_r1%realvalue = org_r1%realvalue
!
      end subroutine copy_real_ctl
!
!   --------------------------------------------------------------------
!
      logical function cmp_read_real_item(r_item1, r_item2)
!
      use skip_comment_f
!
      type(read_real_item), intent(in) :: r_item1, r_item2
!
      cmp_read_real_item = .FALSE.
      if(cmp_no_case(trim(r_item1%item_name),                           &
     &               trim(r_item2%item_name)) .eqv. .FALSE.) return
      if(r_item1%iflag .ne. r_item2%iflag) return
!
      if(r_item1%iflag .gt. 0) then
        if(r_item1%realvalue .ne. r_item2%realvalue) return
      end if
!
      cmp_read_real_item = .TRUE.
!
      end function cmp_read_real_item
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_control_array_real(array_real)
!
      type(ctl_array_real), intent(inout) :: array_real
!
!
      allocate( array_real%vect(array_real%num) )
!
      if(array_real%num .eq. 0) return
      array_real%vect = 0.0d0
!
      end subroutine alloc_control_array_real
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_array_real(array_real)
!
      type(ctl_array_real), intent(inout) :: array_real
!
!
      if(allocated(array_real%vect) .eqv. .FALSE.) return
      deallocate(array_real%vect)
      array_real%num = 0
!
      end subroutine dealloc_control_array_real
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine init_real_ctl_array_label(label, array_real)
      character(len=kchara), intent(in) :: label
      type(ctl_array_real), intent(inout) :: array_real
!
      array_real%array_name = trim(label)
      end subroutine init_real_ctl_array_label
!
!   --------------------------------------------------------------------
!
      subroutine read_control_array_r1                                  &
     &         (id_control, label, array_real, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: label
      type(ctl_array_real), intent(inout) :: array_real
      type(buffer_for_control), intent(inout)  :: c_buf
!
      type(read_real_item) :: read_r1
!
!
      if(array_real%icou .gt. 0) return
      array_real%array_name = trim(label)
      if(check_array_flag(c_buf, label) .eqv. .FALSE.) return
!
      read_r1%iflag = 0
      array_real%num =  0
      call alloc_control_array_real(array_real)
!
      do
        call load_one_line_from_control(id_control, label, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, label)) exit
!
        if(c_buf%header_chara.eq.label) then
          call read_real_ctl_type(c_buf, label, read_r1)
          call append_control_array_real(read_r1, array_real)
        end if
      end do
!
      end subroutine read_control_array_r1
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_r1                                 &
     &         (id_control, level, array_real)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(ctl_array_real), intent(in) :: array_real
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i, length
!
!
      if(array_real%num .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 array_real%array_name)
      do i = 1, array_real%num
        length = len_trim(array_real%array_name)
        call write_real_ctl_item(id_control, level, length,             &
     &      array_real%array_name, array_real%vect(i))
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     array_real%array_name)
!
      end subroutine write_control_array_r1
!
!   --------------------------------------------------------------------
!
      logical function cmp_control_array_r1(r_array1, r_array2)
!
      use skip_comment_f
!
      type(ctl_array_real), intent(in) :: r_array1, r_array2
      integer(kind = kint) :: i
!
      cmp_control_array_r1 = .FALSE.
      if(cmp_no_case(trim(r_array1%array_name),                         &
     &               trim(r_array2%array_name)) .eqv. .FALSE.) return
      if(r_array1%num .ne.  r_array2%num) return
      if(r_array1%icou .ne. r_array2%icou) return
      do i = 1, r_array1%num
        if(r_array1%vect(i) .ne. r_array2%vect(i)) return
      end do
      cmp_control_array_r1 = .TRUE.
!
      end function cmp_control_array_r1
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_control_array_real(read_r1, array_r1)
!
      type(read_real_item), intent(inout) ::    read_r1
      type(ctl_array_real), intent(inout) :: array_r1
!
      type(ctl_array_real) ::    org_r1
!
!
      org_r1%num = array_r1%num
      call alloc_control_array_real(org_r1)
      call copy_control_array_real(org_r1%num, array_r1, org_r1)
      call dealloc_control_array_real(array_r1)
!
      array_r1%num = org_r1%num + 1
      call alloc_control_array_real(array_r1)
      call copy_control_array_real(org_r1%num, org_r1, array_r1)
      call append_control_item_real(read_r1, array_r1)
      read_r1%iflag = 0
!
      call dealloc_control_array_real(org_r1)
!
      end subroutine append_control_array_real
!
! -----------------------------------------------------------------------
!
      subroutine copy_control_array_real(num_copy, org_r1, tgt_r1)
!
      integer(kind = kint), intent(in) ::  num_copy
      type(ctl_array_real), intent(in) ::    org_r1
      type(ctl_array_real), intent(inout) :: tgt_r1
!
      tgt_r1%array_name = org_r1%array_name
      tgt_r1%icou =       org_r1%icou
!
      if(num_copy .le. 0) return
      tgt_r1%vect(1:num_copy) = org_r1%vect(1:num_copy)
!
      end subroutine copy_control_array_real
!
! -----------------------------------------------------------------------
!
      subroutine append_control_item_real(read_r1, array_r1)
!
      type(read_real_item), intent(in) ::    read_r1
      type(ctl_array_real), intent(inout) :: array_r1
!
!
      array_r1%icou = array_r1%icou + read_r1%iflag
      array_r1%vect(array_r1%num) = read_r1%realvalue
!
      end subroutine append_control_item_real
!
! -----------------------------------------------------------------------
!
      end module t_control_array_real
