!>@file   t_control_elements.f90
!!@brief  module t_control_elements
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief  Structure for reading control items
!!
!!@verbatim
!!      subroutine read_real_ctl_type(label, real_item)
!!        character(len=kchara), intent(in) :: label
!!        type(read_real_item), intent(inout) :: real_item
!!      subroutine read_integer_ctl_type(label, int_item)
!!        character(len=kchara), intent(in) :: label
!!        type(read_integer_item), intent(inout) :: int_item
!!      subroutine read_chara_ctl_type(label, chara_item)
!!        character(len=kchara), intent(in) :: label
!!        type(read_character_item), intent(inout) :: chara_item
!!      subroutine read_real2_ctl_type(label, real2_item)
!!        character(len=kchara), intent(in) :: label
!!        type(read_real2_item), intent(inout) :: real2_item
!!      subroutine read_real3_ctl_type(label, real3_item)
!!        character(len=kchara), intent(in) :: label
!!        type(read_real3_item), intent(inout) :: real3_item
!!      subroutine read_integer3_ctl_type(label, int3_item)
!!        character(len=kchara), intent(in) :: label
!!        type(read_int3_item), intent(inout) :: int3_item
!!      subroutine read_character3_ctl_type(label, chara3_item)
!!        character(len=kchara), intent(in) :: label
!!        type(read_chara3_item), intent(inout) :: chara3_item
!!@endverbatim
!!
!!@n @param  label   Read label for control items
!
      module t_control_elements
!
      use m_precision
      use m_read_control_elements
!
      implicit none
!
!
!>        structure of control real item
      type read_real_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_real_item
!
!>        structure of control integer item
      type read_integer_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer item
        integer(kind = kint) ::  intvalue
      end type read_integer_item
!
!>        structure of control character item
      type read_character_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character item
        character(len=kchara) :: charavalue
      end type read_character_item
!
!>        structure of control item with two reals
      type read_real2_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read real items
        real(kind = kreal) ::    realvalue(2)
      end type read_real2_item
!
!
!>        structure of control item with three reals
      type read_real3_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read real items
        real(kind = kreal) ::    realvalue(3)
      end type read_real3_item
!
!>        structure of control item with three integers
      type read_int3_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue(3)
      end type read_int3_item
!
!>        structure of control item with three characters
      type read_chara3_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue(3)
      end type read_chara3_item
!
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_real_ctl_type(label, real_item)
!
      character(len=kchara), intent(in) :: label
      type(read_real_item), intent(inout) :: real_item
!
!
      call read_real_ctl_item(label, real_item%iflag,                   &
     &    real_item%realvalue)
!
      end subroutine read_real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_integer_ctl_type(label, int_item)
!
      character(len=kchara), intent(in) :: label
      type(read_integer_item), intent(inout) :: int_item
!
!
      call read_integer_ctl_item(label, int_item%iflag,                 &
     &    int_item%intvalue)
!
       end subroutine read_integer_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_chara_ctl_type(label, chara_item)
!
      character(len=kchara), intent(in) :: label
      type(read_character_item), intent(inout) :: chara_item
!
!
      call read_character_ctl_item(label, chara_item%iflag,             &
     &    chara_item%charavalue)
!
       end subroutine read_chara_ctl_type
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_real2_ctl_type(label, real2_item)
!
      character(len=kchara), intent(in) :: label
      type(read_real2_item), intent(inout) :: real2_item
!
!
      call read_real2_ctl_item(label, real2_item%iflag,                 &
     &    real2_item%realvalue)
!
       end subroutine read_real2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_real3_ctl_type(label, real3_item)
!
      character(len=kchara), intent(in) :: label
      type(read_real3_item), intent(inout) :: real3_item
!
!
      call read_real3_ctl_item(label, real3_item%iflag,                 &
     &    real3_item%realvalue)
!
       end subroutine read_real3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_integer3_ctl_type(label, int3_item)
!
      character(len=kchara), intent(in) :: label
      type(read_int3_item), intent(inout) :: int3_item
!
      call read_integer3_ctl_item(label, int3_item%iflag,               &
     &    int3_item%intvalue)
!
      end subroutine read_integer3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_character3_ctl_type(label, chara3_item)
!
      character(len=kchara), intent(in) :: label
      type(read_chara3_item), intent(inout) :: chara3_item
!
!
      call read_character3_ctl_item(label, chara3_item%iflag,          &
     &    chara3_item%charavalue)
!
       end subroutine read_character3_ctl_type
!
!   --------------------------------------------------------------------
!
      end module t_control_elements
