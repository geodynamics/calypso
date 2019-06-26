!>@file   t_control_elements.f90
!!@brief  module t_control_elements
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief  Structure for reading control items
!!
!!@verbatim
!!      subroutine read_real_ctl_type(c_buf, label, real_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_real_item), intent(inout) :: real_item
!!      subroutine read_integer_ctl_type(c_buf, label, int_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_integer_item), intent(inout) :: int_item
!!      subroutine read_chara_ctl_type(c_buf, label, chara_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_character_item), intent(inout) :: chara_item
!!      subroutine read_real2_ctl_type(c_buf, label, real2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_real2_item), intent(inout) :: real2_item
!!      subroutine read_real3_ctl_type(c_buf, label, real3_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_real3_item), intent(inout) :: real3_item
!!      subroutine read_integer2_ctl_type(c_buf, label, int2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int2_item), intent(inout) :: int2_item
!!      subroutine read_integer3_ctl_type(c_buf, label, int3_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int3_item), intent(inout) :: int3_item
!!      subroutine read_character2_ctl_type(c_buf, label, chara2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara2_item), intent(inout) :: chara2_item
!!      subroutine read_character3_ctl_type(c_buf, label, chara3_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara3_item), intent(inout) :: chara3_item
!!      subroutine read_charreal2_ctl_type(c_buf, label, cr2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara_real2_item), intent(inout) :: cr2_item
!!      subroutine read_char2real_ctl_type(c_buf, label, c2r_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara2_real_item), intent(inout) :: c2r_item
!!      subroutine read_charareal_ctl_type(c_buf, label, cr_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara_real_item), intent(inout) :: cr_item
!!      subroutine read_charaint_ctl_type(c_buf, label, ci_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara_int_item), intent(inout) :: ci_item
!!      subroutine read_intchrreal_ctl_type(c_buf, label, icr_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int_chara_real_item), intent(inout) :: icr_item
!!      subroutine read_intreal_ctl_type(c_buf, label, ir_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int_real_item), intent(inout) :: ir_item
!!      subroutine read_int2real_ctl_type(c_buf, label, i2r_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int2_real_item), intent(inout) :: i2r_item
!!      subroutine read_int2real2_ctl_type(c_buf, label, i2r2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int2_real2_item), intent(inout) :: i2r2_item
!!
!!      subroutine write_real_ctl_type                                  &
!!     &         (id_file, level, maxlen, label, real_item)
!!        type(read_real_item), intent(in) :: real_item
!!      subroutine write_integer_ctl_type                               &
!!     &         (id_file, level, maxlen, label, int_item)
!!        type(read_integer_item), intent(in) :: int_item
!!      subroutine write_chara_ctl_type                                 &
!!     &         (id_file, level, maxlen, label, chara_item)
!!        type(read_character_item), intent(in) :: chara_item
!!      subroutine write_real2_ctl_type                                 &
!!     &         (id_file, level, label, real2_item)
!!        type(read_real2_item), intent(in) :: real2_item
!!      subroutine write_real3_ctl_type                                 &
!!     &         (id_file, level, label, real3_item)
!!        type(read_real3_item), intent(in) :: real3_item
!!      subroutine write_integer3_ctl_type                              &
!!     &         (id_file, level, label, int3_item)
!!        type(read_int3_item), intent(in) :: int3_item
!!      subroutine write_character3_ctl_type                            &
!!     &         (id_file, level, label, chara3_item)
!!        type(read_chara3_item), intent(in) :: chara3_item
!!@endverbatim
!!
!!@n @param  label   Read label for control items
!
      module t_control_elements
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
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
!>        structure of control integer item
      type read_int2_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer item
        integer(kind = kint) ::  intvalue(2)
      end type read_int2_item
!
!>        structure of control item with three integers
      type read_int3_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue(3)
      end type read_int3_item
!
!>        structure of control character item
      type read_character_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character item
        character(len=kchara) :: charavalue
      end type read_character_item
!
!>        structure of control item with three characters
      type read_chara2_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue(2)
      end type read_chara2_item
!
!>        structure of control item with three characters
      type read_chara3_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue(3)
      end type read_chara3_item
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
!>        structure of control item with three characters
      type read_chara_real2_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue
!>        array for read real item
        real(kind = kreal) ::    realvalue(2)
      end type read_chara_real2_item
!
!>        structure of control item with three characters
      type read_chara2_real_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue(2)
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_chara2_real_item
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
!>        structure of control item with three characters
      type read_int_chara_real_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue
!>        array for read character items
        character(len=kchara) ::  charavalue
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_int_chara_real_item
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
!>        structure of control item with three characters
      type read_int2_real_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue(2)
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_int2_real_item
!
!>        structure of control item with three characters
      type read_int2_real2_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue(2)
!>        array for read real item
        real(kind = kreal) ::    realvalue(2)
      end type read_int2_real2_item
!
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_real_ctl_type(c_buf, label, real_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_real_item), intent(inout) :: real_item
!
!
      call read_real_ctl_item(c_buf, label, real_item%iflag,            &
     &    real_item%realvalue)
!
      end subroutine read_real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_integer_ctl_type(c_buf, label, int_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_integer_item), intent(inout) :: int_item
!
!
      call read_integer_ctl_item(c_buf, label, int_item%iflag,          &
     &    int_item%intvalue)
!
       end subroutine read_integer_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_chara_ctl_type(c_buf, label, chara_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_character_item), intent(inout) :: chara_item
!
!
      call read_character_ctl_item(c_buf, label, chara_item%iflag,      &
     &    chara_item%charavalue)
!
       end subroutine read_chara_ctl_type
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_real2_ctl_type(c_buf, label, real2_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_real2_item), intent(inout) :: real2_item
!
!
      call read_real2_ctl_item(c_buf, label, real2_item%iflag,          &
     &    real2_item%realvalue(1), real2_item%realvalue(2))
!
       end subroutine read_real2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_real3_ctl_type(c_buf, label, real3_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_real3_item), intent(inout) :: real3_item
!
!
      call read_real3_ctl_item(c_buf, label, real3_item%iflag,          &
     &    real3_item%realvalue(1), real3_item%realvalue(2),             &
     &    real3_item%realvalue(3))
!
       end subroutine read_real3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_integer2_ctl_type(c_buf, label, int2_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int2_item), intent(inout) :: int2_item
!
      call read_integer2_ctl_item(c_buf, label, int2_item%iflag,        &
     &    int2_item%intvalue(1), int2_item%intvalue(2))
!
      end subroutine read_integer2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_integer3_ctl_type(c_buf, label, int3_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int3_item), intent(inout) :: int3_item
!
      call read_integer3_ctl_item(c_buf, label, int3_item%iflag,        &
     &    int3_item%intvalue(1), int3_item%intvalue(2),                 &
     &    int3_item%intvalue(3))
!
      end subroutine read_integer3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_character2_ctl_type(c_buf, label, chara2_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara2_item), intent(inout) :: chara2_item
!
!
      call read_character2_ctl_item(c_buf, label, chara2_item%iflag,    &
     &    chara2_item%charavalue(1), chara2_item%charavalue(2))
!
       end subroutine read_character2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_character3_ctl_type(c_buf, label, chara3_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara3_item), intent(inout) :: chara3_item
!
!
      call read_character3_ctl_item(c_buf, label, chara3_item%iflag,    &
     &    chara3_item%charavalue(1), chara3_item%charavalue(2),         &
     &    chara3_item%charavalue(3))
!
       end subroutine read_character3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_charreal2_ctl_type(c_buf, label, cr2_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara_real2_item), intent(inout) :: cr2_item
!
!
      call read_charreal2_ctl_item                                      &
     &   (c_buf, label, cr2_item%iflag, cr2_item%charavalue,            &
     &    cr2_item%realvalue(1), cr2_item%realvalue(2))
!
       end subroutine read_charreal2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_char2real_ctl_type(c_buf, label, c2r_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara2_real_item), intent(inout) :: c2r_item
!
!
      call read_char2real_ctl_item(c_buf, label, c2r_item%iflag,        &
     &    c2r_item%charavalue(1), c2r_item%charavalue(2),               &
     &    c2r_item%realvalue)
!
       end subroutine read_char2real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_charareal_ctl_type(c_buf, label, cr_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara_real_item), intent(inout) :: cr_item
!
!
      call read_charareal_ctl_item(c_buf, label, cr_item%iflag,         &
     &    cr_item%charavalue, cr_item%realvalue)
!
       end subroutine read_charareal_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_charaint_ctl_type(c_buf, label, ci_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara_int_item), intent(inout) :: ci_item
!
!
      call read_charaint_ctl_item(c_buf, label, ci_item%iflag,          &
     &    ci_item%charavalue, ci_item%intvalue)
!
       end subroutine read_charaint_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_intchrreal_ctl_type(c_buf, label, icr_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int_chara_real_item), intent(inout) :: icr_item
!
!
      call read_intchrreal_ctl_item(c_buf, label, icr_item%iflag,       &
     &    icr_item%intvalue, icr_item%charavalue, icr_item%realvalue)
!
       end subroutine read_intchrreal_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_intreal_ctl_type(c_buf, label, ir_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int_real_item), intent(inout) :: ir_item
!
!
      call read_intreal_ctl_item(c_buf, label, ir_item%iflag,           &
     &    ir_item%intvalue, ir_item%realvalue)
!
       end subroutine read_intreal_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_int2real_ctl_type(c_buf, label, i2r_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int2_real_item), intent(inout) :: i2r_item
!
!
      call read_int2real_ctl_item(c_buf, label, i2r_item%iflag,         &
     &    i2r_item%intvalue(1), i2r_item%intvalue(2),                   &
     &    i2r_item%realvalue)
!
       end subroutine read_int2real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_int2real2_ctl_type(c_buf, label, i2r2_item)
!
      use read_control_elements
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int2_real2_item), intent(inout) :: i2r2_item
!
!
      call read_int2real2_ctl_item(c_buf, label, i2r2_item%iflag,       &
     &    i2r2_item%intvalue(1), i2r2_item%intvalue(2),                 &
     &    i2r2_item%realvalue(1), i2r2_item%realvalue(2))
!
       end subroutine read_int2real2_ctl_type
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_real_ctl_type                                    &
     &         (id_file, level, maxlen, label, real_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      type(read_real_item), intent(in) :: real_item
!
!
      if(real_item%iflag .eq. 0) return
      call write_real_ctl_item                                          &
     &   (id_file, level, maxlen, label, real_item%realvalue)
!
      end subroutine write_real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_integer_ctl_type                                 &
     &         (id_file, level, maxlen, label, int_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      type(read_integer_item), intent(in) :: int_item
!
!
      if(int_item%iflag .eq. 0) return
      call write_integer_ctl_item                                       &
     &   (id_file, level, maxlen, label, int_item%intvalue)
!
       end subroutine write_integer_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_chara_ctl_type                                   &
     &         (id_file, level, maxlen, label, chara_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      type(read_character_item), intent(in) :: chara_item
!
!
      if(chara_item%iflag .eq. 0) return
      call write_character_ctl_item                                     &
     &   (id_file, level, maxlen, label, chara_item%charavalue)
!
       end subroutine write_chara_ctl_type
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_real2_ctl_type                                   &
     &         (id_file, level, label, real2_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(read_real2_item), intent(in) :: real2_item
!
!
      if(real2_item%iflag .eq. 0) return
      call write_real2_ctl_item(id_file, level, label,                  &
     &    real2_item%realvalue(1), real2_item%realvalue(2))
!
       end subroutine write_real2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_real3_ctl_type                                   &
     &         (id_file, level, label, real3_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(read_real3_item), intent(in) :: real3_item
!
!
      if(real3_item%iflag .eq. 0) return
      call write_real3_ctl_item(id_file, level, label,                  &
     &    real3_item%realvalue(1), real3_item%realvalue(2),             &
     &    real3_item%realvalue(3))
!
       end subroutine write_real3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_integer3_ctl_type                                &
     &         (id_file, level, label, int3_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(read_int3_item), intent(in) :: int3_item
!
      if(int3_item%iflag .eq. 0) return
      call write_integer3_ctl_item(id_file, level, label,               &
     &    int3_item%intvalue(1), int3_item%intvalue(2),                 &
     &    int3_item%intvalue(3))
!
      end subroutine write_integer3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_character3_ctl_type                              &
     &         (id_file, level, label, chara3_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(read_chara3_item), intent(in) :: chara3_item
!
      integer(kind = kint) :: i
      integer(kind = kint) :: maxlen(0:2)
!
!
      if(chara3_item%iflag .eq. 0) return
!
      maxlen(0) = len_trim(label)
      do i = 1, 2
        maxlen(i) = len_trim(chara3_item%charavalue(i))                 &
     &           + iflag_divide(chara3_item%charavalue(i))
      end do
      call write_character3_ctl_item(id_file, level, label, maxlen,     &
     &    chara3_item%charavalue(1), chara3_item%charavalue(2),         &
     &    chara3_item%charavalue(3))
!
       end subroutine write_character3_ctl_type
!
!   --------------------------------------------------------------------
!
      end module t_control_elements
