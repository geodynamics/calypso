!>@file   copy_control_elements.f90
!!@brief  module copy_control_elements
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief  Structure for reading control items
!!
!!@verbatim
!!      subroutine copy_real_ctl(org_r1, new_r1)
!!        type(read_real_item), intent(in) :: org_r1
!!        type(read_real_item), intent(inout) :: new_r1
!!      subroutine copy_integer_ctl(org_i1, new_i1)
!!        type(read_integer_item), intent(in) :: org_i1
!!        type(read_integer_item), intent(inout) :: new_i1
!!      subroutine copy_chara_ctl(org_c1, new_c1)
!!        type(read_character_item), intent(in) :: org_c1
!!        type(read_character_item), intent(inout) :: new_c1
!!      subroutine copy_real2_ctl(org_r2, new_r2)
!!        type(read_real2_item), intent(inout) :: org_r2
!!        type(read_real2_item), intent(inout) :: new_r2
!!      subroutine copy_real3_ctl(org_r3, new_r3)
!!        type(read_real3_item), intent(in) :: org_r3
!!        type(read_real3_item), intent(inout) :: new_r3
!!      subroutine copy_integer2_ctl(org_i2, new_i2)
!!        type(read_int2_item), intent(in) :: org_i2
!!        type(read_int2_item), intent(inout) :: new_i2
!!      subroutine copy_integer3_ctl(org_i3, new_i3)
!!        type(read_int3_item), intent(in) :: org_i3
!!        type(read_int3_item), intent(inout) :: new_i3
!!      subroutine copy_character2_ctl(org_c2, new_c2)
!!        type(read_chara2_item), intent(in) :: org_c2
!!        type(read_chara2_item), intent(inout) :: new_c2
!!      subroutine copy_character3_ctl(org_c3, new_c3)
!!        type(read_chara3_item), intent(in) :: org_c3
!!        type(read_chara3_item), intent(inout) :: new_c3
!!      subroutine copy_charreal2_ctl(org_cr2, new_cr2)
!!        type(read_chara_real2_item), intent(in) :: org_cr2
!!        type(read_chara_real2_item), intent(inout) :: new_cr2
!!      subroutine copy_char2real_ctl(org_c2r, new_c2r)
!!        type(read_chara2_real_item), intent(in) :: org_c2r
!!        type(read_chara2_real_item), intent(inout) :: new_c2r
!!      subroutine copy_charareal_ctl(org_cr, new_cr)
!!        type(read_chara_real_item), intent(in) :: org_cr
!!        type(read_chara_real_item), intent(inout) :: new_cr
!!      subroutine copy_charaint_ctl(org_ci, new_ci)
!!        type(read_chara_int_item), intent(in) :: org_ci
!!        type(read_chara_int_item), intent(inout) :: new_ci
!!      subroutine copy_intchrreal_ctl(org_icr, new_icr)
!!        type(read_int_chara_real_item), intent(in) :: org_icr
!!        type(read_int_chara_real_item), intent(inout) :: new_icr
!!      subroutine copy_intreal_ctl(org_ir, new_ir)
!!        type(read_int_real_item), intent(in) :: org_ir
!!        type(read_int_real_item), intent(inout) :: new_ir
!!      subroutine copy_int2real_ctl(org_i2r, new_i2r)
!!        type(read_int2_real_item), intent(in) :: org_i2r
!!        type(read_int2_real_item), intent(inout) :: new_i2r
!!      subroutine copy_int2real2_ctl(org_i2r2, new_i2r2)
!!        type(read_int2_real2_item), intent(in) :: org_i2r2
!!        type(read_int2_real2_item), intent(inout) :: new_i2r2
!!@endverbatim
!
      module copy_control_elements
!
      use m_precision
      use m_machine_parameter
      use t_control_elements
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine copy_real_ctl(org_r1, new_r1)
!
      type(read_real_item), intent(in) :: org_r1
      type(read_real_item), intent(inout) :: new_r1
!
!
      new_r1%iflag =     org_r1%iflag
      new_r1%realvalue = org_r1%realvalue
!
      end subroutine copy_real_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_integer_ctl(org_i1, new_i1)
!
      type(read_integer_item), intent(in) :: org_i1
      type(read_integer_item), intent(inout) :: new_i1
!
!
      new_i1%iflag =    org_i1%iflag
      new_i1%intvalue = org_i1%intvalue
!
       end subroutine copy_integer_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_chara_ctl(org_c1, new_c1)
!
      type(read_character_item), intent(in) :: org_c1
      type(read_character_item), intent(inout) :: new_c1
!
!
      new_c1%iflag =      org_c1%iflag
      new_c1%charavalue = org_c1%charavalue
!
       end subroutine copy_chara_ctl
!
!   --------------------------------------------------------------------
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
!
      subroutine copy_integer2_ctl(org_i2, new_i2)
!
      type(read_int2_item), intent(in) :: org_i2
      type(read_int2_item), intent(inout) :: new_i2
!
      new_i2%iflag =         org_i2%iflag
      new_i2%intvalue(1:2) = org_i2%intvalue(1:2)
!
      end subroutine copy_integer2_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_integer3_ctl(org_i3, new_i3)
!
      type(read_int3_item), intent(in) :: org_i3
      type(read_int3_item), intent(inout) :: new_i3
!
      new_i3%iflag =         org_i3%iflag
      new_i3%intvalue(1:3) = org_i3%intvalue(1:3)
!
      end subroutine copy_integer3_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_character2_ctl(org_c2, new_c2)
!
      type(read_chara2_item), intent(in) :: org_c2
      type(read_chara2_item), intent(inout) :: new_c2
!
!
      new_c2%iflag =           org_c2%iflag
      new_c2%charavalue(1:2) = org_c2%charavalue(1:2)
!
       end subroutine copy_character2_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_character3_ctl(org_c3, new_c3)
!
      type(read_chara3_item), intent(in) :: org_c3
      type(read_chara3_item), intent(inout) :: new_c3
!
!
      new_c3%iflag =           org_c3%iflag
      new_c3%charavalue(1:3) = org_c3%charavalue(1:3)
!
       end subroutine copy_character3_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_charreal2_ctl(org_cr2, new_cr2)
!
      type(read_chara_real2_item), intent(in) :: org_cr2
      type(read_chara_real2_item), intent(inout) :: new_cr2
!
!
      new_cr2%iflag =          org_cr2%iflag
      new_cr2%charavalue =     org_cr2%charavalue
      new_cr2%realvalue(1:2) = org_cr2%realvalue(1:2)
!
      end subroutine copy_charreal2_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_char2real_ctl(org_c2r, new_c2r)
!
      type(read_chara2_real_item), intent(in) :: org_c2r
      type(read_chara2_real_item), intent(inout) :: new_c2r
!
!
      new_c2r%iflag =            org_c2r%iflag
      new_c2r%charavalue(1:2) =  org_c2r%charavalue(1:2)
      new_c2r%realvalue =        org_c2r%realvalue
!
      end subroutine copy_char2real_ctl
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
!
      subroutine copy_intchrreal_ctl(org_icr, new_icr)
!
      type(read_int_chara_real_item), intent(in) :: org_icr
      type(read_int_chara_real_item), intent(inout) :: new_icr
!
!
      new_icr%iflag =       org_icr%iflag
      new_icr%intvalue =    org_icr%intvalue
      new_icr%charavalue =  org_icr%charavalue
      new_icr%realvalue =   org_icr%realvalue
!
       end subroutine copy_intchrreal_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_intreal_ctl(org_ir, new_ir)
!
      type(read_int_real_item), intent(in) :: org_ir
      type(read_int_real_item), intent(inout) :: new_ir
!
!
      new_ir%iflag =     org_ir%iflag
      new_ir%intvalue =  org_ir%intvalue
      new_ir%realvalue = org_ir%realvalue
!
       end subroutine copy_intreal_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_int2real_ctl(org_i2r, new_i2r)
!
      type(read_int2_real_item), intent(in) :: org_i2r
      type(read_int2_real_item), intent(inout) :: new_i2r
!
!
      new_i2r%iflag =         org_i2r%iflag
      new_i2r%intvalue(1:2) = org_i2r%intvalue(1:2)
      new_i2r%realvalue =     org_i2r%realvalue
!
       end subroutine copy_int2real_ctl
!
!   --------------------------------------------------------------------
!
      subroutine copy_int2real2_ctl(org_i2r2, new_i2r2)
!
      type(read_int2_real2_item), intent(in) :: org_i2r2
      type(read_int2_real2_item), intent(inout) :: new_i2r2
!
!
      new_i2r2%iflag =          org_i2r2%iflag
      new_i2r2%intvalue(1:2) =  org_i2r2%intvalue(1:2)
      new_i2r2%realvalue(1:2) = org_i2r2%realvalue(1:2)
!
       end subroutine copy_int2real2_ctl
!
!   --------------------------------------------------------------------
!
      end module copy_control_elements
