!>@file   bcast_control_arrays.f90
!!@brief  module bcast_control_arrays
!!
!!@author H. Matsui
!!@date Programmed in June, 2016
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine bcast_ctl_type_r1(real_item)
!!        type(read_real_item), intent(inout) :: real_item
!!      subroutine bcast_ctl_type_i1(int_item)
!!        type(read_integer_item), intent(inout) :: int_item
!!      subroutine bcast_ctl_type_c1(chara_item)
!!        type(read_character_item), intent(inout) :: chara_item
!!      subroutine bcast_ctl_type_r2(real2_item)
!!        type(read_real2_item), intent(inout) :: real2_item
!!      subroutine bcast_ctl_type_r3(real3_item)
!!        type(read_real3_item), intent(inout) :: real3_item
!!      subroutine bcast_ctl_type_i2(int2_item)
!!        type(read_int2_item), intent(inout) :: int2_item
!!      subroutine bcast_ctl_type_i3(int3_item)
!!        type(read_int3_item), intent(inout) :: int3_item
!!      subroutine bcast_ctl_type_c3(chara3_item)
!!        type(read_chara3_item), intent(inout) :: chara3_item
!!      subroutine bcast_ctl_type_c_i3(ci3_item)
!!        type(read_chara_int3_item), intent(inout) :: ci3_item
!!
!!      subroutine bcast_ctl_array_r1(array_real)
!!        type(ctl_array_real), intent(inout) :: array_real
!!      subroutine bcast_ctl_array_r2(array_r2)
!!        type(ctl_array_r2), intent(inout) :: array_r2
!!      subroutine bcast_ctl_array_r3(array_r3)
!!        type(ctl_array_r3), intent(inout) :: array_r3
!!      subroutine bcast_ctl_array_i1(array_int)
!!        type(ctl_array_int), intent(inout) :: array_int
!!      subroutine bcast_ctl_array_i2(array_i2)
!!        type(ctl_array_i2), intent(inout) :: array_i2
!!      subroutine bcast_ctl_array_i3(array_i3)
!!        type(ctl_array_i3), intent(inout) :: array_i3
!!      subroutine bcast_ctl_array_c1(array_chara)
!!        type(ctl_array_chara), intent(inout) :: array_chara
!!      subroutine bcast_ctl_array_c2(array_c2)
!!        type(ctl_array_c2), intent(inout) :: array_c2
!!      subroutine bcast_ctl_array_c3(array_c3)
!!        type(ctl_array_c3), intent(inout) :: array_c3
!!      subroutine bcast_ctl_array_cr(array_cr)
!!        type(ctl_array_cr), intent(inout) :: array_cr
!!      subroutine bcast_ctl_array_ci(array_ci)
!!        type(ctl_array_ci), intent(inout) :: array_ci
!!      subroutine bcast_ctl_array_cr2(array_cr2)
!!        type(ctl_array_cr2), intent(inout) :: array_cr2
!!      subroutine bcast_ctl_array_c2r(array_c2r)
!!        type(ctl_array_c2r), intent(inout) :: array_c2r
!!      subroutine bcast_ctl_array_icr(array_icr)
!!        type(ctl_array_icr), intent(inout) :: array_icr
!!      subroutine bcast_ctl_array_ir(array_ir)
!!        type(ctl_array_ir), intent(inout) :: array_ir
!!      subroutine bcast_ctl_array_i2r(array_i2r)
!!        type(ctl_array_i2r), intent(inout) :: array_i2r
!!      subroutine bcast_ctl_array_i2r2(array_i2r2)
!!        type(ctl_array_i2r2), intent(inout) :: array_i2r2
!!      subroutine bcast_ctl_array_ci3(array_ci3)
!!        type(ctl_array_ci3), intent(inout) :: array_ci3
!!@endverbatim
!!
!!@n @param  label           label for control items
!!@n @param  array_real      structures for array
!!@n @param  array_r2        structures for array
!!@n @param  array_r3        structures for array
!!@n @param  array_int       structures for array
!!@n @param  array_i2        structures for array
!!@n @param  array_chara     structures for array
!!@n @param  array_c2        structures for array
!!@n @param  array_c3        structures for array
!!@n @param  array_ci        structures for array
!!@n @param  array_cr        structures for array
!!@n @param  array_cr2       structures for array
!!@n @param  array_c2r       structures for array
!!@n @param  array_icr       structures for array
!!@n @param  array_ir        structures for array
!!@n @param  array_i2r       structures for array
!!@n @param  array_i2r2      structures for array
!!@n @param  array_ci3       structures for array
!!
      module bcast_control_arrays
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_r1(real_item)
!
      use t_control_array_real
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(read_real_item), intent(inout) :: real_item
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(real_item%iflag, 0)
      call calypso_mpi_bcast_character                                  &
      &   (real_item%item_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_real(real_item%realvalue, 0)
!
      end subroutine bcast_ctl_type_r1
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_i1(int_item)
!
      use t_control_array_integer
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(read_integer_item), intent(inout) :: int_item
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(int_item%iflag, 0)
      call calypso_mpi_bcast_character                                  &
     &   (int_item%item_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(int_item%intvalue, 0)
!
      end subroutine bcast_ctl_type_i1
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_c1(chara_item)
!
      use t_control_array_character
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(read_character_item), intent(inout) :: chara_item
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(chara_item%iflag, 0)
      call calypso_mpi_bcast_character                                  &
     &   (chara_item%item_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_character                                  &
     &   (chara_item%charavalue, cast_long(kchara), 0)
!
      end subroutine bcast_ctl_type_c1
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_r2(real2_item)
!
      use t_control_array_real2
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(read_real2_item), intent(inout) :: real2_item
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(real2_item%iflag, 0)
      call calypso_mpi_bcast_character                                  &
     &   (real2_item%item_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_real                                       &
     &   (real2_item%realvalue, cast_long(2), 0)
!
      end subroutine bcast_ctl_type_r2
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_r3(real3_item)
!
      use t_control_array_real3
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(read_real3_item), intent(inout) :: real3_item
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(real3_item%iflag, 0)
      call calypso_mpi_bcast_character                                  &
     &   (real3_item%item_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_real                                       &
     &   (real3_item%realvalue, cast_long(3), 0)
!
      end subroutine bcast_ctl_type_r3
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_i2(int2_item)
!
      use t_control_array_integer2
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(read_int2_item), intent(inout) :: int2_item
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(int2_item%iflag, 0)
      call calypso_mpi_bcast_character                                  &
     &   (int2_item%item_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_int                                        &
     &   (int2_item%intvalue, cast_long(2), 0)
!
      end subroutine bcast_ctl_type_i2
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_i3(int3_item)
!
      use t_control_array_integer3
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(read_int3_item), intent(inout) :: int3_item
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(int3_item%iflag, 0)
      call calypso_mpi_bcast_character                                  &
     &   (int3_item%item_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_int                                        &
     &   (int3_item%intvalue, cast_long(3), 0)
!
      end subroutine bcast_ctl_type_i3
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_c3(chara3_item)
!
      use t_control_array_character3
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(read_chara3_item), intent(inout) :: chara3_item
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(chara3_item%iflag, 0)
      call calypso_mpi_bcast_character                                  &
     &   (chara3_item%item_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_character                                  &
     &   (chara3_item%charavalue, cast_long(3*kchara), 0)
!
      end subroutine bcast_ctl_type_c3
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_c_i3(ci3_item)
!
      use t_control_array_charaint3
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(read_chara_int3_item), intent(inout) :: ci3_item
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(ci3_item%iflag, 0)
      call calypso_mpi_bcast_character                                  &
     &   (ci3_item%item_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_character                                  &
     &   (ci3_item%charavalue, cast_long(kchara), 0)
      call calypso_mpi_bcast_int                                        &
     &   (ci3_item%intvalue, cast_long(3), 0)
!
      end subroutine bcast_ctl_type_c_i3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_r1(array_real)
!
      use t_control_array_real
      use transfer_to_long_integers
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
!
      type(ctl_array_real), intent(inout) :: array_real
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_real%num,  0)
      call calypso_mpi_bcast_one_int(array_real%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_real%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_real(array_real)
!
      call calypso_mpi_bcast_real                                       &
     &   (array_real%vect, cast_long(array_real%num), 0)
!
      end subroutine bcast_ctl_array_r1
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_r2(array_r2)
!
      use t_control_array_real2
      use transfer_to_long_integers
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
!
      type(ctl_array_r2), intent(inout) :: array_r2
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_r2%num,  0)
      call calypso_mpi_bcast_one_int(array_r2%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_r2%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_r2(array_r2)
!
      call calypso_mpi_bcast_real                                       &
     &   (array_r2%vec1, cast_long(array_r2%num), 0)
      call calypso_mpi_bcast_real                                       &
     &   (array_r2%vec2, cast_long(array_r2%num), 0)
!
      end subroutine bcast_ctl_array_r2
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_r3(array_r3)
!
      use t_control_array_real3
      use transfer_to_long_integers
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
!
      type(ctl_array_r3), intent(inout) :: array_r3
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_r3%num,  0)
      call calypso_mpi_bcast_one_int(array_r3%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_r3%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_r3(array_r3)
!
      call calypso_mpi_bcast_real                                       &
     &   (array_r3%vec1, cast_long(array_r3%num), 0)
      call calypso_mpi_bcast_real                                       &
     &   (array_r3%vec2, cast_long(array_r3%num), 0)
      call calypso_mpi_bcast_real                                       &
     &   (array_r3%vec3, cast_long(array_r3%num), 0)
!
      end subroutine bcast_ctl_array_r3
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_i1(array_int)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_integer
      use transfer_to_long_integers
!
      type(ctl_array_int), intent(inout) :: array_int
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_int%num,  0)
      call calypso_mpi_bcast_one_int(array_int%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_int%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_int(array_int)
!
      call calypso_mpi_bcast_int                                        &
     &   (array_int%ivec, cast_long(array_int%num), 0)
!
      end subroutine bcast_ctl_array_i1
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_i2(array_i2)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_integer2
      use transfer_to_long_integers
!
      type(ctl_array_i2), intent(inout) :: array_i2
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_i2%num,  0)
      call calypso_mpi_bcast_one_int(array_i2%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_i2%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_i2(array_i2)
!
      call calypso_mpi_bcast_int                                        &
     &   (array_i2%int1, cast_long(array_i2%num), 0)
      call calypso_mpi_bcast_int                                        &
     &   (array_i2%int2, cast_long(array_i2%num), 0)
!
      end subroutine bcast_ctl_array_i2
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_i3(array_i3)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_integer3
      use transfer_to_long_integers
!
      type(ctl_array_i3), intent(inout) :: array_i3
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_i3%num,  0)
      call calypso_mpi_bcast_one_int(array_i3%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_i3%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_i3(array_i3)
!
      call calypso_mpi_bcast_int                                        &
     &   (array_i3%int1, cast_long(array_i3%num), 0)
      call calypso_mpi_bcast_int                                        &
     &   (array_i3%int2, cast_long(array_i3%num), 0)
      call calypso_mpi_bcast_int                                        &
     &   (array_i3%int3, cast_long(array_i3%num), 0)
!
      end subroutine bcast_ctl_array_i3
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_c1(array_chara)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_character
      use transfer_to_long_integers
!
      type(ctl_array_chara), intent(inout) :: array_chara
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_chara%num,  0)
      call calypso_mpi_bcast_one_int(array_chara%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_chara%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_chara(array_chara)
!
      call calypso_mpi_bcast_character                                  &
     &   (array_chara%c_tbl, cast_long(array_chara%num*kchara), 0)
!
      end subroutine bcast_ctl_array_c1
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_c2(array_c2)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_character2
      use transfer_to_long_integers
!
      type(ctl_array_c2), intent(inout) :: array_c2
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_c2%num,  0)
      call calypso_mpi_bcast_one_int(array_c2%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_c2%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_c2(array_c2)
!
      call calypso_mpi_bcast_character                                  &
     &   (array_c2%c1_tbl, cast_long(array_c2%num*kchara), 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_c2%c2_tbl, cast_long(array_c2%num*kchara), 0)
!
      end subroutine bcast_ctl_array_c2
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_c3(array_c3)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_character3
      use transfer_to_long_integers
!
      type(ctl_array_c3), intent(inout) :: array_c3
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_c3%num,  0)
      call calypso_mpi_bcast_one_int(array_c3%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_c3%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_c3(array_c3)
!
      call calypso_mpi_bcast_character                                  &
     &   (array_c3%c1_tbl, cast_long(array_c3%num*kchara), 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_c3%c2_tbl, cast_long(array_c3%num*kchara), 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_c3%c3_tbl, cast_long(array_c3%num*kchara), 0)
!
      end subroutine bcast_ctl_array_c3
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_cr(array_cr)
!
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_charareal
      use transfer_to_long_integers
!
      type(ctl_array_cr), intent(inout) :: array_cr
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_cr%num,  0)
      call calypso_mpi_bcast_one_int(array_cr%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_cr%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_c_r(array_cr)
!
      call calypso_mpi_bcast_character                                  &
     &   (array_cr%c_tbl, cast_long(array_cr%num*kchara), 0)
      call calypso_mpi_bcast_real                                       &
     &   (array_cr%vect, cast_long(array_cr%num), 0)
!
      end subroutine bcast_ctl_array_cr
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_ci(array_ci)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_charaint
      use transfer_to_long_integers
!
      type(ctl_array_ci), intent(inout) :: array_ci
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_ci%num,  0)
      call calypso_mpi_bcast_one_int(array_ci%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_ci%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_c_i(array_ci)
!
      call calypso_mpi_bcast_character                                  &
     &   (array_ci%c_tbl, cast_long(array_ci%num*kchara), 0)
      call calypso_mpi_bcast_int                                        &
     &   (array_ci%ivec, cast_long(array_ci%num), 0)
!
      end subroutine bcast_ctl_array_ci
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_cr2(array_cr2)
!
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_charareal2
      use transfer_to_long_integers
!
      type(ctl_array_cr2), intent(inout) :: array_cr2
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_cr2%num,  0)
      call calypso_mpi_bcast_one_int(array_cr2%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_cr2%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_c_r2(array_cr2)
!
      call calypso_mpi_bcast_character                                  &
     &   (array_cr2%c_tbl, cast_long(array_cr2%num*kchara), 0)
      call calypso_mpi_bcast_real                                       &
     &   (array_cr2%vec1, cast_long(array_cr2%num), 0)
      call calypso_mpi_bcast_real                                       &
     &   (array_cr2%vec2, cast_long(array_cr2%num), 0)
!
      end subroutine bcast_ctl_array_cr2
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_c2r(array_c2r)
!
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_chara2real
      use transfer_to_long_integers
!
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_c2r%num,  0)
      call calypso_mpi_bcast_one_int(array_c2r%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_c2r%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_c2_r(array_c2r)
!
      call calypso_mpi_bcast_character                                  &
     &   (array_c2r%c1_tbl, cast_long(array_c2r%num*kchara), 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_c2r%c2_tbl, cast_long(array_c2r%num*kchara), 0)
      call calypso_mpi_bcast_real                                       &
     &   (array_c2r%vect, cast_long(array_c2r%num), 0)
!
      end subroutine bcast_ctl_array_c2r
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_icr(array_icr)
!
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_intcharreal
      use transfer_to_long_integers
!
      type(ctl_array_icr), intent(inout) :: array_icr
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_icr%num,  0)
      call calypso_mpi_bcast_one_int(array_icr%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_icr%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_i_c_r(array_icr)
!
      call calypso_mpi_bcast_int                                        &
     &   (array_icr%ivec, cast_long(array_icr%num), 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_icr%c_tbl, cast_long(array_icr%num*kchara), 0)
      call calypso_mpi_bcast_real                                       &
     &   (array_icr%vect, cast_long(array_icr%num), 0)
!
      end subroutine bcast_ctl_array_icr
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_ir(array_ir)
!
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_intreal
      use transfer_to_long_integers
!
      type(ctl_array_ir), intent(inout) :: array_ir
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_ir%num,  0)
      call calypso_mpi_bcast_one_int(array_ir%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_ir%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_i_r(array_ir)
!
      call calypso_mpi_bcast_int                                        &
     &   (array_ir%ivec, cast_long(array_ir%num), 0)
      call calypso_mpi_bcast_real                                       &
     &   (array_ir%vect, cast_long(array_ir%num), 0)
!
      end subroutine bcast_ctl_array_ir
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_i2r(array_i2r)
!
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_int2real
      use transfer_to_long_integers
!
      type(ctl_array_i2r), intent(inout) :: array_i2r
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_i2r%num,  0)
      call calypso_mpi_bcast_one_int(array_i2r%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_i2r%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_i2_r(array_i2r)
!
      call calypso_mpi_bcast_int                                        &
     &   (array_i2r%int1, cast_long(array_i2r%num), 0)
      call calypso_mpi_bcast_int                                        &
     &   (array_i2r%int2, cast_long(array_i2r%num), 0)
      call calypso_mpi_bcast_real                                       &
     &   (array_i2r%vect, cast_long(array_i2r%num), 0)
!
      end subroutine bcast_ctl_array_i2r
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_i2r2(array_i2r2)
!
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_int2real2
      use transfer_to_long_integers
!
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_i2r2%num,  0)
      call calypso_mpi_bcast_one_int(array_i2r2%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_i2r2%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_i2_r2(array_i2r2)
!
      call calypso_mpi_bcast_int                                        &
     &   (array_i2r2%int1, cast_long(array_i2r2%num), 0)
      call calypso_mpi_bcast_int                                        &
     &   (array_i2r2%int2, cast_long(array_i2r2%num), 0)
      call calypso_mpi_bcast_real                                       &
     &   (array_i2r2%vec1, cast_long(array_i2r2%num), 0)
      call calypso_mpi_bcast_real                                       &
     &   (array_i2r2%vec2, cast_long(array_i2r2%num), 0)
!
      end subroutine bcast_ctl_array_i2r2
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_ci3(array_ci3)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use t_control_array_charaint3
      use transfer_to_long_integers
!
      type(ctl_array_ci3), intent(inout) :: array_ci3
!
!
      if(nprocs .eq. 1) return
!
      call calypso_mpi_bcast_one_int(array_ci3%num,  0)
      call calypso_mpi_bcast_one_int(array_ci3%icou, 0)
      call calypso_mpi_bcast_character                                  &
     &   (array_ci3%array_name, cast_long(kchara), 0)
!
      if(my_rank .ne. 0) call alloc_control_array_c_i3(array_ci3)
!
      call calypso_mpi_bcast_character                                  &
     &   (array_ci3%c_tbl, cast_long(array_ci3%num*kchara), 0)
      call calypso_mpi_bcast_int                                        &
     &   (array_ci3%ivec1, cast_long(array_ci3%num), 0)
      call calypso_mpi_bcast_int                                        &
     &   (array_ci3%ivec2, cast_long(array_ci3%num), 0)
      call calypso_mpi_bcast_int                                        &
     &   (array_ci3%ivec3, cast_long(array_ci3%num), 0)
!
      end subroutine bcast_ctl_array_ci3
!
!   --------------------------------------------------------------------
!
      end module bcast_control_arrays
