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
!!      subroutine bcast_ctl_type_i3(int3_item)
!!        type(read_int3_item), intent(inout) :: int3_item
!!      subroutine bcast_ctl_type_c3(chara3_item)
!!        type(read_chara3_item), intent(inout) :: chara3_item
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
!!
      module bcast_control_arrays
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_control_elements
      use t_read_control_arrays
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
      type(read_real_item), intent(inout) :: real_item
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(real_item%iflag, ione,                             &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(real_item%realvalue, ione,                         &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_type_r1
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_i1(int_item)
!
      type(read_integer_item), intent(inout) :: int_item
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(int_item%iflag, ione,                              &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(int_item%intvalue, ione,                           &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_type_i1
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_c1(chara_item)
!
      type(read_character_item), intent(inout) :: chara_item
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(chara_item%iflag, ione,                            &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(chara_item%charavalue, kchara,                     &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_type_c1
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_r2(real2_item)
!
      type(read_real2_item), intent(inout) :: real2_item
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(real2_item%iflag, ione,                            &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(real2_item%realvalue, itwo,                        &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_type_r2
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_r3(real3_item)
!
      type(read_real3_item), intent(inout) :: real3_item
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(real3_item%iflag, ione,                            &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(real3_item%realvalue, ithree,                      &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_type_r3
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_i3(int3_item)
!
      type(read_int3_item), intent(inout) :: int3_item
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(int3_item%iflag, ione,                             &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(int3_item%intvalue, ithree,                        &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_type_i3
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_type_c3(chara3_item)
!
      type(read_chara3_item), intent(inout) :: chara3_item
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(chara3_item%iflag, ione,                           &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(chara3_item%charavalue, ithree*kchara,             &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_type_c3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_r1(array_real)
!
      type(ctl_array_real), intent(inout) :: array_real
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_real%num,  ione,                             &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_real%icou, ione,                             &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_real(array_real)
!
      call MPI_BCAST(array_real%vect, array_real%num,                   &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_r1
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_r2(array_r2)
!
      type(ctl_array_r2), intent(inout) :: array_r2
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_r2%num,  ione,                               &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_r2%icou, ione,                               &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_r2(array_r2)
!
      call MPI_BCAST(array_r2%vec1, array_r2%num,                       &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_r2%vec2, array_r2%num,                       &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_r2
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_r3(array_r3)
!
      type(ctl_array_r3), intent(inout) :: array_r3
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_r3%num,  ione,                               &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_r3%icou, ione,                               &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_r3(array_r3)
!
      call MPI_BCAST(array_r3%vec1, array_r3%num,                       &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_r3%vec2, array_r3%num,                       &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_r3%vec3, array_r3%num,                       &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_r3
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_i1(array_int)
!
      type(ctl_array_int), intent(inout) :: array_int
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_int%num,  ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_int%icou, ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_int(array_int)
!
      call MPI_BCAST(array_int%ivec, array_int%num,                     &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_i1
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_i2(array_i2)
!
      type(ctl_array_i2), intent(inout) :: array_i2
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_i2%num,  ione,                               &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_i2%icou, ione,                               &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_i2(array_i2)
!
      call MPI_BCAST(array_i2%int1, array_i2%num,                       &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_i2%int2, array_i2%num,                       &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_i2
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_c1(array_chara)
!
      type(ctl_array_chara), intent(inout) :: array_chara
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_chara%num,  ione,                            &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_chara%icou, ione,                            &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_chara(array_chara)
!
      call MPI_BCAST(array_chara%c_tbl, array_chara%num*kchara,         &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_c1
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_c2(array_c2)
!
      type(ctl_array_c2), intent(inout) :: array_c2
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_c2%num,  ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_c2%icou, ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_c2(array_c2)
!
      call MPI_BCAST(array_c2%c1_tbl, array_c2%num*kchara,              &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_c2%c2_tbl, array_c2%num*kchara,              &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_c2
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_c3(array_c3)
!
      type(ctl_array_c3), intent(inout) :: array_c3
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_c3%num,  ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_c3%icou, ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_c3(array_c3)
!
      call MPI_BCAST(array_c3%c1_tbl, array_c3%num*kchara,              &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_c3%c2_tbl, array_c3%num*kchara,              &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_c3%c3_tbl, array_c3%num*kchara,              &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_c3
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_cr(array_cr)
!
      type(ctl_array_cr), intent(inout) :: array_cr
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_cr%num,  ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_cr%icou, ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_c_r(array_cr)
!
      call MPI_BCAST(array_cr%c_tbl, array_cr%num*kchara,               &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_cr%vect, array_cr%num,                       &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_cr
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_ci(array_ci)
!
      type(ctl_array_ci), intent(inout) :: array_ci
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_ci%num,  ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_ci%icou, ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_c_i(array_ci)
!
      call MPI_BCAST(array_ci%c_tbl, array_ci%num*kchara,               &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_ci%ivec, array_ci%num,                       &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_ci
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_cr2(array_cr2)
!
      type(ctl_array_cr2), intent(inout) :: array_cr2
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_cr2%num,  ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_cr2%icou, ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_c_r2(array_cr2)
!
      call MPI_BCAST(array_cr2%c_tbl, array_cr2%num*kchara,             &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_cr2%vec1, array_cr2%num,                     &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_cr2%vec2, array_cr2%num,                     &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_cr2
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_c2r(array_c2r)
!
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_c2r%num,  ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_c2r%icou, ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_c2_r(array_c2r)
!
      call MPI_BCAST(array_c2r%c1_tbl, array_c2r%num*kchara,            &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_c2r%c2_tbl, array_c2r%num*kchara,            &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_c2r%vect, array_c2r%num,                     &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_c2r
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_icr(array_icr)
!
      type(ctl_array_icr), intent(inout) :: array_icr
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_icr%num,  ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_icr%icou, ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_i_c_r(array_icr)
!
      call MPI_BCAST(array_icr%ivec, array_icr%num,                     &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_icr%c_tbl, array_icr%num*kchara,             &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_icr%vect, array_icr%num,                     &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_icr
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_ir(array_ir)
!
      type(ctl_array_ir), intent(inout) :: array_ir
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_ir%num,  ione,                               &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_ir%icou, ione,                               &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_i_r(array_ir)
!
      call MPI_BCAST(array_ir%ivec, array_ir%num,                       &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_ir%vect, array_ir%num,                       &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_ir
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_i2r(array_i2r)
!
      type(ctl_array_i2r), intent(inout) :: array_i2r
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_i2r%num,  ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_i2r%icou, ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_i2_r(array_i2r)
!
      call MPI_BCAST(array_i2r%int1, array_i2r%num,                     &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_i2r%int2, array_i2r%num,                     &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_i2r%vect, array_i2r%num,                     &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_i2r
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_i2r2(array_i2r2)
!
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_i2r2%num,  ione,                             &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_i2r2%icou, ione,                             &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_control_array_i2_r2(array_i2r2)
!
      call MPI_BCAST(array_i2r2%int1, array_i2r2%num,                   &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_i2r2%int2, array_i2r2%num,                   &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_i2r2%vec1, array_i2r2%num,                   &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_i2r2%vec2, array_i2r2%num,                   &
     &               CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_array_i2r2
!
!   --------------------------------------------------------------------
!
      end module bcast_control_arrays
