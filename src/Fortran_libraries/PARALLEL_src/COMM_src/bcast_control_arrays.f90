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
      call MPI_BCAST(real_item%iflag, 1,                                &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(real_item%realvalue, 1,                            &
     &               CALYPSO_REAL, 0, CALYPSO_COMM, ierr_MPI)
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
      call MPI_BCAST(int_item%iflag, 1,                                 &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(int_item%intvalue, 1,                              &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      call MPI_BCAST(chara_item%iflag, 1,                               &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(chara_item%charavalue, kchara,                     &
     &               CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
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
      call MPI_BCAST(real2_item%iflag, 1,                               &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(real2_item%realvalue, 2,                           &
     &               CALYPSO_REAL, 0, CALYPSO_COMM, ierr_MPI)
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
      call MPI_BCAST(real3_item%iflag, 1,                               &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(real3_item%realvalue, 3,                           &
     &               CALYPSO_REAL, 0, CALYPSO_COMM, ierr_MPI)
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
      call MPI_BCAST(int3_item%iflag, 1,                                &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(int3_item%intvalue, 3,                             &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      call MPI_BCAST(chara3_item%iflag, 1,                              &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(chara3_item%charavalue, (3*kchara),                &
     &               CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_type_c3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_array_r1(array_real)
!
      use transfer_to_long_integers
!
      type(ctl_array_real), intent(inout) :: array_real
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_real%num,  1,                                &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_real%icou, 1,                                &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_r2), intent(inout) :: array_r2
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_r2%num,  1,                                  &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_r2%icou, 1,                                  &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_r3), intent(inout) :: array_r3
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_r3%num,  1,                                  &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_r3%icou, 1,                                  &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_int), intent(inout) :: array_int
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_int%num,  1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_int%icou, 1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_i2), intent(inout) :: array_i2
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_i2%num,  1,                                  &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_i2%icou, 1,                                  &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      subroutine bcast_ctl_array_c1(array_chara)
!
      use transfer_to_long_integers
!
      type(ctl_array_chara), intent(inout) :: array_chara
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_chara%num,  1,                               &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_chara%icou, 1,                               &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_c2), intent(inout) :: array_c2
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_c2%num,  1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_c2%icou, 1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_c3), intent(inout) :: array_c3
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_c3%num,  1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_c3%icou, 1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_cr), intent(inout) :: array_cr
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_cr%num,  1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_cr%icou, 1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_ci), intent(inout) :: array_ci
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_ci%num,  1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_ci%icou, 1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_cr2), intent(inout) :: array_cr2
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_cr2%num,  1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_cr2%icou, 1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_c2r), intent(inout) :: array_c2r
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_c2r%num,  1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_c2r%icou, 1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_icr), intent(inout) :: array_icr
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_icr%num,  1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_icr%icou, 1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_ir), intent(inout) :: array_ir
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_ir%num,  1,                                  &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_ir%icou, 1,                                  &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_i2r), intent(inout) :: array_i2r
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_i2r%num,  1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_i2r%icou, 1,                                 &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      use transfer_to_long_integers
!
      type(ctl_array_i2r2), intent(inout) :: array_i2r2
!
!
      if(nprocs .eq. 1) return
!
      call MPI_BCAST(array_i2r2%num,  1,                                &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(array_i2r2%icou, 1,                                &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      end module bcast_control_arrays
