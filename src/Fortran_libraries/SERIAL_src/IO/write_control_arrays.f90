!>@file   write_control_arrays.f90
!!@brief  module write_control_arrays
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      subroutine write_control_array_r1                               &
!!     &         (id_file, level, label, array_real)
!!        type(ctl_array_real), intent(in) :: array_real
!!      subroutine write_control_array_r2                               &
!!     &         (id_file, level, label, array_r2)
!!        type(ctl_array_r2), intent(in) :: array_r2
!!      subroutine write_control_array_r3                               &
!!     &         (id_file, level, label, array_r3)
!!        type(ctl_array_r3), intent(in) :: array_r3
!!      subroutine write_control_array_i1                               &
!!     &         (id_file, level, label, array_int)
!!        type(ctl_array_int), intent(in) :: array_int
!!      subroutine write_control_array_i2                               &
!!     &         (id_file, level, label, array_i2)
!!        type(ctl_array_i2), intent(in) :: array_i2
!!      subroutine write_control_array_c1                               &
!!     &          (id_file, level, label, array_chara)
!!        type(ctl_array_chara), intent(in) :: array_chara
!!      subroutine write_control_array_c2                               &
!!     &         (id_file, level, label, array_c2)
!!        type(ctl_array_c2), intent(in) :: array_c2
!!      subroutine write_control_array_c3                               &
!!     &         (id_file, level, label, array_c3)
!!        type(ctl_array_c3), intent(in) :: array_c3
!!      subroutine write_control_array_c_r                              &
!!     &         (id_file, level, label, array_cr)
!!        type(ctl_array_cr), intent(in) :: array_cr
!!      subroutine write_control_array_c_i                              &
!!     &         (id_file, level, label, array_ci)
!!        type(ctl_array_ci), intent(in) :: array_ci
!!      subroutine write_control_array_c_r2                             &
!!     &         (id_file, level, label, array_cr2)
!!        type(ctl_array_cr2), intent(in) :: array_cr2
!!      subroutine write_control_array_c2_r                             &
!!     &         (id_file, level, label, array_c2r)
!!        type(ctl_array_c2r), intent(in) :: array_c2r
!!      subroutine write_control_array_i_c_r                            &
!!     &         (id_file, level, label, array_icr)
!!        type(ctl_array_icr), intent(in) :: array_icr
!!      subroutine write_control_array_i_r                              &
!!     &         (id_file, level, label, array_ir)
!!        type(ctl_array_ir), intent(in) :: array_ir
!!      subroutine write_control_array_i2_r                             &
!!     &         (id_file, level, label, array_i2r)
!!        type(ctl_array_i2r), intent(in) :: array_i2r
!!      subroutine write_control_array_i2_r2                            &
!!     &         (id_file, level, label, array_i2r2)
!!        type(ctl_array_i2r2), intent(in) :: array_i2r2
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
      module write_control_arrays
!
      use m_precision
      use t_read_control_arrays
      use skip_comment_f
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_r1                                 &
     &         (id_file, level, label, array_real)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_real), intent(in) :: array_real
!
!
      if(array_real%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_real_list(id_file, level, label,       &
     &      array_real%num, array_real%vect)
      end if
!
      end subroutine write_control_array_r1
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_r2                                 &
     &         (id_file, level, label, array_r2)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_r2), intent(in) :: array_r2
!
!
      if(array_r2%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_real2_list(id_file, level, label,      &
     &      array_r2%num, array_r2%vec1, array_r2%vec2)
      end if
!
      end subroutine write_control_array_r2
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_r3                                 &
     &         (id_file, level, label, array_r3)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_r3), intent(in) :: array_r3
!
!
      if(array_r3%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_real3_list(id_file, level, label,      &
     &      array_r3%num, array_r3%vec1, array_r3%vec2, array_r3%vec3)
      end if
!
      end subroutine write_control_array_r3
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i1                                 &
     &         (id_file, level, label, array_int)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_int), intent(in) :: array_int
!
!
      if(array_int%num.gt.0 .and. array_int%icou.eq.0) then
        write(id_file,'(a1)') '!'
        call write_control_array_int_list(id_file, level, label,        &
     &      array_int%num, array_int%ivec)
      end if
!
      end subroutine write_control_array_i1
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i2                                 &
     &         (id_file, level, label, array_i2)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_i2), intent(in) :: array_i2
!
!
      if(array_i2%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_int2_list(id_file, level, label,       &
     &      array_i2%num, array_i2%int1, array_i2%int2)
      end if
!
      end subroutine write_control_array_i2
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c1                                 &
     &          (id_file, level, label, array_chara)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_chara), intent(in) :: array_chara
!
!
      if(array_chara%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_chara_list(id_file, level, label,      &
     &      array_chara%num, array_chara%c_tbl)
      end if
!
      end subroutine write_control_array_c1
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c2                                 &
     &         (id_file, level, label, array_c2)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_c2), intent(in) :: array_c2
!
!
      if(array_c2%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_chara2_list(id_file, level, label,     &
     &      array_c2%num, array_c2%c1_tbl, array_c2%c2_tbl)
      end if
!
      end subroutine write_control_array_c2
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c3                                 &
     &         (id_file, level, label, array_c3)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_c3), intent(in) :: array_c3
!
!
      if(array_c3%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_chara3_list(id_file, level, label,    &
     &      array_c3%num, array_c3%c1_tbl, array_c3%c2_tbl,            &
     &      array_c3%c3_tbl)
      end if
!
      end subroutine write_control_array_c3
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c_r                                &
     &         (id_file, level, label, array_cr)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_cr), intent(in) :: array_cr
!
!
      if(array_cr%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_vect_list(id_file, level, label,       &
     &      array_cr%num, array_cr%c_tbl, array_cr%vect)
      end if
!
      end subroutine write_control_array_c_r
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c_i                                &
     &         (id_file, level, label, array_ci)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_ci), intent(in) :: array_ci
!
!
      if(array_ci%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_int_v_list(id_file, level, label,      &
     &      array_ci%num, array_ci%c_tbl, array_ci%ivec)
      end if
!
      end subroutine write_control_array_c_i
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c_r2                               &
     &         (id_file, level, label, array_cr2)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_cr2), intent(in) :: array_cr2
!
!
      if(array_cr2%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_c_r2_list(id_file, level, label,       &
     &      array_cr2%num, array_cr2%c_tbl,                             &
     &      array_cr2%vec1, array_cr2%vec2)
      end if
!
      end subroutine write_control_array_c_r2
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_c2_r                               &
     &         (id_file, level, label, array_c2r)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_c2r), intent(in) :: array_c2r
!
!
      if(array_c2r%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_c2_r_list(id_file, level, label,       &
     &    array_c2r%num, array_c2r%c1_tbl, array_c2r%c2_tbl,            &
     &    array_c2r%vect)
      end if
!
      end subroutine write_control_array_c2_r
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i_c_r                              &
     &         (id_file, level, label, array_icr)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_icr), intent(in) :: array_icr
!
!
      if(array_icr%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_i_c_r_list(id_file, level, label,      &
     &    array_icr%num, array_icr%ivec, array_icr%c_tbl,               &
     &    array_icr%vect)
      end if
!
      end subroutine write_control_array_i_c_r
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i_r                                &
     &         (id_file, level, label, array_ir)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_ir), intent(in) :: array_ir
!
!
      if(array_ir%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_int_r_list(id_file, level, label,      &
     &      array_ir%num, array_ir%ivec, array_ir%vect)
      end if
!
      end subroutine write_control_array_i_r
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i2_r                               &
     &         (id_file, level, label, array_i2r)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_i2r), intent(in) :: array_i2r
!
!
      if(array_i2r%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_i2_r_list(id_file, level, label,       &
     &    array_i2r%num, array_i2r%int1, array_i2r%int2,                &
     &    array_i2r%vect)
      end if
!
      end subroutine write_control_array_i2_r
!
!   --------------------------------------------------------------------
!
      subroutine write_control_array_i2_r2                              &
     &         (id_file, level, label, array_i2r2)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(ctl_array_i2r2), intent(in) :: array_i2r2
!
!
      if(array_i2r2%num .gt. 0) then
        write(id_file,'(a1)') '!'
        call write_control_array_i2_r2_list(id_file, level, label,      &
     &    array_i2r2%num, array_i2r2%int1, array_i2r2%int2,             &
     &    array_i2r2%vec1, array_i2r2%vec2)
      end if
!
      end subroutine write_control_array_i2_r2
!
!   --------------------------------------------------------------------
!
      end module write_control_arrays
