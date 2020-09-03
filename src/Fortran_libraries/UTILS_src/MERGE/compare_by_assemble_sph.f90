!>@file   compare_by_assemble_sph.f90
!!@brief  module compare_by_assemble_sph
!!
!!@author H. Matsui 
!!@date Programmed in Feb., 2011
!
!>@brief functions to compare assembled data
!!
!!@verbatim
!!      integer(kind = kint) function compare_assembled_sph_data        &
!!     &          (my_rank, time_d, new_sph, new_phys, new_fst_IO, t_IO)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_grids), intent(in) :: new_sph
!!        type(phys_data), intent(inout) :: new_phys
!!        type(field_IO), intent(inout) :: new_fst_IO
!!        type(time_data), intent(inout) :: t_IO
!!@endverbatim
!
      module compare_by_assemble_sph
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use t_time_data
      use t_spheric_parameter
      use t_phys_data
      use t_field_data_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function compare_assembled_sph_data          &
     &          (my_rank, time_d, new_sph, new_phys, new_fst_IO, t_IO)
!
      use t_spheric_parameter
!
      integer, intent(in) :: my_rank
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) :: new_sph
!
      type(phys_data), intent(inout) :: new_phys
      type(field_IO), intent(inout) :: new_fst_IO
      type(time_data), intent(inout) :: t_IO
!
      integer(kind = kint) :: iflag
!
!
      iflag = compare_time_step_with_IO(time_d, t_IO)
      if(iflag .gt. 0) then
        write(*,*) 'time parameter does not match in ', my_rank
        compare_assembled_sph_data = iflag
        return
      end if
!
      iflag = compare_rj_phys_name_with_IO(new_phys, new_fst_IO)
      if(iflag .gt. 0) then
        write(*,*) 'Field parameter does not match in ', my_rank
        compare_assembled_sph_data = iflag
        return
      end if
!
      if(new_fst_IO%nnod_IO .ne. new_sph%sph_rj%nnod_rj) then
        write(*,*) 'Number of point does not match in', my_rank
        compare_assembled_sph_data = 1
        return
      end if
!
      compare_assembled_sph_data                                        &
     &    = compare_rj_phys_data_with_IO(my_rank, new_phys, new_fst_IO)
!
      end function compare_assembled_sph_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &            compare_rj_phys_data_with_IO(my_rank, rj_fld, fld_IO)
!
      integer, intent(in) :: my_rank
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(in) :: fld_IO
!
      integer(kind = kint) :: i_fld
      integer(kind = kint) :: iflag
!
!
      do i_fld = 1, rj_fld%num_phys
        if (rj_fld%num_component(i_fld) .eq. n_vector) then
          iflag = compare_each_sph_vector_with_IO(rj_fld, fld_IO,       &
     &                                            i_fld, i_fld)
        else
          iflag = compare_each_sph_field_with_IO(rj_fld, fld_IO,        &
     &                                           i_fld, i_fld)
        end if
        if(iflag .gt. 0) then
          write(*,*) trim(rj_fld%phys_name(i_fld)),                     &
     &            ' data does not match. in', my_rank
          compare_rj_phys_data_with_IO = iflag
          return
        end if
      end do
      return
!
      end function compare_rj_phys_data_with_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      integer(kind = kint) function compare_each_sph_solnid_with_IO     &
     &                            (rj_fld, fld_IO, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(in) :: fld_IO
!
      integer(kind = kint) :: ist, jst, inod
      real(kind = kreal) :: diff
!
!
      compare_each_sph_solnid_with_IO = 1
      ist = rj_fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
      do inod = 1, rj_fld%n_point
        diff = fld_IO%d_IO(inod,jst+1) - rj_fld%d_fld(inod,ist+1)
        if(abs(diff) .gt. TINY) return
        diff = fld_IO%d_IO(inod,jst+2) - rj_fld%d_fld(inod,ist+3)
        if(abs(diff) .gt. TINY) return
      end do
      compare_each_sph_solnid_with_IO = 0
      return
!
      end function compare_each_sph_solnid_with_IO
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function compare_each_sph_vector_with_IO     &
     &                            (rj_fld, fld_IO, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(in) :: fld_IO
!
      integer(kind = kint) :: ist, jst, inod
      real(kind = kreal) :: diff
!
!
      compare_each_sph_vector_with_IO = 1
      ist = rj_fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
      do inod = 1, rj_fld%n_point
        diff = fld_IO%d_IO(inod,jst+1) - rj_fld%d_fld(inod,ist+1)
        if(abs(diff) .gt. TINY) return
        diff = fld_IO%d_IO(inod,jst+2) - rj_fld%d_fld(inod,ist+3)
        if(abs(diff) .gt. TINY) return
        diff = fld_IO%d_IO(inod,jst+3) - rj_fld%d_fld(inod,ist+2)
        if(abs(diff) .gt. TINY) return
      end do
      compare_each_sph_vector_with_IO = 0
      return
!
      end function compare_each_sph_vector_with_IO
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function compare_each_sph_field_with_IO      &
     &                            (rj_fld, fld_IO, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(in) :: fld_IO
!
      integer(kind = kint) :: ist, jst, nd, inod
      real(kind = kreal) :: diff
!
!
      compare_each_sph_field_with_IO = 1
      ist = rj_fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1 )
      do nd = 1, rj_fld%num_component(i_fld)
        do inod = 1, rj_fld%n_point
          diff = fld_IO%d_IO(inod,jst+nd) - rj_fld%d_fld(inod,ist+nd)
          if(abs(diff) .gt. TINY) return
        end do
      end do
      compare_each_sph_field_with_IO = 0
      return
!
      end function compare_each_sph_field_with_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                   compare_rj_phys_name_with_IO(rj_fld, fld_IO)
!
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(in) :: fld_IO
!
      integer(kind = kint) :: i
!
      compare_rj_phys_name_with_IO = 1
      if(fld_IO%nnod_IO .ne. rj_fld%n_point) return
      if(fld_IO%num_field_IO .ne. rj_fld%num_phys) return
      if(fld_IO%ntot_comp_IO .ne. rj_fld%ntot_phys) return
!
      do i = 1, rj_fld%num_phys
        if(fld_IO%num_comp_IO(i) .ne. rj_fld%num_component(i)) return
        if(fld_IO%istack_comp_IO(i)                                     &
     &         .ne. rj_fld%istack_component(i)) return
        if(fld_IO%fld_name(i) .ne. rj_fld%phys_name(i)) return
      end do
      compare_rj_phys_name_with_IO = 0
      return
!
      end function compare_rj_phys_name_with_IO
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function compare_time_step_with_IO           &
     &                            (time_org, time_IO)
!
      type(time_data), intent(in) :: time_org
      type(time_data), intent(in) :: time_IO
!
!
      compare_time_step_with_IO = 1
      if(time_IO%i_time_step .ne. time_org%i_time_step) return
      if(abs(time_IO%time - time_org%time) .gt. TINY)   return
      if(abs(time_IO%dt - time_org%dt) .gt. TINY)       return
      compare_time_step_with_IO = 0
!
      end function compare_time_step_with_IO
!
!  ---------------------------------------------------------------------
!
      end module compare_by_assemble_sph
