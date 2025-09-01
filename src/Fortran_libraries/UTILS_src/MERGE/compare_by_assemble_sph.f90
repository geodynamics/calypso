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
!!     &          (delta, time_d, new_sph, new_phys, new_fst_IO, t_IO)
!!        real(kind = kreal), intent(in) :: delta
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
      private :: compare_rj_phys_data_with_IO
      private :: compare_each_sph_vector_with_IO
      private :: compare_each_sph_field_with_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function compare_assembled_sph_data          &
     &          (delta, time_d, new_sph, new_phys, new_fst_IO, t_IO)
!
      use t_spheric_parameter
!
      real(kind = kreal), intent(in) :: delta
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
     &    = compare_rj_phys_data_with_IO(delta, new_sph%sph_rj,         &
     &                                   new_phys, new_fst_IO)
!
      end function compare_assembled_sph_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function compare_rj_phys_data_with_IO        &
     &                            (delta, sph_rj, rj_fld, fld_IO)
!
      use calypso_mpi_int
      use transfer_to_long_integers
!
      real(kind = kreal), intent(in) :: delta
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(in) :: fld_IO
!
      integer(kind = kint) :: i_fld
      integer(kind = kint) :: iflag(1), iflag_gl(1)
!
!
      compare_rj_phys_data_with_IO = 0
      do i_fld = 1, rj_fld%num_phys
        if(i_debug .gt. 0) then
          write(100+my_rank,*) 'field name: ',                          &
     &                        trim(rj_fld%phys_name(i_fld))
        end if
!
        if (rj_fld%num_component(i_fld) .eq. n_vector) then
          iflag(1) = compare_each_sph_vector_with_IO                    &
     &                    (sph_rj, rj_fld, fld_IO, i_fld, i_fld, delta)
        else
          iflag(1) = compare_each_sph_field_with_IO(sph_rj, rj_fld,     &
     &                    fld_IO, rj_fld%num_component(i_fld),          &
     &                    i_fld, i_fld, delta)
        end if
        call calypso_mpi_allreduce_int(iflag(1), iflag_gl(1),           &
     &                                 cast_long(ione), MPI_SUM)
!
        compare_rj_phys_data_with_IO                                    &
     &         = compare_rj_phys_data_with_IO + iflag_gl(1)
!
        if(iflag_gl(1) .gt. 0) then
          write(*,*) trim(rj_fld%phys_name(i_fld)),                     &
     &            ' data does not match. in', my_rank
          compare_rj_phys_data_with_IO = iflag_gl(1)
          if(i_debug .gt. 0) return
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
     &                   (sph_rj, rj_fld, fld_IO, i_fld, j_IO, delta)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(in) :: fld_IO
      real(kind = kreal), intent(in) :: delta
!
      integer(kind = kint) :: ist, jst, inod, iflag, nd
      real(kind = kreal) :: diff(3), swap(3), tgt(3)
!
!
      compare_each_sph_vector_with_IO = 0
      ist = rj_fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
      do inod = 1, rj_fld%n_point
        iflag = 0
        swap(1) = rj_fld%d_fld(inod,ist+1)
        swap(2) = rj_fld%d_fld(inod,ist+3)
        swap(3) = rj_fld%d_fld(inod,ist+2)
        tgt(1:3) = fld_IO%d_IO(inod,jst+1:jst+3)
!
        do nd = 1, 3
          diff(nd) = tgt(nd) - swap(nd)
          if(diff(nd) .eq. 0.0d0) cycle
!
          diff(nd) = diff(nd) / max(abs(swap(nd)), abs(tgt(nd)))
!
          if(abs(diff(nd)) .gt. delta) then
            iflag = iflag + 1
          end if
        end do
!
        compare_each_sph_vector_with_IO                                 &
     &                   = compare_each_sph_vector_with_IO + iflag
        if(iflag .gt. 0) then
          if(i_debug .eq. 0) return
!
          write(100+my_rank,'(3i15,1p6e25.13e3)') inod,                 &
     &                        sph_rj%idx_global_rj(inod,1:2),           &
     &                        fld_IO%d_IO(inod,jst+1:jst+3), diff(1:3)
        end if
      end do
      return
!
      end function compare_each_sph_vector_with_IO
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function compare_each_sph_field_with_IO      &
     &           (sph_rj, rj_fld, fld_IO, numdir, i_fld, j_IO, delta)
!
      integer(kind = kint), intent(in) :: numdir, i_fld, j_IO
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(in) :: fld_IO
      real(kind = kreal), intent(in) :: delta
!
      integer(kind = kint) :: ist, jst, nd, inod, iflag
      real(kind = kreal) :: diff(numdir)
!
!
      compare_each_sph_field_with_IO = 0
      ist = rj_fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1 )
      do inod = 1, rj_fld%n_point
        iflag = 0
        do nd = 1, numdir
          diff(nd) = fld_IO%d_IO(inod,jst+nd)                           &
     &              - rj_fld%d_fld(inod,ist+nd)
          if(diff(nd) .eq. 0.0d0) cycle
!
          diff(nd) = diff(nd) / max(abs(fld_IO%d_IO(inod,jst+nd)),      &
     &                              abs(rj_fld%d_fld(inod,ist+nd)))
!
          if(abs(diff(nd)) .gt. delta) iflag = iflag + 1
          compare_each_sph_field_with_IO                                &
     &                   = compare_each_sph_field_with_IO + iflag
        end do
!
        if(iflag .gt. 0) then
          if(i_debug .eq. 0) return
!
          write(100+my_rank,'(3i15,1p12e25.13e3)') inod,                &
     &                        sph_rj%idx_global_rj(inod,1:2),           &
     &                        fld_IO%d_IO(inod,jst+1:jst+numdir),       &
     &                        diff(1:numdir)
        end if
      end do
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
