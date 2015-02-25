!
!      module copy_rj_phys_type_4_IO
!
!      Written by H. Matsui on Oct., 2007
!
!!      subroutine copy_rj_all_phys_name_t_to_IO(nnod_rj, fld)
!!      subroutine copy_rj_all_phys_type_to_IO(nnod_rj, fld)
!!      subroutine copy_rj_viz_phys_data_t_to_IO(nnod_rj, fld)
!!      subroutine copy_rj_viz_phys_name_t_to_IO(nnod_rj, fld)
!
!!      subroutine copy_rj_phys_name_t_from_IO(nnod_rj, fld)
!!      subroutine copy_rj_phys_type_from_IO(nnod_rj, fld)
!!      subroutine set_rj_phys_type_from_IO(nnod_rj, fld)
!!
!!      subroutine copy_each_sph_sol_type_to_IO                         &
!!     &         (nnod_rj, fld, i_fld, j_IO)
!!      subroutine copy_each_sph_vect_type_to_IO                        &
!!     &         (nnod_rj, fld, i_fld, j_IO)
!!      subroutine copy_each_sph_fld_type_to_IO                         &
!!     &         (nnod_rj, fld, i_fld, j_IO)
!!
!!      subroutine copy_each_sph_sol_type_from_IO                       &
!!     &          (nnod_rj, fld, i_fld, j_IO)
!!      subroutine copy_each_sph_vect_type_from_IO                      &
!!     &         (nnod_rj, fld, i_fld, j_IO)
!!      subroutine copy_each_sph_fld_type_from_IO                       &
!!     &         (nnod_rj, fld, i_fld, j_IO)
!
      module copy_rj_phys_type_4_IO
!
      use m_precision
!
      use m_phys_constants
      use m_field_data_IO
      use t_sph_spectr_data
!
      implicit none
!
      private :: copy_rj_phys_name_t_to_IO, copy_rj_phys_data_t_to_IO
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_all_phys_name_t_to_IO(nnod_rj, fld)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: nnod_rj
!
!
      call copy_rj_phys_name_t_to_IO(nnod_rj, fld, fld%num_phys)
!
      end subroutine copy_rj_all_phys_name_t_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_viz_phys_name_t_to_IO(nnod_rj, fld)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: nnod_rj
!
!
      call copy_rj_phys_name_t_to_IO(nnod_rj, fld, fld%num_phys_viz)
!
      end subroutine copy_rj_viz_phys_name_t_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_all_phys_type_to_IO(nnod_rj, fld)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: nnod_rj
!
!
      call copy_rj_phys_data_t_to_IO(nnod_rj, fld, fld%num_phys)
!
      end subroutine copy_rj_all_phys_type_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_viz_phys_data_t_to_IO(nnod_rj, fld)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: nnod_rj
!
!
      call copy_rj_phys_data_t_to_IO(nnod_rj, fld, fld%num_phys_viz)
!
      end subroutine copy_rj_viz_phys_data_t_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_name_t_to_IO(nnod_rj, fld, num_fld)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: num_fld, nnod_rj
!
!
      numgrid_phys_IO =   nnod_rj
      num_phys_data_IO =  num_fld
      ntot_phys_data_IO = fld%ntot_phys
!
      call allocate_phys_data_name_IO
!
      num_phys_comp_IO(1:num_fld) =    fld%num_component(1:num_fld)
      istack_phys_comp_IO(0:num_fld) = fld%istack_component(0:num_fld)
      phys_data_name_IO(1:num_fld) =   fld%phys_name(1:num_fld)
!
      end subroutine copy_rj_phys_name_t_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_data_t_to_IO(nnod_rj, fld, num_fld)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: nnod_rj
!
      integer(kind = kint), intent(in) :: num_fld
      integer(kind = kint) :: i_fld
!
!
      do i_fld = 1, num_fld
        if (fld%num_component(i_fld) .eq. n_vector) then
          call copy_each_sph_vect_type_to_IO                            &
     &       (nnod_rj, fld, i_fld, i_fld)
        else
          call copy_each_sph_fld_type_to_IO(nnod_rj, fld, i_fld, i_fld)
        end if
      end do
!
      end subroutine copy_rj_phys_data_t_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_name_t_from_IO(nnod_rj, fld)
!
      type(phys_data), intent(inout) :: fld
      integer(kind = kint), intent(in) :: nnod_rj
!
      fld%num_phys =  num_phys_data_IO
      fld%ntot_phys = ntot_phys_data_IO
!
      call alloc_phys_name_type(fld)
      call alloc_phys_data_type(nnod_rj, fld)
!
      fld%num_component(1:fld%num_phys)                                 &
     &      = num_phys_comp_IO(1:fld%num_phys)
      fld%istack_component(0:fld%num_phys)                              &
     &      = istack_phys_comp_IO(0:fld%num_phys)
      fld%phys_name(1:fld%num_phys)                                     &
     &      = phys_data_name_IO(1:fld%num_phys)
      fld%iflag_monitor(1:fld%num_phys) = 1
!
      end subroutine copy_rj_phys_name_t_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_type_from_IO(nnod_rj, fld)
!
      type(phys_data), intent(inout) :: fld
      integer(kind = kint), intent(in) :: nnod_rj
      integer(kind = kint) :: i_fld
!
!
      do i_fld = 1, fld%num_phys
        if (fld%num_component(i_fld) .eq. 3) then
          call copy_each_sph_vect_type_from_IO                          &
     &       (nnod_rj, fld, i_fld, i_fld)
        else
          call copy_each_sph_fld_type_from_IO                           &
     &       (nnod_rj, fld, i_fld, i_fld)
        end if
      end do
!
      end subroutine copy_rj_phys_type_from_IO
!
! -------------------------------------------------------------------
!
      subroutine set_rj_phys_type_from_IO(nnod_rj, fld)
!
      type(phys_data), intent(inout) :: fld
      integer(kind = kint), intent(in) :: nnod_rj
      integer(kind = kint) :: i_fld, j_IO
!
      do i_fld = 1, fld%num_phys
        do j_IO = 1, num_phys_data_IO
          if (fld%phys_name(i_fld) .eq. phys_data_name_IO(j_IO)) then
            if (num_phys_comp_IO(j_IO) .eq. 3) then
              call copy_each_sph_vect_type_from_IO                      &
     &           (nnod_rj, fld, i_fld, j_IO)
            else if (num_phys_comp_IO(j_IO) .eq. 2) then
              call copy_each_sph_sol_type_from_IO                       &
     &           (nnod_rj, fld, i_fld, j_IO)
            else
              call copy_each_sph_fld_type_from_IO                       &
     &           (nnod_rj, fld, i_fld, j_IO)
            end if
            exit
          end if
        end do
      end do
!
      end subroutine set_rj_phys_type_from_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_sol_type_to_IO                           &
     &         (nnod_rj, fld, i_fld, j_IO)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: i_fld, j_IO, nnod_rj
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = fld%istack_component(i_fld-1)
      jst = istack_phys_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        phys_data_IO(inod,jst+1) = fld%d_fld(inod,ist+1)
        phys_data_IO(inod,jst+2) = fld%d_fld(inod,ist+3)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_sol_type_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_vect_type_to_IO                          &
     &         (nnod_rj, fld, i_fld, j_IO)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: i_fld, j_IO, nnod_rj
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = fld%istack_component(i_fld-1)
      jst = istack_phys_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        phys_data_IO(inod,jst+1) = fld%d_fld(inod,ist+1)
        phys_data_IO(inod,jst+2) = fld%d_fld(inod,ist+3)
        phys_data_IO(inod,jst+3) = fld%d_fld(inod,ist+2)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_vect_type_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_fld_type_to_IO                           &
     &         (nnod_rj, fld, i_fld, j_IO)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: i_fld, j_IO, nnod_rj
      integer(kind = kint) :: ist, jst, nd, inod
!
!
      ist = fld%istack_component(i_fld-1)
      jst = istack_phys_comp_IO(j_IO-1 )
!$omp parallel
      do nd = 1, fld%num_component(i_fld)
!$omp do
        do inod = 1, nnod_rj
          phys_data_IO(inod,jst+nd) = fld%d_fld(inod,ist+nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_each_sph_fld_type_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_sol_type_from_IO                         &
     &          (nnod_rj, fld, i_fld, j_IO)
!
      type(phys_data), intent(inout) :: fld
      integer(kind = kint), intent(in) :: i_fld, j_IO, nnod_rj
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = fld%istack_component(i_fld-1)
      jst = istack_phys_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        fld%d_fld(inod,ist+1) = phys_data_IO(inod,jst+1)
        fld%d_fld(inod,ist+3) = phys_data_IO(inod,jst+2)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_sol_type_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_vect_type_from_IO                        &
     &         (nnod_rj, fld, i_fld, j_IO)
!
      type(phys_data), intent(inout) :: fld
      integer(kind = kint), intent(in) :: i_fld, j_IO, nnod_rj
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = fld%istack_component(i_fld-1)
      jst = istack_phys_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        fld%d_fld(inod,ist+1) = phys_data_IO(inod,jst+1)
        fld%d_fld(inod,ist+3) = phys_data_IO(inod,jst+2)
        fld%d_fld(inod,ist+2) = phys_data_IO(inod,jst+3)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_vect_type_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_fld_type_from_IO                         &
     &         (nnod_rj, fld, i_fld, j_IO)
!
      type(phys_data), intent(inout) :: fld
      integer(kind = kint), intent(in) :: i_fld, j_IO, nnod_rj
      integer(kind = kint) :: ist, jst, nd, inod
!
!
      ist = fld%istack_component(i_fld-1)
      jst = istack_phys_comp_IO(j_IO-1 )
!$omp parallel
      do nd = 1, fld%num_component(i_fld)
!$omp do
        do inod = 1, nnod_rj
          fld%d_fld(inod,ist+nd) = phys_data_IO(inod,jst+nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_each_sph_fld_type_from_IO
!
! -------------------------------------------------------------------
!
      end module copy_rj_phys_type_4_IO
