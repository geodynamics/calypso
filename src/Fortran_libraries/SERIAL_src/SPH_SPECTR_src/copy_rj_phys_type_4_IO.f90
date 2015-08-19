!> @file  copy_rj_phys_type_4_IO.f90
!!      module copy_rj_phys_type_4_IO
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2007
!
!> @brief Copy between field data and IO data
!!
!!@verbatim
!!      subroutine copy_rj_all_phys_name_t_to_IO(nnod_rj, fld, fld_IO)
!!      subroutine copy_rj_viz_phys_name_t_to_IO(nnod_rj, fld, fld_IO)
!!      subroutine copy_rj_all_phys_type_to_IO(nnod_rj, fld, fld_IO)
!!      subroutine copy_rj_viz_phys_data_t_to_IO(nnod_rj, fld, fld_IO)
!!
!!      subroutine copy_rj_phys_name_t_from_IO(nnod_rj, fld_IO, fld)
!!      subroutine copy_rj_phys_type_from_IO(nnod_rj, fld_IO, fld)
!!      subroutine set_rj_phys_type_from_IO(nnod_rj, fld_IO, fld)
!!
!!      subroutine copy_each_sph_sol_type_to_IO                         &
!!     &         (nnod_rj, fld, fld_IO, i_fld, j_IO)
!!      subroutine copy_each_sph_vect_type_to_IO                        &
!!     &         (nnod_rj, fld, fld_IO, i_fld, j_IO)
!!      subroutine copy_each_sph_fld_type_to_IO                         &
!!     &         (nnod_rj, fld, fld_IO, i_fld, j_IO)
!!
!!      subroutine copy_each_sph_sol_type_from_IO                       &
!!     &          (nnod_rj, fld_IO, fld, i_fld, j_IO)
!!      subroutine copy_each_sph_vect_type_from_IO                      &
!!     &         (nnod_rj, fld_IO, fld, i_fld, j_IO)
!!      subroutine copy_each_sph_fld_type_from_IO                       &
!!     &         (nnod_rj, fld_IO, fld, i_fld, j_IO)
!!@endverbatim
!
      module copy_rj_phys_type_4_IO
!
      use m_precision
!
      use m_phys_constants
      use t_field_data_IO
      use t_sph_spectr_data
!
      implicit none
!
      private :: copy_rj_phys_name_t_to_IO, copy_rj_phys_data_t_to_IO
      private :: copy_each_sph_sol_type_to_IO
      private :: copy_each_sph_vect_type_to_IO
      private :: copy_each_sph_fld_type_to_IO
      private :: copy_each_sph_sol_type_from_IO
      private :: copy_each_sph_vect_type_from_IO
      private :: copy_each_sph_fld_type_from_IO
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_all_phys_name_t_to_IO(nnod_rj, fld, fld_IO)
!
      integer(kind = kint), intent(in) :: nnod_rj
      type(phys_data), intent(in) :: fld
      type(field_IO), intent(inout) :: fld_IO
!
!
      call copy_rj_phys_name_t_to_IO                                    &
     &   (nnod_rj, fld%num_phys, fld, fld_IO)
!
      end subroutine copy_rj_all_phys_name_t_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_viz_phys_name_t_to_IO(nnod_rj, fld, fld_IO)
!
      integer(kind = kint), intent(in) :: nnod_rj
      type(phys_data), intent(in) :: fld
      type(field_IO), intent(inout) :: fld_IO
!
!
      call copy_rj_phys_name_t_to_IO                                    &
     &   (nnod_rj, fld%num_phys_viz, fld, fld_IO)
!
      end subroutine copy_rj_viz_phys_name_t_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_all_phys_type_to_IO(nnod_rj, fld, fld_IO)
!
      integer(kind = kint), intent(in) :: nnod_rj
      type(phys_data), intent(in) :: fld
      type(field_IO), intent(inout) :: fld_IO
!
!
      call copy_rj_phys_data_t_to_IO                                    &
     &   (nnod_rj, fld%num_phys, fld, fld_IO)
!
      end subroutine copy_rj_all_phys_type_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_viz_phys_data_t_to_IO(nnod_rj, fld, fld_IO)
!
      integer(kind = kint), intent(in) :: nnod_rj
      type(phys_data), intent(in) :: fld
      type(field_IO), intent(inout) :: fld_IO
!
!
      call copy_rj_phys_data_t_to_IO                                    &
     &   (nnod_rj, fld%num_phys_viz, fld, fld_IO)
!
      end subroutine copy_rj_viz_phys_data_t_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_name_t_to_IO                              &
     &         (nnod_rj, num_fld, fld, fld_IO)
!
      integer(kind = kint), intent(in) :: num_fld, nnod_rj
      type(phys_data), intent(in) :: fld
      type(field_IO), intent(inout) :: fld_IO
!
!
      fld_IO%nnod_IO =   nnod_rj
      fld_IO%num_field_IO =  num_fld
      fld_IO%ntot_comp_IO = fld%ntot_phys
!
      call alloc_phys_name_IO(fld_IO)
!
      fld_IO%num_comp_IO(1:num_fld) =    fld%num_component(1:num_fld)
      fld_IO%istack_comp_IO(0:num_fld) = fld%istack_component(0:num_fld)
      fld_IO%fld_name(1:num_fld) =   fld%phys_name(1:num_fld)
!
      end subroutine copy_rj_phys_name_t_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_data_t_to_IO                              &
     &         (nnod_rj, num_fld, fld, fld_IO)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: nnod_rj
      integer(kind = kint), intent(in) :: num_fld
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint) :: i_fld
!
!
      do i_fld = 1, num_fld
        if (fld%num_component(i_fld) .eq. n_vector) then
          call copy_each_sph_vect_type_to_IO                            &
     &       (nnod_rj, fld, fld_IO, i_fld, i_fld)
        else
          call copy_each_sph_fld_type_to_IO                             &
     &       (nnod_rj, fld, fld_IO, i_fld, i_fld)
        end if
      end do
!
      end subroutine copy_rj_phys_data_t_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_name_t_from_IO(nnod_rj, fld_IO, fld)
!
      integer(kind = kint), intent(in) :: nnod_rj
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: fld
!
      fld%num_phys =  fld_IO%num_field_IO
      fld%ntot_phys = fld_IO%ntot_comp_IO
!
      call alloc_phys_name_type(fld)
      call alloc_phys_data_type(nnod_rj, fld)
!
      fld%num_component(1:fld%num_phys)                                 &
     &      = fld_IO%num_comp_IO(1:fld%num_phys)
      fld%istack_component(0:fld%num_phys)                              &
     &      = fld_IO%istack_comp_IO(0:fld%num_phys)
      fld%phys_name(1:fld%num_phys)                                     &
     &      = fld_IO%fld_name(1:fld%num_phys)
      fld%iflag_monitor(1:fld%num_phys) = 1
!
      end subroutine copy_rj_phys_name_t_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_type_from_IO(nnod_rj, fld_IO, fld)
!
      integer(kind = kint), intent(in) :: nnod_rj
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: fld
      integer(kind = kint) :: i_fld
!
!
      do i_fld = 1, fld%num_phys
        if (fld%num_component(i_fld) .eq. 3) then
          call copy_each_sph_vect_type_from_IO                          &
     &       (nnod_rj, fld_IO, fld, i_fld, i_fld)
        else
          call copy_each_sph_fld_type_from_IO                           &
     &       (nnod_rj, fld_IO, fld, i_fld, i_fld)
        end if
      end do
!
      end subroutine copy_rj_phys_type_from_IO
!
! -------------------------------------------------------------------
!
      subroutine set_rj_phys_type_from_IO(nnod_rj, fld_IO, fld)
!
      integer(kind = kint), intent(in) :: nnod_rj
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: fld
      integer(kind = kint) :: i_fld, j_IO
!
      do i_fld = 1, fld%num_phys
        do j_IO = 1, fld_IO%num_field_IO
          if (fld%phys_name(i_fld) .eq. fld_IO%fld_name(j_IO)) then
            if (fld_IO%num_comp_IO(j_IO) .eq. 3) then
              call copy_each_sph_vect_type_from_IO                      &
     &           (nnod_rj, fld_IO, fld, i_fld, j_IO)
            else if (fld_IO%num_comp_IO(j_IO) .eq. 2) then
              call copy_each_sph_sol_type_from_IO                       &
     &           (nnod_rj, fld_IO, fld, i_fld, j_IO)
            else
              call copy_each_sph_fld_type_from_IO                       &
     &           (nnod_rj, fld_IO, fld, i_fld, j_IO)
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
     &         (nnod_rj, fld, fld_IO, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO, nnod_rj
      type(phys_data), intent(in) :: fld
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        fld_IO%d_IO(inod,jst+1) = fld%d_fld(inod,ist+1)
        fld_IO%d_IO(inod,jst+2) = fld%d_fld(inod,ist+3)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_sol_type_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_vect_type_to_IO                          &
     &         (nnod_rj, fld, fld_IO, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO, nnod_rj
      type(phys_data), intent(in) :: fld
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        fld_IO%d_IO(inod,jst+1) = fld%d_fld(inod,ist+1)
        fld_IO%d_IO(inod,jst+2) = fld%d_fld(inod,ist+3)
        fld_IO%d_IO(inod,jst+3) = fld%d_fld(inod,ist+2)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_vect_type_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_fld_type_to_IO                           &
     &         (nnod_rj, fld, fld_IO, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO, nnod_rj
      type(phys_data), intent(in) :: fld
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint) :: ist, jst, nd, inod
!
!
      ist = fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1 )
!$omp parallel
      do nd = 1, fld%num_component(i_fld)
!$omp do
        do inod = 1, nnod_rj
          fld_IO%d_IO(inod,jst+nd) = fld%d_fld(inod,ist+nd)
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
     &          (nnod_rj, fld_IO, fld, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO, nnod_rj
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: fld
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        fld%d_fld(inod,ist+1) = fld_IO%d_IO(inod,jst+1)
        fld%d_fld(inod,ist+3) = fld_IO%d_IO(inod,jst+2)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_sol_type_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_vect_type_from_IO                        &
     &         (nnod_rj, fld_IO, fld, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO, nnod_rj
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: fld
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        fld%d_fld(inod,ist+1) = fld_IO%d_IO(inod,jst+1)
        fld%d_fld(inod,ist+3) = fld_IO%d_IO(inod,jst+2)
        fld%d_fld(inod,ist+2) = fld_IO%d_IO(inod,jst+3)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_vect_type_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_fld_type_from_IO                         &
     &         (nnod_rj, fld_IO, fld, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO, nnod_rj
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: fld
      integer(kind = kint) :: ist, jst, nd, inod
!
!
      ist = fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1 )
!$omp parallel
      do nd = 1, fld%num_component(i_fld)
!$omp do
        do inod = 1, nnod_rj
          fld%d_fld(inod,ist+nd) = fld_IO%d_IO(inod,jst+nd)
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
