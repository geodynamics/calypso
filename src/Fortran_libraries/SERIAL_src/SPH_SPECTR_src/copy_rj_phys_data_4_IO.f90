!> @file  copy_rj_phys_data_4_IO.f90
!!      module copy_rj_phys_data_4_IO
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2007
!
!> @brief Copy between field data and IO data
!!
!!@verbatim
!!      subroutine copy_rj_phys_name_to_IO(num_fld, rj_fld, fld_IO)
!!      subroutine copy_rj_phys_data_to_IO(num_fld, rj_fld, fld_IO)
!!
!!      subroutine copy_rj_phys_name_from_IO(fld_IO, rj_fld)
!!      subroutine copy_rj_phys_data_from_IO(fld_IO, rj_fld)
!!      subroutine set_rj_phys_data_from_IO(fld_IO, rj_fld)
!!
!!      subroutine copy_each_sph_solenoid_to_IO                         &
!!     &         (rj_fld, fld_IO, i_fld, j_IO)
!!      subroutine copy_each_sph_vector_to_IO                           &
!!     &         (rj_fld, fld_IO, i_fld, j_IO)
!!      subroutine copy_each_sph_field_to_IO                            &
!!     &         (rj_fld, fld_IO, i_fld, j_IO)
!!
!!      subroutine copy_each_sph_solenoid_from_IO                       &
!!     &         (fld_IO, rj_fld, i_fld, j_IO)
!!      subroutine copy_each_sph_vector_from_IO                         &
!!     &         (fld_IO, rj_fld, i_fld, j_IO)
!!      subroutine copy_each_sph_field_from_IO                          &
!!     &         (fld_IO, rj_fld, i_fld, j_IO)
!!@endverbatim
!
      module copy_rj_phys_data_4_IO
!
      use m_precision
!
      use m_phys_constants
      use t_field_data_IO
      use t_sph_spectr_data
!
      implicit none
!
      private :: copy_each_sph_solenoid_to_IO
      private :: copy_each_sph_solenoid_from_IO
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_name_to_IO(num_fld, rj_fld, fld_IO)
!
      integer(kind = kint), intent(in) :: num_fld
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
!
!
      fld_IO%nnod_IO =       rj_fld%n_point
      fld_IO%num_field_IO =  num_fld
      fld_IO%ntot_comp_IO =  rj_fld%ntot_phys
!
      call alloc_phys_name_IO(fld_IO)
!
      fld_IO%num_comp_IO(1:num_fld)                                     &
     &             =   rj_fld%num_component(1:num_fld)
      fld_IO%istack_comp_IO(0:num_fld)                                  &
     &             = rj_fld%istack_component(0:num_fld)
      fld_IO%fld_name(1:num_fld)                                        &
     &             =   rj_fld%phys_name(1:num_fld)
!
      end subroutine copy_rj_phys_name_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_data_to_IO(num_fld, rj_fld, fld_IO)
!
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: num_fld
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint) :: i_fld
!
!
      do i_fld = 1, num_fld
        if (rj_fld%num_component(i_fld) .eq. n_vector) then
          call copy_each_sph_vector_to_IO                               &
     &       (rj_fld, fld_IO, i_fld, i_fld)
        else
          call copy_each_sph_field_to_IO                                &
     &       (rj_fld, fld_IO, i_fld, i_fld)
        end if
      end do
!
      end subroutine copy_rj_phys_data_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_name_from_IO(fld_IO, rj_fld)
!
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: i_fld
!
!
      rj_fld%num_phys =  fld_IO%num_field_IO
      call alloc_phys_name_type(rj_fld)
!
      rj_fld%phys_name(1:rj_fld%num_phys)                               &
     &      = fld_IO%fld_name(1:rj_fld%num_phys)
      rj_fld%num_component(1:rj_fld%num_phys)                           &
     &      = fld_IO%num_comp_IO(1:rj_fld%num_phys)
      rj_fld%iflag_monitor(1:rj_fld%num_phys) = 1
!
      do i_fld = 1, rj_fld%num_phys
        if(rj_fld%num_component(i_fld) .eq. 2) then
          rj_fld%num_component(i_fld) = 3
        end if
      end do
!
      rj_fld%istack_component(0)  = 0
      do i_fld = 1, rj_fld%num_phys
        rj_fld%istack_component(i_fld)                                  &
     &         = rj_fld%istack_component(i_fld-1)                       &
     &          + rj_fld%num_component(i_fld)
      end do
!
      rj_fld%ntot_phys = rj_fld%istack_component(rj_fld%num_phys)
!
      end subroutine copy_rj_phys_name_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_data_from_IO(fld_IO, rj_fld)
!
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: rj_fld
      integer(kind = kint) :: i_fld
!
!
      do i_fld = 1, rj_fld%num_phys
        if (rj_fld%num_component(i_fld) .eq. 3) then
          call copy_each_sph_vector_from_IO                             &
     &       (fld_IO, rj_fld, i_fld, i_fld)
        else
          call copy_each_sph_field_from_IO                              &
     &       (fld_IO, rj_fld, i_fld, i_fld)
        end if
      end do
!
      end subroutine copy_rj_phys_data_from_IO
!
! -------------------------------------------------------------------
!
      subroutine set_rj_phys_data_from_IO(fld_IO, rj_fld)
!
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: rj_fld
      integer(kind = kint) :: i_fld, j_IO
!
      do i_fld = 1, rj_fld%num_phys
        do j_IO = 1, fld_IO%num_field_IO
          if (rj_fld%phys_name(i_fld) .eq. fld_IO%fld_name(j_IO)) then
            if (fld_IO%num_comp_IO(j_IO) .eq. 3) then
              call copy_each_sph_vector_from_IO                         &
     &           (fld_IO, rj_fld, i_fld, j_IO)
            else if (fld_IO%num_comp_IO(j_IO) .eq. 2) then
              call copy_each_sph_solenoid_from_IO                       &
     &           (fld_IO, rj_fld, i_fld, j_IO)
            else
              call copy_each_sph_field_from_IO                          &
     &           (fld_IO, rj_fld, i_fld, j_IO)
            end if
            exit
          end if
        end do
      end do
!
      end subroutine set_rj_phys_data_from_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_solenoid_to_IO                           &
     &         (rj_fld, fld_IO, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = rj_fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, rj_fld%n_point
        fld_IO%d_IO(inod,jst+1) = rj_fld%d_fld(inod,ist+1)
        fld_IO%d_IO(inod,jst+2) = rj_fld%d_fld(inod,ist+3)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_solenoid_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_vector_to_IO                             &
     &         (rj_fld, fld_IO, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = rj_fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, rj_fld%n_point
        fld_IO%d_IO(inod,jst+1) = rj_fld%d_fld(inod,ist+1)
        fld_IO%d_IO(inod,jst+2) = rj_fld%d_fld(inod,ist+3)
        fld_IO%d_IO(inod,jst+3) = rj_fld%d_fld(inod,ist+2)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_vector_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_field_to_IO                              &
     &         (rj_fld, fld_IO, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint) :: ist, jst, nd, inod
!
!
      ist = rj_fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1 )
!$omp parallel
      do nd = 1, rj_fld%num_component(i_fld)
!$omp do
        do inod = 1, rj_fld%n_point
          fld_IO%d_IO(inod,jst+nd) = rj_fld%d_fld(inod,ist+nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_each_sph_field_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_solenoid_from_IO                         &
     &          (fld_IO, rj_fld, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: rj_fld
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = rj_fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, rj_fld%n_point
        rj_fld%d_fld(inod,ist+1) = fld_IO%d_IO(inod,jst+1)
        rj_fld%d_fld(inod,ist+3) = fld_IO%d_IO(inod,jst+2)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_solenoid_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_vector_from_IO                           &
     &         (fld_IO, rj_fld, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: rj_fld
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = rj_fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, rj_fld%n_point
        rj_fld%d_fld(inod,ist+1) = fld_IO%d_IO(inod,jst+1)
        rj_fld%d_fld(inod,ist+3) = fld_IO%d_IO(inod,jst+2)
        rj_fld%d_fld(inod,ist+2) = fld_IO%d_IO(inod,jst+3)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_vector_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_field_from_IO                            &
     &         (fld_IO, rj_fld, i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: rj_fld
      integer(kind = kint) :: ist, jst, nd, inod
!
!
      ist = rj_fld%istack_component(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1 )
!$omp parallel
      do nd = 1, rj_fld%num_component(i_fld)
!$omp do
        do inod = 1, rj_fld%n_point
          rj_fld%d_fld(inod,ist+nd) = fld_IO%d_IO(inod,jst+nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_each_sph_field_from_IO
!
! -------------------------------------------------------------------
!
      end module copy_rj_phys_data_4_IO
