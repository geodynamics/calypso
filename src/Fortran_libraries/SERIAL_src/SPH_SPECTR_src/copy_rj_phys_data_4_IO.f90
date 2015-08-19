!> @file  copy_rj_phys_data_4_IO.f90
!!      module copy_rj_phys_data_4_IO
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2007
!
!> @brief Copy between field data and IO data
!!
!!@verbatim
!!      subroutine copy_rj_all_phys_name_to_IO(fld_IO)
!!      subroutine copy_rj_all_phys_data_to_IO(fld_IO)
!!      subroutine copy_rj_viz_phys_name_to_IO(fld_IO)
!!      subroutine copy_rj_viz_phys_data_to_IO(fld_IO)
!!
!!      subroutine copy_rj_phys_name_from_IO(fld_IO)
!!      subroutine copy_rj_phys_data_from_IO(fld_IO)
!!      subroutine set_rj_phys_data_from_IO(fld_IO)
!!
!!      subroutine copy_each_sph_solenoid_to_IO(i_fld, j_IO, fld_IO)
!!      subroutine copy_each_sph_vector_to_IO(i_fld, j_IO, fld_IO)
!!      subroutine copy_each_sph_field_to_IO(i_fld, j_IO, fld_IO)
!!
!!      subroutine copy_each_sph_solenoid_from_IO(i_fld, j_IO, fld_IO)
!!      subroutine copy_each_sph_vector_from_IO(i_fld, j_IO, fld_IO)
!!      subroutine copy_each_sph_field_from_IO(i_fld, j_IO, fld_IO)
!!@endverbatim
!
      module copy_rj_phys_data_4_IO
!
      use m_precision
!
      use m_phys_constants
      use m_sph_spectr_data
      use m_spheric_parameter
      use t_field_data_IO
!
      implicit none
!
      private :: copy_rj_phys_name_to_IO, copy_rj_phys_data_to_IO
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_all_phys_name_to_IO(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call copy_rj_phys_name_to_IO(num_phys_rj, fld_IO)
!
      end subroutine copy_rj_all_phys_name_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_viz_phys_name_to_IO(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call copy_rj_phys_name_to_IO(num_phys_rj_vis, fld_IO)
!
      end subroutine copy_rj_viz_phys_name_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_all_phys_data_to_IO(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call copy_rj_phys_data_to_IO(num_phys_rj, fld_IO)
!
      end subroutine copy_rj_all_phys_data_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_viz_phys_data_to_IO(fld_IO)
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      call copy_rj_phys_data_to_IO(num_phys_rj_vis, fld_IO)
!
      end subroutine copy_rj_viz_phys_data_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_name_to_IO(num_fld, fld_IO)
!
      integer(kind = kint), intent(in) :: num_fld
      type(field_IO), intent(inout) :: fld_IO
!
!
      fld_IO%nnod_IO =   nnod_rj
      fld_IO%num_field_IO =  num_fld
      fld_IO%ntot_comp_IO = ntot_phys_rj
!
      call alloc_phys_name_IO(fld_IO)
!
      fld_IO%num_comp_IO(1:num_fld) =    num_phys_comp_rj(1:num_fld)
      fld_IO%istack_comp_IO(0:num_fld) = istack_phys_comp_rj(0:num_fld)
      fld_IO%fld_name(1:num_fld) =   phys_name_rj(1:num_fld)
!
      end subroutine copy_rj_phys_name_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_data_to_IO(num_fld, fld_IO)
!
      integer(kind = kint), intent(in) :: num_fld
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint) :: i_fld
!
!
      do i_fld = 1, num_fld
        if (num_phys_comp_rj(i_fld) .eq. n_vector) then
          call copy_each_sph_vector_to_IO(i_fld, i_fld, fld_IO)
        else
          call copy_each_sph_field_to_IO(i_fld, i_fld, fld_IO)
        end if
      end do
!
      end subroutine copy_rj_phys_data_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_name_from_IO(fld_IO)
!
      type(field_IO), intent(in) :: fld_IO
!
!
      num_phys_rj =  fld_IO%num_field_IO
      ntot_phys_rj = fld_IO%ntot_comp_IO
!
      call allocate_phys_rj_name
      call allocate_phys_rj_data
!
      num_phys_comp_rj(1:num_phys_rj)                                   &
     &      = fld_IO%num_comp_IO(1:num_phys_rj)
      istack_phys_comp_rj(0:num_phys_rj)                                &
     &      = fld_IO%istack_comp_IO(0:num_phys_rj)
      phys_name_rj(1:num_phys_rj) = fld_IO%fld_name(1:num_phys_rj)
      iflag_monitor_rj(1:num_phys_rj) = 1
!
      end subroutine copy_rj_phys_name_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_data_from_IO(fld_IO)
!
      type(field_IO), intent(in) :: fld_IO
      integer(kind = kint) :: i_fld
!
!
      do i_fld = 1, num_phys_rj
        if (num_phys_comp_rj(i_fld) .eq. 3) then
          call copy_each_sph_vector_from_IO(i_fld, i_fld, fld_IO)
        else
          call copy_each_sph_field_from_IO(i_fld, i_fld, fld_IO)
        end if
      end do
!
      end subroutine copy_rj_phys_data_from_IO
!
! -------------------------------------------------------------------
!
      subroutine set_rj_phys_data_from_IO(fld_IO)
!
      type(field_IO), intent(in) :: fld_IO
      integer(kind = kint) :: i_fld, j_IO
!
      do i_fld = 1, num_phys_rj
        do j_IO = 1, fld_IO%num_field_IO
          if (phys_name_rj(i_fld) .eq. fld_IO%fld_name(j_IO)) then
            if (fld_IO%num_comp_IO(j_IO) .eq. 3) then
              call copy_each_sph_vector_from_IO(i_fld, j_IO, fld_IO)
            else if (fld_IO%num_comp_IO(j_IO) .eq. 2) then
              call copy_each_sph_solenoid_from_IO(i_fld, j_IO, fld_IO)
            else
              call copy_each_sph_field_from_IO(i_fld, j_IO, fld_IO)
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
      subroutine copy_each_sph_solenoid_to_IO(i_fld, j_IO, fld_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = istack_phys_comp_rj(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        fld_IO%d_IO(inod,jst+1) = d_rj(inod,ist+1)
        fld_IO%d_IO(inod,jst+2) = d_rj(inod,ist+3)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_solenoid_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_vector_to_IO(i_fld, j_IO, fld_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = istack_phys_comp_rj(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        fld_IO%d_IO(inod,jst+1) = d_rj(inod,ist+1)
        fld_IO%d_IO(inod,jst+2) = d_rj(inod,ist+3)
        fld_IO%d_IO(inod,jst+3) = d_rj(inod,ist+2)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_vector_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_field_to_IO(i_fld, j_IO, fld_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      type(field_IO), intent(inout) :: fld_IO
      integer(kind = kint) :: ist, jst, nd, inod
!
!
      ist = istack_phys_comp_rj(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1 )
!$omp parallel
      do nd = 1, num_phys_comp_rj(i_fld)
!$omp do
        do inod = 1, nnod_rj
          fld_IO%d_IO(inod,jst+nd) = d_rj(inod,ist+nd)
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
      subroutine copy_each_sph_solenoid_from_IO(i_fld, j_IO, fld_IO)
!
      type(field_IO), intent(in) :: fld_IO
      integer(kind = kint), intent(in) :: i_fld, j_IO
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = istack_phys_comp_rj(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ist+1) = fld_IO%d_IO(inod,jst+1)
        d_rj(inod,ist+3) = fld_IO%d_IO(inod,jst+2)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_solenoid_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_vector_from_IO(i_fld, j_IO, fld_IO)
!
      type(field_IO), intent(in) :: fld_IO
      integer(kind = kint), intent(in) :: i_fld, j_IO
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = istack_phys_comp_rj(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ist+1) = fld_IO%d_IO(inod,jst+1)
        d_rj(inod,ist+3) = fld_IO%d_IO(inod,jst+2)
        d_rj(inod,ist+2) = fld_IO%d_IO(inod,jst+3)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_vector_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_field_from_IO(i_fld, j_IO, fld_IO)
!
      type(field_IO), intent(in) :: fld_IO
      integer(kind = kint), intent(in) :: i_fld, j_IO
      integer(kind = kint) :: ist, jst, nd, inod
!
!
      ist = istack_phys_comp_rj(i_fld-1)
      jst = fld_IO%istack_comp_IO(j_IO-1 )
!$omp parallel
      do nd = 1, num_phys_comp_rj(i_fld)
!$omp do
        do inod = 1, nnod_rj
          d_rj(inod,ist+nd) = fld_IO%d_IO(inod,jst+nd)
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
