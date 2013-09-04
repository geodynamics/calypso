!
!      module copy_rj_phys_data_4_IO
!
!      Written by H. Matsui on Oct., 2007
!
!      subroutine copy_rj_phys_name_to_IO
!      subroutine copy_rj_all_phys_data_to_IO
!      subroutine copy_rj_viz_phys_data_to_IO
!
!      subroutine copy_rj_viz_phys_name_to_IO
!
!      subroutine copy_rj_phys_name_from_IO
!      subroutine copy_rj_phys_data_from_IO
!      subroutine set_rj_phys_data_from_IO
!
!      subroutine copy_each_sph_solenoid_to_IO(i_fld, j_IO)
!      subroutine copy_each_sph_vector_to_IO(i_fld, j_IO)
!      subroutine copy_each_sph_field_to_IO(i_fld, j_IO)
!
!      subroutine copy_each_sph_solenoid_from_IO(i_fld, j_IO)
!      subroutine copy_each_sph_vector_from_IO(i_fld, j_IO)
!      subroutine copy_each_sph_field_from_IO(i_fld, j_IO)
!
      module copy_rj_phys_data_4_IO
!
      use m_precision
!
      use m_phys_constants
      use m_sph_spectr_data
      use m_spheric_parameter
      use m_field_data_IO
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
      subroutine copy_rj_all_phys_name_to_IO
!
!
      call copy_rj_phys_name_to_IO(num_phys_rj)
!
      end subroutine copy_rj_all_phys_name_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_viz_phys_name_to_IO
!
!
      call copy_rj_phys_name_to_IO(num_phys_rj_vis)
!
      end subroutine copy_rj_viz_phys_name_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_all_phys_data_to_IO
!
!
      call copy_rj_phys_data_to_IO(num_phys_rj)
!
      end subroutine copy_rj_all_phys_data_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_viz_phys_data_to_IO
!
!
      call copy_rj_phys_data_to_IO(num_phys_rj_vis)
!
      end subroutine copy_rj_viz_phys_data_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_name_to_IO(num_fld)
!
      integer(kind = kint), intent(in) :: num_fld
!
!
      numgrid_phys_IO =   nnod_rj
      num_phys_data_IO =  num_fld
      ntot_phys_data_IO = ntot_phys_rj
!
      call allocate_phys_data_name_IO
!
      num_phys_comp_IO(1:num_fld) =    num_phys_comp_rj(1:num_fld)
      istack_phys_comp_IO(0:num_fld) = istack_phys_comp_rj(0:num_fld)
      phys_data_name_IO(1:num_fld) =   phys_name_rj(1:num_fld)
!
      end subroutine copy_rj_phys_name_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_data_to_IO(num_fld)
!
      integer(kind = kint), intent(in) :: num_fld
      integer(kind = kint) :: i_fld
!
!
      do i_fld = 1, num_fld
        if (num_phys_comp_rj(i_fld) .eq. n_vector) then
          call copy_each_sph_vector_to_IO(i_fld, i_fld)
        else
          call copy_each_sph_field_to_IO(i_fld, i_fld)
        end if
      end do
!
      end subroutine copy_rj_phys_data_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_name_from_IO
!
      num_phys_rj =  num_phys_data_IO
      ntot_phys_rj = ntot_phys_data_IO
!
      call allocate_phys_rj_name
      call allocate_phys_rj_data
!
      num_phys_comp_rj(1:num_phys_rj)                                   &
     &      = num_phys_comp_IO(1:num_phys_rj)
      istack_phys_comp_rj(0:num_phys_rj)                                &
     &      = istack_phys_comp_IO(0:num_phys_rj)
      phys_name_rj(1:num_phys_rj) = phys_data_name_IO(1:num_phys_rj)
      iflag_monitor_rj(1:num_phys_rj) = 1
!
      end subroutine copy_rj_phys_name_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_phys_data_from_IO
!
      integer(kind = kint) :: i_fld
!
!
      do i_fld = 1, num_phys_rj
        if (num_phys_comp_rj(i_fld) .eq. 3) then
          call copy_each_sph_vector_from_IO(i_fld, i_fld)
        else
          call copy_each_sph_field_from_IO(i_fld, i_fld)
        end if
      end do
!
      end subroutine copy_rj_phys_data_from_IO
!
! -------------------------------------------------------------------
!
      subroutine set_rj_phys_data_from_IO
!
      integer(kind = kint) :: i_fld, j_IO
!
      do i_fld = 1, num_phys_rj
        do j_IO = 1, num_phys_data_IO
          if (phys_name_rj(i_fld) .eq. phys_data_name_IO(j_IO)) then
            if (num_phys_comp_IO(j_IO) .eq. 3) then
              call copy_each_sph_vector_from_IO(i_fld, j_IO)
            else if (num_phys_comp_IO(j_IO) .eq. 2) then
              call copy_each_sph_solenoid_from_IO(i_fld, j_IO)
            else
              call copy_each_sph_field_from_IO(i_fld, j_IO)
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
      subroutine copy_each_sph_solenoid_to_IO(i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = istack_phys_comp_rj(i_fld-1)
      jst = istack_phys_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        phys_data_IO(inod,jst+1) = d_rj(inod,ist+1)
        phys_data_IO(inod,jst+2) = d_rj(inod,ist+3)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_solenoid_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_vector_to_IO(i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = istack_phys_comp_rj(i_fld-1)
      jst = istack_phys_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        phys_data_IO(inod,jst+1) = d_rj(inod,ist+1)
        phys_data_IO(inod,jst+2) = d_rj(inod,ist+3)
        phys_data_IO(inod,jst+3) = d_rj(inod,ist+2)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_vector_to_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_field_to_IO(i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      integer(kind = kint) :: ist, jst, nd, inod
!
!
      ist = istack_phys_comp_rj(i_fld-1)
      jst = istack_phys_comp_IO(j_IO-1 )
!$omp parallel
      do nd = 1, num_phys_comp_rj(i_fld)
!$omp do
        do inod = 1, nnod_rj
          phys_data_IO(inod,jst+nd) = d_rj(inod,ist+nd)
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
      subroutine copy_each_sph_solenoid_from_IO(i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = istack_phys_comp_rj(i_fld-1)
      jst = istack_phys_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ist+1) = phys_data_IO(inod,jst+1)
        d_rj(inod,ist+3) = phys_data_IO(inod,jst+2)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_solenoid_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_vector_from_IO(i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      integer(kind = kint) :: ist, jst, inod
!
!
      ist = istack_phys_comp_rj(i_fld-1)
      jst = istack_phys_comp_IO(j_IO-1)
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ist+1) = phys_data_IO(inod,jst+1)
        d_rj(inod,ist+3) = phys_data_IO(inod,jst+2)
        d_rj(inod,ist+2) = phys_data_IO(inod,jst+3)
      end do
!$omp end parallel do
!
      end subroutine copy_each_sph_vector_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_each_sph_field_from_IO(i_fld, j_IO)
!
      integer(kind = kint), intent(in) :: i_fld, j_IO
      integer(kind = kint) :: ist, jst, nd, inod
!
!
      ist = istack_phys_comp_rj(i_fld-1)
      jst = istack_phys_comp_IO(j_IO-1 )
!$omp parallel
      do nd = 1, num_phys_comp_rj(i_fld)
!$omp do
        do inod = 1, nnod_rj
          d_rj(inod,ist+nd) = phys_data_IO(inod,jst+nd)
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
