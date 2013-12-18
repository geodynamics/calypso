!copy_all_spec_4_sph_trans.f90
!      module copy_all_spec_4_sph_trans
!
!        programmed by H.Matsui on Jan., 2008
!
!      subroutine set_all_scalar_spec_to_sph_t
!      subroutine set_all_scalar_spec_from_sph_t
!
!      subroutine set_all_vec_spec_to_sph_t
!      subroutine set_all_vec_spec_from_sph_t
!
!      subroutine set_all_tensor_spec_to_sph_t
!      subroutine set_all_tensor_spec_from_sph_t
!
      module copy_all_spec_4_sph_trans
!
      use m_precision
!
      use m_work_4_sph_trans
      use m_sph_spectr_data
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_all_scalar_spec_to_sph_t
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint) :: i, j, j0, is_fld, ir_fld
!
!
      do j = 1, num_scalar_rtp
        j0 = j + istart_scalar_rtp - 1
        do i = 1, num_phys_rj
          if ( phys_name_rtp(j0) .eq. phys_name_rj(i) ) then
            is_fld = istack_phys_comp_rj(i-1) + 1
            ir_fld = istack_phys_comp_rtp(j0-1) + 1
!$omp parallel
            call copy_scalar_spec_to_trans                              &
     &         (ntot_phys_rtp, is_fld, ir_fld)
!$omp end parallel
            exit
          end if
        end do
      end do
!
      end subroutine set_all_scalar_spec_to_sph_t
!
! -------------------------------------------------------------------
!
      subroutine set_all_scalar_spec_from_sph_t
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint) :: i, j, j0, is_fld, ir_fld
!
!
      do j = 1, num_scalar_rtp
        j0 = j + istart_scalar_rtp - 1
        do i = 1, num_phys_rj
          if ( phys_name_rtp(j0) .eq. phys_name_rj(i) ) then
            is_fld = istack_phys_comp_rj(i-1) + 1
            ir_fld = istack_phys_comp_rtp(j0-1) + 1
!$omp parallel
            call copy_scalar_spec_from_trans                            &
     &         (ntot_phys_rtp, is_fld, ir_fld)
!$omp end parallel
            exit
          end if
        end do
      end do
!
      end subroutine set_all_scalar_spec_from_sph_t
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_all_vec_spec_to_sph_t
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint) :: i, j, j0, is_fld, ir_fld
!
!
      do j = 1, num_vector_rtp
        j0 = j + istart_vector_rtp - 1
        is_fld = istack_phys_comp_rtp(j-1) + 1
        do i = 1, num_phys_rj
          if ( phys_name_rtp(j0) .eq. phys_name_rj(i) ) then
            is_fld = istack_phys_comp_rj(i-1) + 1
            ir_fld = istack_phys_comp_rtp(j0-1) + 1
!$omp parallel
            call copy_vec_spec_to_trans                                 &
     &         (ntot_phys_rtp, is_fld, ir_fld)
!$omp end parallel
            exit
          end if
        end do
      end do
!
      end subroutine set_all_vec_spec_to_sph_t
!
! -------------------------------------------------------------------
!
      subroutine set_all_vec_spec_from_sph_t
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint) :: i, j, j0, is_fld, ir_fld
!
!
      do j = 1, num_vector_rtp
        j0 = j + istart_vector_rtp - 1
        do i = 1, num_phys_rj
          if ( phys_name_rtp(j0) .eq. phys_name_rj(i) ) then
            is_fld = istack_phys_comp_rj(i-1) + 1
            ir_fld = istack_phys_comp_rtp(j0-1) + 1
!$omp parallel
            call copy_vec_spec_from_trans                               &
     &         (ntot_phys_rtp, is_fld, ir_fld)
!$omp end parallel
            exit
          end if
        end do
      end do
!
      end subroutine set_all_vec_spec_from_sph_t
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_all_tensor_spec_to_sph_t
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint) :: i, j, j0, is_fld, ir_fld
!
!
      do j = 1, num_tensor_rtp
        j0 = j + istart_tensor_rtp - 1
        do i = 1, num_phys_rj
          if ( phys_name_rtp(j0) .eq. phys_name_rj(i) ) then
            is_fld = istack_phys_comp_rj(i-1) + 1
            ir_fld = istack_phys_comp_rtp(j0-1) + 1
!$omp parallel
            call copy_tensor_spec_to_trans                              &
     &         (ntot_phys_rtp, is_fld, ir_fld)
!$omp end parallel
            exit
          end if
        end do
      end do
!
      end subroutine set_all_tensor_spec_to_sph_t
!
! -------------------------------------------------------------------
!
      subroutine set_all_tensor_spec_from_sph_t
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint) :: i, j, j0, is_fld, ir_fld
!
!
      do j = 1, num_tensor_rtp
        j0 = j + istart_tensor_rtp - 1
        do i = 1, num_phys_rj
          if ( phys_name_rtp(j0) .eq. phys_name_rj(i) ) then
            is_fld = istack_phys_comp_rj(i-1) + 1
            ir_fld = istack_phys_comp_rtp(j0-1) + 1
!$omp parallel
            call copy_tensor_spec_from_trans                            &
     &         (ntot_phys_rtp, is_fld, ir_fld)
!$omp end parallel
            exit
          end if
        end do
      end do
!
      end subroutine set_all_tensor_spec_from_sph_t
!
!-----------------------------------------------------------------------
!
      end module copy_all_spec_4_sph_trans
