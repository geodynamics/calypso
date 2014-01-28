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
      integer(kind = kint) :: i, j, j0, nd
!
!
      do j = 1, num_scalar_rtp
        j0 = j + istart_scalar_rtp - 1
        do i = 1, num_phys_rj
          if ( phys_name_rtp(j0) .eq. phys_name_rj(i) ) then
            nd = istack_phys_comp_rj(i-1) + 1
!$omp parallel
            call copy_scalar_spec_to_trans(num_scalar_rtp, nd, j)
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
      integer(kind = kint) :: i, j, j0, nd
!
!
      do j = 1, num_scalar_rtp
        j0 = j + istart_scalar_rtp - 1
        do i = 1, num_phys_rj
          if ( phys_name_rtp(j0) .eq. phys_name_rj(i) ) then
            nd = istack_phys_comp_rj(i-1) + 1
!$omp parallel
            call copy_scalar_spec_from_trans(num_scalar_rtp, nd, j)
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
      integer(kind = kint) :: i, j, j0, nd
!
!
      do j = 1, num_vector_rtp
        j0 = j + istart_vector_rtp - 1
        do i = 1, num_phys_rj
          if ( phys_name_rtp(j0) .eq. phys_name_rj(i) ) then
            nd = istack_phys_comp_rj(i-1) + 1
!$omp parallel
            call copy_vec_spec_to_trans(3*num_vector_rtp, nd, 3*j-2)
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
      integer(kind = kint) :: i, j, j0, nd
!
!
      do j = 1, num_vector_rtp
        j0 = j + istart_vector_rtp - 1
        do i = 1, num_phys_rj
          if ( phys_name_rtp(j0) .eq. phys_name_rj(i) ) then
            nd = istack_phys_comp_rj(i-1) + 1
!$omp parallel
            call copy_vec_spec_from_trans(3*num_vector_rtp, nd, 3*j-2)
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
      integer(kind = kint) :: i, j, j0, nd
!
!
      do j = 1, num_tensor_rtp
        j0 = j + istart_tensor_rtp - 1
        do i = 1, num_phys_rj
          if ( phys_name_rtp(j0) .eq. phys_name_rj(i) ) then
            nd = istack_phys_comp_rj(i-1) + 1
!$omp parallel
            call copy_tsr_spec_to_trans(6*num_tensor_rtp, nd, 6*j-5)
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
      integer(kind = kint) :: i, j, j0, nd
!
!
      do j = 1, num_tensor_rtp
        j0 = j + istart_tensor_rtp - 1
        do i = 1, num_phys_rj
          if ( phys_name_rtp(j0) .eq. phys_name_rj(i) ) then
            nd = istack_phys_comp_rj(i-1) + 1
!$omp parallel
            call copy_tsr_spec_from_trans(6*num_tensor_rtp, nd, 6*j-5)
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
