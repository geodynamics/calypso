!> @file  volume_average_4_sph.f90
!!      module volume_average_4_sph
!!
!! @author  H. Matsui
!! @date Programmed in Feb. 2008
!
!> @brief Output mean square of spectr data
!!
!!@verbatim
!!      subroutine cal_volume_average_sph(kg_st, kg_ed, avol)
!!@endverbatim
!!
!!@n @param istep         time step number
!!@n @param time          time
!
      module volume_average_4_sph
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
!
      implicit none
!
!
!>      File prefix for volume average file
      character(len = kchara) :: fhead_ave_vol =    'sph_ave_volume'
!
      private :: cal_sphere_average_sph, averaging_4_sph_ave_int
      private :: cal_ave_vector_sph_spectr, cal_ave_scalar_sph_spectr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_volume_average_sph(kg_st, kg_ed, avol)
!
      use m_phys_constants
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_sph_phys_address
      use cal_ave_4_rms_vector_sph
      use radial_int_for_sph_spec
!
      integer(kind = kint), intent(in) :: kg_st, kg_ed
      real(kind = kreal), intent(in) :: avol
!
!
      if(idx_rj_degree_zero .gt. izero) then
        call cal_sphere_average_sph
!
        call radial_integration(kg_st, kg_ed, nidx_rj(1),               &
     &      radius_1d_rj_r, ntot_rms_rj, ave_sph(0,1),  ave_sph_vol(1))
        call averaging_4_sph_ave_int(avol)
      end if
!
      end subroutine cal_volume_average_sph
!
! -----------------------------------------------------------------------
!
      subroutine cal_sphere_average_sph
!
      use m_phys_constants
      use m_sph_spectr_data
      use m_sph_phys_address
      use cal_rms_by_sph_spectr
      use m_rms_4_sph_spectr
!
      integer(kind = kint) :: i_fld, j_fld, icomp_st, jcomp_st
!
!
!
      ave_sph = 0.0d0
!
      do j_fld = 1, num_rms_rj
        i_fld = ifield_rms_rj(j_fld)
        icomp_st = istack_phys_comp_rj(i_fld-1) + 1
        jcomp_st = istack_rms_comp_rj(j_fld-1) +  1
        if (num_phys_comp_rj(i_fld) .eq. n_scalar) then
          call cal_ave_scalar_sph_spectr(icomp_st, jcomp_st)
        else if (num_phys_comp_rj(i_fld) .eq. n_vector) then
          call cal_ave_vector_sph_spectr(icomp_st, jcomp_st)
        end if
      end do
!
      end subroutine cal_sphere_average_sph
!
! -----------------------------------------------------------------------
!
      subroutine averaging_4_sph_ave_int(avol)
!
      use m_rms_4_sph_spectr
!
      real(kind = kreal), intent(in) :: avol
      integer(kind = kint) :: k, icou
!
!
!$omp parallel do private(k,icou)
      do icou = 1, ntot_rms_rj
        ave_sph_vol(icou) = avol * ave_sph_vol(icou)
      end do
!$omp end parallel do
!
      end subroutine averaging_4_sph_ave_int
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_ave_scalar_sph_spectr(icomp, jcomp)
!
      use m_constants
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: icomp, jcomp
      integer(kind = kint) :: k, inod
!
!
      do k = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (k-1) * nidx_rj(2)
        ave_sph(k,jcomp) = d_rj(inod,icomp) * radius_1d_rj_r(k)**2
      end do
!
      if(inod_rj_center .eq. 0) return
      ave_sph(0,jcomp) = d_rj(inod_rj_center,icomp)
!
      end subroutine cal_ave_scalar_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine cal_ave_vector_sph_spectr(icomp, jcomp)
!
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
!
      integer(kind = kint), intent(in) :: icomp, jcomp
      integer(kind = kint) :: k, inod
!
!
      do k = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (k-1) * nidx_rj(2)
        ave_sph(k,jcomp  ) = d_rj(inod,icomp) * radius_1d_rj_r(k)**2
        ave_sph(k,jcomp+1) = zero
        ave_sph(k,jcomp+2) = ave_sph(k,jcomp  )
      end do
!
      if(inod_rj_center .eq. 0) return
      ave_sph(0,jcomp  ) = d_rj(inod_rj_center,icomp)
      ave_sph(0,jcomp+1) = zero
      ave_sph(0,jcomp+2) = ave_sph(0,jcomp  )
!
      end subroutine cal_ave_vector_sph_spectr
!
! -----------------------------------------------------------------------
!
      end module volume_average_4_sph
