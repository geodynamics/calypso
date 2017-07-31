!> @file  volume_average_4_sph.f90
!!      module volume_average_4_sph
!!
!! @author  H. Matsui
!! @date Programmed in Feb. 2008
!
!> @brief Output mean square of spectr data
!!
!!@verbatim
!!      subroutine cal_volume_average_sph(sph_rj, rj_fld, pwr)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!
!!      subroutine cal_ave_scalar_sph_spectr(icomp, jcomp,              &
!!     &          n_point, nidx_rj, idx_rj_degree_zero, inod_rj_center, &
!!     &          ntot_phys_rj, d_rj, radius_1d_rj_r,                   &
!!     &          nri_ave, ntot_rms_rj, ave_sph)
!
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
!
      implicit none
!
!
      private :: cal_sphere_average_sph, averaging_4_sph_ave_int
      private :: cal_ave_vector_sph_spectr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_volume_average_sph(sph_rj, rj_fld, pwr)
!
      use m_phys_constants
!
      use t_rms_4_sph_spectr
      use t_spheric_rj_data
      use t_phys_data
!
      use cal_ave_4_rms_vector_sph
      use radial_int_for_sph_spec
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: i
!
!
      if(sph_rj%idx_rj_degree_zero .eq. izero) return
      call cal_sphere_average_sph(sph_rj, rj_fld, pwr)
!
      do i = 1, pwr%num_vol_spectr
        call radial_integration                                         &
     &       (pwr%v_spectr(i)%kr_inside, pwr%v_spectr(i)%kr_outside,    &
     &        sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                 &
     &        pwr%ntot_comp_sq, pwr%shl_ave, pwr%v_spectr(i)%v_ave)
        call averaging_4_sph_ave_int(pwr%ntot_comp_sq,                  &
     &        pwr%v_spectr(i)%avol, pwr%v_spectr(i)%v_ave)
      end do
!
      end subroutine cal_volume_average_sph
!
! -----------------------------------------------------------------------
!
      subroutine cal_sphere_average_sph(sph_rj, rj_fld, pwr)
!
      use m_phys_constants
!
      use t_rms_4_sph_spectr
      use t_spheric_rj_data
      use t_phys_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: i_fld, j_fld, icomp_st, jcomp_st
!
!
      pwr%shl_ave = 0.0d0
!
      do j_fld = 1, pwr%num_fld_sq
        i_fld =    pwr%id_field(j_fld)
        icomp_st = rj_fld%istack_component(i_fld-1) + 1
        jcomp_st = pwr%istack_comp_sq(j_fld-1) +  1
        if (rj_fld%num_component(i_fld) .eq. n_scalar) then
          call cal_ave_scalar_sph_spectr                                &
     &       (icomp_st, jcomp_st, rj_fld%n_point, sph_rj%nidx_rj,       &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        rj_fld%ntot_phys, rj_fld%d_fld, sph_rj%radius_1d_rj_r,    &
     &        pwr%nri_ave, pwr%ntot_comp_sq, pwr%shl_ave)
        else if (rj_fld%num_component(i_fld) .eq. n_vector) then
          call cal_ave_vector_sph_spectr                                &
     &       (icomp_st, jcomp_st, rj_fld%n_point, sph_rj%nidx_rj,       &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        rj_fld%ntot_phys, rj_fld%d_fld, sph_rj%radius_1d_rj_r,    &
     &        pwr%nri_ave, pwr%ntot_comp_sq, pwr%shl_ave)
        end if
      end do
!
      end subroutine cal_sphere_average_sph
!
! -----------------------------------------------------------------------
!
      subroutine averaging_4_sph_ave_int                                &
     &         (ntot_rms_rj, avol, ave_sph_vol)
!
      integer(kind = kint), intent(in) :: ntot_rms_rj
      real(kind = kreal), intent(in) :: avol
!
      real(kind = kreal), intent(inout) :: ave_sph_vol(ntot_rms_rj)
!
      integer(kind = kint) :: icou
!
!
!$omp parallel do private(icou)
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
      subroutine cal_ave_scalar_sph_spectr(icomp, jcomp,                &
     &          n_point, nidx_rj, idx_rj_degree_zero, inod_rj_center,   &
     &          ntot_phys_rj, d_rj, radius_1d_rj_r,                     &
     &          nri_ave, ntot_rms_rj, ave_sph)
!
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: inod_rj_center
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
!
      integer(kind = kint), intent(in) :: icomp, jcomp
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint), intent(in) :: nri_ave, ntot_rms_rj
!
      real(kind = kreal), intent(inout)                                 &
     &           :: ave_sph(0:nri_ave,ntot_rms_rj)
!
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
      subroutine cal_ave_vector_sph_spectr(icomp, jcomp,                &
     &          n_point, nidx_rj, idx_rj_degree_zero, inod_rj_center,   &
     &          ntot_phys_rj, d_rj, radius_1d_rj_r,                     &
     &          nri_ave, ntot_rms_rj, ave_sph)
!
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: inod_rj_center
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
!
      integer(kind = kint), intent(in) :: icomp, jcomp
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint), intent(in) :: nri_ave, ntot_rms_rj
!
      real(kind = kreal), intent(inout)                                 &
     &           :: ave_sph(0:nri_ave,ntot_rms_rj)
!
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
