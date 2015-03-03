!>@file   cal_ave_4_rms_vector_sph.f90
!!@brief  module cal_ave_4_rms_vector_sph
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief  Take surface or volume average for mean square data
!!
!!@verbatim
!!      subroutine cal_one_over_volume(kg_st, kg_ed, avol)
!!      subroutine r_int_sph_rms_data(kg_st, kg_ed, avol)
!!@endverbatim
!!@f$ 
!!        1/V \int (\phi_l^m)^2 r^{2} sin \theta dr d\theta d\phi
!!          = [3/(ro^3-ri^3)] \int 1/(2l+1) (\phi_l^m)^2 r^{2} dr
!!@f$ 
!!
!!@verbatim
!!      subroutine surf_ave_4_sph_rms_int
!!@endverbatim
!!
!!@n @param  avol    1 / Volume
!
      module cal_ave_4_rms_vector_sph
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
!
      implicit none
!
      private :: surf_ave_4_sph_rms_int, vol_ave_4_rms_sph
!
!-----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_one_over_volume(kg_st, kg_ed, avol)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: kg_st, kg_ed
      real(kind = kreal), intent(inout) :: avol
!
!
      if(kg_st .eq. 0) then
        avol = three / (radius_1d_rj_r(kg_ed)**3)
      else
        avol = three / (radius_1d_rj_r(kg_ed)**3                        &
     &                - radius_1d_rj_r(kg_st)**3 )
      end if
!
      end subroutine cal_one_over_volume
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine r_int_sph_rms_data(kg_st, kg_ed, avol)
!
      use calypso_mpi
      use m_spheric_parameter
      use m_phys_constants
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_sph_phys_address
      use cal_rms_by_sph_spectr
      use radial_int_for_sph_spec
      use sum_sph_rms_data
!
      integer(kind = kint), intent(in) :: kg_st, kg_ed
      real(kind = kreal), intent(in) :: avol
!
      integer(kind = kint) :: num
!
!
      if(my_rank .gt. 0) return
!
      num = (l_truncation + 1) * ntot_rms_rj
      call radial_integration(kg_st, kg_ed, nidx_rj(1),                 &
     &    radius_1d_rj_r, num, rms_sph_l(0,0,1),  rms_sph_vol_l(0,1))
      call radial_integration(kg_st, kg_ed, nidx_rj(1),                 &
     &    radius_1d_rj_r, num, rms_sph_m(0,0,1),   rms_sph_vol_m(0,1))
      call radial_integration(kg_st, kg_ed, nidx_rj(1),                 &
     &    radius_1d_rj_r, num, rms_sph_lm(0,0,1),  rms_sph_vol_lm(0,1))
!
      call radial_integration(kg_st, kg_ed, nidx_rj(1),                 &
     &    radius_1d_rj_r, ntot_rms_rj, rms_sph(0,1), rms_sph_vol(1))
!
      call surf_ave_4_sph_rms_int
      call vol_ave_4_rms_sph(avol)
!
      end subroutine r_int_sph_rms_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine surf_ave_4_sph_rms_int
!
      integer(kind = kint) :: lm, k, icou
!
!
!$omp parallel do private(k,lm,icou)
      do icou = 1, ntot_rms_rj
        do k = 1, nidx_rj(1)
          do lm = 0, l_truncation
            rms_sph_l(k,lm,icou) =  rms_sph_l(k,lm,icou)                &
     &                              * a_r_1d_rj_r(k)**2
            rms_sph_m(k,lm,icou) =  rms_sph_m(k,lm,icou)                &
     &                              * a_r_1d_rj_r(k)**2
            rms_sph_lm(k,lm,icou) = rms_sph_lm(k,lm,icou)               &
     &                              * a_r_1d_rj_r(k)**2
          end do
!
          rms_sph(k,icou) = rms_sph(k,icou) * a_r_1d_rj_r(k)**2
        end do
      end do
!$omp end parallel do
!
      end subroutine surf_ave_4_sph_rms_int
!
! -----------------------------------------------------------------------
!
      subroutine vol_ave_4_rms_sph(avol)
!
      real(kind = kreal), intent(in) :: avol
      integer(kind = kint) :: lm, icou
!
!
!$omp parallel do private(icou)
      do icou = 1, ntot_rms_rj
        rms_sph_vol(icou) = avol * rms_sph_vol(icou)
      end do
!$omp end parallel do
!
!$omp parallel do private(icou,lm)
      do icou = 1, ntot_rms_rj
        do lm = 0, l_truncation
          rms_sph_vol_l(lm,icou) =  avol * rms_sph_vol_l(lm,icou)
          rms_sph_vol_m(lm,icou) =  avol * rms_sph_vol_m(lm,icou)
          rms_sph_vol_lm(lm,icou) = avol * rms_sph_vol_lm(lm,icou)
        end do
      end do
!$omp end parallel do
!
      end subroutine vol_ave_4_rms_sph
!
! -----------------------------------------------------------------------
!
      end module cal_ave_4_rms_vector_sph
