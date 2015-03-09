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
!!
!!      subroutine surf_ave_4_sph_rms_int
!!      subroutine vol_ave_4_rms_sph(avol)
!!
!!      subroutine sum_sph_vol_rms_all_modes(ltr, ntot_rms,             &
!!     &          rms_sph_vl, rms_v_sph)
!!      subroutine sum_sph_rms_all_modes(ltr, nri_rms, ntot_rms,        &
!!     &          rms_sph_l, rms_sph)
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
!
      implicit none
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
      subroutine surf_ave_4_sph_rms_int
!
      use m_rms_4_sph_spectr
!
      integer(kind = kint) :: lm, k, kg, icou
!
!
      if(nri_rms .le. 0) return
!
!$omp parallel do private(k,kg,lm,icou)
      do icou = 1, ntot_rms_rj
        do k = 1, nri_rms
          kg = kr_for_rms(k)
          do lm = 0, l_truncation
            rms_sph_l(k,lm,icou) =  rms_sph_l(k,lm,icou)                &
     &                              * a_r_1d_rj_r(kg)**2
            rms_sph_m(k,lm,icou) =  rms_sph_m(k,lm,icou)                &
     &                              * a_r_1d_rj_r(kg)**2
            rms_sph_lm(k,lm,icou) = rms_sph_lm(k,lm,icou)               &
     &                              * a_r_1d_rj_r(kg)**2
          end do
!
          rms_sph(k,icou) = rms_sph(k,icou) * a_r_1d_rj_r(kg)**2
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
      use m_rms_4_sph_spectr
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
! -----------------------------------------------------------------------
!
      subroutine sum_sph_vol_rms_all_modes(ltr, ntot_rms,               &
     &          rms_sph_vl, rms_v_sph)
!
      integer(kind = kint), intent(in) :: ltr, ntot_rms
      real(kind = kreal), intent(in) :: rms_sph_vl(0:ltr,ntot_rms)
!
      real(kind = kreal), intent(inout) :: rms_v_sph(ntot_rms)
!
      integer(kind = kint) :: lm, nd
!
!
!$omp parallel do private(lm,nd)
      do nd = 1, ntot_rms
        rms_v_sph(nd) = rms_sph_vl(0,nd)
!
        do lm = 1, ltr
          rms_v_sph(nd) = rms_v_sph(nd) + rms_sph_vl(lm,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_sph_vol_rms_all_modes
!
! -----------------------------------------------------------------------
!
      subroutine sum_sph_rms_all_modes(ltr, nri_rms, ntot_rms,          &
     &          rms_sph_l, rms_sph)
!
      integer(kind = kint), intent(in) :: ltr, nri_rms
      integer(kind = kint), intent(in) :: ntot_rms
      real(kind = kreal), intent(in)                                    &
     &                   :: rms_sph_l(nri_rms,0:ltr,ntot_rms)
!
      real(kind = kreal), intent(inout) :: rms_sph(nri_rms,ntot_rms)
!
      integer(kind = kint) :: lm, nd
!
!
!$omp parallel do private(lm,nd)
      do nd = 1, ntot_rms
        rms_sph(1:nri_rms,nd) = rms_sph_l(1:nri_rms,0,nd)
!
        do lm = 1, ltr
          rms_sph(1:nri_rms,nd) = rms_sph(1:nri_rms,nd)                 &
     &                           + rms_sph_l(1:nri_rms,lm,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_sph_rms_all_modes
!
! -----------------------------------------------------------------------
!
      end module cal_ave_4_rms_vector_sph
