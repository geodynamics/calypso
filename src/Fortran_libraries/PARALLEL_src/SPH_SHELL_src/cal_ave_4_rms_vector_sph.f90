!>@file   cal_ave_4_rms_vector_sph.f90
!!@brief  module cal_ave_4_rms_vector_sph
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief  Take surface or volume average for mean square data
!!
!!@verbatim
!!      subroutine cal_one_over_volume                                  &
!!     &         (kg_st, kg_ed, nri, radius_1d_rj_r, avol)
!!
!!      subroutine surf_ave_4_sph_rms_int(l_truncation, nri,            &
!!     &          a_r_1d_rj_r, nri_rms, ntot_rms_rj, kr_for_rms,        &
!!     &          rms_sph_l)
!!      subroutine surf_ave_4_sph_rms(nri, a_r_1d_rj_r,                 &
!!     &          nri_rms, ntot_rms_rj, kr_for_rms, rms_sph)
!!
!!      subroutine vol_ave_4_rms_sph_int(l_truncation, ntot_rms_rj,     &
!!     &          avol, sq_sph_vol_l)
!!      subroutine vol_ave_4_rms_sph(ntot_rms_rj, avol, rms_sph_vol)
!!
!!      subroutine sum_sph_vol_rms_all_modes(ltr, ntot_rms,             &
!!     &          sq_sph_vl, sq_v_sph)
!!      subroutine sum_sph_rms_all_modes(ltr, nri_rms, ntot_rms,        &
!!     &          rms_sph_l, rms_sph)
!!
!!      subroutine pick_axis_sph_vol_pwr(ltr, ntot_rms,                 &
!!     &          rms_sph_vm, sq_v_sph, rms_v_sph_m0, ratio_v_sph_m0)
!!      subroutine pick_axis_sph_power(ltr, nri_rms, ntot_rms,          &
!!     &          rms_sph_m, rms_sph, rms_sph_m0, ratio_sph_m0)
!!@endverbatim
!!@f$ 
!!        1/V \int (\phi_l^m)^2 r^{2} sin \theta dr d\theta d\phi
!!          = [3/(ro^3-ri^3)] \int 1/(2l+1) (\phi_l^m)^2 r^{2} dr
!!@f$ 
!!
!!@n @param  avol    1 / Volume
!
      module cal_ave_4_rms_vector_sph
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_one_over_volume                                    &
     &         (kg_st, kg_ed, nri, radius_1d_rj_r, avol)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
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
      subroutine surf_ave_4_sph_rms_int(l_truncation, nri,              &
     &          a_r_1d_rj_r, nri_rms, ntot_rms_rj, kr_for_rms,          &
     &          rms_sph_l)
!
      integer(kind = kint), intent(in) :: nri, l_truncation
      integer(kind = kint), intent(in) :: nri_rms, ntot_rms_rj
      integer(kind=kint), intent(in) :: kr_for_rms(nri_rms)
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nri)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: rms_sph_l(nri_rms,0:l_truncation,ntot_rms_rj)
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
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine surf_ave_4_sph_rms_int
!
! -----------------------------------------------------------------------
!
      subroutine surf_ave_4_sph_rms(nri, a_r_1d_rj_r,                   &
     &          nri_rms, ntot_rms_rj, kr_for_rms, rms_sph)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nri)
      integer(kind = kint), intent(in) :: nri_rms, ntot_rms_rj
      integer(kind=kint), intent(in) :: kr_for_rms(nri_rms)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: rms_sph(nri_rms,ntot_rms_rj)
!
      integer(kind = kint) :: k, kg, icou
!
!
      if(nri_rms .le. 0) return
!
!$omp parallel do private(k,kg,icou)
      do icou = 1, ntot_rms_rj
        do k = 1, nri_rms
          kg = kr_for_rms(k)
          rms_sph(k,icou) =    rms_sph(k,icou) * a_r_1d_rj_r(kg)**2
        end do
      end do
!$omp end parallel do
!
      end subroutine surf_ave_4_sph_rms
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine vol_ave_4_rms_sph_int(l_truncation, ntot_rms_rj,       &
     &          avol, rms_sph_vol_l)
!
      integer(kind = kint), intent(in) :: l_truncation, ntot_rms_rj
      real(kind = kreal), intent(in) :: avol
!
      real(kind = kreal), intent(inout)                                 &
     &           :: rms_sph_vol_l(0:l_truncation,ntot_rms_rj)
!
      integer(kind = kint) :: lm, icou
!
!
!$omp parallel do private(icou,lm)
      do icou = 1, ntot_rms_rj
        do lm = 0, l_truncation
          rms_sph_vol_l(lm,icou) =  avol * rms_sph_vol_l(lm,icou)
        end do
      end do
!$omp end parallel do
!
      end subroutine vol_ave_4_rms_sph_int
!
! -----------------------------------------------------------------------
!
      subroutine vol_ave_4_rms_sph(ntot_rms_rj, avol, rms_sph_vol)
!
      integer(kind = kint), intent(in) :: ntot_rms_rj
      real(kind = kreal), intent(in) :: avol
!
      real(kind = kreal), intent(inout) :: rms_sph_vol(ntot_rms_rj)
!
      integer(kind = kint) :: icou
!
!
!$omp parallel do private(icou)
      do icou = 1, ntot_rms_rj
        rms_sph_vol(icou) =    avol * rms_sph_vol(icou)
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
! -----------------------------------------------------------------------
!
      subroutine pick_axis_sph_vol_pwr(ltr, ntot_rms,                   &
     &          rms_sph_vm, sq_v_sph, rms_v_sph_m0, ratio_v_sph_m0)
!
      integer(kind = kint), intent(in) :: ltr, ntot_rms
      real(kind = kreal), intent(in) :: rms_sph_vm(0:ltr,ntot_rms)
      real(kind = kreal), intent(in) :: sq_v_sph(ntot_rms)
!
      real(kind = kreal), intent(inout) :: rms_v_sph_m0(ntot_rms)
      real(kind = kreal), intent(inout) :: ratio_v_sph_m0(ntot_rms)
!
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(nd)
      do nd = 1, ntot_rms
        rms_v_sph_m0(nd) = rms_sph_vm(0,nd)
        if(sq_v_sph(nd) .eq. zero) then
          ratio_v_sph_m0(nd) = zero
        else
          ratio_v_sph_m0(nd) = sqrt(rms_v_sph_m0(nd) / sq_v_sph(nd))
        end if
      end do
!$omp end parallel do
!
      end subroutine pick_axis_sph_vol_pwr
!
! -----------------------------------------------------------------------
!
      subroutine pick_axis_sph_power(ltr, nri_rms, ntot_rms,            &
     &          rms_sph_m, rms_sph, rms_sph_m0, ratio_sph_m0)
!
      integer(kind = kint), intent(in) :: ltr, nri_rms
      integer(kind = kint), intent(in) :: ntot_rms
      real(kind = kreal), intent(in)                                    &
     &                   :: rms_sph_m(nri_rms,0:ltr,ntot_rms)
      real(kind = kreal), intent(in) :: rms_sph(nri_rms,ntot_rms)
!
      real(kind = kreal), intent(inout) :: rms_sph_m0(nri_rms,ntot_rms)
      real(kind = kreal), intent(inout)                                 &
     &                   :: ratio_sph_m0(nri_rms,ntot_rms)
!
      integer(kind = kint) :: kr, nd
!
!
!$omp parallel do private(kr,nd)
      do nd = 1, ntot_rms
        rms_sph_m0(1:nri_rms,nd) = rms_sph_m(1:nri_rms,0,nd)
!
        do kr = 1, nri_rms
          if(rms_sph(kr,nd) .eq. zero) then
            ratio_sph_m0(kr,nd) = zero
          else
            ratio_sph_m0(kr,nd)                                         &
     &              = sqrt(rms_sph_m0(kr,nd) / rms_sph(kr,nd))
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine pick_axis_sph_power
!
! -----------------------------------------------------------------------
!
      end module cal_ave_4_rms_vector_sph
