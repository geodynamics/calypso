!>@file   cal_ave_4_rms_vector_sph.f90
!!@brief  module cal_ave_4_rms_vector_sph
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief  Take surface or volume average for mean square data
!!
!!@verbatim
!!      subroutine surf_ave_4_each_sph_rms
!!      subroutine vol_ave_4_each_sph_rms(avol)
!!      subroutine vol_ave_4_rms_sph(avol)
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
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine surf_ave_4_each_sph_rms
!
      integer(kind = kint) :: j, kg, idx, icou
!
!
!$omp parallel do private(j,kg,idx,icou)
      do kg = 1, nidx_global_rj(1)
        do j = 1, nidx_rj(2)
          do icou = 1, ntot_rms_rj
            idx = j + (kg-1) * nidx_rj(2)
            rms_sph_dat(icou,idx) =  rms_sph_dat(icou,idx)              &
     &                              * a_r_1d_rj_r(kg)**2
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine surf_ave_4_each_sph_rms
!
! -----------------------------------------------------------------------
!
      subroutine vol_ave_4_each_sph_rms(avol)
!
      real(kind = kreal), intent(in) :: avol
      integer(kind = kint) :: j, icou
!
!
!$omp parallel do private(j,icou)
      do j = 1, nidx_rj(2)
        do icou = 1, ntot_rms_rj
          rms_sph_vol_dat(icou,j) = avol * rms_sph_vol_dat(icou,j)
        end do
      end do
!$omp end parallel do
!
      end subroutine vol_ave_4_each_sph_rms
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine surf_ave_4_sph_rms_int
!
      integer(kind = kint) :: lm, kg, icou
!
!
!$omp parallel do private(kg,lm,icou)
      do kg = 1, nidx_global_rj(1)
        do lm = 0, l_truncation
          do icou = 1, ntot_rms_rj
            rms_sph_l(icou,lm,kg) =  rms_sph_l(icou,lm,kg)              &
     &                              * a_r_1d_rj_r(kg)**2
            rms_sph_m(icou,lm,kg) =  rms_sph_m(icou,lm,kg)              &
     &                              * a_r_1d_rj_r(kg)**2
            rms_sph_lm(icou,lm,kg) = rms_sph_lm(icou,lm,kg)             &
     &                              * a_r_1d_rj_r(kg)**2
          end do
        end do
!
        do icou = 1, ntot_rms_rj
          ave_sph(icou,kg) = ave_sph(icou,kg) * a_r_1d_rj_r(kg)**2
          rms_sph(icou,kg) = rms_sph(icou,kg) * a_r_1d_rj_r(kg)**2
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
        ave_sph_vol(icou) = avol * ave_sph_vol(icou)
      end do
!$omp end parallel do
!
!$omp parallel do private(icou)
      do lm = 0, l_truncation
        do icou = 1, ntot_rms_rj
          rms_sph_vol_l(icou,lm) =  avol * rms_sph_vol_l(icou,lm)
          rms_sph_vol_m(icou,lm) =  avol * rms_sph_vol_m(icou,lm)
          rms_sph_vol_lm(icou,lm) = avol * rms_sph_vol_lm(icou,lm)
        end do
      end do
!$omp end parallel do
!
      end subroutine vol_ave_4_rms_sph
!
! -----------------------------------------------------------------------
!
      end module cal_ave_4_rms_vector_sph
