!>@file   adjust_fixed_flux_sph.f90
!!@brief  module adjust_fixed_flux_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Apr., 2009
!
!>@brief Adjust temperature and composition boundary conditions
!!       if perturbation is solved
!!
!!@verbatim
!!      subroutine adjust_icb_fix_h_flux_sph
!!      subroutine adjust_cmb_fix_h_flux_sph
!!
!!      subroutine adjust_icb_fix_c_flux_sph
!!      subroutine adjust_cmb_fix_c_flux_sph
!!@endverbatim
!
      module adjust_fixed_flux_sph
!
      use m_precision
!
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine adjust_in_fixed_flux_sph(jmax, kr_in, r_ICB,           &
     &          fdm2_fix_dr_ICB, flux_IN, coef_diffusion,               &
     &          coef_imp, dt, is_fld)
!
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: coef_diffusion, coef_imp, dt
      real(kind = kreal), intent(in) :: flux_IN(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
!
        d_rj(inod,is_fld) =  d_rj(inod,is_fld)                          &
     &                     + dt * coef_imp * coef_diffusion             &
     &                      * ( fdm2_fix_dr_ICB(-1,3)                   &
     &                       + two*r_ICB(1) ) * flux_IN(j)
!
      end do
!$omp end parallel do
!
      end subroutine adjust_in_fixed_flux_sph
!
! -----------------------------------------------------------------------
!
      subroutine adjust_out_fixed_flux_sph(jmax, kr_out, r_CMB,         &
     &          fdm2_fix_dr_CMB, flux_OUT, coef_diffusion,              &
     &          coef_imp,  dt, is_fld)
!
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: coef_diffusion, coef_imp, dt
      real(kind = kreal), intent(in) :: flux_OUT(jmax)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
!
        d_rj(inod,is_fld) = d_rj(inod,is_fld)                           &
     &                     + dt * coef_imp * coef_diffusion             &
     &                      * (fdm2_fix_dr_CMB( 1,3)                    &
     &                       + two*r_CMB(1) ) * flux_OUT(j)
!
      end do
!$omp end parallel do
!
      end subroutine adjust_out_fixed_flux_sph
!
! -----------------------------------------------------------------------
!
      end module adjust_fixed_flux_sph
