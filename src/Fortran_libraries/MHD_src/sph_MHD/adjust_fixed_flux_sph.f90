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
      use m_spheric_parameter
!
      implicit none
!
       private :: adjust_in_fixed_flux_sph, adjust_out_fixed_flux_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine adjust_icb_fix_h_flux_sph
!
      use m_t_int_parameter
      use m_physical_property
      use m_sph_phys_address
      use m_coef_fdm_fixed_ICB
      use m_boundary_params_sph_MHD
!
!
      call adjust_in_fixed_flux_sph(nidx_rj(2), coef_fdm_fix_dr_ICB_2,  &
     &    nlayer_ICB, sph_bc_T%ICB_flux, coef_d_temp, coef_imp_t, dt,   &
     &    ipol%i_temp)
!
      end subroutine adjust_icb_fix_h_flux_sph
!
! -----------------------------------------------------------------------
!
      subroutine adjust_cmb_fix_h_flux_sph
!
      use m_t_int_parameter
      use m_physical_property
      use m_sph_phys_address
      use m_coef_fdm_fixed_CMB
      use m_boundary_params_sph_MHD
!
!
      call adjust_out_fixed_flux_sph(nidx_rj(2), coef_fdm_fix_dr_CMB_2, &
     &    nlayer_CMB, sph_bc_T%CMB_flux, coef_d_temp, coef_imp_t, dt,   &
     &    ipol%i_temp)
!
      end subroutine adjust_cmb_fix_h_flux_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine adjust_icb_fix_c_flux_sph
!
      use m_t_int_parameter
      use m_physical_property
      use m_sph_phys_address
      use m_coef_fdm_fixed_ICB
      use m_boundary_params_sph_MHD
!
!
      call adjust_in_fixed_flux_sph(nidx_rj(2), coef_fdm_fix_dr_ICB_2,  &
     &    nlayer_ICB, sph_bc_C%ICB_flux, coef_d_light, coef_imp_c, dt,  &
     &    ipol%i_light)
!
      end subroutine adjust_icb_fix_c_flux_sph
!
! -----------------------------------------------------------------------
!
      subroutine adjust_cmb_fix_c_flux_sph
!
      use m_t_int_parameter
      use m_physical_property
      use m_sph_phys_address
      use m_coef_fdm_fixed_CMB
      use m_boundary_params_sph_MHD
!
!
      call adjust_out_fixed_flux_sph(nidx_rj(2), coef_fdm_fix_dr_CMB_2, &
     &    nlayer_CMB, sph_bc_C%CMB_flux, coef_d_light, coef_imp_c, dt,  &
     &    ipol%i_light)
!
      end subroutine adjust_cmb_fix_c_flux_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine adjust_in_fixed_flux_sph(jmax, coef_fdm_fix_dr_in_2,   &
     &          kr_in, flux_IN, coef_diffusion, coef_imp, dt, is_fld)
!
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: coef_diffusion, coef_imp, dt
      real(kind = kreal), intent(in) :: flux_IN(jmax)
      real(kind = kreal), intent(in) :: coef_fdm_fix_dr_in_2(-1:1,3)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, nidx_rj(2)
        inod = j + (kr_in-1) * nidx_rj(2)
!
        d_rj(inod,is_fld) =  d_rj(inod,is_fld)                          &
     &                     + dt * coef_imp * coef_diffusion             &
     &                      * ( coef_fdm_fix_dr_in_2(-1,3)              &
     &                       + two*ar_1d_rj(kr_in,1) ) * flux_IN(j)
!
      end do
!$omp end parallel do
!
      end subroutine adjust_in_fixed_flux_sph
!
! -----------------------------------------------------------------------
!
      subroutine adjust_out_fixed_flux_sph(jmax, coef_fdm_fix_dr_out_2, &
     &          kr_out, flux_OUT, coef_diffusion, coef_imp, dt, is_fld)
!
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: coef_diffusion, coef_imp, dt
      real(kind = kreal), intent(in) :: coef_fdm_fix_dr_out_2(-1:1,3)
      real(kind = kreal), intent(in) :: flux_OUT(jmax)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, nidx_rj(2)
        inod = j + (kr_out-1) * nidx_rj(2)
!
        d_rj(inod,is_fld) = d_rj(inod,is_fld)                           &
     &                     + dt * coef_imp * coef_diffusion             &
     &                      * (coef_fdm_fix_dr_out_2( 1,3)              &
     &                       + two*ar_1d_rj(kr_out,1) ) * flux_OUT(j)
!
      end do
!$omp end parallel do
!
      end subroutine adjust_out_fixed_flux_sph
!
! -----------------------------------------------------------------------
!
      end module adjust_fixed_flux_sph
