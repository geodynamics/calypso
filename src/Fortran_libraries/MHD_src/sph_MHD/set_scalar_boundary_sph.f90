!>@file   set_scalar_boundary_sph.f90
!!@brief  module set_scalar_boundary_sph
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!
!>@brief  Evaluate scalar fields at boundaries
!!@n     Adjust temperature and composition boundary conditions
!!       if perturbation is solved
!!
!!
!!@verbatim
!!      subroutine set_fixed_scalar_sph(jmax, kr_bc_st, kr_bc_ed,       &
!!     &          is_fld, fixed_bc)
!!
!!      subroutine adjust_in_fixed_flux_sph(jmax, kr_in, r_ICB,         &
!!     &          fdm2_fix_dr_ICB, flux_ICB, coef_d,                    &
!!     &          coef_imp, dt, is_fld)
!!      subroutine adjust_out_fixed_flux_sph(jmax, kr_out, r_CMB,       &
!!     &          fdm2_fix_dr_CMB, flux_CMB, coef_d,                    &
!!     &          coef_imp,  dt, is_fld)
!!@endverbatim
!!
!!@param  jmax        Number of modes for local spectrum
!!@param  kr_bc_st    Start radial address to set fixed field
!!@param  kr_bc_ed    End radial address to set fixed field
!!@param  fixed_bc(jmax)   Boundary condition spectrum
!!
!!@param kr_in        Radial ID for inner boundary
!!@param kr_out       Radial ID for outer boundary
!!@param r_ICB(0:2)   Radius at ICB
!!@param r_CMB(0:2)   Radius at CMB
!!@param fdm2_fix_dr_ICB(-1:1,3)
!!         Matrix to evaluate field at ICB with fiexed radial derivative
!!@param fdm2_fix_dr_CMB(-1:1,3)
!!         Matrix to evaluate field at CMB with fiexed radial derivative
!!
!!@param flux_ICB(jamx)  Spectrum of fixed flux at ICB
!!@param flux_CMB(jamx)  Spectrum of fixed flux at CMB
!!@param coef_imp   Coefficient for contribution of implicit term
!!@param coef_d     Coefficient for magnetic diffusion
!!@param is_fld     Input field address for d_rj
!!
      module set_scalar_boundary_sph
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_scalar_sph(jmax, kr_bc_st, kr_bc_ed, is_fld, &
     &          fixed_bc)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: jmax, kr_bc_st, kr_bc_ed
      real(kind = kreal), intent(in) :: fixed_bc(jmax)
      integer(kind = kint) :: j, inod, k
!
!
!$omp parallel do private (k,j,inod)
      do k = kr_bc_st, kr_bc_ed
        do j = 1, jmax
          inod = j + (k-1) * jmax
          d_rj(inod,is_fld) = fixed_bc(j)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_fixed_scalar_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine adjust_in_fixed_flux_sph(jmax, kr_in, r_ICB,           &
     &          fdm2_fix_dr_ICB, flux_ICB, coef_d,                      &
     &          coef_imp, dt, is_fld)
!
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: coef_d, coef_imp, dt
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
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
     &                     + dt * coef_imp * coef_d                     &
     &                      * ( fdm2_fix_dr_ICB(-1,3)                   &
     &                       + two*r_ICB(1) ) * flux_ICB(j)
      end do
!$omp end parallel do
!
      end subroutine adjust_in_fixed_flux_sph
!
! -----------------------------------------------------------------------
!
      subroutine adjust_out_fixed_flux_sph(jmax, kr_out, r_CMB,         &
     &          fdm2_fix_dr_CMB, flux_CMB, coef_d,                      &
     &          coef_imp,  dt, is_fld)
!
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: coef_d, coef_imp, dt
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
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
     &                     + dt * coef_imp * coef_d                     &
     &                      * (fdm2_fix_dr_CMB( 1,3)                    &
     &                       + two*r_CMB(1) ) * flux_CMB(j)
      end do
!$omp end parallel do
!
      end subroutine adjust_out_fixed_flux_sph
!
! -----------------------------------------------------------------------
!
      end module set_scalar_boundary_sph
