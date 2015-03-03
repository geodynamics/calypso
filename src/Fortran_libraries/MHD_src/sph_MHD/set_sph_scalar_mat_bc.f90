!>@file   set_sph_scalar_mat_bc.f90
!!@brief  module set_sph_scalar_mat_bc
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Construct matrix for scalar fields at boundaries
!!
!!@verbatim
!!      subroutine set_fix_fld_icb_poisson_mat(nri, jmax, kr_in,        &
!!     &          evo_mat)
!!      subroutine add_fix_flux_icb_poisson_mat(nri, jmax, kr_in,       &
!!     &          r_ICB, fdm2_fix_dr_ICB, coef_p, evo_mat)
!!      subroutine add_icb_scalar_poisson_mat(nri, jmax, kr_in,         &
!!     &          r_ICB, fdm2_fix_dr_ICB, coef_p, p_mat)
!!
!!      subroutine set_fix_fld_cmb_poisson_mat(nri, jmax, kr_out,       &
!!     &          evo_mat)
!!      subroutine add_fix_flux_cmb_poisson_mat(nri, jmax, kr_out,      &
!!     &          r_CMB, fdm2_fix_dr_CMB, coef_p, evo_mat)
!!      subroutine add_cmb_scalar_poisson_mat(nri, jmax, kr_out, r_CMB, &
!!     &          fdm2_fix_dr_CMB, coef_p, p_mat)
!!@endverbatim
!!
!!@n @param nri     Number of radial points
!!@n @param jmax    Number of spherical harmonics modes
!!@n @param j0       Local harmonics mode address for l = m = 0
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out       Radial ID for outer boundary
!!@n @param r_ICB(0:2)   Radius at ICB
!!@n @param r_CMB(0:2)   Radius at CMB
!!@n @param coef_imp   Coefficient for contribution of implicit term
!!@n @param coef_d     Coefficient of diffusiotn term
!!@n @param fdm2_fix_dr_ICB(-1:1,3)
!!         Matrix to evaluate field at ICB with fixed radial derivative
!!@n @param fdm2_fix_dr_CMB(-1:1,3)
!!         Matrix to evaluate field at CMB with fixed radial derivative
!!
!!@n @param evo_mat(3,nri,jmax)  Band matrix for time evolution
!
      module set_sph_scalar_mat_bc
!
      use m_precision
!
      use m_constants
      use m_t_int_parameter
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_radial_matrices_sph
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_fix_fld_icb_poisson_mat(nri, jmax, kr_in,          &
     &          evo_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri, kr_in
      real(kind = kreal), intent(inout) :: evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!       evo_mat(3,kr_in-1,j) = zero
        evo_mat(2,kr_in,  j) = one
        evo_mat(1,kr_in+1,j) = zero
      end do
!
      end subroutine set_fix_fld_icb_poisson_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_fix_flux_icb_poisson_mat(nri, jmax, kr_in,         &
     &          r_ICB, fdm2_fix_dr_ICB, coef_p, evo_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri, kr_in
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      real(kind = kreal), intent(inout) :: evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!       evo_mat(3,kr_in-1,j) = evo_mat(3,kr_in-1,j)                     &
!     &                          - coef_p * fdm2_fix_dr_ICB(-1,3)
        evo_mat(2,kr_in,  j) = evo_mat(2,kr_in,  j)                     &
     &                          - coef_p * (fdm2_fix_dr_ICB( 0,3)       &
     &                           - g_sph_rj(j,3)*r_ICB(2))
        evo_mat(1,kr_in+1,j) = evo_mat(1,kr_in+1,j)                     &
     &                          - coef_p * fdm2_fix_dr_ICB( 1,3)
      end do
!
      end subroutine add_fix_flux_icb_poisson_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_icb_scalar_poisson_mat(nri, jmax, kr_in,           &
     &          r_ICB, fdm2_fix_dr_ICB, coef_p, p_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
      real(kind = kreal), intent(in) :: coef_p
!
      real(kind = kreal), intent(inout) :: p_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        p_mat(2,kr_in,  j) = p_mat(2,kr_in,  j)                        &
     &                      - coef_p * (fdm2_fix_dr_ICB( 0,3)          &
     &                       + two * r_ICB(1) * fdm2_fix_dr_ICB( 0,2)  &
     &                       - g_sph_rj(j,3)*r_ICB(2))
        p_mat(1,kr_in+1,j) =  p_mat(1,kr_in+1,j)                        &
     &                       - coef_p * (fdm2_fix_dr_ICB( 1,3)          &
     &                       + two * r_ICB(1) * fdm2_fix_dr_ICB( 1,2) )
      end do
!
      end subroutine add_icb_scalar_poisson_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_fix_fld_cmb_poisson_mat(nri, jmax, kr_out,         &
     &          evo_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri, kr_out
      real(kind = kreal), intent(inout) :: evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        evo_mat(3,kr_out-1,j) = zero
        evo_mat(2,kr_out,  j) = one
!       evo_mat(1,kr_out+1,j) = zero
      end do
!
      end subroutine set_fix_fld_cmb_poisson_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_fix_flux_cmb_poisson_mat(nri, jmax, kr_out,        &
     &          r_CMB, fdm2_fix_dr_CMB, coef_p, evo_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri, kr_out
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      real(kind = kreal), intent(inout) :: evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        evo_mat(3,kr_out-1,j) = evo_mat(3,kr_out-1,j)                   &
     &                         - coef_p *  fdm2_fix_dr_CMB(-1,3)
        evo_mat(2,kr_out,  j) = evo_mat(2,kr_out,  j)                   &
     &                         - coef_p * (fdm2_fix_dr_CMB( 0,3)        &
     &                          - g_sph_rj(j,3)*r_CMB(2))
!       evo_mat(1,kr_out+1,j) = evo_mat(1,kr_out+1,j)                   &
!                              - coef_p * fdm2_fix_dr_CMB(1,3)
      end do
!
      end subroutine add_fix_flux_cmb_poisson_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_cmb_scalar_poisson_mat(nri, jmax, kr_out, r_CMB,   &
     &          fdm2_fix_dr_CMB, coef_p, p_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_out
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
      real(kind = kreal), intent(in) :: coef_p
!
      real(kind = kreal), intent(inout) :: p_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        p_mat(3,kr_out-1,j) = p_mat(3,kr_out-1,j)                       &
     &                       - coef_p * (fdm2_fix_dr_CMB(-1,3)          &
     &                        + two*r_CMB(1) * fdm2_fix_dr_CMB(-1,2))
        p_mat(2,kr_out,  j) = p_mat(2,kr_out,  j)                       &
     &                       - coef_p * (fdm2_fix_dr_CMB( 0,3)          &
     &                        + two*r_CMB(1) * fdm2_fix_dr_CMB( 0,2)    &
     &                        - g_sph_rj(j,3)*r_CMB(2))
      end do
!
      end subroutine add_cmb_scalar_poisson_mat
!
! -----------------------------------------------------------------------
!
      end module set_sph_scalar_mat_bc
