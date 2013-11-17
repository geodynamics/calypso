!>@file   set_sph_mom_mat_bc.f90
!!@brief  module set_sph_mom_mat_bc
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for fixed velocity at boundaries
!!
!!@verbatim
!!      subroutine set_icb_wt_sph_evo_mat(nri, jmax, kr_in, r_ICB,      &
!!     &          fdm2_fix_dr_ICB, wt_evo_mat)
!!      subroutine set_icb_vp_sph_poisson_mat(nri, jmax, kr_in,         &
!!     &          poisson_mat)
!!      subroutine set_icb_p_sph_poisson_mat(nri, jmax, kr_in, r_ICB,   &
!!     &          fdm2_fix_dr_ICB, poisson_mat)
!!
!!      subroutine set_cmb_wt_sph_evo_mat(nri, jmax, kr_out, r_CMB,     &
!!     &          fdm2_fix_dr_CMB, wt_evo_mat)
!!      subroutine set_cmb_vp_sph_poisson_mat(nri, jmax, kr_out,        &
!!     &          poisson_mat)
!!      subroutine set_cmb_p_sph_poisson_mat(nri, jmax, kr_out, r_CMB,  &
!!     &          fdm2_fix_dr_CMB, poisson_mat)
!!@endverbatim
!!
!!@n @param nri          Number of radial points
!!@n @param jmax         Number of local spherical harmonics mode
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out       Radial ID for outer boundary
!!@n @param r_ICB(0:2)   Radius at ICB
!!@n @param r_CMB(0:2)   Radius at CMB
!!@n @param fdm2_fix_dr_ICB(-1:1,3)
!!         Matrix to evaluate field at ICB with fiexed radial derivative
!!@n @param fdm2_fix_dr_CMB(-1:1,3)
!!         Matrix to evaluate field at CMB with fiexed radial derivative
!!
!!@n @param wt_evo_mat(3,nri,jmax)    3-band matrix for evolution of 
!!                                    toroidal vorticity
!!@n @param poisson_mat(3,nri,jmax)   3-band matrix for Poisson equation
!
      module set_sph_mom_mat_bc
!
      use m_precision
!
      use m_constants
      use m_t_int_parameter
      use m_physical_property
      use m_schmidt_poly_on_rtm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_icb_wt_sph_evo_mat(nri, jmax, kr_in, r_ICB,        &
     &          fdm2_fix_dr_ICB, wt_evo_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      real(kind = kreal), intent(inout) :: wt_evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!        wt_evo_mat(2,kr_in,  j)                                        &
!     &                   = one + coef_imp_v*dt*coef_d_velo             &
!     &                      * g_sph_rj(j,3)*r_ICB(2)
!        wt_evo_mat(1,kr_in+1,j) =  zero
!
        wt_evo_mat(2,kr_in,  j)                                        &
     &                   = one + coef_imp_v*dt*coef_d_velo             &
     &                           * ( -fdm2_fix_dr_ICB( 0,3)            &
     &                      + g_sph_rj(j,3)*r_ICB(2))
        wt_evo_mat(1,kr_in+1,j)                                        &
     &                   =     - coef_imp_v*dt*coef_d_velo             &
     &                          *    fdm2_fix_dr_ICB( 1,3)
      end do
!
      end subroutine set_icb_wt_sph_evo_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_icb_vp_sph_poisson_mat(nri, jmax, kr_in,           &
     &          poisson_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(inout) :: poisson_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        poisson_mat(2,kr_in,  j) = one
        poisson_mat(1,kr_in+1,j) = zero
      end do
!
      end subroutine set_icb_vp_sph_poisson_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_icb_p_sph_poisson_mat(nri, jmax, kr_in,            &
     &          r_ICB, fdm2_fix_dr_ICB, poisson_mat)
!
      use m_fdm_coefs
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      real(kind = kreal), intent(inout) :: poisson_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        poisson_mat(2,kr_in,  j) = coef_press * (fdm2_fix_dr_ICB( 0,3)  &
     &                    - g_sph_rj(j,3)*r_ICB(2)                      &
     &                     + two * r_ICB(1) * fdm2_fix_dr_ICB( 0,2) )
        poisson_mat(1,kr_in+1,j) = coef_press * (fdm2_fix_dr_ICB( 1,3)  &
     &                     + two * r_ICB(1) * fdm2_fix_dr_ICB( 1,2) )
      end do
!
      end subroutine set_icb_p_sph_poisson_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_cmb_wt_sph_evo_mat(nri, jmax, kr_out, r_CMB,       &
     &          fdm2_fix_dr_CMB, wt_evo_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_out
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      real(kind = kreal), intent(inout) :: wt_evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!        wt_evo_mat(3,kr_out-1,j) = zero
!        wt_evo_mat(2,kr_out,  j)                                       &
!     &                   = one + coef_imp_v*dt*coef_d_velo             &
!     &                        * g_sph_rj(j,3)*r_CMB(2)
!
        wt_evo_mat(3,kr_out-1,j)                                        &
     &                   =     - coef_imp_v*dt*coef_d_velo              &
     &                          *    fdm2_fix_dr_CMB(-1,3)
        wt_evo_mat(2,kr_out,  j)                                        &
     &                   = one + coef_imp_v*dt*coef_d_velo              &
     &                          * ( -fdm2_fix_dr_CMB( 0,3)              &
     &                             + g_sph_rj(j,3)*r_CMB(2))
      end do
!
      end subroutine set_cmb_wt_sph_evo_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_cmb_vp_sph_poisson_mat(nri, jmax, kr_out,          &
     &          poisson_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_out
      real(kind = kreal), intent(inout) :: poisson_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        poisson_mat(3,kr_out-1,j) = zero
        poisson_mat(2,kr_out,  j) = one
      end do
!
      end subroutine set_cmb_vp_sph_poisson_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_cmb_p_sph_poisson_mat(nri, jmax, kr_out, r_CMB,    &
     &          fdm2_fix_dr_CMB, poisson_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_out
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      real(kind = kreal), intent(inout) :: poisson_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        poisson_mat(3,kr_out-1,j) = coef_press * (fdm2_fix_dr_CMB(-1,3) &
     &                   + two* r_CMB(1) * fdm2_fix_dr_CMB(-1,2) )
        poisson_mat(2,kr_out,  j) = coef_press * (fdm2_fix_dr_CMB( 0,3) &
     &                           - g_sph_rj(j,3)*r_CMB(2)     &
     &                   + two * r_CMB(1) * fdm2_fix_dr_CMB( 0,2) )
      end do
!
      end subroutine set_cmb_p_sph_poisson_mat
!
! -----------------------------------------------------------------------
!
      end module set_sph_mom_mat_bc
