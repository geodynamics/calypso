!>@file   set_non_slip_sph_mat_bc.f90
!!@brief  module set_non_slip_sph_mat_bc
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for non-slip boundaries
!!
!!@verbatim
!!      subroutine set_non_slip_icb_vt_sph_mat(nri, jmax, kr_in,        &
!!     &          vt_evo_mat)
!!      subroutine set_rgd_icb_vp_poisson3_mat(nri, jmax, kr_in, r_ICB, &
!!     &          fdm2_fix_dr_ICB, poisson_mat)
!!
!!      subroutine set_non_slip_cmb_vt_sph_mat(nri, jmax, kr_out,       &
!!     &          vt_evo_mat)
!!      subroutine set_rgd_cmb_vp_poisson3_mat(nri, jmax, kr_out, r_CMB,&
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
!!@n @param vt_evo_mat(3,nri,jmax)    3-band matrix for evolution of 
!!                                toroidal velocity (poloidal vorticity)
!!@n @param poisson_mat(3,nri,jmax)   3-band matrix for Poisson equation
!
      module set_non_slip_sph_mat_bc
!
      use m_precision
!
      use m_constants
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
      subroutine set_non_slip_icb_vt_sph_mat(nri, jmax, kr_in,          &
     &          vt_evo_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(inout) :: vt_evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!       vt_evo_mat(3,kr_in-1,j) = zero
        vt_evo_mat(2,kr_in,  j) = one
        vt_evo_mat(1,kr_in+1,j) = zero
      end do
!
      end subroutine set_non_slip_icb_vt_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_rgd_icb_vp_poisson3_mat(nri, jmax, kr_in, r_ICB,   &
     &          fdm2_fix_dr_ICB, poisson_mat)
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
!       poisson_mat(3,kr_in-1,j) = -fdm2_fix_dr_ICB(-1,3)
        poisson_mat(2,kr_in,  j) = -fdm2_fix_dr_ICB(0,3)                &
     &                               + g_sph_rj(j,3)*r_ICB(2)
        poisson_mat(1,kr_in+1,j) = -fdm2_fix_dr_ICB(1,3)
      end do
!
      end subroutine set_rgd_icb_vp_poisson3_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_non_slip_cmb_vt_sph_mat(nri, jmax, kr_out,         &
     &          vt_evo_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_out
      real(kind = kreal), intent(inout) :: vt_evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        vt_evo_mat(3,kr_out-1,j) = zero
        vt_evo_mat(2,kr_out,  j) = one
!       vt_evo_mat(1,kr_out+1,j) = zero
      end do
!
      end subroutine set_non_slip_cmb_vt_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_rgd_cmb_vp_poisson3_mat(nri, jmax, kr_out, r_CMB,  &
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
        poisson_mat(3,kr_out-1,j) = -fdm2_fix_dr_CMB(-1,3)
        poisson_mat(2,kr_out,  j) = -fdm2_fix_dr_CMB(0,3)               &
     &                             + g_sph_rj(j,3)*r_CMB(2)
!       poisson_mat(1,kr_out+1,j) = -fdm2_fix_dr_CMB(1,3)
      end do
!
      end subroutine set_rgd_cmb_vp_poisson3_mat
!
! -----------------------------------------------------------------------
!
      end module set_non_slip_sph_mat_bc
