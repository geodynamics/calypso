!>@file   set_free_slip_sph_mat_bc.f90
!!@brief  module set_free_slip_sph_mat_bc
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for free-slip boundaries
!!
!!@verbatim
!!      subroutine free_slip_icb_vt_sph_mat
!!      subroutine free_icb_vp_poisson3_mat
!!
!!      subroutine free_slip_cmb_vt_sph_mat
!!      subroutine free_cmb_vp_poisson3_mat
!!@endverbatim
!!
!!@n @param nri          Number of radial points
!!@n @param jmax         Number of local spherical harmonics mode
!!
!!@n @param nri          Number of radial points
!!@n @param jmax         Number of local spherical harmonics mode
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out       Radial ID for outer boundary
!!@n @param r_ICB(0:2)   Radius at ICB
!!@n @param r_CMB(0:2)   Radius at CMB
!!
!!@n @param fdm2_free_vp_ICB(0:1,3)
!!         Matrix to evaluate poloidal velocity
!!         with free slip boundary at ICB
!!@n @param fdm2_free_vt_ICB(0:1,3)
!!         Matrix to evaluate toroidal velocity
!!         with free slip boundary at ICB
!!@n @param fdm2_free_vp_CMB(-1:0,3)
!!         Matrix to evaluate poloidal velocity
!!         with free slip boundary at CMB
!!@n @param fdm2_free_vt_CMB(-1:0,3)
!!         Matrix to evaluate toroidal velocity
!!         with free slip boundary at CMB
!!
!!@n @param wt_evo_mat(3,nri,jmax)    3-band matrix for evolution of 
!!                                    toroidal vorticity
!!@n @param poisson_mat(3,nri,jmax)   3-band matrix for Poisson equation
!
      module set_free_slip_sph_mat_bc
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
      subroutine free_slip_icb_vt_sph_mat(nri, jmax, kr_in, r_ICB,      &
     &          fdm2_free_vt_ICB, vt_evo_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_free_vt_ICB(0:1,3)
!
      real(kind = kreal), intent(inout) :: vt_evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!       vt_evo_mat(3,kr_in-1,j) = zero
        vt_evo_mat(2,kr_in,  j) = one + coef_imp_v*dt*coef_d_velo       &
     &                             *(-fdm2_free_vt_ICB(0,3)             &
     &                            + g_sph_rj(j,3)*r_ICB(2) )
        vt_evo_mat(1,kr_in+1,j) =     - fdm2_free_vt_ICB(1,3)           &
     &                            * coef_imp_v*dt*coef_d_velo
      end do
!
      end subroutine free_slip_icb_vt_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine free_icb_vp_poisson3_mat(nri, jmax, kr_in, r_ICB,      &
     &          fdm2_free_vp_ICB, poisson_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_free_vp_ICB(0:1,3)
!
      real(kind = kreal), intent(inout) :: poisson_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!       poisson_mat(3,kr_in-1,j) = -fdm2_free_vp_ICB(-1,3)
        poisson_mat(2,kr_in,  j) = -fdm2_free_vp_ICB(0,3)               &
     &                           + g_sph_rj(j,3)*r_ICB(2)
        poisson_mat(1,kr_in+1,j) = -fdm2_free_vp_ICB(1,3)
      end do
!
      end subroutine free_icb_vp_poisson3_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine free_slip_cmb_vt_sph_mat(nri, jmax, kr_out, r_CMB,     &
     &          fdm2_free_vt_CMB, vt_evo_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_out
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in)  :: fdm2_free_vt_CMB(-1:0,3)
!
      real(kind = kreal), intent(inout) :: vt_evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        vt_evo_mat(3,kr_out-1,j) =     - coef_imp_v*dt*coef_d_velo      &
     &                                * fdm2_free_vt_CMB(-1,3)
        vt_evo_mat(2,kr_out,  j) = one + coef_imp_v*dt*coef_d_velo      &
     &                             *(-fdm2_free_vt_CMB(0,3)             &
     &                            + g_sph_rj(j,3)*r_CMB(2))
!       vt_evo_mat(1,kr_out+1,j) =     - fdm2_free_vt_CMB(1,3)
      end do
!
      end subroutine free_slip_cmb_vt_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine free_cmb_vp_poisson3_mat(nri, jmax, kr_out, r_CMB,     &
     &          fdm2_free_vp_CMB, poisson_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_out
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in)  :: fdm2_free_vp_CMB(-1:0,3)
!
      real(kind = kreal), intent(inout) :: poisson_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        poisson_mat(3,kr_out-1,j) = -fdm2_free_vp_CMB(-1,3)
        poisson_mat(2,kr_out,  j) = -fdm2_free_vp_CMB(0,3)              &
     &                             + g_sph_rj(j,3)*r_CMB(2)
      end do
!
      end subroutine free_cmb_vp_poisson3_mat
!
! -----------------------------------------------------------------------
!
      end module set_free_slip_sph_mat_bc
