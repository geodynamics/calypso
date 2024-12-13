!>@file   set_sph_scalar_matrix_ICB.f90
!!@brief  module set_sph_scalar_matrix_ICB
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Construct matrix for scalar fields at boundaries
!!
!!@verbatim
!!      subroutine set_fix_fld_icb_poisson_mat(nri, jmax, kr_in,        &
!!     &          evo_mat)
!!      subroutine add_fix_flux_icb_poisson_mat(nri, jmax, g_sph_rj,    &
!!     &          kr_in, r_ICB, fdm2_fix_dr_ICB, coef_p, evo_mat)
!!      subroutine add_icb_scalar_poisson_mat(nri, jmax, g_sph_rj,      &
!!     &          kr_in, r_ICB, fdm2_fix_dr_ICB, coef_p, p_mat)
!!
!!      subroutine set_fix_fld_icb_poisson00_mat(nri, kr_in, evo_mat)
!!      subroutine add_fix_flux_icb_poisson00_mat                       &
!!     &         (nri, kr_in, fdm2_fix_dr_ICB, coef_p, evo_mat)
!!        integer(kind = kint), intent(in) :: nri, kr_in
!!        real(kind = kreal), intent(inout) :: evo_mat(3,0:kr_in+1)
!!
!!      subroutine set_ins_magne_icb_rmat_sph(nri, jmax, g_sph_rj,      &
!!     &          kr_in, r_ICB, fdm2_fix_dr_ICB, coef_dbt, bs_evo_mat)
!!        real(kind = kreal), intent(inout) :: bs_evo_mat(3,nri,jmax)
!!@endverbatim
!!
!!@n @param nri     Number of radial points
!!@n @param jmax    Number of spherical harmonics modes
!!@n @param j0       Local harmonics mode address for l = m = 0
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param r_ICB(0:2)   Radius at ICB
!!@n @param coef_d       Coefficient of diffusiotn term
!!@n @param coef_dbt     Coefficient for implicit diffusion term
!!@n @param fdm2_fix_dr_ICB(-1:1,3)
!!         Matrix to evaluate field at ICB with fixed radial derivative
!!
!!@n @param evo_mat(3,nri,jmax)  Band matrix for time evolution
!!@n @param bs_evo_mat(3,nri,jmax)    3-band matrix for evolution of 
!!                                    poloidal magnetic field
!
      module set_sph_scalar_matrix_ICB
!
      use m_precision
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
      subroutine add_fix_flux_icb_poisson_mat(nri, jmax, g_sph_rj,      &
     &          kr_in, r_ICB, fdm2_fix_dr_ICB, coef_p, evo_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
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
      subroutine add_icb_scalar_poisson_mat(nri, jmax, g_sph_rj,        &
     &          kr_in, r_ICB, fdm2_fix_dr_ICB, coef_p, p_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
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
        p_mat(2,kr_in,  j) = p_mat(2,kr_in,  j)                         &
     &                      - coef_p * (fdm2_fix_dr_ICB( 0,3)           &
     &                       + two * r_ICB(1) * fdm2_fix_dr_ICB( 0,2)   &
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
      subroutine set_fix_fld_icb_poisson00_mat(nri, kr_in, evo_mat)
!
      integer(kind = kint), intent(in) :: nri, kr_in
      real(kind = kreal), intent(inout) :: evo_mat(3,0:nri)
!
!     evo_mat(3,kr_in-1,j) = zero
      evo_mat(2,kr_in  ) = one
      evo_mat(1,kr_in+1) = zero
!
      end subroutine set_fix_fld_icb_poisson00_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_fix_flux_icb_poisson00_mat                         &
     &         (nri, kr_in, fdm2_fix_dr_ICB, coef_p, evo_mat)
!
      integer(kind = kint), intent(in) :: nri, kr_in
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      real(kind = kreal), intent(inout) :: evo_mat(3,0:nri)
!
!       evo_mat(3,kr_in-1) = evo_mat(3,kr_in-1)                         &
!     &                          - coef_p * fdm2_fix_dr_ICB(-1,3)
        evo_mat(2,kr_in  ) = evo_mat(2,kr_in  )                         &
     &                          - coef_p * (fdm2_fix_dr_ICB( 0,3))
        evo_mat(1,kr_in+1) = evo_mat(1,kr_in+1)                         &
     &                          - coef_p * fdm2_fix_dr_ICB( 1,3)
!
      end subroutine add_fix_flux_icb_poisson00_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ins_magne_icb_rmat_sph(nri, jmax, g_sph_rj,        &
     &          kr_in, r_ICB, fdm2_fix_dr_ICB, coef_dbt, bs_evo_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
      real(kind = kreal), intent(in) :: coef_dbt
!
      real(kind = kreal), intent(inout) :: bs_evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        bs_evo_mat(2,kr_in,  j) = bs_evo_mat(2,kr_in,  j)               &
     &                           - coef_dbt * ( fdm2_fix_dr_ICB( 0,3)   &
     &                             - g_sph_rj(j,3)*r_ICB(2)             &
     &                             + (g_sph_rj(j,1)+one) * r_ICB(1)     &
     &                              * fdm2_fix_dr_ICB(-1,3) )
        bs_evo_mat(1,kr_in+1,j) = bs_evo_mat(1,kr_in+1,j)               &
     &                           - coef_dbt * fdm2_fix_dr_ICB( 1,3)
      end do
!
      end subroutine set_ins_magne_icb_rmat_sph
!
! -----------------------------------------------------------------------
!
      end module set_sph_scalar_matrix_ICB
