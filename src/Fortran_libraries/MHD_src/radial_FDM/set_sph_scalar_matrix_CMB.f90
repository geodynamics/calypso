!>@file   set_sph_scalar_matrix_CMB.f90
!!@brief  module set_sph_scalar_matrix_CMB
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Construct matrix for scalar fields at boundaries
!!
!!@verbatim
!!      subroutine set_fix_fld_cmb_poisson_mat(nri, jmax, kr_out,       &
!!     &          evo_mat)
!!      subroutine add_fix_flux_cmb_poisson_mat(nri, jmax, g_sph_rj,    &
!!     &          kr_out, r_CMB, fdm2_fix_dr_CMB, coef_p, evo_mat)
!!      subroutine add_cmb_scalar_poisson_mat(nri, jmax, g_sph_rj,      &
!!     &          kr_out, r_CMB, fdm2_fix_dr_CMB, coef_p, p_mat)
!!
!!      subroutine set_fix_fld_cmb_poisson00_mat(nri, kr_out, evo_mat)
!!      subroutine add_fix_flux_cmb_poisson00_mat                       &
!!     &         (nri, kr_out, fdm2_fix_dr_CMB, coef_p, evo_mat)
!!        integer(kind = kint), intent(in) :: nri, kr_out
!!        real(kind = kreal), intent(inout) :: evo_mat(3,0:kr_out)
!!
!!      subroutine set_ins_magne_cmb_rmat_sph(nri, jmax, g_sph_rj,      &
!!     &          kr_out, r_CMB, fdm2_fix_dr_CMB, coef_dbt, bs_evo_mat)
!!        real(kind = kreal), intent(inout) :: bs_evo_mat(3,nri,jmax)
!!@endverbatim
!!
!!@n @param nri     Number of radial points
!!@n @param jmax    Number of spherical harmonics modes
!!@n @param j0       Local harmonics mode address for l = m = 0
!!@n @param kr_out       Radial ID for outer boundary
!!@n @param r_CMB(0:2)   Radius at CMB
!!@n @param coef_d       Coefficient of diffusiotn term
!!@n @param coef_dbt     Coefficient for implicit diffusion term
!!@n @param fdm2_fix_dr_CMB(-1:1,3)
!!         Matrix to evaluate field at CMB with fixed radial derivative
!!
!!@n @param evo_mat(3,nri,jmax)  Band matrix for time evolution
!!@n @param bs_evo_mat(3,nri,jmax)    3-band matrix for evolution of 
!!                                    poloidal magnetic field
!
      module set_sph_scalar_matrix_CMB
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
      subroutine add_fix_flux_cmb_poisson_mat(nri, jmax, g_sph_rj,      &
     &          kr_out, r_CMB, fdm2_fix_dr_CMB, coef_p, evo_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
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
      subroutine add_cmb_scalar_poisson_mat(nri, jmax, g_sph_rj,        &
     &          kr_out, r_CMB, fdm2_fix_dr_CMB, coef_p, p_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
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
! -----------------------------------------------------------------------
!
      subroutine set_fix_fld_cmb_poisson00_mat(nri, kr_out, evo_mat)
!
      integer(kind = kint), intent(in) :: nri, kr_out
      real(kind = kreal), intent(inout) :: evo_mat(3,0:nri)
!
!
      evo_mat(3,kr_out-1) = zero
      evo_mat(2,kr_out  ) = one
!     evo_mat(1,kr_out+1) = zero
!
      end subroutine set_fix_fld_cmb_poisson00_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_fix_flux_cmb_poisson00_mat                         &
     &         (nri, kr_out, fdm2_fix_dr_CMB, coef_p, evo_mat)
!
      integer(kind = kint), intent(in) :: nri, kr_out
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      real(kind = kreal), intent(inout) :: evo_mat(3,0:nri)
!
!
      evo_mat(3,kr_out-1) = evo_mat(3,kr_out-1)                         &
     &                         - coef_p * fdm2_fix_dr_CMB(-1,3)
      evo_mat(2,kr_out  ) = evo_mat(2,kr_out  )                         &
     &                         - coef_p * fdm2_fix_dr_CMB( 0,3)
!     evo_mat(1,kr_out+1) = evo_mat(1,kr_out+1)                         &
!
      end subroutine add_fix_flux_cmb_poisson00_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ins_magne_cmb_rmat_sph(nri, jmax, g_sph_rj,        &
     &          kr_out, r_CMB, fdm2_fix_dr_CMB, coef_dbt, bs_evo_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
      real(kind = kreal), intent(in) :: coef_dbt
!
      real(kind = kreal), intent(inout) :: bs_evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        bs_evo_mat(3,kr_out-1,j) = bs_evo_mat(3,kr_out-1,j)             &
     &                            - coef_dbt * fdm2_fix_dr_CMB(-1,3)
        bs_evo_mat(2,kr_out,  j) = bs_evo_mat(2,kr_out,  j)             &
     &                            - coef_dbt * (fdm2_fix_dr_CMB( 0,3)   &
     &                               - g_sph_rj(j,3)*r_CMB(2)           &
     &                               - g_sph_rj(j,1)*r_CMB(1)           &
     &                             * fdm2_fix_dr_CMB( 1,3) )
      end do
!
      end subroutine set_ins_magne_cmb_rmat_sph
!
! -----------------------------------------------------------------------
!
      end module set_sph_scalar_matrix_CMB
