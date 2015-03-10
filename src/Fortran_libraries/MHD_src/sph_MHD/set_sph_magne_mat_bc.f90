!>@file   set_sph_magne_mat_bc.f90
!!@brief  module set_sph_magne_mat_bc
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for magnetic field at boundaries
!!
!!@verbatim
!!    Boundary condition to connect potential field
!!      subroutine set_ins_magne_icb_rmat_sph(nri, jmax, kr_in, r_ICB,  &
!!     &          fdm2_fix_dr_ICB, coef_dbt, bs_evo_mat)
!!      subroutine set_ins_magne_cmb_rmat_sph(nri, jmax, kr_out, r_CMB, &
!!     &          fdm2_fix_dr_CMB, coef_dbt, bs_evo_mat)
!!
!!@endverbatim
!
!!@n @param jmax         Number of local spherical harmonics mode
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out       Radial ID for outer boundary
!!@n @param r_ICB(0:2)   Radius at ICB
!!@n @param r_CMB(0:2)   Radius at CMB
!!@n @param fdm2_fix_dr_ICB(-1:1,3)
!!         Matrix to evaluate field at ICB with fixed radial derivative
!!@n @param fdm2_fix_dr_CMB(-1:1,3)
!!         Matrix to evaluate field at CMB with fixed radial derivative
!!@n @param coef_dbt     coefficient for implicit diffusion term
!!
!!@n @param bs_evo_mat(3,nri,jmax)    3-band matrix for evolution of 
!!                                    poloidal magnetic field
!
      module set_sph_magne_mat_bc
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
      subroutine set_ins_magne_icb_rmat_sph(nri, jmax, kr_in, r_ICB,    &
     &          fdm2_fix_dr_ICB, coef_dbt, bs_evo_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
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
      subroutine set_ins_magne_cmb_rmat_sph(nri, jmax, kr_out, r_CMB,   &
     &          fdm2_fix_dr_CMB, coef_dbt, bs_evo_mat)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_out
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
      end module set_sph_magne_mat_bc
