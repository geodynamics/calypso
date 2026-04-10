!>@file   sph_exp_fix_scl_diffuse_CMB.f90
!!@brief  module sph_exp_fix_scl_diffuse_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Set diffusiton term at outer boundary for explicit method
!!
!!@verbatim
!!      subroutine sph_out_fix_scalar_diffuse2(nnod_rj, jmax, g_sph_rj, &
!!     &          kr_out, r_CMB, fdm2_fix_fld_CMB, fix_CMB, coef_d,     &
!!     &          scl_rj, dfs_rj)
!!      subroutine sph_out_fix_scl_val_diffuse2(nnod_rj, jmax, g_sph_rj,&
!!     &          kr_out, r_CMB, fdm2_fix_fld_CMB, fix_CMB,             &
!!     &          coef_d, k_ratio, dk_dr, scl_rj, dfs_rj)
!!        integer(kind = kint), intent(in) :: nnod_rj
!!        integer(kind = kint), intent(in) :: jmax, kr_out
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: k_ratio, dk_dr
!!        real(kind = kreal), intent(in) :: fix_CMB(jmax)
!!        real(kind = kreal), intent(in) :: r_CMB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!!        real(kind = kreal), intent(inout) :: scl_rj(nnod_rj)
!!        real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!!@endverbatim
!!
!!@n @param idx_rj_degree_zero    Local address for degree 0
!!@n @param nnod_rj       Number of points for spectrum data
!!@n @param jmax          Number of local spherical harmonics mode
!!@n @param kr_out        Radial ID for outer boundary
!!@n @param r_CMB(0:2)    Radius at CMB
!!@n @param fdm2_fix_fld_CMB(-2:0,3)
!!         Matrix to evaluate radial derivative at CMB with fixed field
!!
!!@n @param fix_CMB(jmax) Spectr data for fixed scalar at CMB
!!@n @param coef_d        Coefficient for diffusion term
!!
!!@n @param scl_rj         Scalar spherical harmonic coefficients
!!@n @param dfs_rj         Diffusion term spherical harmonic coefficients
!
      module sph_exp_fix_scl_diffuse_CMB
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
      subroutine sph_out_fix_scalar_diffuse2(nnod_rj, jmax, g_sph_rj,   &
     &          kr_out, r_CMB, fdm2_fix_fld_CMB, fix_CMB, coef_d,       &
     &          scl_rj, dfs_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj
      integer(kind = kint), intent(in) :: jmax, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fix_CMB(jmax)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!
      real(kind = kreal), intent(inout) :: scl_rj(nnod_rj)
      real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!
      real(kind = kreal) :: d1s_dr1, d2s_dr2
      integer(kind = kint) :: inod, i_n1, i_n2, j
!
!
!$omp parallel do private(inod,i_n1,i_n2,d2s_dr2,d1s_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d1s_dr1 =  fdm2_fix_fld_CMB(-2,2) * scl_rj(i_n2)                &
     &           + fdm2_fix_fld_CMB(-1,2) * scl_rj(i_n1)                &
     &           + fdm2_fix_fld_CMB( 0,2) * fix_CMB(j)
        d2s_dr2 =  fdm2_fix_fld_CMB(-2,3) * scl_rj(i_n2)                &
     &           + fdm2_fix_fld_CMB(-1,3) * scl_rj(i_n1)                &
     &           + fdm2_fix_fld_CMB( 0,3) * fix_CMB(j)
!
        scl_rj(inod) = fix_CMB(j)
        dfs_rj(inod) = coef_d * (d2s_dr2 + two*r_CMB(1) * d1s_dr1       &
     &                - g_sph_rj(j,3)*r_CMB(2) * scl_rj(inod))
!
      end do
!$omp end parallel do
!
      end subroutine sph_out_fix_scalar_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine sph_out_fix_scl_val_diffuse2(nnod_rj, jmax, g_sph_rj,  &
     &          kr_out, r_CMB, fdm2_fix_fld_CMB, fix_CMB,               &
     &          coef_d, k_ratio, dk_dr, scl_rj, dfs_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj
      integer(kind = kint), intent(in) :: jmax, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: k_ratio, dk_dr
!
      real(kind = kreal), intent(in) :: fix_CMB(jmax)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!
      real(kind = kreal), intent(inout) :: scl_rj(nnod_rj)
      real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!
      real(kind = kreal) :: d1s_dr1, d2s_dr2
      integer(kind = kint) :: inod, i_n1, i_n2, j
!
!
!$omp parallel do private(inod,i_n1,i_n2,d2s_dr2,d1s_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d1s_dr1 =  fdm2_fix_fld_CMB(-2,2) * scl_rj(i_n2)                &
     &           + fdm2_fix_fld_CMB(-1,2) * scl_rj(i_n1)                &
     &           + fdm2_fix_fld_CMB( 0,2) * fix_CMB(j)
        d2s_dr2 =  fdm2_fix_fld_CMB(-2,3) * scl_rj(i_n2)                &
     &           + fdm2_fix_fld_CMB(-1,3) * scl_rj(i_n1)                &
     &           + fdm2_fix_fld_CMB( 0,3) * fix_CMB(j)
!
        scl_rj(inod) = fix_CMB(j)
        dfs_rj(inod) = coef_d * k_ratio                                 &
     &                * (d2s_dr2 + two*r_CMB(1) * d1s_dr1               &
     &                   - g_sph_rj(j,3)*r_CMB(2) * scl_rj(inod))       &
     &                + coef_d * dk_dr * d1s_dr1
      end do
!$omp end parallel do
!
      end subroutine sph_out_fix_scl_val_diffuse2
!
! -----------------------------------------------------------------------
!
      end module sph_exp_fix_scl_diffuse_CMB
