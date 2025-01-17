!>@file   sum_sph_pol_vscs_FDM2_exp.f90
!!@brief  module sum_sph_pol_vscs_FDM2_exp
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2024
!
!>@brief Set poloidal diffusivity to 4-th order FDM matrix
!!
!!@verbatim
!!      subroutine sum_exp2_sph_pol_viscous(kr, nnod_rj, jmax,          &
!!     &          d_vpol, mat2_viscous, d_viscous_p)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat2_viscous(jmax,-1:1)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!
!!      subroutine sum_exp2_sph_viscous_CTR1(kr, nnod_rj, jmax,         &
!!     &          d_vpol, mat2_viscous_CTR1, d_viscous_p)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: mat2_viscous_CTR1(jmax,0:1)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!      subroutine sum_exp2_sph_pol_viscous_ICB(kr, nnod_rj, jmax,      &
!!     &          d_vpol, mat2_viscous_ICB, d_viscous_p)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat2_viscous_ICB(jmax,0:1)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!      subroutine sum_exp2_sph_pol_viscous_CMB(kr, nnod_rj, jmax,      &
!!     &          d_vpol, mat2_viscous_CMB, d_viscous_p)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: mat2_viscous_CMB(jmax,-1:0)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!@endverbatim
!
      module sum_sph_pol_vscs_FDM2_exp
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
      subroutine sum_exp2_sph_pol_viscous(kr, nnod_rj, jmax,            &
     &          d_vpol, mat2_viscous, d_viscous_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat2_viscous(jmax,-1:1)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, inod
!
!
      do j = 1, jmax
        inod = j + (kr-1) * jmax
        i_n1 = inod - jmax
        i_p1 = inod + jmax
!
        d_viscous_p(inod) =  mat2_viscous(j,-1) * d_vpol(i_n1)          &
     &                     + mat2_viscous(j, 0) * d_vpol(inod)          &
     &                     + mat2_viscous(j, 1) * d_vpol(i_p1)
      end do
!
      end subroutine sum_exp2_sph_pol_viscous
!
!  -------------------------------------------------------------------
!
      subroutine sum_exp2_sph_viscous_CTR1(kr, nnod_rj, jmax,           &
     &          d_vpol, mat2_viscous_CTR1, d_viscous_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: mat2_viscous_CTR1(jmax,0:1)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p1, inod
!
!
!$omp parallel do private(j,i_p1,inod)
      do j = 1, jmax
        inod = j + (kr-1) * jmax
!        i_n1 = inod - jmax
        i_p1 = inod + jmax
!
        d_viscous_p(inod) =  mat2_viscous_CTR1(j, 0) * d_vpol(inod)     &
     &                     + mat2_viscous_CTR1(j, 1) * d_vpol(i_p1)
      end do
!$omp end parallel do
!
      end subroutine sum_exp2_sph_viscous_CTR1
!
! -----------------------------------------------------------------------
!
      subroutine sum_exp2_sph_pol_viscous_ICB(kr, nnod_rj, jmax,        &
     &          d_vpol, mat2_viscous_ICB, d_viscous_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: mat2_viscous_ICB(jmax,0:1)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p1, inod
!
!
!$omp parallel do private(j,inod,i_p1)
      do j = 1, jmax
        inod = j + (kr-1) * jmax
        i_p1 = inod + jmax
!
        d_viscous_p(inod) =  mat2_viscous_ICB(j, 0) * d_vpol(inod)      &
     &                     + mat2_viscous_ICB(j, 1) * d_vpol(i_p1)
      end do
!$omp end parallel do
!
      end subroutine sum_exp2_sph_pol_viscous_ICB
!
!  -------------------------------------------------------------------
!
      subroutine sum_exp2_sph_pol_viscous_CMB(kr, nnod_rj, jmax,        &
     &          d_vpol, mat2_viscous_CMB, d_viscous_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: mat2_viscous_CMB(jmax,-1:0)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, inod
!
!
!$omp parallel do private(j,inod,i_n1)
      do j = 1, jmax
        inod = j + (kr-1) * jmax
        i_n1 = inod - jmax
!
        d_viscous_p(inod) =  mat2_viscous_CMB(j,-1) * d_vpol(i_n1)      &
     &                     + mat2_viscous_CMB(j, 0) * d_vpol(inod)
      end do
!$omp end parallel do
!
      end subroutine sum_exp2_sph_pol_viscous_CMB
!
! -----------------------------------------------------------------------
!
      end module sum_sph_pol_vscs_FDM2_exp
