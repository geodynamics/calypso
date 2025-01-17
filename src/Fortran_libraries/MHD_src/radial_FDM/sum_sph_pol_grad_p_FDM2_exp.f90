!>@file   sum_sph_pol_grad_p_FDM2_exp.f90
!!@brief  module sum_sph_pol_grad_p_FDM2_exp
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2024
!
!>@brief Set poloidal diffusivity to 4-th order FDM matrix
!!
!!@verbatim
!!      subroutine sum_exp2_sph_pol_grad_p(kr, nnod_rj, jmax,           &
!!     &          press_e, mat1_grad_p, d_grad_p)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat1_grad_p(jmax,0:1)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!!
!!      subroutine sum_exp2_sph_pol_grad_p_ICB(kr, nnod_rj, jmax,       &
!!     &          press_e, mat1_grad_p_ICB, d_grad_p)
!!        integer(kind = kint), intent(in) :: kr, nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(in) :: mat1_grad_p_ICB(jmax,1:1)
!!        real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!!      subroutine sum_exp2_sph_pol_grad_p_CMB(kr, nnod_rj, jmax,       &
!!     &          press_e, mat1_grad_p_CMB, d_grad_p)
!!        integer(kind = kint), intent(in) :: kr, nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(in) :: mat1_grad_p_CMB(jmax,0:0)
!!        real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!!@endverbatim
!
      module sum_sph_pol_grad_p_FDM2_exp
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
      subroutine sum_exp2_sph_pol_grad_p(kr, nnod_rj, jmax,             &
     &          press_e, mat1_grad_p, d_grad_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat1_grad_p(jmax,0:1)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p1, inod
!
!
      do j = 1, jmax
        inod = j + (kr-1) * jmax
        i_p1 = inod + jmax
!
        d_grad_p(inod) =  mat1_grad_p(j, 0) *  press_e(inod)            &
     &                  + mat1_grad_p(j, 1) *  press_e(i_p1)
      end do
!
      end subroutine sum_exp2_sph_pol_grad_p
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_exp2_sph_pol_grad_p_ICB(kr, nnod_rj, jmax,         &
     &          press_e, mat1_grad_p_ICB, d_grad_p)
!
      integer(kind = kint), intent(in) :: kr, nnod_rj, jmax
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
      real(kind = kreal), intent(in) :: mat1_grad_p_ICB(jmax,1:1)
!
      real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p1, inod
!
!
!$omp parallel do private(j,inod,i_p1)
      do j = 1, jmax
        inod = j + (kr-1) * jmax
        i_p1 = inod + jmax
!
        d_grad_p(inod) = mat1_grad_p_ICB(j, 1) *  press_e(i_p1)
      end do
!$omp end parallel do
!
      end subroutine sum_exp2_sph_pol_grad_p_ICB
!
!  -------------------------------------------------------------------
!
      subroutine sum_exp2_sph_pol_grad_p_CMB(kr, nnod_rj, jmax,         &
     &          press_e, mat1_grad_p_CMB, d_grad_p)
!
      integer(kind = kint), intent(in) :: kr, nnod_rj, jmax
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
      real(kind = kreal), intent(in) :: mat1_grad_p_CMB(jmax,0:0)
!
      real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!
      integer(kind = kint) :: j, inod
!
!
!$omp parallel do private(j,inod)
      do j = 1, jmax
        inod = j + (kr-1) * jmax
!
        d_grad_p(inod) =  mat1_grad_p_CMB(j, 0) * press_e(inod)
      end do
!$omp end parallel do
!
      end subroutine sum_exp2_sph_pol_grad_p_CMB
!
! -----------------------------------------------------------------------
!
      end module sum_sph_pol_grad_p_FDM2_exp
