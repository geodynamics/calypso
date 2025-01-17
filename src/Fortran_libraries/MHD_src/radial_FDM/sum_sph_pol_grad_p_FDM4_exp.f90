!>@file   sum_sph_pol_grad_p_FDM4_exp.f90
!!@brief  module sum_sph_pol_grad_p_FDM4_exp
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2024
!
!>@brief Set poloidal diffusivity to 4-th order FDM matrix
!!
!!@verbatim
!!      subroutine sum_exp4_sph_pol_grad_p(kr, nnod_rj, jmax,           &
!!     &          press_e, mat3_grad_p, d_grad_p)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(in) :: mat3_grad_p(-1:2)
!!        real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!!
!!      subroutine sum_exp4_sph_pol_grad_p_CTR1(kr, nnod_rj, jmax,      &
!!     &          press_e, mat3_grad_p_CTR1, d_grad_p)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CTR1(0:2)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!!
!!      subroutine sum_exp4_sph_pol_grad_p_ICB(kr, nnod_rj, jmax,       &
!!     &          press_e, mat3_grad_p_ICB, d_grad_p)
!!      subroutine sum_exp4_sph_pol_grad_p_ICB1(kr, nnod_rj, jmax,      &
!!     &          press_e, mat3_grad_p_ICB1, d_grad_p)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(in) :: mat3_grad_p_ICB(jmax,1:2)
!!        real(kind = kreal), intent(in) :: mat4_viscous_ICB(jmax,0:2)
!!        real(kind = kreal), intent(in) :: mat3_grad_p_ICB1(jmax,0:2)
!!        real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!!
!!      subroutine sum_exp4_sph_pol_grad_p_CMB1(kr, nnod_rj, jmax,      &
!!     &          press_e, mat3_grad_p_CMB1, d_grad_p)
!!      subroutine sum_exp4_sph_pol_grad_p_CMB(kr, nnod_rj, jmax,       &
!!     &          press_e, mat3_grad_p_CMB, d_grad_p)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CMB1(jmax,-1:1)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CMB1(jmax,-2:1)
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CMB(jmax,-1:0)
!!        real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!!@endverbatim
!
      module sum_sph_pol_grad_p_FDM4_exp
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
      subroutine sum_exp4_sph_pol_grad_p(kr, nnod_rj, jmax,             &
     &          press_e, mat3_grad_p, d_grad_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
      real(kind = kreal), intent(in) :: mat3_grad_p(-1:2)
!
      real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, i_p2, inod
!
!
      do j = 1, jmax
        inod = j + (kr-1) * jmax
        i_n1 = inod - jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d_grad_p(inod) =  mat3_grad_p(-1) *  press_e(i_n1)              &
     &                  + mat3_grad_p( 0) *  press_e(inod)              &
     &                  + mat3_grad_p( 1) *  press_e(i_p1)              &
     &                  + mat3_grad_p( 2) *  press_e(i_p2)
      end do
!
      end subroutine sum_exp4_sph_pol_grad_p
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_exp4_sph_pol_grad_p_CTR1(kr, nnod_rj, jmax,        &
     &          press_e, mat3_grad_p_CTR1, d_grad_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
      real(kind = kreal), intent(in) :: mat3_grad_p_CTR1(jmax,0:2)
!
      real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p1, i_p2, inod
!
!
!$omp parallel do private(j,i_p1,i_p2,inod)
      do j = 1, jmax
        inod = j + (kr-1) * jmax
!        i_n1 = inod - jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d_grad_p(inod) =  mat3_grad_p_CTR1(j,0) *  press_e(inod)        &
     &                  + mat3_grad_p_CTR1(j,1) *  press_e(i_p1)        &
     &                  + mat3_grad_p_CTR1(j,2) *  press_e(i_p2)
      end do
!$omp end parallel do
!
      end subroutine sum_exp4_sph_pol_grad_p_CTR1
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_exp4_sph_pol_grad_p_ICB(kr, nnod_rj, jmax,         &
     &          press_e, mat3_grad_p_ICB, d_grad_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
      real(kind = kreal), intent(in) :: mat3_grad_p_ICB(jmax,1:2)
!
      real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p2, i_p1, inod
!
!
!$omp parallel do private(j,inod,i_p1,i_p2)
      do j = 1, jmax
        inod = j + (kr-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d_grad_p(inod) =  mat3_grad_p_ICB(j, 1) *  press_e(i_p1)        &
     &                  + mat3_grad_p_ICB(j, 2) *  press_e(i_p2)
      end do
!$omp end parallel do
!
      end subroutine sum_exp4_sph_pol_grad_p_ICB
!
! -----------------------------------------------------------------------
!
      subroutine sum_exp4_sph_pol_grad_p_ICB1(kr, nnod_rj, jmax,        &
     &          press_e, mat3_grad_p_ICB1, d_grad_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
      real(kind = kreal), intent(in) :: mat3_grad_p_ICB1(jmax,0:2)
!
      real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p1, i_p2, inod
!
!
!$omp parallel do private(j,inod,i_p1,i_p2)
      do j = 1, jmax
        inod = j + (kr-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d_grad_p(inod) =  mat3_grad_p_ICB1(j, 0) *  press_e(inod)       &
     &                  + mat3_grad_p_ICB1(j, 1) *  press_e(i_p1)       &
     &                  + mat3_grad_p_ICB1(j, 2) *  press_e(i_p2)
      end do
!$omp end parallel do
!
      end subroutine sum_exp4_sph_pol_grad_p_ICB1
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine sum_exp4_sph_pol_grad_p_CMB1(kr, nnod_rj, jmax,        &
     &          press_e, mat3_grad_p_CMB1, d_grad_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
      real(kind = kreal), intent(in) :: mat3_grad_p_CMB1(jmax,-1:1)
!
      real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, inod
!
!
!$omp parallel do private(j,inod,i_n1,i_p1)
      do j = 1, jmax
        inod = j + (kr-1) * jmax
        i_n1 = inod - jmax
        i_p1 = inod + jmax
!
        d_grad_p(inod) =  mat3_grad_p_CMB1(jmax,-1) *  press_e(i_n1)    &
     &                  + mat3_grad_p_CMB1(jmax, 0) *  press_e(inod)    &
     &                  + mat3_grad_p_CMB1(jmax, 1) *  press_e(i_p1)
      end do
!$omp end parallel do
!
      end subroutine sum_exp4_sph_pol_grad_p_CMB1
!
!  -------------------------------------------------------------------
!
      subroutine sum_exp4_sph_pol_grad_p_CMB(kr, nnod_rj, jmax,         &
     &          press_e, mat3_grad_p_CMB, d_grad_p)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
      real(kind = kreal), intent(in) :: mat3_grad_p_CMB(-1:0)
!
      real(kind = kreal), intent(inout) :: d_grad_p(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, inod
!
!
!$omp parallel do private(j,inod,i_n1)
      do j = 1, jmax
        inod = j + (kr-1) * jmax
        i_n1 = inod - jmax
!
        d_grad_p(inod) =  mat3_grad_p_CMB(-1) *  press_e(i_n1)          &
     &                  + mat3_grad_p_CMB( 0) *  press_e(inod)
      end do
!$omp end parallel do
!
      end subroutine sum_exp4_sph_pol_grad_p_CMB
!
! -----------------------------------------------------------------------
!
      end module sum_sph_pol_grad_p_FDM4_exp
