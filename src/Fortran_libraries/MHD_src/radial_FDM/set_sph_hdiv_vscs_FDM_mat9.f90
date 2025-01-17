!>@file   set_sph_hdiv_vscs_FDM_mat9.f90
!!@brief  module set_sph_hdiv_vscs_FDM_mat9
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2024
!
!>@brief Set poloidal diffusivity to 2nd order FDM matrix
!!
!!@verbatim
!!      subroutine add_sph_ele_pressure_FDM_mat9(kr, nri, jmax,         &
!!     &                                         coef_p, mat9)
!!        integer(kind = kint), intent(in) :: kr, nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_FDM_mat9(kr, nri, jmax,         &
!!     &                                         hdiv_visous_mat, mat9)
!!        integer(kind = kint), intent(in) :: kr, nri, jmax
!!        real(kind = kreal), intent(in) :: hdiv_visous_mat(jmax,-2:1)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!
!!      subroutine sub_sph_hdiv_viscous_mat9_CTR                        &
!!     &         (nri, jmax, hdiv_visous_mat_CTR, mat9)
!!      subroutine sub_sph_hdiv_viscous_mat9_CTR1                       &
!!     &         (nri, jmax, hdiv_visous_mat_CTR1, mat9)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CTR(jmax,0:1)
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CTR1(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!      subroutine set_sph_hdiv_viscous_mat9_ICB(k_ICB, nri, jmax, mat9)
!!      subroutine sub_sph_hdiv_viscous_mat9_ICB1                       &
!!     &         (kr, nri, jmax, hdiv_visous_mat_ICB, mat9)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: k_ICB, nri, jmax
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_ICB(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_mat9_CMB1(kr, nri, jmax,       &
!!     &          hdiv_visous_mat_CMB1, mat9)
!!      subroutine sub_sph_hdiv_viscous_mat9_CMB(k_CMB, nri, jmax,      &
!!     &          hdiv_visous_mat_CMB, mat9)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: k_CMB, nri, jmax
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CMB1(jmax,-2:1)
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CMB(jmax,-2:0)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!@endverbatim
!
      module set_sph_hdiv_vscs_FDM_mat9
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
      subroutine add_sph_ele_pressure_FDM_mat9(kr, nri, jmax,           &
     &                                         coef_p, mat9)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
      real(kind = kreal), intent(in) :: coef_p
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
!
      mat9(5,2*kr-1,1:jmax) = mat9(5,2*kr-1,1:jmax) + coef_p
!
      end subroutine add_sph_ele_pressure_FDM_mat9
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_FDM_mat9(kr, nri, jmax,           &
     &                                         hdiv_visous_mat, mat9)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_mat(jmax,-2:1)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!        mat9(9,2*kr-5,j) = mat9(9,2*kr-5,j)
        mat9(8,2*kr-4,j) = mat9(8,2*kr-4,j) - hdiv_visous_mat(j,-2)
!        mat9(7,2*kr-3,j) = mat9(7,2*kr-3,j)
        mat9(6,2*kr-2,j) = mat9(6,2*kr-2,j) - hdiv_visous_mat(j,-1)
!
!        mat9(5,2*kr-1,j) = mat9(5,2*kr-1,j)\
!
        mat9(4,2*kr,  j) = mat9(4,2*kr,  j) - hdiv_visous_mat(j, 0)
!        mat9(3,2*kr+1,j) = mat9(3,2*kr+1,j)
        mat9(2,2*kr+2,j) = mat9(2,2*kr+2,j) - hdiv_visous_mat(j, 1)
!        mat9(1,2*kr+3,j) = mat9(1,2*kr+3,j)
      end do
!
      end subroutine sub_sph_hdiv_viscous_FDM_mat9
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat9_CTR                          &
     &         (nri, jmax, hdiv_visous_mat_CTR, mat9)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CTR(jmax,0:1)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
!        mat9(9,-3,j) = mat9(9,-3,j)
!        mat9(8,-2,j) = mat9(8,-2,j) - hdiv_visous_mat_CTR(j,-2)
!        mat9(7,-1,j) = mat9(7,-1,j)
!        mat9(6, 0,j) = mat9(6, 0,j) - hdiv_visous_mat_CTR(j,-1)
!
!        mat9(5,1,j) = mat9(5,1,j)
!
        mat9(4,2,j) = mat9(4,2,j) - hdiv_visous_mat_CTR(j, 0)
!        mat9(3,3,j) = mat9(3,3,j)
        mat9(2,4,j) = mat9(2,4,j) - hdiv_visous_mat_CTR(j, 1)
!        mat9(1,5,j) = mat9(1,5,j)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat9_CTR
!
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat9_CTR1                         &
     &         (nri, jmax, hdiv_visous_mat_CTR1, mat9)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CTR1(jmax,-1:1)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
!        mat9(9,-1,j) = mat9(9,-1,j)
!        mat9(8, 0,j) = mat9(8, 0,j) - hdiv_visous_mat_CTR1(j,-2)
!        mat9(7, 1,j) = mat9(7, 1,j)
        mat9(6, 2,j) = mat9(6, 2,j) - hdiv_visous_mat_CTR1(j,-1)
!
!        mat9(5,3,j) = mat9(5,3,j)
!
        mat9(4,4,j) = mat9(4,4,j) - hdiv_visous_mat_CTR1(j, 0)
!        mat9(3,5,j) = mat9(3,5,j)
        mat9(2,6,j) = mat9(2,6,j) - hdiv_visous_mat_CTR1(j, 1)
!        mat9(1,7,j) = mat9(1,7,j)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat9_CTR1
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_sph_hdiv_viscous_mat9_ICB(k_ICB, nri, jmax, mat9)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nri, jmax
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*k_ICB-5 .gt. 0) mat9(9,2*k_ICB-5,j) = zero
        if(2*k_ICB-4 .gt. 0) mat9(8,2*k_ICB-4,j) = zero
        if(2*k_ICB-3 .gt. 0) mat9(7,2*k_ICB-3,j) = zero
        if(2*k_ICB-2 .gt. 0) mat9(6,2*k_ICB-2,j) = zero
!
        mat9(5,2*k_ICB-1,j) = zero
!
        mat9(4,2*k_ICB,  j) = one
        mat9(3,2*k_ICB+1,j) = zero
        mat9(2,2*k_ICB+2,j) = zero
        mat9(1,2*k_ICB+3,j) = zero
      end do
!$omp end parallel do
!
      end subroutine set_sph_hdiv_viscous_mat9_ICB
!
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat9_ICB1                         &
     &         (kr, nri, jmax, hdiv_visous_mat_ICB, mat9)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_mat_ICB(jmax,-1:1)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*kr-5 .gt. 0) mat9(9,2*kr-5,j) = zero
        if(2*kr-4 .gt. 0) mat9(8,2*kr-4,j) = zero
!        mat9(7,2*kr-3,j) = mat9(7,2*kr-3,j)
        mat9(6,2*kr-2,j) = mat9(6,2*kr-2,j) - hdiv_visous_mat_ICB(j,-1)
!
!        mat9(5,2*kr-1,j) = mat9(5,2*kr-1,j)
!
        mat9(4,2*kr,  j) = mat9(4,2*kr,  j) - hdiv_visous_mat_ICB(j, 0)
!        mat9(3,2*kr+1,j) = mat9(3,2*kr+1,j)
        mat9(2,2*kr+2,j) = mat9(2,2*kr+2,j) - hdiv_visous_mat_ICB(j, 1)
!        mat9(1,2*kr+3,j) = mat9(1,2*kr+3,j)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat9_ICB1
!
!  -------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat9_CMB1(kr, nri, jmax,       &
     &          hdiv_visous_mat_CMB1, mat9)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CMB1(jmax,-2:1)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
!       mat9(9,2*kr-5,j) = mat9(9,2*kr-5,j)
        mat9(8,2*kr-4,j) = mat9(8,2*kr-4,j)                             &
     &                       - hdiv_visous_mat_CMB1(j,-2)
!       mat9(7,2*kr-3,j) = mat9(7,2*kr-3,j)
        mat9(6,2*kr-2,j) = mat9(6,2*kr-2,j)                             &
     &                       - hdiv_visous_mat_CMB1(j,-1)
!
!        mat9(5,2*kr-1,j) = mat9(5,2*kr-1,j)
!
        mat9(4,2*kr,  j) = mat9(4,2*kr,  j)                             &
     &                       - hdiv_visous_mat_CMB1(j, 0)
!        mat9(3,2*kr+1,j) = mat9(3,2*kr+1,j)
        mat9(2,2*kr+2,j) = mat9(2,2*kr+2,j)                             &
     &                       - hdiv_visous_mat_CMB1(j, 1)
        if(2*kr+3 .gt. 2*nri) mat9(1,2*kr+3,j) = zero
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat9_CMB1
!
!  -------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat9_CMB(k_CMB, nri, jmax,        &
     &          hdiv_visous_mat_CMB, mat9)
!
      integer(kind = kint), intent(in) :: k_CMB, nri, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CMB(jmax,-2:0)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
!       mat9(9,2*k_CMB-5,j) = mat9(9,2*k_CMB-5,j)
        mat9(8,2*k_CMB-4,j) = mat9(8,2*k_CMB-4,j)                       &
     &                       - hdiv_visous_mat_CMB(j,-2)
!       mat9(7,2*k_CMB-3,j) = mat9(7,2*k_CMB-3,j)
        mat9(6,2*k_CMB-2,j) = mat9(6,2*k_CMB-2,j)                       &
     &                       - hdiv_visous_mat_CMB(j,-1)
!
!        mat9(5,2*k_CMB-1,j) = mat9(5,2*k_CMB-1,j)
!
        mat9(4,2*k_CMB,  j) = mat9(4,2*k_CMB,  j)                       &
     &                       - hdiv_visous_mat_CMB(j, 0)
        if(2*k_CMB+1 .gt. 2*nri) mat9(3,2*k_CMB+1,j) = zero
        if(2*k_CMB+2 .gt. 2*nri) mat9(2,2*k_CMB+2,j) = zero
        if(2*k_CMB+3 .gt. 2*nri) mat9(1,2*k_CMB+3,j) = zero
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat9_CMB
!
!  -------------------------------------------------------------------
!
      end module set_sph_hdiv_vscs_FDM_mat9
