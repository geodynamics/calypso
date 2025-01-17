!>@file   set_sph_hdiv_vscs_FDM_mat7.f90
!!@brief  module set_sph_hdiv_vscs_FDM_mat7
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2024
!
!>@brief Set poloidal diffusivity to 2nd order FDM matrix
!!
!!@verbatim
!!      subroutine set_sph_ele_pressure_FDM_mat7(kr, nri, jmax,         &
!!     &                                         coef_p, mat7)
!!        integer(kind = kint), intent(in) :: kr, nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_FDM_mat7(kr, nri, jmax,         &
!!     &                                         hdiv_visous_mat, mat7)
!!        integer(kind = kint), intent(in) :: kr, nri, jmax
!!        real(kind = kreal), intent(in) :: hdiv_visous_mat(jmax,-2:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!
!!      subroutine sub_sph_hdiv_viscous_mat7_CTR(nri, jmax,             &
!!     &          hdiv_visous_mat_CTR, mat7)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: hdiv_visous_mat_CTR(jmax,0:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_mat7_CTR1(nri, jmax,            &
!!     &          hdiv_visous_mat_CTR1, mat7)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: hdiv_visous_mat_CTR1(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_mat7_ICB1                       &
!!     &         (k_ICB, nri, jmax, hdiv_visous_mat_ICB, mat7)
!!        integer(kind = kint), intent(in) :: k_ICB
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_ICB(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_mat7_CMB                        &
!!     &         (k_CMB, nri, jmax, hdiv_visous_mat_CMB, mat7)
!!        integer(kind = kint), intent(in) :: k_CMB
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CMB(jmax,-2:0)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!@endverbatim
!
      module set_sph_hdiv_vscs_FDM_mat7
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
      subroutine set_sph_ele_pressure_FDM_mat7(kr, nri, jmax,           &
     &                                         coef_p, mat7)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
      real(kind = kreal), intent(in) :: coef_p
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
!
      mat7(4,2*kr-1,1:jmax) = coef_p
!
      end subroutine set_sph_ele_pressure_FDM_mat7
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_FDM_mat7(kr, nri, jmax,           &
     &                                         hdiv_visous_mat, mat7)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_mat(jmax,-2:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        mat7(7,2*kr-4,j) = mat7(7,2*kr-4,j) - hdiv_visous_mat(j,-2)
!        mat7(6,2*kr-3,j) = mat7(6,2*kr-3,j)
        mat7(5,2*kr-2,j) = mat7(5,2*kr-2,j) - hdiv_visous_mat(j,-1)
!
!        mat7(4,2*kr-1,j) = mat7(4,2*kr-1,j)
!
        mat7(3,2*kr,  j) = mat7(3,2*kr,  j) - hdiv_visous_mat(j, 0)
!        mat7(2,2*kr+1,j) = mat7(2,2*kr+1,j)
        mat7(1,2*kr+2,j) = mat7(1,2*kr+2,j) - hdiv_visous_mat(j, 1)
      end do
!
      end subroutine sub_sph_hdiv_viscous_FDM_mat7
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat7_CTR(nri, jmax,               &
     &          hdiv_visous_mat_CTR, mat7)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CTR(jmax,0:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
!       mat7(7,-2,j) = mat7(7,-2,j) - hdiv_visous_mat_CTR(j,-2)
!       mat7(6,-1,j) = mat7(6,-1,j)
!       mat7(5, 0,j) = mat7(5, 0,j) - hdiv_visous_mat_CTR(j,-1)
!
!        mat7(4,1,j) = mat7(4,1,j)
!
        mat7(3,2,j) = mat7(3,2,j) - hdiv_visous_mat_CTR(j, 0)
!       mat7(2,3,j) = mat7(2,3,j)
        mat7(1,4,j) = mat7(1,4,j) - hdiv_visous_mat_CTR(j, 1)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat7_CTR
!
!  -------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat7_CTR1(nri, jmax,              &
     &          hdiv_visous_mat_CTR1, mat7)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in)                                    &
     &                   :: hdiv_visous_mat_CTR1(jmax,-1:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
!       mat7(7,0,j) = mat7(7,0,j) - hdiv_visous_mat_CTR1(j,-2)
!       mat7(6,1,j) = mat7(6,1,j)
        mat7(5,2,j) = mat7(5,2,j) - hdiv_visous_mat_CTR1(j,-1)
!
!       mat7(4,3,j) = mat7(4,3,j)
!
        mat7(3,4,j) = mat7(3,4,j) - hdiv_visous_mat_CTR1(j, 0)
!       mat7(2,5,j) = mat7(2,5,j)
        mat7(1,6,j) = mat7(1,6,j) - hdiv_visous_mat_CTR1(j, 1)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat7_CTR1
!
!  -------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat7_ICB1                         &
     &         (k_ICB, nri, jmax, hdiv_visous_mat_ICB, mat7)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_mat_ICB(jmax,-1:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*k_ICB-4 .gt. 0) mat7(7,2*k_ICB-4,j) = zero
!       mat7(6,2*k_ICB-3,j) = mat7(6,2*k_ICB-3,j)
        mat7(5,2*k_ICB-2,j) = mat7(5,2*k_ICB-2,j)                       &
     &                       - hdiv_visous_mat_ICB(j,-1)
!
!       mat7(4,2*k_ICB-1,j) = mat7(4,2*k_ICB-1,j)
!
        mat7(3,2*k_ICB,  j) = mat7(3,2*k_ICB,  j)                       &
     &                       - hdiv_visous_mat_ICB(j, 0)
!        mat7(2,2*k_ICB+1,j) = mat7(2,2*k_ICB+1,j)
        mat7(1,2*k_ICB+2,j) = mat7(1,2*k_ICB+2,j)                       &
     &                       - hdiv_visous_mat_ICB(j, 1)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat7_ICB1
!
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat7_CMB                          &
     &         (k_CMB, nri, jmax, hdiv_visous_mat_CMB, mat7)
!
      integer(kind = kint), intent(in) :: k_CMB
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CMB(jmax,-2:0)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        mat7(7,2*k_CMB-4,j) = mat7(7,2*k_CMB-4,j)                       &
     &                       - hdiv_visous_mat_CMB(j,-2)
!       mat7(6,2*k_CMB-3,j) = mat7(6,2*k_CMB-3,j)
        mat7(5,2*k_CMB-2,j) = mat7(5,2*k_CMB-2,j)                       &
     &                       - hdiv_visous_mat_CMB(j,-1)
!
!       mat7(4,2*k_CMB-1,j) = mat7(4,2*k_CMB-1,j)
!
        mat7(3,2*k_CMB,  j) = mat7(3,2*k_CMB,  j)                       &
     &                       - hdiv_visous_mat_CMB(j, 0)
        if(2*k_CMB+1 .gt. 2*nri) mat7(2,2*k_CMB+1,j) = zero
        if(2*k_CMB+2 .gt. 2*nri) mat7(1,2*k_CMB+2,j) = zero
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat7_CMB
!
! -----------------------------------------------------------------------
!
      end module set_sph_hdiv_vscs_FDM_mat7
