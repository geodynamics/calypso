!>@file   set_sph_pol_vscs_FDM2_mat.f90
!!@brief  module set_sph_pol_vscs_FDM2_mat
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2024
!
!>@brief Set poloidal diffusivity to 2nd order FDM matrix
!!
!!@verbatim
!!      subroutine sub_sph_pol_viscous_FDM2_mat(kr, nri, jmax,          &
!!     &          mat1_grad_p, mat2_viscous, mat7)
!!      subroutine set_sph_pol_viscous_mat7_ICB(kr, nri, jmax, mat7)
!!      subroutine set_sph_pol_viscous_mat7_CMB(kr, nri, jmax, mat7)
!!        integer(kind = kint), intent(in) :: kr, nri, jmax
!!        real(kind = kreal), intent(in) :: mat1_grad_p(jmax,0:1)
!!        real(kind = kreal), intent(in) :: mat2_viscous(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!
!!      subroutine sub_sph_pol_viscous_mat7_CTR1(nri, jmax,             &
!!     &          mat1_grad_p_CTR1, mat2_viscous_CTR1, mat7)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: mat1_grad_p_CTR1(jmax,0:1)
!!        real(kind = kreal), intent(in) :: mat2_viscous_CTR1(jmax,0:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,nri,jmax)
!!@endverbatim
!
      module set_sph_pol_vscs_FDM2_mat
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
      subroutine sub_sph_pol_viscous_FDM2_mat(kr, nri, jmax,            &
     &          mat1_grad_p, mat2_viscous, mat7)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
      real(kind = kreal), intent(in) :: mat1_grad_p(jmax,0:1)
      real(kind = kreal), intent(in) :: mat2_viscous(jmax,-1:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!        mat7(7,2*kr-3,j) = mat7(7,2*kr-3,j)
        mat7(6,2*kr-2,j) = mat7(6,2*kr-2,j) - mat2_viscous(j,-1)
        mat7(5,2*kr-1,j) = mat7(5,2*kr-1,j) + mat1_grad_p(j,0)
!
        mat7(4,2*kr,  j) = mat7(4,2*kr,  j) - mat2_viscous(j, 0)
!
        mat7(3,2*kr+1,j) = mat7(3,2*kr+1,j) + mat1_grad_p(j,1)
        mat7(2,2*kr+2,j) = mat7(2,2*kr+2,j) - mat2_viscous(j, 1)
!        mat7(1,2*kr+3,j) = mat7(1,2*kr+3,j)
      end do
!
      end subroutine sub_sph_pol_viscous_FDM2_mat
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_sph_pol_viscous_mat7_ICB(kr, nri, jmax, mat7)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*kr-3 .gt. 0) mat7(7,2*kr-3,j) = zero
        if(2*kr-2 .gt. 0) mat7(6,2*kr-2,j) = zero
        if(2*kr-1 .gt. 0) mat7(5,2*kr-1,j) = zero
!
        mat7(4,2*kr,  j) = one
!
        mat7(3,2*kr+1,j) = zero
        mat7(2,2*kr+2,j) = zero
        mat7(1,2*kr+3,j) = zero
      end do
!$omp end parallel do
!
      end subroutine set_sph_pol_viscous_mat7_ICB
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_pol_viscous_mat7_CMB(kr, nri, jmax, mat7)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        mat7(7,2*kr-3,j) = zero
        mat7(6,2*kr-2,j) = zero
        mat7(5,2*kr-1,j) = zero
!
        mat7(4,2*kr,  j) = one
!
       if(2*kr+1 .le. 2*nri)  mat7(3,2*kr+1,j) = zero
       if(2*kr+2 .le. 2*nri)  mat7(2,2*kr+2,j) = zero
       if(2*kr+3 .le. 2*nri)  mat7(1,2*kr+3,j) = zero
      end do
!$omp end parallel do
!
      end subroutine set_sph_pol_viscous_mat7_CMB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat7_CTR1(nri, jmax,               &
     &          mat1_grad_p_CTR1, mat2_viscous_CTR1, mat7)
!
      integer(kind = kint), intent(in) :: nri, jmax
!
      real(kind = kreal), intent(in) :: mat1_grad_p_CTR1(jmax,0:1)
      real(kind = kreal), intent(in) :: mat2_viscous_CTR1(jmax,0:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!       mat7(7,-1,j) = mat7(7,-1,j)
!       mat7(6, 0,j) = mat7(6, 0,j) - mat2_viscous_CTR1(j,-1)
        mat7(5, 1,j) = mat7(5, 1,j) + mat1_grad_p_CTR1(j,0)
!
        mat7(4, 2,j) = mat7(4, 2,j) - mat2_viscous_CTR1(j, 0)
!
        mat7(3, 3,j) = mat7(3, 3,j) + mat1_grad_p_CTR1(j,1)
        mat7(2, 4,j) = mat7(2, 4,j) - mat2_viscous_CTR1(j, 1)
!       mat7(1, 5,j) = mat7(1, 5,j)
      end do
!
      end subroutine sub_sph_pol_viscous_mat7_CTR1
!
!  -------------------------------------------------------------------
!
      end module set_sph_pol_vscs_FDM2_mat
