!>@file   set_sph_pol_vscs_FDM4_mat.f90
!!@brief  module set_sph_pol_vscs_FDM4_mat
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2024
!
!>@brief Set poloidal diffusivity to 4-th order FDM matrix
!!
!!@verbatim
!!      subroutine sub_sph_pol_viscous_FDM4_mat(kr, nri, jmax,          &
!!     &          mat3_grad_p, mat4_viscous, mat9)
!!      subroutine set_sph_pol_viscous_mat9_ICB(kr, nri, jmax, mat9)
!!      subroutine set_sph_pol_viscous_mat9_CMB(kr, nri, jmax, mat9)
!!        integer(kind = kint), intent(in) :: kr, nri, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p(jmax,-1:2)
!!        real(kind = kreal), intent(in) :: mat4_viscous(jmax,-2:2)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!
!!      subroutine sub_sph_pol_viscous_mat9_CTR1(nri, jmax,             &
!!     &          mat3_grad_p_CTR1, mat4_viscous_CTR1, mat9)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CTR1(jmax,0:2)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CTR1(jmax,0:2)
!!        real(kind = kreal), intent(inout) :: mat9(9,nri,jmax)
!!      subroutine sub_sph_pol_viscous_mat9_CTR2(nri, jmax,             &
!!     &          mat3_grad_p_CTR2, mat4_viscous_CTR2, mat9)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CTR2(jmax,-1:2)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CTR2(jmax,-1:2)
!!        real(kind = kreal), intent(inout) :: mat9(9,nri,jmax)
!!
!!      subroutine sub_sph_pol_viscous_mat9_ICB1(kr, nri, jmax,         &
!!     &          mat3_grad_p_CMB1, mat4_viscous_CMB1, mat9)
!!       integer(kind = kint), intent(in) :: kr, nri, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CMB1(jmax,0:2)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CMB1(jmax,-1:2)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!      subroutine sub_sph_pol_viscous_mat9_CMB1(kr, nri, jmax,         &
!!     &          mat3_grad_p_CMB1, mat4_viscous_CMB1, mat9)
!!      integer(kind = kint), intent(in) :: kr, nri, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CMB1(jmax,-1:1)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CMB1(jmax,-2:1)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!@endverbatim
!
      module set_sph_pol_vscs_FDM4_mat
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
      subroutine sub_sph_pol_viscous_FDM4_mat(kr, nri, jmax,            &
     &          mat3_grad_p, mat4_viscous, mat9)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
      real(kind = kreal), intent(in) :: mat3_grad_p(jmax,-1:2)
      real(kind = kreal), intent(in) :: mat4_viscous(jmax,-2:2)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        mat9(9,2*kr-4,j) = mat9(9,2*kr-4,j) - mat4_viscous(j,-2)
        mat9(8,2*kr-3,j) = mat9(8,2*kr-3,j) + mat3_grad_p(j,-1)
        mat9(7,2*kr-2,j) = mat9(7,2*kr-2,j) - mat4_viscous(j,-1)
        mat9(6,2*kr-1,j) = mat9(6,2*kr-1,j) + mat3_grad_p(j,0)
!
        mat9(5,2*kr,  j) = mat9(5,2*kr,  j) - mat4_viscous(j, 0)
!
        mat9(4,2*kr+1,j) = mat9(4,2*kr+1,j) + mat3_grad_p(j,1)
        mat9(3,2*kr+2,j) = mat9(3,2*kr+2,j) - mat4_viscous(j, 1)
        mat9(2,2*kr+3,j) = mat9(2,2*kr+3,j) + mat3_grad_p(j,2)
        mat9(1,2*kr+4,j) = mat9(1,2*kr+4,j) - mat4_viscous(j, 2)
      end do
!
      end subroutine sub_sph_pol_viscous_FDM4_mat
!
!  -------------------------------------------------------------------
!
      subroutine set_sph_pol_viscous_mat9_ICB(kr, nri, jmax, mat9)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*kr-4 .gt. 0) mat9(9,2*kr-4,j) = zero
        if(2*kr-3 .gt. 0) mat9(8,2*kr-3,j) = zero
        if(2*kr-2 .gt. 0) mat9(7,2*kr-2,j) = zero
        if(2*kr-1 .gt. 0) mat9(6,2*kr-1,j) = zero
!
        mat9(5,2*kr,  j) = one
!
        mat9(4,2*kr+1,j) = zero
        mat9(3,2*kr+2,j) = zero
        mat9(2,2*kr+3,j) = zero
        mat9(1,2*kr+4,j) = zero
      end do
!$omp end parallel do
!
      end subroutine set_sph_pol_viscous_mat9_ICB
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_pol_viscous_mat9_CMB(kr, nri, jmax, mat9)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
!$omp parallel do private(j)
      do j = 1, jmax
        mat9(9,2*kr-4,j) = zero
        mat9(8,2*kr-3,j) = zero
        mat9(7,2*kr-2,j) = zero
        mat9(6,2*kr-1,j) = zero
!
        mat9(5,2*kr,  j) = one
!
       if(2*kr+1 .le. 2*nri)  mat9(4,2*kr+1,j) = zero
       if(2*kr+2 .le. 2*nri)  mat9(3,2*kr+2,j) = zero
       if(2*kr+3 .le. 2*nri)  mat9(2,2*kr+3,j) = zero
       if(2*kr+4 .le. 2*nri)  mat9(1,2*kr+4,j) = zero
      end do
!$omp end parallel do
!
      end subroutine set_sph_pol_viscous_mat9_CMB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat9_CTR1(nri, jmax,               &
     &          mat3_grad_p_CTR1, mat4_viscous_CTR1, mat9)
!
      integer(kind = kint), intent(in) :: nri, jmax
!
      real(kind = kreal), intent(in) :: mat3_grad_p_CTR1(jmax,0:2)
      real(kind = kreal), intent(in) :: mat4_viscous_CTR1(jmax,0:2)
!
      real(kind = kreal), intent(inout) :: mat9(9,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!        mat9(9,-2,j) = mat9(9,-2,j) - mat4_viscous_CTR1(j,-2)
!        mat9(8,-1,j) = mat9(8,-1,j) + mat3_grad_p_CTR1(j,-1)
!        mat9(7, 0,j) = mat9(7, 0,j) - mat4_viscous_CTR1(j,-1)
        mat9(6, 1,j) = mat9(6, 1,j) + mat3_grad_p_CTR1(j, 0)
!
        mat9(5, 2,j) = mat9(5, 2,j) - mat4_viscous_CTR1(j, 0)
!
        mat9(4, 3,j) = mat9(4, 3,j) + mat3_grad_p_CTR1(j, 1)
        mat9(3, 4,j) = mat9(3, 4,j) - mat4_viscous_CTR1(j, 1)
        mat9(2, 5,j) = mat9(2, 5,j) + mat3_grad_p_CTR1(j, 2)
        mat9(1, 6,j) = mat9(1, 6,j) - mat4_viscous_CTR1(j, 2)
      end do
!
      end subroutine sub_sph_pol_viscous_mat9_CTR1
!
! -----------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat9_CTR2(nri, jmax,               &
     &          mat3_grad_p_CTR2, mat4_viscous_CTR2, mat9)
!
      integer(kind = kint), intent(in) :: nri, jmax
!
      real(kind = kreal), intent(in) :: mat3_grad_p_CTR2(jmax,-1:2)
      real(kind = kreal), intent(in) :: mat4_viscous_CTR2(jmax,-1:2)
!
      real(kind = kreal), intent(inout) :: mat9(9,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!       mat9(9, 0,j) = mat9(9, 0,j) - mat4_viscous_CTR2(j,-2)
        mat9(8, 1,j) = mat9(8, 1,j) + mat3_grad_p_CTR2(j,-1)
        mat9(7, 2,j) = mat9(7, 2,j) - mat4_viscous_CTR2(j,-1)
        mat9(6, 3,j) = mat9(6, 3,j) + mat3_grad_p_CTR2(j, 0)
!
        mat9(5, 4,j) = mat9(5, 4,j) - mat4_viscous_CTR2(j, 0)
!
        mat9(4, 5,j) = mat9(4, 5,j) + mat3_grad_p_CTR2(j, 1)
        mat9(3, 6,j) = mat9(3, 6,j) - mat4_viscous_CTR2(j, 1)
        mat9(2, 7,j) = mat9(2, 7,j) + mat3_grad_p_CTR2(j, 2)
        mat9(1, 8,j) = mat9(1, 8,j) - mat4_viscous_CTR2(j, 2)
      end do
!
      end subroutine sub_sph_pol_viscous_mat9_CTR2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat9_ICB1(kr, nri, jmax,           &
     &          mat3_grad_p_CMB1, mat4_viscous_CMB1, mat9)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
      real(kind = kreal), intent(in) :: mat3_grad_p_CMB1(jmax,0:2)
      real(kind = kreal), intent(in) :: mat4_viscous_CMB1(jmax,-1:2)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*kr-4 .gt. 0)  mat9(9,2*kr-4,j) = zero
        if(2*kr-3 .gt. 0)  mat9(8,2*kr-3,j) = zero
        mat9(7,2*kr-2,j) = mat9(7,2*kr-2,j) - mat4_viscous_CMB1(j,-1)
        mat9(6,2*kr-1,j) = mat9(6,2*kr-1,j) + mat3_grad_p_CMB1(j,0)
!
        mat9(5,2*kr,  j) = mat9(5,2*kr,  j) - mat4_viscous_CMB1(j, 0)
!
        mat9(4,2*kr+1,j) = mat9(4,2*kr+1,j) + mat3_grad_p_CMB1(j,1)
        mat9(3,2*kr+2,j) = mat9(3,2*kr+2,j) - mat4_viscous_CMB1(j, 1)
        mat9(2,2*kr+3,j) = mat9(2,2*kr+3,j) + mat3_grad_p_CMB1(j,2)
        mat9(1,2*kr+4,j) = mat9(1,2*kr+4,j) - mat4_viscous_CMB1(j, 2)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_pol_viscous_mat9_ICB1
!
! -----------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat9_CMB1(kr, nri, jmax,           &
     &          mat3_grad_p_CMB1, mat4_viscous_CMB1, mat9)
!
      integer(kind = kint), intent(in) :: kr, nri, jmax
      real(kind = kreal), intent(in) :: mat3_grad_p_CMB1(jmax,-1:1)
      real(kind = kreal), intent(in) :: mat4_viscous_CMB1(jmax,-2:1)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
!$omp parallel do private(j)
      do j = 1, jmax
        mat9(9,2*kr-4,j) = mat9(9,2*kr-4,j) - mat4_viscous_CMB1(j,-2)
        mat9(8,2*kr-3,j) = mat9(8,2*kr-3,j) + mat3_grad_p_CMB1(j,-1)
        mat9(7,2*kr-2,j) = mat9(7,2*kr-2,j) - mat4_viscous_CMB1(j,-1)
        mat9(6,2*kr-1,j) = mat9(6,2*kr-1,j) + mat3_grad_p_CMB1(j, 0)
!
        mat9(5,2*kr,  j) = mat9(5,2*kr,  j) - mat4_viscous_CMB1(j, 0)
!
        mat9(4,2*kr+1,j) = mat9(4,2*kr+1,j) + mat3_grad_p_CMB1(j, 1)
        mat9(3,2*kr+2,j) = mat9(3,2*kr+2,j) - mat4_viscous_CMB1(j, 1)
        if(2*kr+3 .le. 2*nri) mat9(2,2*kr+3,j) = zero
        if(2*kr+4 .le. 2*nri) mat9(1,2*kr+4,j) = zero
      end do
!$omp end parallel do
!
      end subroutine sub_sph_pol_viscous_mat9_CMB1
!
! -----------------------------------------------------------------------
!
      end module set_sph_pol_vscs_FDM4_mat
