!>@file   sph_zero_degree_matrices.f90
!!@brief  module sph_zero_degree_matrices
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for degree 0
!!
!!@verbatim
!!      subroutine copy_to_band3_mat_w_center(nri, c_evo, mat3, mat00_3)
!!      subroutine copy_to_band3_mat_no_center(nri, mat3, mat00_3)
!!      subroutine add_scalar_poisson_mat_fill_ctr(nri, r_CTR1,         &
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: c_evo
!!        real(kind = kreal), intent(in) :: mat3(3,nri)
!!        real(kind = kreal), intent(inout) :: mat00_3(3,0:nri)
!!
!!     &         fdm2_fix_dr_center, fdm2_fix_fld_ctr1, coef_p, mat00_3)
!!      subroutine add_scl_val_dfse_mat_fill_ctr(nri, r_CTR1,           &
!!     &          fdm2_fix_dr_center, fdm2_fix_fld_ctr1,                &
!!     &          coef_p, k_ratio, dk_dr, mat00_3)
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: r_CTR1(0:2)
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_center(-1:1,3)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!!        real(kind = kreal), intent(inout) :: mat00_3(3,0:nri)
!!
!!      subroutine add_scalar_poisson_mat_fix_ctr(nri, r_CTR1,          &
!!     &          fdm2_fix_fld_ctr1, coef_p, mat00_3)
!!      subroutine add_scl_val_diffuse_mat_fix_ctr(nri, r_CTR1,         &
!!     &          fdm2_fix_fld_ctr1, coef_p, k_ratio, dk_dr, mat00_3)
!!      subroutine add_scalar_poisson_mat_no_fld(nri, mat00_3)
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: c_evo
!!        real(kind = kreal), intent(in) :: mat3(3,nri)
!!        real(kind = kreal), intent(in) :: r_CTR1(0:2)
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: k_ratio, dk_dr
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_center(-1:1,3)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!!        real(kind = kreal), intent(inout) :: mat00_3(3,0:nri)
!!@endverbatim
!
!!@n @param jmax         Number of local spherical harmonics mode
!!@n @param fdm2_fix_fld_ctr1(-1:1,3)
!!         Matrix to evaluate radial derivative
!!         for center with fixed field
!
!
      module sph_zero_degree_matrices
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
      subroutine copy_to_band3_mat_w_center(nri, c_evo, mat3, mat00_3)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: c_evo
      real(kind = kreal), intent(in) :: mat3(3,nri)
      real(kind = kreal), intent(inout) :: mat00_3(3,0:nri)
!
      integer(kind = kint) :: k
!
!
      if(c_evo .eq. zero) then
        mat00_3(2,0) = zero
      else
        mat00_3(2,0) = one
      end if
      mat00_3(1,1) = zero
!
      mat00_3(3,0) = zero
      if(c_evo .eq. zero) then
        mat00_3(2,1) = zero
      else
        mat00_3(2,1) = one
      end if
      mat00_3(1,2) = zero
!
      do k = 2, nri-1
        mat00_3(3,k-1) = mat3(3,k-1)
        mat00_3(2,k  ) = mat3(2,k  )
        mat00_3(1,k+1) = mat3(1,k+1)
      end do
      mat00_3(3,nri-1) = mat3(3,nri-1)
      mat00_3(2,nri  ) = mat3(2,nri  )
!
      end subroutine copy_to_band3_mat_w_center
!
! -----------------------------------------------------------------------
!
      subroutine copy_to_band3_mat_no_center(nri, mat3, mat00_3)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: mat3(3,nri)
      real(kind = kreal), intent(inout) :: mat00_3(3,0:nri)
!
      integer(kind = kint) :: k
!
!
      mat00_3(2,0) = one
      mat00_3(1,1) = zero
!
      mat00_3(3,0) = zero
      mat00_3(2,1) = mat3(2,1)
      mat00_3(1,2) = mat3(1,2)
!
      do k = 2, nri-1
        mat00_3(3,k-1) = mat3(3,k-1)
        mat00_3(2,k  ) = mat3(2,k  )
        mat00_3(1,k+1) = mat3(1,k+1)
      end do
      mat00_3(3,nri-1) = mat3(3,nri-1)
      mat00_3(2,nri  ) = mat3(2,nri  )
!
      end subroutine copy_to_band3_mat_no_center
!
! -----------------------------------------------------------------------
!
      subroutine add_scalar_poisson_mat_fill_ctr(nri, r_CTR1,           &
     &          fdm2_fix_dr_center, fdm2_fix_fld_ctr1, coef_p, mat00_3)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: fdm2_fix_dr_center(-1:1,3)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!
      real(kind = kreal), intent(inout) :: mat00_3(3,0:nri)
!
!
!
!      mat00_3(3,-1) = mat00_3(3,-1) - coef_p * fdm2_fix_dr_center(-1,3)
      mat00_3(2,0) = mat00_3(2,0) - coef_p * fdm2_fix_dr_center(0,3)
      mat00_3(1,1) = mat00_3(1,1) - coef_p * fdm2_fix_dr_center(1,3)
!
      mat00_3(3,0) = mat00_3(3,0) - coef_p * (fdm2_fix_fld_ctr1(-1,3)   &
     &                     + two*r_CTR1(1) * fdm2_fix_fld_ctr1(-1,2))
      mat00_3(2,1) = mat00_3(2,1) - coef_p * (fdm2_fix_fld_ctr1(0,3)    &
     &                     + two*r_CTR1(1) * fdm2_fix_fld_ctr1(0,2))
      mat00_3(1,2) = mat00_3(1,2) - coef_p * (fdm2_fix_fld_ctr1(1,3)    &
     &                     + two*r_CTR1(1) * fdm2_fix_fld_ctr1(1,2))
!
      end subroutine add_scalar_poisson_mat_fill_ctr
!
! -----------------------------------------------------------------------
!
      subroutine add_scl_val_dfse_mat_fill_ctr(nri, r_CTR1,             &
     &          fdm2_fix_dr_center, fdm2_fix_fld_ctr1,                  &
     &          coef_p, k_ratio, dk_dr, mat00_3)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: k_ratio, dk_dr
      real(kind = kreal), intent(in) :: fdm2_fix_dr_center(-1:1,3)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!
      real(kind = kreal), intent(inout) :: mat00_3(3,0:nri)
!
!
!
!      mat00_3(3,-1) = mat00_3(3,-1) - coef_p * k_ratio * fdm2_fix_dr_center(-1,3)
      mat00_3(2,0) = mat00_3(2,0)                                       &
     &              - coef_p * k_ratio * fdm2_fix_dr_center( 0,3)
      mat00_3(1,1) = mat00_3(1,1)                                       &
     &              - coef_p * k_ratio * fdm2_fix_dr_center( 1,3)
!
      mat00_3(3,0) = mat00_3(3,0)                                       &
     &              - coef_p * k_ratio * (fdm2_fix_fld_ctr1(-1,3)       &
     &                  + two*r_CTR1(1) * fdm2_fix_fld_ctr1(-1,2))      &
     &                 - coef_p * dk_dr * fdm2_fix_fld_ctr1(-1,2)
      mat00_3(2,1) = mat00_3(2,1)                                       &
     &              - coef_p * k_ratio * (fdm2_fix_fld_ctr1( 0,3)       &
     &                  + two*r_CTR1(1) * fdm2_fix_fld_ctr1( 0,2))      &
     &                 - coef_p * dk_dr * fdm2_fix_fld_ctr1( 0,2)
      mat00_3(1,2) = mat00_3(1,2)                                       &
     &              - coef_p * k_ratio * (fdm2_fix_fld_ctr1( 1,3)       &
     &                  + two*r_CTR1(1) * fdm2_fix_fld_ctr1( 1,2))      &
     &                 - coef_p * dk_dr * fdm2_fix_fld_ctr1( 1,2)
!
      end subroutine add_scl_val_dfse_mat_fill_ctr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_scalar_poisson_mat_fix_ctr(nri, r_CTR1,            &
     &          fdm2_fix_fld_ctr1, coef_p, mat00_3)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!
      real(kind = kreal), intent(inout) :: mat00_3(3,0:nri)
!
!
      mat00_3(2,0) = one
      mat00_3(1,1) = zero
!
      mat00_3(3,0) = mat00_3(3,0) - coef_p * (fdm2_fix_fld_ctr1(-1,3)   &
     &                     + two*r_CTR1(1) * fdm2_fix_fld_ctr1(-1,2))
      mat00_3(2,1) = mat00_3(2,1) - coef_p * (fdm2_fix_fld_ctr1(0,3)    &
     &                     + two*r_CTR1(1) * fdm2_fix_fld_ctr1(0,2))
      mat00_3(1,2) = mat00_3(1,2) - coef_p * (fdm2_fix_fld_ctr1(1,3)    &
     &                     + two*r_CTR1(1) * fdm2_fix_fld_ctr1(1,2))
!
      end subroutine add_scalar_poisson_mat_fix_ctr
!
! -----------------------------------------------------------------------
!
      subroutine add_scl_val_diffuse_mat_fix_ctr(nri, r_CTR1,           &
     &          fdm2_fix_fld_ctr1, coef_p, k_ratio, dk_dr, mat00_3)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: k_ratio, dk_dr
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!
      real(kind = kreal), intent(inout) :: mat00_3(3,0:nri)
!
!
      mat00_3(2,0) = one
      mat00_3(1,1) = zero
!
      mat00_3(3,0) = mat00_3(3,0)                                       &
     &              - coef_p * k_ratio * (fdm2_fix_fld_ctr1(-1,3)       &
     &                  + two*r_CTR1(1) * fdm2_fix_fld_ctr1(-1,2))      &
     &                 - coef_p * dk_dr * fdm2_fix_fld_ctr1(-1,2)
      mat00_3(2,1) = mat00_3(2,1)                                       &
     &              - coef_p * k_ratio * (fdm2_fix_fld_ctr1( 0,3)       &
     &                  + two*r_CTR1(1) * fdm2_fix_fld_ctr1( 0,2))      &
     &                 - coef_p * dk_dr * fdm2_fix_fld_ctr1( 0,2)
      mat00_3(1,2) = mat00_3(1,2)                                       &
     &              - coef_p * k_ratio * (fdm2_fix_fld_ctr1( 1,3)       &
     &                  + two*r_CTR1(1) * fdm2_fix_fld_ctr1( 1,2))      &
     &                 - coef_p * dk_dr * fdm2_fix_fld_ctr1( 1,2)
!
      end subroutine add_scl_val_diffuse_mat_fix_ctr
!
! -----------------------------------------------------------------------
!
      subroutine add_scalar_poisson_mat_no_fld(nri, mat00_3)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(inout) :: mat00_3(3,0:nri)
!
!
      mat00_3(2,0) = one
      mat00_3(1,1) = zero
!
      mat00_3(3,0) = zero
      mat00_3(2,1) = one
      mat00_3(1,2) = zero
!
      end subroutine add_scalar_poisson_mat_no_fld
!
! -----------------------------------------------------------------------
!
      end module sph_zero_degree_matrices
