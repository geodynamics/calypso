!>@file   m_coef_fdm_to_center.f90
!!@brief  module m_coef_fdm_to_center
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate radial derivative
!!       toward center
!!
!!@verbatim
!!      subroutine cal_2nd_to_center_fixed_fdm(radius)
!!      subroutine cal_2nd_to_center_fix_df_fdm(radius)
!!
!!      subroutine check_coef_fdm_fix_dr_2ctr
!!
!!   Matrix for derivatives with fixed field
!!      dfdr =      fdm2_fix_fld_center(-1,2) * d_center(0)
!!                + fdm2_fix_fld_center( 0,2) * d_rj(1)
!!                + fdm2_fix_fld_center( 1,2) * d_rj(2)
!!      d2fdr2 =    fdm2_fix_fld_center(-1,3) * d_rj(0)
!!                + fdm2_fix_fld_center( 0,3) * d_rj(1)
!!                + fdm2_fix_fld_center( 1,3) * d_rj(2)
!!
!!   Matrix for field and 2nd derivatives with fixed gradient
!!      (Only used for l = m = 0 component of scalar)
!!      dfdr =      fdm2_fix_dr_center(-1,2) * dfdr(0)
!!                + fdm2_fix_dr_center( 0,2) * d_rj(1)
!!                + fdm2_fix_dr_center( 1,2) * d_rj(2)
!!      d2fdr2 =    fdm2_fix_dr_center(-1,3) * dfdr(0)
!!                + fdm2_fix_dr_center( 0,3) * d_rj(1)
!!                + fdm2_fix_dr_center( 1,3) * d_rj(2)
!!@endverbatim
!!
!!@n @param radius(1:2) radius at two innermost grids
!!
      module m_coef_fdm_to_center
!
      use m_precision
!
      use m_constants
      use cal_inverse_small_matrix
!
      implicit none
!
!
!>      Matrix to evaluate radial derivative at ICB with fiexed field
      real(kind = kreal) :: fdm2_fix_fld_center(-1:1,3)
!>      Matrix to evaluate field at ICB with fiexed radial derivative
      real(kind = kreal) :: fdm2_fix_dr_center(-1:1,3)
!
!
!>      Work matrix to evaluate fdm2_fix_fld_center(-1:1,3)
!!@verbatim
!!      dfdr =      mat_fdm_ctr_fix_2(2,1) * d_rj(0)
!!                + mat_fdm_ctr_fix_2(2,2) * d_rj(1)
!!                + mat_fdm_ctr_fix_2(2,3) * d_rj(2)
!!      d2fdr2 =    mat_fdm_ctr_fix_2(3,1) * d_rj(0)
!!                + mat_fdm_ctr_fix_2(3,2) * d_rj(1)
!!                + mat_fdm_ctr_fix_2(3,3) * d_rj(2)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ctr_fix_2(3,3)
!
!>      Work matrix to evaluate fdm2_fix_dr_center(-1:1,3)
!!@verbatim
!!      dfdr =     mat_fdm_ctr_fix_dr_2(2,1) * d_rj(1)
!!               + mat_fdm_ctr_fix_dr_2(2,2) * dfdr(0)
!!               + mat_fdm_ctr_fix_dr_2(2,3) * d_rj(2)
!!      d2fdr2 =   mat_fdm_ctr_fix_dr_2(3,1) * d_rj(1)
!!               + mat_fdm_ctr_fix_dr_2(3,2) * dfdr(0)
!!               + mat_fdm_ctr_fix_dr_2(3,3) * d_rj(2)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ctr_fix_dr_2(3,3)
!
      private :: mat_fdm_ctr_fix_2, mat_fdm_ctr_fix_dr_2
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_to_center_fixed_fdm(radius)
!
      real(kind = kreal) :: radius(2)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
      real(kind = kreal) :: dr_p1, dr_n1
!
!
      dr_n1 = radius(1)
      dr_p1 = radius(2) - radius(1)
!
      mat_taylor_3(1,1) =  one
      mat_taylor_3(1,2) =  zero
      mat_taylor_3(1,3) =  zero
!
      mat_taylor_3(2,1) =  one
      mat_taylor_3(2,2) = -dr_n1
      mat_taylor_3(2,3) =  dr_n1*dr_n1 / two
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) = dr_p1
      mat_taylor_3(3,3) = dr_p1*dr_p1 / two
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_ctr_fix_2, ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_2nd_to_center_fixed_fdm ',      &
     &            radius(1:2)
      end if
!
      fdm2_fix_fld_center(-1,1:3) = mat_fdm_ctr_fix_2(1:3,2)
      fdm2_fix_fld_center( 0,1:3) = mat_fdm_ctr_fix_2(1:3,1)
      fdm2_fix_fld_center( 1,1:3) = mat_fdm_ctr_fix_2(1:3,3)
!
      end subroutine cal_2nd_to_center_fixed_fdm
!
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_to_center_fix_df_fdm(radius)
!
      real(kind = kreal) :: radius(2)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
      real(kind = kreal) :: dr_p1, dr_n1
!
!
      dr_n1 = radius(1)
      dr_p1 = radius(2) - radius(1)
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) =  zero
      mat_taylor_3(2,2) =  one
      mat_taylor_3(2,3) = -dr_n1
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) = dr_p1
      mat_taylor_3(3,3) = dr_p1*dr_p1 / two
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_ctr_fix_dr_2,    &
     &      ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_2nd_to_center_fix_df_fdm ',     &
     &             radius(1:2)
      end if
!
      fdm2_fix_dr_center(-1,1:3) = mat_fdm_ctr_fix_dr_2(1:3,2)
      fdm2_fix_dr_center( 0,1:3) = mat_fdm_ctr_fix_dr_2(1:3,1)
      fdm2_fix_dr_center( 1,1:3) = mat_fdm_ctr_fix_dr_2(1:3,3)
!
      end subroutine cal_2nd_to_center_fix_df_fdm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_coef_fdm_fix_dr_2ctr
!
!
      write(50,*) ' fdm2_fix_fld_center'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') fdm2_fix_fld_center(-1:1,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') fdm2_fix_fld_center(-1:1,3)
!
      write(50,*) ' fdm2_fix_dr_center'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') fdm2_fix_dr_center(-1:1,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') fdm2_fix_dr_center(-1:1,3)
!
      end subroutine check_coef_fdm_fix_dr_2ctr
!
! -----------------------------------------------------------------------
!
      end module m_coef_fdm_to_center
