!>@file   cal_fdm_coefs_4_boundaries.f90
!!@brief  module cal_fdm_coefs_4_boundaries
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!!@n    Modified in Nov., 2013
!
!>@brief Obtain FDM matrix for basic boundary conditions  at boundaries
!!
!!@verbatim
!!      subroutine cal_fdm1_coef_fix_fld_ICB(r_from_ICB,                &
!!     &          fdm1_fix_fld_ICB)
!!
!!      subroutine cal_fdm2_coef_fix_fld_ICB(r_from_ICB,                &
!!     &          fdm2_fix_fld_ICB)
!!      subroutine cal_fdm2_coef_fix_df_ICB(r_from_ICB,                 &
!!     &          fdm2_fix_dr_ICB)
!!
!!      subroutine cal_fdm1_coef_fix_fld_CMB(r_from_CMB1,               &
!!     &          fdm1_fix_fld_CMB)
!!      subroutine cal_fdm2_coef_fix_fld_CMB(r_from_CMB2,               &
!!     &          fdm2_fix_fld_CMB)
!!      subroutine cal_fdm2_coef_fix_df_CMB(r_from_CMB1,                &
!!     &          fdm2_fix_dr_CMB)
!!
!!   Matrix for derivatives with fixed field
!!    at inner boundary of the shell
!!      dfdr =      fdm1_fix_fld_ICB( 0,2) * d_rj(ICB  )
!!                + fdm1_fix_fld_ICB( 1,2) * d_rj(ICB+1)
!!
!!      dfdr =      fdm2_fix_fld_ICB( 0,2) * d_rj(ICB  )
!!                + fdm2_fix_fld_ICB( 1,2) * d_rj(ICB+1)
!!                + fdm2_fix_fld_ICB( 2,2) * d_rj(ICB+2)
!!      d2fdr2 =    fdm2_fix_fld_ICB( 0,3) * d_rj(ICB  )
!!                + fdm2_fix_fld_ICB( 1,3) * d_rj(ICB+1)
!!                + fdm2_fix_fld_ICB( 2,3) * d_rj(ICB+2)
!!
!!   Matrix for field and 2nd derivatives with fixed gradient
!!    at inner boundary of the shell
!!      d_rj(k) =   fdm2_fix_dr_ICB(-1,2) * dfdr(ICB)
!!                + fdm2_fix_dr_ICB( 0,2) * d_rj(ICB  )
!!                + fdm2_fix_dr_ICB( 1,2) * d_rj(ICB+1)
!!      d2fdr2 =    fdm2_fix_dr_ICB(-1,3) * dfdr(ICB)
!!                + fdm2_fix_dr_ICB( 0,3) * d_rj(ICB  )
!!                + fdm2_fix_dr_ICB( 1,3) * d_rj(ICB+1)
!!
!!   Matrix for derivatives with fixed field
!!    at outer boundary of the shell
!!      dfdr =      fdm1_fix_fld_CMB( 1,2) * d_rj(CMB-1)
!!                + fdm1_fix_fld_CMB( 0,2) * d_rj(CMB  )
!!
!!      dfdr =      fdm2_fix_fld_CMB( 2,2) * d_rj(CMB-2)
!!                + fdm2_fix_fld_CMB( 1,2) * d_rj(CMB-1)
!!                + fdm2_fix_fld_CMB( 0,2) * d_rj(CMB  )
!!      d2fdr2 =    fdm2_fix_fld_CMB( 2,3) * d_rj(CMB-2)
!!                + fdm2_fix_fld_CMB( 1,3) * d_rj(CMB-1)
!!                + fdm2_fix_fld_CMB( 0,3) * d_rj(CMB  )
!!
!!   Matrix for field and 2nd derivatives with fixed gradient
!!    at outer boundary of the shell
!!      d_rj(k) =  fdm2_fix_dr_CMB(-1,1) * d_rj(CMB-1)
!!               + fdm2_fix_dr_CMB( 0,1) * d_rj(CMB  )
!!               + fdm2_fix_dr_CMB( 1,1) * dfdr(CMB)
!!      d2fdr2 =   fdm2_fix_dr_CMB(-1,3) * d_rj(CMB-1)
!!               + fdm2_fix_dr_CMB( 0,3) * d_rj(CMB  )
!!               + fdm2_fix_dr_CMB( 1,3) * dfdr(CMB)
!!@endverbatim
!!
!!@n @param r_from_ICB(0:2) radius to teo next points of ICB
!!@n @param r_from_CMB2(-2:0) radius from two next points to CMB
!!@n @param r_from_CMB1(-1:0) radius from next points to CMB
!!
      module cal_fdm_coefs_4_boundaries
!
      use m_precision
!
      use m_constants
      use cal_inverse_small_matrix
!
      implicit none
!
!>      Work matrix to evaluate fdm2_fix_fld_ICB(0:2,3)
!!@verbatim
!!      dfdr =      mat_fdm_ICB_fix_2(2,1) * d_rj(ICB  )
!!                + mat_fdm_ICB_fix_2(2,2) * d_rj(ICB+1)
!!                + mat_fdm_ICB_fix_2(2,3) * d_rj(ICB+2)
!!      d2fdr2 =    mat_fdm_ICB_fix_2(3,1) * d_rj(ICB  )
!!                + mat_fdm_ICB_fix_2(3,2) * d_rj(ICB+1)
!!                + mat_fdm_ICB_fix_2(3,3) * d_rj(ICB+2)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ICB_fix_2(3,3)
!
!>      Work matrix to evaluate fdm2_fix_dr_ICB(-1:1,3)
!!@verbatim
!!      d_rj(k) =  mat_fdm_ICB_fix_dr_2(2,1) * d_rj(ICB  )
!!               + mat_fdm_ICB_fix_dr_2(2,2) * dfdr(ICB)
!!               + mat_fdm_ICB_fix_dr_2(2,3) * d_rj(ICB+1)
!!      d2fdr2 =   mat_fdm_ICB_fix_dr_2(3,1) * d_rj(ICB  )
!!               + mat_fdm_ICB_fix_dr_2(3,2) * dfdr(ICB)
!!               + mat_fdm_ICB_fix_dr_2(3,3) * d_rj(ICB+1)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ICB_fix_dr_2(3,3)
!
!>      Work matrix to evaluate fdm2_fix_fld_CMB(0:2,3)
!!@verbatim
!!      dfdr =      mat_fdm_CMB_fix_2(2,1) * d_rj(CMB  )
!!                + mat_fdm_CMB_fix_2(2,2) * d_rj(CMB-1)
!!                + mat_fdm_CMB_fix_2(2,3) * d_rj(CMB-2)
!!      d2fdr2 =    mat_fdm_CMB_fix_2(3,1) * d_rj(CMB  )
!!                + mat_fdm_CMB_fix_2(3,2) * d_rj(CMB-1)
!!                + mat_fdm_CMB_fix_2(3,3) * d_rj(CMB-2)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_CMB_fix_2(3,3)
!
!>      Work matrix to evaluate fdm2_fix_dr_CMB(-1:1,3)
!!@verbatim
!!      d_rj(k) =  mat_fdm_CMB_fix_dr_2(1,1) * d_rj(CMB  )
!!               + mat_fdm_CMB_fix_dr_2(1,2) * dfdr(CMB)
!!               + mat_fdm_CMB_fix_dr_2(1,3) * d_rj(CMB-1)
!!      d2fdr2 =   mat_fdm_CMB_fix_dr_2(3,1) * d_rj(CMB  )
!!               + mat_fdm_CMB_fix_dr_2(3,2) * dfdr(CMB)
!!               + mat_fdm_CMB_fix_dr_2(3,3) * d_rj(CMB-1)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_CMB_fix_dr_2(3,3)
!
      private :: mat_fdm_ICB_fix_2, mat_fdm_ICB_fix_dr_2
      private :: mat_fdm_CMB_fix_2, mat_fdm_CMB_fix_dr_2
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm1_coef_fix_fld_ICB(r_from_ICB,                  &
     &          fdm1_fix_fld_ICB)
!
      real(kind = kreal), intent(in) :: r_from_ICB(0:1)
      real(kind = kreal), intent(inout) :: fdm1_fix_fld_ICB(0:1,2)
!
      real(kind = kreal) :: dr_p1
!
!
      dr_p1 = r_from_ICB(1) - r_from_ICB(0)
!
      fdm1_fix_fld_ICB(0,1) =  one
      fdm1_fix_fld_ICB(1,1) =  zero
      fdm1_fix_fld_ICB(0,2) = -one / dr_p1
      fdm1_fix_fld_ICB(1,2) =  one / dr_p1
!
      end subroutine cal_fdm1_coef_fix_fld_ICB
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm2_coef_fix_fld_ICB(r_from_ICB,                  &
     &          fdm2_fix_fld_ICB)
!
      real(kind = kreal), intent(in) :: r_from_ICB(0:2)
      real(kind = kreal), intent(inout) :: fdm2_fix_fld_ICB(0:2,3)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
      real(kind = kreal) :: dr_p1, dr_p2
!
!
      dr_p1 = r_from_ICB(1) - r_from_ICB(0)
      dr_p2 = r_from_ICB(2) - r_from_ICB(0)
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) = one
      mat_taylor_3(2,2) = dr_p1
      mat_taylor_3(2,3) = dr_p1*dr_p1 / two
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) = dr_p2
      mat_taylor_3(3,3) = dr_p2*dr_p2 / two
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_ICB_fix_2, ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_fdm2_coef_fix_fld_ICB ',        &
     &            r_from_ICB(0)
      end if
!
      fdm2_fix_fld_ICB(0,1:3) = mat_fdm_ICB_fix_2(1:3,1)
      fdm2_fix_fld_ICB(1,1:3) = mat_fdm_ICB_fix_2(1:3,2)
      fdm2_fix_fld_ICB(2,1:3) = mat_fdm_ICB_fix_2(1:3,3)
!
      end subroutine cal_fdm2_coef_fix_fld_ICB
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm2_coef_fix_df_ICB(r_from_ICB,                   &
     &          fdm2_fix_dr_ICB)
!
      real(kind = kreal), intent(in) :: r_from_ICB(0:1)
      real(kind = kreal), intent(inout) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
      real(kind = kreal) :: dr_p1
!
!
      dr_p1 = r_from_ICB(1) - r_from_ICB(0)
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) = zero
      mat_taylor_3(2,2) = one
      mat_taylor_3(2,3) = zero
!
      mat_taylor_3(3,1) = dr_p1
      mat_taylor_3(3,2) = one
      mat_taylor_3(3,3) = dr_p1*dr_p1 / two
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_ICB_fix_dr_2,    &
     &      ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_fdm2_coef_fix_df_ICB ',         &
     &             r_from_ICB(0)
      end if
!
      fdm2_fix_dr_ICB(-1,1:3) = mat_fdm_ICB_fix_dr_2(1:3,1)
      fdm2_fix_dr_ICB( 0,1:3) = mat_fdm_ICB_fix_dr_2(1:3,2)
      fdm2_fix_dr_ICB( 1,1:3) = mat_fdm_ICB_fix_dr_2(1:3,3)
!
      end subroutine cal_fdm2_coef_fix_df_ICB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_fdm1_coef_fix_fld_CMB(r_from_CMB1,                 &
     &          fdm1_fix_fld_CMB)
!
      real(kind = kreal), intent(in) :: r_from_CMB1(-1:0)
      real(kind = kreal), intent(inout) :: fdm1_fix_fld_CMB(0:1,2)
!
      real(kind = kreal) :: dr_n1
!
!
      dr_n1 = r_from_CMB1(0) - r_from_CMB1(-1)
!
      fdm1_fix_fld_CMB(1,1) = zero
      fdm1_fix_fld_CMB(0,1) = one
      fdm1_fix_fld_CMB(1,2) = - one / dr_n1
      fdm1_fix_fld_CMB(0,2) =   one / dr_n1
!
      end subroutine cal_fdm1_coef_fix_fld_CMB
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm2_coef_fix_fld_CMB(r_from_CMB2,                 &
     &          fdm2_fix_fld_CMB)
!
      real(kind = kreal), intent(in) :: r_from_CMB2(-2:0)
      real(kind = kreal), intent(inout) :: fdm2_fix_fld_CMB(0:2,3)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
      real(kind = kreal) :: dr_n1, dr_n2
!
!
      dr_n1 = r_from_CMB2(0) - r_from_CMB2(-1)
      dr_n2 = r_from_CMB2(0) - r_from_CMB2(-2)
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) = one
      mat_taylor_3(2,2) =-dr_n1
      mat_taylor_3(2,3) = dr_n1*dr_n1 / two
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) =-dr_n2
      mat_taylor_3(3,3) = dr_n2*dr_n2 / two
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_CMB_fix_2, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_fdm2_coef_fix_fld_CMB ',        &
     &            r_from_CMB2(0)
      end if
!
      fdm2_fix_fld_CMB(2,1:3) = mat_fdm_CMB_fix_2(1:3,3)
      fdm2_fix_fld_CMB(1,1:3) = mat_fdm_CMB_fix_2(1:3,2)
      fdm2_fix_fld_CMB(0,1:3) = mat_fdm_CMB_fix_2(1:3,1)
!
      end subroutine cal_fdm2_coef_fix_fld_CMB
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm2_coef_fix_df_CMB(r_from_CMB1,                  &
     &          fdm2_fix_dr_CMB)
!
      real(kind = kreal), intent(in) :: r_from_CMB1(-1:0)
      real(kind = kreal), intent(inout) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
      real(kind = kreal) :: dr_n1
!
!
      dr_n1 = r_from_CMB1(0) - r_from_CMB1(-1)
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) = zero
      mat_taylor_3(2,2) = one
      mat_taylor_3(2,3) = zero
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) =-dr_n1
      mat_taylor_3(3,3) = dr_n1*dr_n1 / two
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_CMB_fix_dr_2,    &
     &      ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_fdm2_coef_fix_df_CMB ',       &
     &             r_from_CMB1(0)
      end if
!
      fdm2_fix_dr_CMB(-1,1:3) = mat_fdm_CMB_fix_dr_2(1:3,3)
      fdm2_fix_dr_CMB( 0,1:3) = mat_fdm_CMB_fix_dr_2(1:3,1)
      fdm2_fix_dr_CMB( 1,1:3) = mat_fdm_CMB_fix_dr_2(1:3,2)
!
      end subroutine cal_fdm2_coef_fix_df_CMB
!
! -----------------------------------------------------------------------
!
      end module cal_fdm_coefs_4_boundaries
