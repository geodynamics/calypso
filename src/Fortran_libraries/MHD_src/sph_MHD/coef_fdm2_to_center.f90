!>@file   coef_fdm2_to_center.f90
!!@brief  module coef_fdm2_to_center
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate radial derivative
!!       toward center
!!
!!@verbatim
!!      subroutine cal_2nd_to_center_fixed_fdm(radius, fdm2_center)
!!      subroutine cal_2nd_center_fix_df_fdm(radius, fdm2_center)
!!      subroutine cal_2nd_center_fixed_fdm(radius, fdm2_center)
!!        type(fdm2_center_mat), intent(inout) :: fdm2_center
!!
!!   Matrix for derivatives with fixed field
!!      dfdr =      fdm2_center%dmat_fix_fld(-1,2) * d_center(0)
!!                + fdm2_center%dmat_fix_fld( 0,2) * d_rj(1)
!!                + fdm2_center%dmat_fix_fld( 1,2) * d_rj(2)
!!      d2fdr2 =    fdm2_center%dmat_fix_fld(-1,3) * d_center(0)
!!                + fdm2_center%dmat_fix_fld( 0,3) * d_rj(1)
!!                + fdm2_center%dmat_fix_fld( 1,3) * d_rj(2)
!!
!!      Matrix to evaluate field at center fixed radial derivative
!!      (Only used for l = m = 0 component of scalar)
!!      d_center(0) =fdm2_center%dmat_fix_dr(-1,1) * dfdr(0)
!!                 + fdm2_center%dmat_fix_dr( 0,1) * d_center(0)
!!                 + fdm2_center%dmat_fix_dr( 1,1) * d_rj(1)
!!      d2fdr2(0) =  fdm2_center%dmat_fix_dr(-1,3) * dfdr(0)
!!                 + fdm2_center%dmat_fix_dr( 0,3) * d_center(0)
!!                 + fdm2_center%dmat_fix_dr( 1,3) * d_rj(1)
!!
!!      Matrix to evaluate field at center fixed field
!!      (Only used for l = m = 0 component of scalar)
!!      dfdr(0) =    fdm2_center%dmat_fixed( 0,2) * d_center(0)
!!                 + fdm2_center%dmat_fixed( 1,2) * d_rj(1)
!!                 + fdm2_center%dmat_fixed( 2,2) * d_rj(2)
!!      d2fdr2(0) =  fdm2_center%dmat_fixed( 0,3) * d_center(0)
!!                 + fdm2_center%dmat_fixed( 1,3) * d_rj(1)
!!                 + fdm2_center%dmat_fixed( 2,3) * d_rj(2)
!!@endverbatim
!!
!!@n @param radius(1:2) radius at two innermost grids
!!
      module coef_fdm2_to_center
!
      use m_precision
      use m_constants
!
      use t_coef_fdm2_MHD_boundaries
      use cal_inverse_small_matrix
!
      implicit none
!
!
!>      Work matrix to evaluate fdm2_center%dmat_fix_fld(-1:1,3)
!!@verbatim
!!      dfdr =      mat_fdm_ctr_fix_2(2,1) * d_center(0)
!!                + mat_fdm_ctr_fix_2(2,2) * d_rj(1)
!!                + mat_fdm_ctr_fix_2(2,3) * d_rj(2)
!!      d2fdr2 =    mat_fdm_ctr_fix_2(3,1) * d_center(0)
!!                + mat_fdm_ctr_fix_2(3,2) * d_rj(1)
!!                + mat_fdm_ctr_fix_2(3,3) * d_rj(2)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ctr_fix_2(3,3)
!
!>      Work matrix to evaluate fdm2_center%dmat_fix_dr(-1:1,3)
!!@verbatim
!!      dfdr =     mat_fdm_ctr_fix_dr_2(2,1) * d_center(0)
!!               + mat_fdm_ctr_fix_dr_2(2,2) * dfdr(0)
!!               + mat_fdm_ctr_fix_dr_2(2,3) * d_rj(1)
!!      d2fdr2 =   mat_fdm_ctr_fix_dr_2(3,1) * d_center(0)
!!               + mat_fdm_ctr_fix_dr_2(3,2) * dfdr(0)
!!               + mat_fdm_ctr_fix_dr_2(3,3) * d_rj(1)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ctr_fix_dr_2(3,3)
!
!>      Work matrix to evaluate fdm2_center%dmat_fixed(-1:1,3)
!!@verbatim
!!      dfdr =     mat_fdm_ctr_fixed_2(2,1) * d_center(0)
!!               + mat_fdm_ctr_fixed_2(2,2) * d_rj(0)
!!               + mat_fdm_ctr_fixed_2(2,3) * d_rj(2)
!!      d2fdr2 =   mat_fdm_ctr_fixed_2(3,1) * d_center(0)
!!               + mat_fdm_ctr_fixed_2(3,2) * d_rj(0)
!!               + mat_fdm_ctr_fixed_2(3,3) * d_rj(2)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ctr_fixed_2(3,3)
!
      private :: mat_fdm_ctr_fix_2, mat_fdm_ctr_fix_dr_2
      private :: mat_fdm_ctr_fixed_2
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_to_center_fixed_fdm(radius, fdm2_center)
!
      real(kind = kreal), intent(in) :: radius(2)
      type(fdm2_center_mat), intent(inout) :: fdm2_center
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
      fdm2_center%dmat_fix_fld(-1,1:3) = mat_fdm_ctr_fix_2(1:3,2)
      fdm2_center%dmat_fix_fld( 0,1:3) = mat_fdm_ctr_fix_2(1:3,1)
      fdm2_center%dmat_fix_fld( 1,1:3) = mat_fdm_ctr_fix_2(1:3,3)
!
      end subroutine cal_2nd_to_center_fixed_fdm
!
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_center_fix_df_fdm(radius, fdm2_center)
!
      real(kind = kreal), intent(in) :: radius
      type(fdm2_center_mat), intent(inout) :: fdm2_center
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
!
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) =  zero
      mat_taylor_3(2,2) =  one
      mat_taylor_3(2,3) =  zero
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) = radius
      mat_taylor_3(3,3) = radius*radius / two
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_ctr_fix_dr_2,    &
     &      ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_2nd_center_fix_df_fdm ',        &
     &             radius
      end if
!
      fdm2_center%dmat_fix_dr(-1,1:3) = mat_fdm_ctr_fix_dr_2(1:3,2)
      fdm2_center%dmat_fix_dr( 0,1:3) = mat_fdm_ctr_fix_dr_2(1:3,1)
      fdm2_center%dmat_fix_dr( 1,1:3) = mat_fdm_ctr_fix_dr_2(1:3,3)
!
      end subroutine cal_2nd_center_fix_df_fdm
!
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_center_fixed_fdm(radius, fdm2_center)
!
      real(kind = kreal), intent(in) :: radius(2)
      type(fdm2_center_mat), intent(inout) :: fdm2_center
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
!
      real(kind = kreal) :: dr_p1, dr_p2
!
!
      dr_p1 = radius(1)
      dr_p2 = radius(2)
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) =  one
      mat_taylor_3(2,2) =  dr_p1
      mat_taylor_3(2,3) =  dr_p1*dr_p1 / two
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) = dr_p2
      mat_taylor_3(3,3) = dr_p2*dr_p2 / two
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_ctr_fixed_2,     &
     &      ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_2nd_center_fixed_fdm ',         &
     &             radius
      end if
!
      fdm2_center%dmat_fixed( 0,1:3) = mat_fdm_ctr_fixed_2(1:3,1)
      fdm2_center%dmat_fixed( 1,1:3) = mat_fdm_ctr_fixed_2(1:3,2)
      fdm2_center%dmat_fixed( 2,1:3) = mat_fdm_ctr_fixed_2(1:3,3)
!
      end subroutine cal_2nd_center_fixed_fdm
!
! -----------------------------------------------------------------------
!
      end module coef_fdm2_to_center
