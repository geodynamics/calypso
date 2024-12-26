!>@file   t_coef_fdm2_scalar_CMB.f90
!!@brief  module t_coef_fdm2_scalar_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!!@n    Modified in Nov., 2013
!
!>@brief Obtain FDM matrix for basic boundary conditions  at boundaries
!!
!!@verbatim
!!      subroutine check_fdm2_coefs_CMB(id_file,                        &
!!     &          fdm2_fix_fld_CMB, fdm2_fix_dr_CMB)
!!        integer(kind = kint), intent(in) :: id_file
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!!
!!      subroutine cal_fdm2_coef_fix_fld_CMB(r_from_CMB2,               &
!!     &                                     fdm2_fix_fld_CMB)
!!      subroutine cal_fdm2_coef_fix_df_CMB(r_from_CMB1,                &
!!     &                                    fdm2_fix_dr_CMB)
!!
!!   Matrix for derivatives with fixed field
!!    at outer boundary of the shell
!!      dfdr =      fdm2_fix_fld_CMB(-2,2) * d_rj(CMB-2)
!!                + fdm2_fix_fld_CMB(-1,2) * d_rj(CMB-1)
!!                + fdm2_fix_fld_CMB( 0,2) * d_rj(CMB  )
!!      d2fdr2 =    fdm2_fix_fld_CMB(-2,3) * d_rj(CMB-2)
!!                + fdm2_fix_fld_CMB(-1,3) * d_rj(CMB-1)
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
!!@n @param r_from_CMB2(-2:0) radius from two next points to CMB
!!@n @param r_from_CMB1(-1:0) radius from next points to CMB
!!
      module t_coef_fdm2_scalar_CMB
!
      use m_precision
!
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
      subroutine check_fdm2_coefs_CMB(id_file,                          &
     &          fdm2_fix_fld_CMB, fdm2_fix_dr_CMB)
!
      integer(kind = kint), intent(in) :: id_file
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
!
      write(id_file,*) ' fdm2_fix_fld_CMB'
      write(id_file,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(id_file,'(1p9E25.15e3)') fdm2_fix_fld_CMB(-2:0,2)
      write(id_file,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(id_file,'(1p9E25.15e3)') fdm2_fix_fld_CMB(-2:0,3)
!
      write(id_file,*) ' fdm2_fix_dr_CMB'
      write(id_file,*) ' mat_fdm11,  mat_fdm12,  mat_fdm13'
      write(id_file,'(1p9E25.15e3)') fdm2_fix_dr_CMB(-1:1,1)
      write(id_file,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(id_file,'(1p9E25.15e3)') fdm2_fix_dr_CMB(-1:1,3)
!
      end subroutine check_fdm2_coefs_CMB
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm2_coef_fix_fld_CMB(r_from_CMB2,                 &
     &                                     fdm2_fix_fld_CMB)
!
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: r_from_CMB2(-2:0)
      real(kind = kreal), intent(inout) :: fdm2_fix_fld_CMB(-2:0,3)
!
!>      Work matrix to evaluate fdm2_fix_fld_CMB(-2:0,3)
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
      fdm2_fix_fld_CMB(-2,1:3) = mat_fdm_CMB_fix_2(1:3,3)
      fdm2_fix_fld_CMB(-1,1:3) = mat_fdm_CMB_fix_2(1:3,2)
      fdm2_fix_fld_CMB( 0,1:3) = mat_fdm_CMB_fix_2(1:3,1)
!
      end subroutine cal_fdm2_coef_fix_fld_CMB
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm2_coef_fix_df_CMB(r_from_CMB1,                  &
     &                                    fdm2_fix_dr_CMB)
!
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: r_from_CMB1(-1:0)
      real(kind = kreal), intent(inout) :: fdm2_fix_dr_CMB(-1:1,3)
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
        write(*,*) 'singular matrix cal_fdm2_coef_fix_df_CMB ',         &
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
      end module t_coef_fdm2_scalar_CMB
