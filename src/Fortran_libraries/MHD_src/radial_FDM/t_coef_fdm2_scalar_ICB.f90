!>@file   t_coef_fdm2_scalar_ICB.f90
!!@brief  module t_coef_fdm2_scalar_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!!@n    Modified in Nov., 2013
!
!>@brief Obtain FDM matrix for basic boundary conditions  at boundaries
!!
!!@verbatim
!!      subroutine check_fdm2_coefs_ICB(id_file,                        &
!!     &          fdm2_fix_fld_ICB, fdm2_fix_dr_ICB)
!!        integer(kind = kint), intent(in) :: id_file
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!!
!!      subroutine cal_fdm2_coef_fix_fld_ICB(r_from_ICB,                &
!!     &                                     fdm2_fix_fld_ICB)
!!      subroutine cal_fdm2_coef_fix_df_ICB(r_from_ICB,                 &
!!     &                                    fdm2_fix_dr_ICB)
!!
!!   Matrix for derivatives with fixed field
!!    at inner boundary of the shell
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
!!@endverbatim
!!
!!@n @param r_from_ICB(0:2) radius to teo next points of ICB
!!
      module t_coef_fdm2_scalar_ICB
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
      subroutine check_fdm2_coefs_ICB(id_file,                          &
     &          fdm2_fix_fld_ICB, fdm2_fix_dr_ICB)
!
      integer(kind = kint), intent(in) :: id_file
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
!
      write(id_file,*) ' fdm2_fix_fld_ICB'
      write(id_file,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(id_file,'(1p9E25.15e3)') fdm2_fix_fld_ICB(0:2,2)
      write(id_file,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(id_file,'(1p9E25.15e3)') fdm2_fix_fld_ICB(0:2,3)
!
      write(id_file,*) ' fdm2_fix_dr_ICB'
      write(id_file,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(id_file,'(1p9E25.15e3)') fdm2_fix_dr_ICB(-1:1,2)
      write(id_file,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(id_file,'(1p9E25.15e3)') fdm2_fix_dr_ICB(-1:1,3)
!
      end subroutine check_fdm2_coefs_ICB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_fdm2_coef_fix_fld_ICB(r_from_ICB,                  &
     &                                     fdm2_fix_fld_ICB)
!
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: r_from_ICB(0:2)
      real(kind = kreal), intent(inout) :: fdm2_fix_fld_ICB(0:2,3)
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
     &                                    fdm2_fix_dr_ICB)
!
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: r_from_ICB(0:1)
      real(kind = kreal), intent(inout) :: fdm2_fix_dr_ICB(-1:1,3)
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
!
      end module t_coef_fdm2_scalar_ICB
