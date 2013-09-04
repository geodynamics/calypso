!>@file   m_coef_fdm_fixed_CMB.f90
!!@brief  module m_coef_fdm_fixed_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate radial derivative at CMB 
!!
!!@verbatim
!!      subroutine cal_2nd_nod_CMB_fixed_fdm(r_from_CMB2)
!!      subroutine cal_2nd_nod_CMB_fix_df_fdm(r_from_CMB1)
!!
!!      subroutine check_coef_fdm_fix_dr_CMB
!!
!!   Matrix for derivatives with fixed field
!!      dfdr =      coef_fdm_fix_CMB_2( 2,2) * d_rj(CMB-2)
!!                + coef_fdm_fix_CMB_2( 1,2) * d_rj(CMB-1)
!!                + coef_fdm_fix_CMB_2( 0,2) * d_rj(CMB  )
!!      d2fdr2 =    coef_fdm_fix_CMB_2( 2,3) * d_rj(CMB-2)
!!                + coef_fdm_fix_CMB_2( 1,3) * d_rj(CMB-1)
!!                + coef_fdm_fix_CMB_2( 0,3) * d_rj(CMB  )
!!
!!   Matrix for field and 2nd derivatives with fixed gradient
!!      d_rj(k) =  coef_fdm_fix_dr_CMB_2(-1,1) * d_rj(CMB-1)
!!               + coef_fdm_fix_dr_CMB_2( 0,1) * d_rj(CMB  )
!!               + coef_fdm_fix_dr_CMB_2( 1,1) * dfdr(CMB)
!!      d2fdr2 =   coef_fdm_fix_dr_CMB_2(-1,3) * d_rj(CMB-1)
!!               + coef_fdm_fix_dr_CMB_2( 0,3) * d_rj(CMB  )
!!               + coef_fdm_fix_dr_CMB_2( 1,3) * dfdr(CMB)
!!@endverbatim
!!
!!@n @param r_from_CMB2(-2:0) radius from two next points to CMB
!!@n @param r_from_CMB1(-1:0) radius from next points to CMB
!!
      module m_coef_fdm_fixed_CMB
!
      use m_precision
!
      use m_constants
      use cal_inverse_small_matrix
!
      implicit none
!
!
!>      Matrix to evaluate radial derivative at CMB with fiexed field
      real(kind = kreal) :: coef_fdm_fix_CMB_2(0:2,3)
!
!>      Matrix to evaluate field at CMB with fiexed radial derivative
      real(kind = kreal) :: coef_fdm_fix_dr_CMB_2(-1:1,3)
!
!
!>      Work matrix to evaluate coef_fdm_fix_CMB_2(0:2,3)
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
!>      Work matrix to evaluate coef_fdm_fix_dr_CMB_2(-1:1,3)
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
      private :: mat_fdm_CMB_fix_2, mat_fdm_CMB_fix_dr_2
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_nod_CMB_fixed_fdm(r_from_CMB2)
!
      real(kind = kreal) :: r_from_CMB2(-2:0)
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
        write(*,*) 'singular matrix cal_2nd_nod_CMB_fixed_fdm ',        &
     &            r_from_CMB2(0)
      end if
!
      coef_fdm_fix_CMB_2(2,1:3) = mat_fdm_CMB_fix_2(1:3,3)
      coef_fdm_fix_CMB_2(1,1:3) = mat_fdm_CMB_fix_2(1:3,2)
      coef_fdm_fix_CMB_2(0,1:3) = mat_fdm_CMB_fix_2(1:3,1)
!
      end subroutine cal_2nd_nod_CMB_fixed_fdm
!
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_nod_CMB_fix_df_fdm(r_from_CMB1)
!
      real(kind = kreal) :: r_from_CMB1(-1:0)
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
        write(*,*) 'singular matrix cal_2nd_nod_CMB_fix_df_fdm ',       &
     &             r_from_CMB1(0)
      end if
!
      coef_fdm_fix_dr_CMB_2(-1,1:3) = mat_fdm_CMB_fix_dr_2(1:3,3)
      coef_fdm_fix_dr_CMB_2( 0,1:3) = mat_fdm_CMB_fix_dr_2(1:3,1)
      coef_fdm_fix_dr_CMB_2( 1,1:3) = mat_fdm_CMB_fix_dr_2(1:3,2)
!
      end subroutine cal_2nd_nod_CMB_fix_df_fdm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_coef_fdm_fix_dr_CMB
!
!
      write(50,*) ' coef_fdm_fix_CMB_2'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') coef_fdm_fix_CMB_2(0:2,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') coef_fdm_fix_CMB_2(0:2,3)
!
      write(50,*) ' coef_fdm_fix_dr_CMB_2'
      write(50,*) ' mat_fdm11,  mat_fdm12,  mat_fdm13'
      write(50,'(1p9E25.15e3)') coef_fdm_fix_dr_CMB_2(-1:1,1)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') coef_fdm_fix_dr_CMB_2(-1:1,3)
!
      end subroutine check_coef_fdm_fix_dr_CMB
!
! -----------------------------------------------------------------------
!
      end module m_coef_fdm_fixed_CMB
