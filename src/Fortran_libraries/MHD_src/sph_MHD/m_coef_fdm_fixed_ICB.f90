!>@file   m_coef_fdm_fixed_ICB.f90
!!@brief  module m_coef_fdm_fixed_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate radial derivative at ICB
!!
!!@verbatim
!!      subroutine check_coef_fdm_fix_dr_ICB
!!
!!   Matrix for derivatives with fixed field
!!      dfdr =      coef_fdm_fix_ICB_2( 0,2) * d_rj(ICB  )
!!                + coef_fdm_fix_ICB_2( 1,2) * d_rj(ICB+1)
!!                + coef_fdm_fix_ICB_2( 2,2) * d_rj(ICB+2)
!!      d2fdr2 =    coef_fdm_fix_ICB_2( 0,3) * d_rj(ICB  )
!!                + coef_fdm_fix_ICB_2( 1,3) * d_rj(ICB+1)
!!                + coef_fdm_fix_ICB_2( 2,3) * d_rj(ICB+2)
!!
!!   Matrix for field and 2nd derivatives with fixed gradient
!!      d_rj(k) =   coef_fdm_fix_dr_ICB_2(-1,2) * dfdr(ICB)
!!                + coef_fdm_fix_dr_ICB_2( 0,2) * d_rj(ICB  )
!!                + coef_fdm_fix_dr_ICB_2( 1,2) * d_rj(ICB+1)
!!      d2fdr2 =    coef_fdm_fix_dr_ICB_2(-1,3) * dfdr(ICB)
!!                + coef_fdm_fix_dr_ICB_2( 0,3) * d_rj(ICB  )
!!                + coef_fdm_fix_dr_ICB_2( 1,3) * d_rj(ICB+1)
!!@endverbatim
!!
!!@n @param r_from_ICB(0:2) radius to teo next points of ICB
!!
      module m_coef_fdm_fixed_ICB
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
      real(kind = kreal) :: coef_fdm_fix_ICB_2(0:2,3)
!>      Matrix to evaluate field at ICB with fiexed radial derivative
      real(kind = kreal) :: coef_fdm_fix_dr_ICB_2(-1:1,3)
!
!
!>      Work matrix to evaluate coef_fdm_fix_ICB_2(0:2,3)
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
!>      Work matrix to evaluate coef_fdm_fix_dr_ICB_2(-1:1,3)
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
      private :: mat_fdm_ICB_fix_2, mat_fdm_ICB_fix_dr_2
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_coef_fdm_fix_dr_ICB
!
!
      write(50,*) ' coef_fdm_fix_ICB_2'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') coef_fdm_fix_ICB_2(0:2,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') coef_fdm_fix_ICB_2(0:2,3)
!
      write(50,*) ' coef_fdm_fix_dr_ICB_2'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') coef_fdm_fix_dr_ICB_2(-1:1,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') coef_fdm_fix_dr_ICB_2(-1:1,3)
!
      end subroutine check_coef_fdm_fix_dr_ICB
!
! -----------------------------------------------------------------------
!
      end module m_coef_fdm_fixed_ICB
