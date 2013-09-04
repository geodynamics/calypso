!>@file   m_coef_fdm_free_CMB.f90
!!@brief  module m_coef_fdm_free_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate poloidal velocity and toroidal vorticity
!!       at CMB with free slip boundary
!!
!!@verbatim
!!      subroutine cal_2nd_CMB_free_vp_bc_fdm(r_from_CMB)
!!      subroutine cal_2nd_CMB_free_vt_bc_fdm(r_from_CMB)
!!
!!      subroutine check_coef_fdm_free_CMB
!!
!!    Matrix to evaluate radial derivative of poloidal velocity
!!    at CMB with free slip boundary
!!      dfdr =    coef_fdm_free_CMB_vp2(-1,2) * d_rj(CMB-1)
!!              + coef_fdm_free_CMB_vp2( 0,2) * d_rj(CMB  )
!!      d2fdr2 =  coef_fdm_free_CMB_vp2(-1,3) * d_rj(CMB-1)
!!              + coef_fdm_free_CMB_vp2( 0,3) * d_rj(CMB  )
!!
!!    Matrix to evaluate radial derivative of toroidal vorticity
!!    at CMB with free slip boundary
!!      dfdr =    coef_fdm_free_CMB_vt2( 0,2) * d_rj(CMB  )
!!      d2fdr2 =  coef_fdm_free_CMB_vt2(-1,3) * d_rj(CMB-1)
!!              + coef_fdm_free_CMB_vt2( 0,3) * d_rj(CMB  )
!!
!!    Taylor expansion of free slip boundary at CMB
!!      dfdr =    mat_fdm_2(2,1) * d_rj(CMB  )
!!              + mat_fdm_2(2,2)
!!                 * (-2*dfdr(CMB) + r(CMB) * d2fdr2(CMB))
!!              + mat_fdm_2(2,3) * d_rj(CMB-1)
!!      d2fdr2 =  mat_fdm_2(3,1) * d_rj(CMB  )
!!              + mat_fdm_2(3,2)
!!                 * (-2*dfdr(CMB) + r(CMB) * d2fdr2(CMB))
!!              + mat_fdm_2(3,3) * d_rj(CMB-1)
!!@endverbatim
!!
!!@n @param r_from_CMB(-3:0) radius from next points of CMB
!
      module m_coef_fdm_free_CMB
!
      use m_precision
!
      use m_constants
      use cal_inverse_small_matrix
!
      implicit none
!
!>      Matrix to evaluate radial derivative of poloidal velocity
!!      at CMB with free slip boundary
      real(kind = kreal) :: coef_fdm_free_CMB_vp2(-1:0,3)
!>      Matrix to evaluate radial derivative of toroidal vorticity
!!      at CMB with free slip boundary
      real(kind = kreal) :: coef_fdm_free_CMB_vt2(-1:0,3)
!
!
!>      Work matrix to evaluate coef_fdm_free_CMB_vp2(-1:0,3)
!!@verbatim
!!      dsdr =    mat_fdm_CMB_free_vp(2,1) * d_rj(ICB  )
!!              + mat_fdm_CMB_free_vp(2,3) * d_rj(ICB+1)
!!      dsfdr2 =  mat_fdm_CMB_free_vp(3,1) * d_rj(ICB  )
!!              + mat_fdm_CMB_free_vp(3,3) * d_rj(ICB+1)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_CMB_free_vp(3,3)
!
!>      Work matrix to evaluate coef_fdm_free_CMB_vt2(-1:0,3)
!!@verbatim
!!      dtdr =    mat_fdm_CMB_free_vt(2,1) * d_rj(ICB  )
!!              + mat_fdm_CMB_free_vt(2,3) * d_rj(ICB+1)
!!      dtfdr2 =  mat_fdm_CMB_free_vt(3,1) * d_rj(ICB  )
!!              + mat_fdm_CMB_free_vt(3,3) * d_rj(ICB+1)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_CMB_free_vt(3,3)
!
      private :: mat_fdm_CMB_free_vp, mat_fdm_CMB_free_vt
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_CMB_free_vp_bc_fdm(r_from_CMB)
!
      real(kind = kreal) :: r_from_CMB(-1:0)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
      real(kind = kreal) :: dr_n1, r0, r1
!
!
      dr_n1 = r_from_CMB(0) - r_from_CMB(-1)
      r0 = r_from_CMB(0)
      r1 = r_from_CMB(-1)
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) = one
      mat_taylor_3(2,2) = -r0
      mat_taylor_3(2,3) = half * r0*r0
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) =-dr_n1
      mat_taylor_3(3,3) = half * dr_n1*dr_n1
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_CMB_free_vp,     &
     &      ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix free slip CMB mat_vp ',             &
     &            r_from_CMB(0)
      end if
!
      coef_fdm_free_CMB_vp2(0, 1) = one
      coef_fdm_free_CMB_vp2(-1,1) = zero
      coef_fdm_free_CMB_vp2(0, 2) = mat_fdm_CMB_free_vp(2,1)
      coef_fdm_free_CMB_vp2(-1,2) = mat_fdm_CMB_free_vp(2,3)
      coef_fdm_free_CMB_vp2(0, 3) = mat_fdm_CMB_free_vp(3,1)
      coef_fdm_free_CMB_vp2(-1,3) = mat_fdm_CMB_free_vp(3,3)
!
      end subroutine cal_2nd_CMB_free_vp_bc_fdm
!
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_CMB_free_vt_bc_fdm(r_from_CMB)
!
      real(kind = kreal) :: r_from_CMB(-1:0)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
      real(kind = kreal) :: dr_n1, r0, r1
!
!
      dr_n1 = r_from_CMB(0) - r_from_CMB(-1)
      r0 = r_from_CMB( 0)
      r1 = r_from_CMB(-1)
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) = two
      mat_taylor_3(2,2) = -r0
      mat_taylor_3(2,3) = zero
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) =-dr_n1
      mat_taylor_3(3,3) = half * dr_n1*dr_n1
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_CMB_free_vt,     &
     &      ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix free slip CMB mat_vt ',             &
     &            r_from_CMB(0)
      end if
!
      coef_fdm_free_CMB_vt2(0, 1) = one
      coef_fdm_free_CMB_vt2(-1,1) = zero
      coef_fdm_free_CMB_vt2(0, 2) = mat_fdm_CMB_free_vt(2,1)
      coef_fdm_free_CMB_vt2(-1,2) = zero
      coef_fdm_free_CMB_vt2(0, 3) = mat_fdm_CMB_free_vt(3,1)
      coef_fdm_free_CMB_vt2(-1,3) = mat_fdm_CMB_free_vt(3,3)
!
      end subroutine cal_2nd_CMB_free_vt_bc_fdm
!
! -----------------------------------------------------------------------
!
      subroutine check_coef_fdm_free_CMB
!
!
      write(50,*) ' coef_fdm_free_CMB_vp2'
      write(50,*) ' mat_fdm11,  mat_fdm12'
      write(50,'(1p9E25.15e3)') coef_fdm_free_CMB_vp2(-1:0,1)
      write(50,*) ' mat_fdm21,  mat_fdm22'
      write(50,'(1p9E25.15e3)') coef_fdm_free_CMB_vp2(-1:0,2)
      write(50,*) ' mat_fdm31,  mat_fdm32'
      write(50,'(1p9E25.15e3)') coef_fdm_free_CMB_vp2(-1:0,3)
!
      write(50,*) ' coef_fdm_free_CMB_vt2'
      write(50,*) ' mat_fdm11,  mat_fdm12'
      write(50,'(1p9E25.15e3)') coef_fdm_free_CMB_vt2(-1:0,1)
      write(50,*) ' mat_fdm21,  mat_fdm22'
      write(50,'(1p9E25.15e3)') coef_fdm_free_CMB_vt2(-1:0,2)
      write(50,*) ' mat_fdm31,  mat_fdm32'
      write(50,'(1p9E25.15e3)') coef_fdm_free_CMB_vt2(-1:0,3)
!
      end subroutine check_coef_fdm_free_CMB
!
! -----------------------------------------------------------------------
!
      end module m_coef_fdm_free_CMB
