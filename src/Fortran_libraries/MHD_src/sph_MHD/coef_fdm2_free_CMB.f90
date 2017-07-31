!>@file   coef_fdm2_free_CMB.f90
!!@brief  module coef_fdm2_free_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate poloidal velocity and toroidal vorticity
!!       at CMB with free slip boundary
!!
!!@verbatim
!!      subroutine cal_fdm2_CMB_free_vp(r_from_CMB, fdm2_free_CMB)
!!      subroutine cal_fdm2_CMB_free_vt(r_from_CMB, fdm2_free_CMB)
!!        type(fdm2_free_slip), intent(inout) :: fdm2_free_CMB
!!
!!      subroutine check_coef_fdm_free_CMB(fdm2_free_CMB)
!!        type(fdm2_free_slip), intent(in) :: fdm2_free_CMB
!!
!!    Matrix to evaluate radial derivative of poloidal velocity
!!    at CMB with free slip boundary
!!      dfdr =    fdm2_free_CMB%dmat_vp(-1,2) * d_rj(CMB-1)
!!              + fdm2_free_CMB%dmat_vp( 0,2) * d_rj(CMB  )
!!      d2fdr2 =  fdm2_free_CMB%dmat_vp(-1,3) * d_rj(CMB-1)
!!              + fdm2_free_CMB%dmat_vp( 0,3) * d_rj(CMB  )
!!
!!    Matrix to evaluate radial derivative of toroidal vorticity
!!    at CMB with free slip boundary
!!      dfdr =    fdm2_free_CMB%dmat_vt( 0,2) * d_rj(CMB  )
!!      d2fdr2 =  fdm2_free_CMB%dmat_vt(-1,3) * d_rj(CMB-1)
!!              + fdm2_free_CMB%dmat_vt( 0,3) * d_rj(CMB  )
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
      module coef_fdm2_free_CMB
!
      use m_precision
      use m_constants
!
      use t_coef_fdm2_MHD_boundaries
      use cal_inverse_small_matrix
!
      implicit none
!
!>      Work matrix to evaluate fdm2_free_CMB%dmat_vp(-1:1,3)
!!@verbatim
!!      dsdr =    mat_fdm_CMB_free_vp(2,1) * d_rj(ICB  )
!!              + mat_fdm_CMB_free_vp(2,3) * d_rj(ICB+1)
!!      dsfdr2 =  mat_fdm_CMB_free_vp(3,1) * d_rj(ICB  )
!!              + mat_fdm_CMB_free_vp(3,3) * d_rj(ICB+1)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_CMB_free_vp(3,3)
!
!>      Work matrix to evaluate fdm2_free_CMB%dmat_vt(-1:1,3)
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
      subroutine cal_fdm2_CMB_free_vp(r_from_CMB, fdm2_free_CMB)
!
      real(kind = kreal), intent(in) :: r_from_CMB(-1:0)
      type(fdm2_free_slip), intent(inout) :: fdm2_free_CMB
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
      fdm2_free_CMB%dmat_vp(1, 1) = zero
      fdm2_free_CMB%dmat_vp(0, 1) = one
      fdm2_free_CMB%dmat_vp(-1,1) = zero
      fdm2_free_CMB%dmat_vp(1, 2) = zero
      fdm2_free_CMB%dmat_vp(0, 2) = mat_fdm_CMB_free_vp(2,1)
      fdm2_free_CMB%dmat_vp(-1,2) = mat_fdm_CMB_free_vp(2,3)
      fdm2_free_CMB%dmat_vp(1, 3) = zero
      fdm2_free_CMB%dmat_vp(0, 3) = mat_fdm_CMB_free_vp(3,1)
      fdm2_free_CMB%dmat_vp(-1,3) = mat_fdm_CMB_free_vp(3,3)
!
      end subroutine cal_fdm2_CMB_free_vp
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm2_CMB_free_vt(r_from_CMB, fdm2_free_CMB)
!
      real(kind = kreal), intent(in) :: r_from_CMB(-1:0)
      type(fdm2_free_slip), intent(inout) :: fdm2_free_CMB
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
      fdm2_free_CMB%dmat_vt(1, 1) = one
      fdm2_free_CMB%dmat_vt(0, 1) = one
      fdm2_free_CMB%dmat_vt(-1,1) = zero
      fdm2_free_CMB%dmat_vt(1, 2) = one
      fdm2_free_CMB%dmat_vt(0, 2) = mat_fdm_CMB_free_vt(2,1)
      fdm2_free_CMB%dmat_vt(-1,2) = zero
      fdm2_free_CMB%dmat_vt(1, 3) = one
      fdm2_free_CMB%dmat_vt(0, 3) = mat_fdm_CMB_free_vt(3,1)
      fdm2_free_CMB%dmat_vt(-1,3) = mat_fdm_CMB_free_vt(3,3)
!
      end subroutine cal_fdm2_CMB_free_vt
!
! -----------------------------------------------------------------------
!
      subroutine check_coef_fdm_free_CMB(fdm2_free_CMB)
!
      type(fdm2_free_slip), intent(in) :: fdm2_free_CMB
!
!
      write(50,*) ' Free slip for CMB'
      call check_coef_fdm_free_slip(fdm2_free_CMB)
!
      end subroutine check_coef_fdm_free_CMB
!
! -----------------------------------------------------------------------
!
      end module coef_fdm2_free_CMB
