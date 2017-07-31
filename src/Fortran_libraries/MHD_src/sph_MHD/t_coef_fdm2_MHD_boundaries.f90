!>@file   t_coef_fdm2_MHD_boundaries.f90
!!@brief  module t_coef_fdm2_MHD_boundaries
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate poloidal velocity and toroidal vorticity
!!       at CMB with free slip boundary
!!
!!@verbatim
!!      subroutine check_coef_fdm_free_slip(fdm2_free_mat)
!!        type(fdm2_free_slip), intent(in) :: fdm2_free_mat
!!      subroutine check_coef_fdm_fix_dr_2ctr(fdm2_center)
!!        type(fdm4_ICB_vpol), intent(in) :: fdm2_center
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
!!
!!
!!    Matrix to evaluate radial derivative of poloidal velocity
!!    at ICB with free slip boundary
!!      dfdr =    fdm2_free_ICB%dmat_vp( 0,2) * d_rj(ICB  )
!!              + fdm2_free_ICB%dmat_vp( 1,2) * d_rj(ICB+1)
!!      d2fdr2 =  fdm2_free_ICB%dmat_vp( 0,3) * d_rj(ICB  )
!!              + fdm2_free_ICB%dmat_vp( 1,3) * d_rj(ICB+1)
!!
!!    Matrix to evaluate radial derivative of toroidal vorticity
!!    at ICB with free slip boundary
!!      dfdr =    fdm2_free_ICB%dmat_vt( 0,2) * d_rj(ICB  )
!!      d2fdr2 =  fdm2_free_ICB%dmat_vt( 0,3) * d_rj(ICB  )
!!              + fdm2_free_ICB%dmat_vt( 1,3) * d_rj(ICB+1)
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
!!
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
!!@n @param r_from_CMB(-3:0) radius from next points of CMB
!!@n @param radius(1:2) radius at two innermost grids
!
      module t_coef_fdm2_MHD_boundaries
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Structure for FDM matrix of free slip boundary
      type fdm2_free_slip
!>        Matrix to evaluate radial derivative of poloidal velocity
!!        with free slip boundary
        real(kind = kreal) :: dmat_vp(-1:1,3)
!>        Matrix to evaluate radial derivative of toroidal vorticity
!!        with free slip boundary
        real(kind = kreal) :: dmat_vt(-1:1,3)
      end type fdm2_free_slip
!
!
!>      Structure for FDM matrix of center
      type fdm2_center_mat
!>        Matrix to evaluate radial derivative at center
!!        with fixed field
        real(kind = kreal) :: dmat_fix_fld(-1:1,3)
!
!>        Matrix to evaluate field at center
!!        with fixed radial derivative
        real(kind = kreal) :: dmat_fix_dr(-1:1,3)
!>        Matrix to evaluate field at center with fixed scalar
        real(kind = kreal) :: dmat_fixed( 0:2,3)
      end type fdm2_center_mat
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_coef_fdm_free_slip(fdm2_free_mat)
!
      type(fdm2_free_slip), intent(in) :: fdm2_free_mat
!
!
      write(50,*) ' fdm2_free_mat%dmat_vp'
      write(50,*) ' mat_fdm11,  mat_fdm12'
      write(50,'(1p9E25.15e3)') fdm2_free_mat%dmat_vp(-1:0,1)
      write(50,*) ' mat_fdm21,  mat_fdm22'
      write(50,'(1p9E25.15e3)') fdm2_free_mat%dmat_vp(-1:0,2)
      write(50,*) ' mat_fdm31,  mat_fdm32'
      write(50,'(1p9E25.15e3)') fdm2_free_mat%dmat_vp(-1:0,3)
!
      write(50,*) ' fdm2_free_mat%dmat_vt'
      write(50,*) ' mat_fdm11,  mat_fdm12'
      write(50,'(1p9E25.15e3)') fdm2_free_mat%dmat_vt(-1:0,1)
      write(50,*) ' mat_fdm21,  mat_fdm22'
      write(50,'(1p9E25.15e3)') fdm2_free_mat%dmat_vt(-1:0,2)
      write(50,*) ' mat_fdm31,  mat_fdm32'
      write(50,'(1p9E25.15e3)') fdm2_free_mat%dmat_vt(-1:0,3)
!
      end subroutine check_coef_fdm_free_slip
!
! -----------------------------------------------------------------------
!
      subroutine check_coef_fdm_fix_dr_2ctr(fdm2_center)
!
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
!
      write(50,*) ' fdm2_center%dmat_fix_fld'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') fdm2_center%dmat_fix_fld(-1:1,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') fdm2_center%dmat_fix_fld(-1:1,3)
!
      write(50,*) ' fdm2_center%dmat_fix_dr'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') fdm2_center%dmat_fix_dr(-1:1,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') fdm2_center%dmat_fix_dr(-1:1,3)
!
      write(50,*) ' fdm2_center%dmat_fixed'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') fdm2_center%dmat_fixed(0:2,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') fdm2_center%dmat_fixed(0:2,3)
!
      end subroutine check_coef_fdm_fix_dr_2ctr
!
! -----------------------------------------------------------------------
!
      end module t_coef_fdm2_MHD_boundaries
