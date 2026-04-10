!>@file   t_coef_fdm4_vpol_centre.f90
!!@brief  module t_coef_fdm4_vpol_centre
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative for next of center
!!
!!@verbatim
!!      subroutine check_fdm4_vpol_centre(id_file, fdm4_center)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(fdm4_centre_vpol), intent(in) :: fdm4_center
!!
!!      subroutine cal_coef_fdm4_vpol_centre(r, fdm4, fdm4_center)
!!        real(kind = kreal), intent(in) :: r(4)
!!        type(fdm_matrix), intent(in) :: fdm4(4)
!!        type(fdm4_centre_vpol), intent(inout) :: fdm4_center
!!
!!   Matrix for poloidal vector at inner most node
!!      dfdr =      fdm4_center%dmat_vp1(-2,2) * dfdr_Center
!!                + fdm4_center%dmat_vp1(-1,2) * d_Center
!!                + fdm4_center%dmat_vp1( 0,2) * d_rj(1)
!!                + fdm4_center%dmat_vp1( 1,2) * d_rj(2)
!!                + fdm4_center%dmat_vp1( 2,2) * d_rj(3)
!!      d2fdr2 =    fdm4_center%dmat_vp1(-2,3) * dfdr_Center
!!                + fdm4_center%dmat_vp1(-1,3) * d_Center
!!                + fdm4_center%dmat_vp1( 0,3) * d_rj(1)
!!                + fdm4_center%dmat_vp1( 1,3) * d_rj(2)
!!                + fdm4_center%dmat_vp1( 2,3) * d_rj(3)
!!      d3fdr3 =    fdm4_center%dmat_vp1(-2,4) * dfdr_Center
!!                + fdm4_center%dmat_vp1(-1,4) * d_Center
!!                + fdm4_center%dmat_vp1( 0,4) * d_rj(1)
!!                + fdm4_center%dmat_vp1( 1,4) * d_rj(2)
!!                + fdm4_center%dmat_vp1( 2,4) * d_rj(3)
!!      d4fdr4 =    fdm4_center%dmat_vp1(-2,5) * dfdr_Center
!!                + fdm4_center%dmat_vp1(-1,5) * d_Center
!!                + fdm4_center%dmat_vp1( 0,5) * d_rj(1)
!!                + fdm4_center%dmat_vp1( 1,5) * d_rj(2)
!!                + fdm4_center%dmat_vp1( 2,5) * d_rj(3)
!!
!!   Matrix for poloidal vector at next of inner most node
!!      dfdr =      fdm4_center%dmat_vp2(-2,2) * d_Center
!!                + fdm4_center%dmat_vp2(-1,2) * d_rj(1)
!!                + fdm4_center%dmat_vp2( 0,2) * d_rj(2)
!!                + fdm4_center%dmat_vp2( 1,2) * d_rj(3)
!!                + fdm4_center%dmat_vp2( 2,2) * d_rj(4)
!!      d2fdr2 =    fdm4_center%dmat_vp2(-2,3) * d_Center
!!                + fdm4_center%dmat_vp2(-1,3) * d_rj(1)
!!                + fdm4_center%dmat_vp2( 0,3) * d_rj(2)
!!                + fdm4_center%dmat_vp2( 1,3) * d_rj(3)
!!                + fdm4_center%dmat_vp2( 2,3) * d_rj(4)
!!      d3fdr3 =    fdm4_center%dmat_vp2(-2,4) * d_Center
!!                + fdm4_center%dmat_vp2(-1,4) * d_rj(1)
!!                + fdm4_center%dmat_vp2( 0,4) * d_rj(2)
!!                + fdm4_center%dmat_vp2( 1,4) * d_rj(3)
!!                + fdm4_center%dmat_vp2( 2,4) * d_rj(4)
!!      d4fdr4 =    fdm4_center%dmat_vp2(-2,5) * d_Center
!!                + fdm4_center%dmat_vp2(-1,5) * d_rj(1)
!!                + fdm4_center%dmat_vp2( 0,5) * d_rj(2)
!!                + fdm4_center%dmat_vp2( 1,5) * d_rj(3)
!!                + fdm4_center%dmat_vp2( 2,5) * d_rj(4)
!!@endverbatim
!!
      module t_coef_fdm4_vpol_centre
!
      use m_precision
      use m_constants
!
      implicit none
!
      type fdm4_centre_vpol
!>  FDM matrix at innermost node
        real(kind = kreal) :: dmat_vp1(-2:2,5)
!>  FDM matrix at the next of innermost node
        real(kind = kreal) :: dmat_vp2(-2:2,5)
      end type fdm4_centre_vpol
!
      private :: set_forth_taylor_expand_CTR1
      private :: order_each_center_fdm4
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_fdm4_vpol_centre(id_file, fdm4_center)
!
      integer(kind = kint), intent(in) :: id_file
      type(fdm4_centre_vpol), intent(in) :: fdm4_center
!
!
      write(id_file,*) ' 4-th order FDM matrices around center'
      write(id_file,*) ' fdm4_center%dmat_vp1'
      write(id_file,*) 'matrix for Interpolation to element'
      write(id_file,'(1p9E25.15e3)') fdm4_center%dmat_vp1(-2:2,1)
      write(id_file,*) 'matrix for dfdr'
      write(id_file,'(1p9E25.15e3)') fdm4_center%dmat_vp1(-2:2,2)
      write(id_file,*) 'matrix for d2fdr2'
      write(id_file,'(1p9E25.15e3)') fdm4_center%dmat_vp1(-2:2,3)
      write(id_file,*) 'matrix for d3fdr3'
      write(id_file,'(1p9E25.15e3)') fdm4_center%dmat_vp1(-2:2,4)
      write(id_file,*) 'matrix for d3fdr3'
      write(id_file,'(1p9E25.15e3)') fdm4_center%dmat_vp1(-2:2,5)
!
      write(id_file,*) ' fdm4_center%dmat_vp2'
      write(id_file,*) 'matrix for Interpolation to element'
      write(id_file,'(1p9E25.15e3)') fdm4_center%dmat_vp1(-2:2,1)
      write(id_file,*) 'matrix for dfdr'
      write(id_file,'(1p9E25.15e3)') fdm4_center%dmat_vp1(-2:2,2)
      write(id_file,*) 'matrix for d2fdr2'
      write(id_file,'(1p9E25.15e3)') fdm4_center%dmat_vp1(-2:2,3)
      write(id_file,*) 'matrix for d3fdr3'
      write(id_file,'(1p9E25.15e3)') fdm4_center%dmat_vp1(-2:2,4)
      write(id_file,*) 'matrix for d3fdr3'
      write(id_file,'(1p9E25.15e3)') fdm4_center%dmat_vp1(-2:2,5)
!
      end subroutine check_fdm4_vpol_centre
!
! -----------------------------------------------------------------------
!
      subroutine cal_coef_fdm4_vpol_centre(r, fdm4, fdm4_center)
!
      use t_fdm_coefs
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: r(4)
      type(fdm_matrix), intent(in) :: fdm4(4)
!
      type(fdm4_centre_vpol), intent(inout) :: fdm4_center
!
      integer(kind = kint) :: ierr, i
      real(kind = kreal) :: mat_fdm(5,5)
!
!
      mat_fdm(1:5,1:5) = 0.0d0
!
      call set_forth_taylor_expand_CTR1(r(1), mat_fdm)
      call cal_inverse_nn_matrix                                        &
     &   (ifive, mat_fdm, mat_fdm(1,1), ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix in cal_coef_fdm4_vpol_centre '
      end if
!
      call order_each_center_fdm4(mat_fdm, fdm4_center%dmat_vp1)
!
      fdm4_center%dmat_vp2(-2:2,1) = zero
      fdm4_center%dmat_vp2(0,   1) = one
!$omp parallel do private(i)
      do i = 1, 4
        fdm4_center%dmat_vp2(-2:2,i+1) = fdm4(i)%dmat(2,-2:2)
      end do
!$omp end parallel do
!
      end subroutine cal_coef_fdm4_vpol_centre
!
! -----------------------------------------------------------------------
!
      subroutine set_forth_taylor_expand_CTR1(r, mat_taylor_5)
!
      real(kind = kreal), intent(in) :: r(3)
      real(kind = kreal), intent(inout) :: mat_taylor_5(5,5)
!
      real(kind = kreal) :: dr_n1, dr_p1, dr_p2
      real(kind = kreal), parameter :: asix = 1.0d0 / 6.0d0
      real(kind = kreal), parameter :: a24 = 1.0d0 / 24.0d0
!
!
      dr_n1 = r(1)
      dr_p1 = r(2) - r(1)
      dr_p2 = r(3) - r(1)
!
      mat_taylor_5(1,1) =  one
      mat_taylor_5(1,2) =  zero
      mat_taylor_5(1,3) =  zero
      mat_taylor_5(1,4) =  zero
      mat_taylor_5(1,5) =  zero
!
      mat_taylor_5(2,1) =  one
      mat_taylor_5(2,2) = -dr_n1
      mat_taylor_5(2,3) =  half * dr_n1**2
      mat_taylor_5(2,4) = -asix * dr_n1**3
      mat_taylor_5(2,5) =  a24 *  dr_n1**4
!
      mat_taylor_5(3,1) =  one
      mat_taylor_5(3,2) =  dr_p1
      mat_taylor_5(3,3) =  half * dr_p1**2
      mat_taylor_5(3,4) =  asix * dr_p1**3
      mat_taylor_5(3,5) =  a24 *  dr_p1**4
!
      mat_taylor_5(4,1) =  zero
      mat_taylor_5(4,2) =  one
      mat_taylor_5(4,3) = -dr_n1
      mat_taylor_5(4,4) =  half * dr_n1**2
      mat_taylor_5(4,5) = -asix * dr_n1**3
!
      mat_taylor_5(5,1) =  one
      mat_taylor_5(5,2) =  dr_p2
      mat_taylor_5(5,3) =  half * dr_p2**2
      mat_taylor_5(5,4) =  asix * dr_p2**3
      mat_taylor_5(5,5) =  a24 *  dr_p2**4
!
      end subroutine set_forth_taylor_expand_CTR1
!
! -----------------------------------------------------------------------
!
      subroutine order_each_center_fdm4(mat_fdm, dmat)
!
      real(kind = kreal), intent(in) ::    mat_fdm(5,5)
      real(kind = kreal), intent(inout) :: dmat(-2:2,5)
!
!
      dmat(-2,1:5) = mat_fdm(1:5,4)
      dmat(-1,1:5) = mat_fdm(1:5,2)
      dmat( 0,1:5) = mat_fdm(1:5,1)
      dmat( 1,1:5) = mat_fdm(1:5,3)
      dmat( 2,1:5) = mat_fdm(1:5,5)
!
      end subroutine order_each_center_fdm4
!
! -----------------------------------------------------------------------
!
      end module t_coef_fdm4_vpol_centre
