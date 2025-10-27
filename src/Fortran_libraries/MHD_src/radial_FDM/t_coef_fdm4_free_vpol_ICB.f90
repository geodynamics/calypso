!>@file   t_coef_fdm4_free_vpol_ICB.f90
!!@brief  module t_coef_fdm4_free_vpol_ICB
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative for non-slip at ICB
!!
!!@verbatim
!!      subroutine check_4th_ICB_free_vp_fdm(id_file, fdm4_free_ICB)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(fdm4_ICB_free_vpol), intent(in) :: fdm4_free_ICB
!!      subroutine cal_fdm4_ICB0_free_vp(h_rho, r_from_ICB,             &
!!     &                                 fdm4_free_ICB)
!!        real(kind = kreal), intent(in) :: h_rho
!!        real(kind = kreal), intent(in) :: r_from_ICB(0:2)
!!        type(fdm4_ICB_free_vpol), intent(inout) :: fdm4_free_ICB
!!      subroutine cal_fdm4_ICB1_free_vp(r_from_ICB, fdm4_free_ICB)
!!        real(kind = kreal), intent(in) :: r_from_ICB(0:3)
!!        type(fdm4_ICB_free_vpol), intent(inout) :: fdm4_free_ICB
!!
!!   Matrix for poloidal velocity with free-slip boundary at ICB
!!      dfdr =      fdm4_free_ICB%dmat_vp1( 2,2) * d_rj(3)
!!                + fdm4_free_ICB%dmat_vp1( 1,2) * d_rj(2)
!!                + fdm4_free_ICB%dmat_vp1( 0,2) * d_rj(1)
!!      d2fdr2 =    fdm4_free_ICB%dmat_vp1( 2,3) * d_rj(3)
!!                + fdm4_free_ICB%dmat_vp1( 1,3) * d_rj(2)
!!                + fdm4_free_ICB%dmat_vp1( 0,3) * d_rj(1)
!!      d3fdr3 =    fdm4_free_ICB%dmat_vp1( 2,4) * d_rj(3)
!!                + fdm4_free_ICB%dmat_vp1( 1,4) * d_rj(2)
!!                + fdm4_free_ICB%dmat_vp1( 0,4) * d_rj(1)
!!      d4fdr4 =    fdm4_free_ICB%dmat_vp1( 2,5) * d_rj(3)
!!                + fdm4_free_ICB%dmat_vp1( 1,5) * d_rj(2)
!!                + fdm4_free_ICB%dmat_vp1( 0,5) * d_rj(1)
!!
!!   Matrix for poloidal velocity with free-slip boundary at next of ICB
!!      dfdr =      fdm4_free_ICB%dmat_vp1( 2,2) * d_rj(ICB+3)
!!                + fdm4_free_ICB%dmat_vp1( 1,2) * d_rj(ICB+2)
!!                + fdm4_free_ICB%dmat_vp1( 0,2) * d_rj(ICB+1)
!!                + fdm4_free_ICB%dmat_vp1(-1,2) * d_rj(ICB  )
!!      d2fdr2 =    fdm4_free_ICB%dmat_vp1( 2,3) * d_rj(ICB+3)
!!                + fdm4_free_ICB%dmat_vp1( 1,3) * d_rj(ICB+2)
!!                + fdm4_free_ICB%dmat_vp1( 0,3) * d_rj(ICB+1)
!!                + fdm4_free_ICB%dmat_vp1(-1,3) * d_rj(ICB  )
!!      d3fdr3 =    fdm4_free_ICB%dmat_vp1( 2,4) * d_rj(ICB+3)
!!                + fdm4_free_ICB%dmat_vp1( 1,4) * d_rj(ICB+2)
!!                + fdm4_free_ICB%dmat_vp1( 0,4) * d_rj(ICB+1)
!!                + fdm4_free_ICB%dmat_vp1(-1,4) * d_rj(ICB  )
!!      d4fdr4 =    fdm4_free_ICB%dmat_vp1( 2,5) * d_rj(ICB+3)
!!                + fdm4_free_ICB%dmat_vp1( 1,5) * d_rj(ICB+2)
!!                + fdm4_free_ICB%dmat_vp1( 0,5) * d_rj(ICB+1)
!!                + fdm4_free_ICB%dmat_vp1(-1,5) * d_rj(ICB  )
!!@endverbatim
!!
      module t_coef_fdm4_free_vpol_ICB
!
      use m_precision
      use m_constants
!
      implicit none
!
      type fdm4_ICB_free_vpol
!>        Matrix to evaluate radial derivative at ICB
        real(kind = kreal) :: dmat_vp0(-2:2,1:5)
!>        Matrix to evaluate radial derivative at next of ICB
        real(kind = kreal) :: dmat_vp1(-2:2,1:5)
      end type fdm4_ICB_free_vpol
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_4th_ICB_free_vp_fdm(id_file, fdm4_free_ICB)
!
      integer(kind = kint), intent(in) :: id_file
      type(fdm4_ICB_free_vpol), intent(in) :: fdm4_free_ICB
!
!
      write(id_file,*) ' free slip boundary at ICB'
      write(id_file,*) ' fdm4_free_ICB%dmat_vp0'
      write(id_file,*) 'matrix for dfdr'
      write(id_file,'(1p9E25.15e3)') fdm4_free_ICB%dmat_vp0(0:2,2)
      write(id_file,*) 'matrix for d2fdr2'
      write(id_file,'(1p9E25.15e3)') fdm4_free_ICB%dmat_vp0(0:2,3)
      write(id_file,*) 'matrix for d3fdr3'
      write(id_file,'(1p9E25.15e3)') fdm4_free_ICB%dmat_vp0(0:2,4)
!
      write(id_file,*) ' fdm4_free_ICB%dmat_vp1'
      write(id_file,*) 'matrix for dfdr'
      write(id_file,'(1p9E25.15e3)') fdm4_free_ICB%dmat_vp1(-1:2,2)
      write(id_file,*) 'matrix for d2fdr2'
      write(id_file,'(1p9E25.15e3)') fdm4_free_ICB%dmat_vp1(-1:2,3)
      write(id_file,*) 'matrix for d3fdr3'
      write(id_file,'(1p9E25.15e3)') fdm4_free_ICB%dmat_vp1(-1:2,4)
      write(id_file,*) 'matrix for d4fdr4'
      write(id_file,'(1p9E25.15e3)') fdm4_free_ICB%dmat_vp1(-1:2,5)
!
      end subroutine check_4th_ICB_free_vp_fdm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_fdm4_ICB0_free_vp(h_rho, r_from_ICB,               &
     &                                 fdm4_free_ICB)
!
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: h_rho
      real(kind = kreal), intent(in) :: r_from_ICB(0:2)
      type(fdm4_ICB_free_vpol), intent(inout) :: fdm4_free_ICB
!
!>      Work matrix to evaluate fdm4_free_ICB%dmat_vp0(0:2,2:4)
!!@verbatim
!!      dfdr =      mat_fdm4_ICB_free_vp(2,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB_free_vp(2,3) * d_rj(ICB+1)
!!                + mat_fdm4_ICB_free_vp(2,1) * d_rj(ICB  )
!!                + mat_fdm4_ICB_free_vp(2,2) * B.C. (=0)
!!      d2fdr2 =    mat_fdm4_ICB_free_vp(3,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB_free_vp(3,3) * d_rj(ICB+1)
!!                + mat_fdm4_ICB_free_vp(3,1) * d_rj(ICB  )
!!                + mat_fdm4_ICB_free_vp(3,2) * B.C. (=0)
!!      d3fdr3 =    mat_fdm4_ICB_free_vp(4,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB_free_vp(4,3) * d_rj(ICB+1)
!!                + mat_fdm4_ICB_free_vp(4,1) * d_rj(ICB  )
!!                + mat_fdm4_ICB_free_vp(4,2) * B.C. (=0)
!!@endverbatim
      real(kind = kreal) :: mat_fdm4_ICB_free_vp(4,4)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_4(4,4)
      real(kind = kreal) :: dr_p1, dr_p2, r0
!
!
      r0 = r_from_ICB(0)
      dr_p1 = r_from_ICB(1) - r_from_ICB(0)
      dr_p2 = r_from_ICB(2) - r_from_ICB(0)
!
      mat_taylor_4(1,1) = one
      mat_taylor_4(1,2) = zero
      mat_taylor_4(1,3) = zero
      mat_taylor_4(1,4) = zero
!
      mat_taylor_4(2,1) =  zero
      mat_taylor_4(2,2) = -(two / r0 + h_rho)
      mat_taylor_4(2,3) =  one
      mat_taylor_4(2,4) =  zero
!
      mat_taylor_4(3,1) = one
      mat_taylor_4(3,2) = dr_p1
      mat_taylor_4(3,3) = half * dr_p1*dr_p1
      mat_taylor_4(3,4) = (one/six) * dr_p1**3
!
      mat_taylor_4(4,1) = one
      mat_taylor_4(4,2) = dr_p2
      mat_taylor_4(4,3) = half * dr_p2*dr_p2
      mat_taylor_4(4,4) = (one/six) * dr_p2**3
!
      call cal_inverse_44_matrix(mat_taylor_4, mat_fdm4_ICB_free_vp,    &
     &    ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_fdm4_ICB0_free_vp ',            &
     &            r_from_ICB(0)
      end if
!
      fdm4_free_ICB%dmat_vp0(-2,1:4) = zero
      fdm4_free_ICB%dmat_vp0(-1,1:4) = zero
      fdm4_free_ICB%dmat_vp0( 0,1:4) = mat_fdm4_ICB_free_vp(1:4,1)
      fdm4_free_ICB%dmat_vp0( 1,1:4) = mat_fdm4_ICB_free_vp(1:4,3)
      fdm4_free_ICB%dmat_vp0( 2,1:4) = mat_fdm4_ICB_free_vp(1:4,4)
      fdm4_free_ICB%dmat_vp0(-2:2,5) = zero
!
      end subroutine cal_fdm4_ICB0_free_vp
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm4_ICB1_free_vp(r_from_ICB, fdm4_free_ICB)
!
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: r_from_ICB(0:3)
      type(fdm4_ICB_free_vpol), intent(inout) :: fdm4_free_ICB
!
!>      Work matrix to evaluate fdm4_free_ICB%dmat_vp1(-1:1,5)
!!@verbatim
!!      dfdr =      mat_fdm4_ICB1_free_vp(2,5) * d_rj(ICB+3)
!!                + mat_fdm4_ICB1_free_vp(2,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB1_free_vp(2,1) * d_rj(ICB+1)
!!                + mat_fdm4_ICB1_free_vp(2,3) * d_rj(ICB  )
!!                + mat_fdm4_ICB1_free_vp(2,2) * B.C. (=0)
!!      d2fdr2 =    mat_fdm4_ICB1_free_vp(3,5) * d_rj(ICB+3)
!!                + mat_fdm4_ICB1_free_vp(3,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB1_free_vp(3,1) * d_rj(ICB+1)
!!                + mat_fdm4_ICB1_free_vp(3,3) * d_rj(ICB  )
!!                + mat_fdm4_ICB1_free_vp(3,2) * B.C. (=0)
!!      d3fdr3 =    mat_fdm4_ICB1_free_vp(4,5) * d_rj(ICB+3)
!!                + mat_fdm4_ICB1_free_vp(4,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB1_free_vp(4,1) * d_rj(ICB+1)
!!                + mat_fdm4_ICB1_free_vp(4,3) * d_rj(ICB  )
!!                + mat_fdm4_ICB1_free_vp(4,2) * B.C. (=0)
!!      d4fdr4 =    mat_fdm4_ICB1_free_vp(5,5) * d_rj(ICB+3)
!!                + mat_fdm4_ICB1_free_vp(5,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB1_free_vp(5,1) * d_rj(ICB+1)
!!                + mat_fdm4_ICB1_free_vp(5,3) * d_rj(ICB  )
!!                + mat_fdm4_ICB1_free_vp(5,2) * B.C. (=0)
!!@endverbatim
      real(kind = kreal) :: mat_fdm4_ICB1_free_vp(5,5)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_5(5,5)
      real(kind = kreal) :: dr_p1, dr_n1, dr_p2, r0
!
!
      r0 = r_from_ICB(0)
      dr_n1 = r_from_ICB(1) - r_from_ICB(0)
      dr_p1 = r_from_ICB(2) - r_from_ICB(1)
      dr_p2 = r_from_ICB(3) - r_from_ICB(1)
!
      mat_taylor_5(1,1) = one
      mat_taylor_5(1,2) = zero
      mat_taylor_5(1,3) = zero
      mat_taylor_5(1,4) = zero
      mat_taylor_5(1,5) = zero
!
      mat_taylor_5(2,1) = one
      mat_taylor_5(2,2) =-dr_n1                 - r0
      mat_taylor_5(2,3) = dr_n1*dr_n1 / two     + r0*dr_n1              &
    &                     + half*r0*r0
      mat_taylor_5(2,4) =-dr_n1**3 / six        - r0*dr_n1*dr_n1 / two  &
    &                     - half*r0*r0*dr_n1
      mat_taylor_5(2,5) = dr_n1**4 / (six*four) + r0*dr_n1**3 / six     &
    &                     + half*r0*r0*dr_n1*dr_n1 / two
!
      mat_taylor_5(3,1) = one
      mat_taylor_5(3,2) =-dr_n1
      mat_taylor_5(3,3) = dr_n1*dr_n1 / two
      mat_taylor_5(3,4) =-dr_n1**3 / six
      mat_taylor_5(3,5) = dr_n1**4 / (six*four)
!
      mat_taylor_5(4,1) = one
      mat_taylor_5(4,2) = dr_p1
      mat_taylor_5(4,3) = dr_p1*dr_p1 / two
      mat_taylor_5(4,4) = dr_p1**3 / six
      mat_taylor_5(4,5) = dr_p1**4 / (six*four)
!
      mat_taylor_5(5,1) = one
      mat_taylor_5(5,2) = dr_p2
      mat_taylor_5(5,3) = dr_p2*dr_p2 / two
      mat_taylor_5(5,4) = dr_p2**3 / six
      mat_taylor_5(5,5) = dr_p2**4 / (six*four)
!
      call cal_inverse_nn_matrix(ifive, mat_taylor_5,                   &
     &    mat_fdm4_ICB1_free_vp, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix in cal_fdm4_ICB1_free_vp ',         &
     &            r_from_ICB(0)
      end if
!
      fdm4_free_ICB%dmat_vp1( 2,1:5) = mat_fdm4_ICB1_free_vp(1:5,5)
      fdm4_free_ICB%dmat_vp1( 1,1:5) = mat_fdm4_ICB1_free_vp(1:5,4)
      fdm4_free_ICB%dmat_vp1( 0,1:5) = mat_fdm4_ICB1_free_vp(1:5,1)
      fdm4_free_ICB%dmat_vp1(-1,1:5) = mat_fdm4_ICB1_free_vp(1:5,3)
      fdm4_free_ICB%dmat_vp1(-2,1:5) = 0.0
!
      end subroutine cal_fdm4_ICB1_free_vp
!
! -----------------------------------------------------------------------
!
      end module t_coef_fdm4_free_vpol_ICB
