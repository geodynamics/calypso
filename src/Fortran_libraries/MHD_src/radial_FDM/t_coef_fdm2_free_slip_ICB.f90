!>@file   t_coef_fdm2_free_slip_ICB.f90
!!@brief  module t_coef_fdm2_free_slip_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate poloidal velocity and toroidal vorticity
!!       at ICB with free slip boundary
!!
!!@verbatim
!!      subroutine cal_fdm2_ICB_free_vp(h_rho, r_from_ICB,              &
!!     &                                fdm2_free_ICB)
!!      subroutine cal_fdm2_ICB_free_vt(h_rho, r_from_ICB,              &
!!     &                                fdm2_free_ICB)
!!        real(kind = kreal), intent(in) :: h_rho
!!        real(kind = kreal), intent(in) :: r_from_ICB(0:1)
!!        type(fdm2_ICB_free_slip), intent(inout) :: fdm2_free_ICB
!!
!!      subroutine check_fdm2_coef_free_slip_ICB(id_file, fdm2_free_ICB)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(fdm2_ICB_free_slip), intent(in) :: fdm2_free_ICB
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
!!@endverbatim
!!
      module t_coef_fdm2_free_slip_ICB
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Structure for FDM matrix of free slip boundary
      type fdm2_ICB_free_slip
!>        Matrix to evaluate radial derivative of poloidal velocity
!!        with free slip boundary
        real(kind = kreal) :: dmat_vp(-1:1,3)
!>        Matrix to evaluate radial derivative of toroidal vorticity
!!        with free slip boundary
        real(kind = kreal) :: dmat_vt(-1:1,3)
      end type fdm2_ICB_free_slip
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_fdm2_coef_free_slip_ICB(id_file, fdm2_free_mat)
!
      integer(kind = kint), intent(in) :: id_file
      type(fdm2_ICB_free_slip), intent(in) :: fdm2_free_mat
!
!
      write(id_file,*) ' Free slip for ICB'
      write(id_file,*) ' fdm2_free_mat%dmat_vp at ICB'
      write(id_file,*) '     no delivative dmat_vp(0,1),  dmat_vp(1,1)'
      write(id_file,'(1p9E25.15e3)') fdm2_free_mat%dmat_vp(0:1,1)
      write(id_file,*) '  first delivative dmat_vp(0,2),  dmat_vp(1,2)'
      write(id_file,'(1p9E25.15e3)') fdm2_free_mat%dmat_vp(0:1,2)
      write(id_file,*) ' second delivative dmat_vp(0,3),  dmat_vp(1,3)'
      write(id_file,'(1p9E25.15e3)') fdm2_free_mat%dmat_vp(0:1,3)
!
      write(id_file,*) ' fdm2_free_mat%dmat_vt at ICB'
      write(id_file,*) '     no delivative dmat_vt(0,1),  dmat_vt(1,1)'
      write(id_file,'(1p9E25.15e3)') fdm2_free_mat%dmat_vt(0:1,1)
      write(id_file,*) '  first delivative dmat_vt(0,2),  dmat_vt(1,2)'
      write(id_file,'(1p9E25.15e3)') fdm2_free_mat%dmat_vt(0:1,2)
      write(id_file,*) ' second delivative dmat_vt(0,3),  dmat_vt(1,3)'
      write(id_file,'(1p9E25.15e3)') fdm2_free_mat%dmat_vt(0:1,3)
!
      end subroutine check_fdm2_coef_free_slip_ICB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_fdm2_ICB_free_vp(h_rho, r_from_ICB,                &
     &                                fdm2_free_ICB)
!
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: h_rho
      real(kind = kreal), intent(in) :: r_from_ICB(0:1)
      type(fdm2_ICB_free_slip), intent(inout) :: fdm2_free_ICB
!
!>      Work matrix to evaluate fdm2_free_ICB%dmat_vp(-1:1,3)
!!@verbatim
!!      dfdr =    mat_fdm_ICB_free_vp(2,1) * d_rj(ICB  )
!!              + mat_fdm_ICB_free_vp(2,3) * d_rj(ICB+1)
!!      dsfdr2 =  mat_fdm_ICB_free_vp(3,1) * d_rj(ICB  )
!!              + mat_fdm_ICB_free_vp(3,3) * d_rj(ICB+1)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ICB_free_vp(3,3)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
      real(kind = kreal) :: dr_p1, r0
!
!
      dr_p1 = r_from_ICB(1) - r_from_ICB(0)
      r0 = r_from_ICB(0)
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) =  zero
      mat_taylor_3(2,2) = -(two / r0 + h_rho)
      mat_taylor_3(2,3) =  one
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) = dr_p1
      mat_taylor_3(3,3) = half * dr_p1*dr_p1
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_ICB_free_vp,     &
     &      ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix free slip ICB mat_vp ',             &
     &             r_from_ICB(0)
      end if
!
      fdm2_free_ICB%dmat_vp(-1,1) = zero
      fdm2_free_ICB%dmat_vp( 0,1) = one
      fdm2_free_ICB%dmat_vp( 1,1) = zero
      fdm2_free_ICB%dmat_vp(-1,2) = zero
      fdm2_free_ICB%dmat_vp( 0,2) = mat_fdm_ICB_free_vp(2,1)
      fdm2_free_ICB%dmat_vp( 1,2) = mat_fdm_ICB_free_vp(2,3)
      fdm2_free_ICB%dmat_vp(-1,3) = zero
      fdm2_free_ICB%dmat_vp( 0,3) = mat_fdm_ICB_free_vp(3,1)
      fdm2_free_ICB%dmat_vp( 1,3) = mat_fdm_ICB_free_vp(3,3)
!
      end subroutine cal_fdm2_ICB_free_vp
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm2_ICB_free_vt(h_rho, r_from_ICB,                &
     &                                fdm2_free_ICB)
!
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: h_rho
      real(kind = kreal), intent(in) :: r_from_ICB(0:1)
      type(fdm2_ICB_free_slip), intent(inout) :: fdm2_free_ICB
!
!>      Work matrix to evaluate fdm2_free_ICB%dmat_vt(-1:1,3)
!!@verbatim
!!      dfdr =    mat_fdm_ICB_free_vt(2,1) * d_rj(ICB  )
!!              + mat_fdm_ICB_free_vt(2,3) * d_rj(ICB+1)
!!      d2fdr2 =  mat_fdm_ICB_free_vt(3,1) * d_rj(ICB  )
!!              + mat_fdm_ICB_free_vt(3,3) * d_rj(ICB+1)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ICB_free_vt(3,3)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3(3,3)
      real(kind = kreal) :: dr_p1, r0
!
!
      dr_p1 = r_from_ICB(1) - r_from_ICB(0)
      r0 = r_from_ICB(0)
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) = two / r0 + h_rho
      mat_taylor_3(2,2) = -one
      mat_taylor_3(2,3) = zero
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) = dr_p1
      mat_taylor_3(3,3) = half * dr_p1*dr_p1
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm_ICB_free_vt,     &
     &      ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix free slip ICB mat_vt ',             &
     &             r_from_ICB(0)
      end if
!
      fdm2_free_ICB%dmat_vt(-1,1) = zero
      fdm2_free_ICB%dmat_vt( 0,1) = one
      fdm2_free_ICB%dmat_vt( 1,1) = zero
      fdm2_free_ICB%dmat_vt(-1,2) = zero
      fdm2_free_ICB%dmat_vt( 0,2) = mat_fdm_ICB_free_vt(2,1)
      fdm2_free_ICB%dmat_vt( 1,2) = zero
      fdm2_free_ICB%dmat_vt(-1,3) = zero
      fdm2_free_ICB%dmat_vt( 0,3) = mat_fdm_ICB_free_vt(3,1)
      fdm2_free_ICB%dmat_vt( 1,3) = mat_fdm_ICB_free_vt(3,3)
!
      end subroutine cal_fdm2_ICB_free_vt
!
! -----------------------------------------------------------------------
!
      end module t_coef_fdm2_free_slip_ICB
