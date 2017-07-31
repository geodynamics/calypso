!>@file   const_radial_4th_fdm_noequi.f90
!!@brief  module const_radial_4th_fdm_noequi
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Coefficients to obtain radial derivatives
!!        by forth order finite difference method
!!
!!@verbatim
!!
!!@endverbatim
!!      subroutine nod_r_4th_fdm_coefs_nonequi                          &
!!     &         (nlayer_ICB, nri, radius_1d_rj_r, mat_fdm_4)
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module const_radial_4th_fdm_noequi
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_fdm_coefs
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine nod_r_4th_fdm_coefs_nonequi                            &
     &         (nlayer_ICB, nri, radius_1d_rj_r, mat_fdm_4)
!
      integer(kind = kint), intent(in) :: nlayer_ICB
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!
      real(kind = kreal), intent(inout) :: mat_fdm_4(5,5,nri)
!
      integer(kind = kint) :: kr
      real(kind = kreal) :: dr_p1, dr_n1
      real(kind = kreal) :: dr_p2, dr_n2
!
!
      if(nlayer_ICB .gt. 1) then
        dr_p1 = radius_1d_rj_r(2) - radius_1d_rj_r(1)
        dr_n1 = radius_1d_rj_r(1)
        dr_p2 = radius_1d_rj_r(3) - radius_1d_rj_r(1)
        dr_n2 = 2.0 * radius_1d_rj_r(1)
        call nod_r_4th_fdm_coef_noequi(ione, dr_p1, dr_n1,              &
     &     dr_p2, dr_n2, mat_fdm_4(1,1,1))
      else
        dr_p1 = radius_1d_rj_r(2) - radius_1d_rj_r(1)
        dr_p2 = radius_1d_rj_r(3) - radius_1d_rj_r(1)
        call nod_r_4th_fdm_coef_noequi(ione, dr_p1, dr_p1,              &
     &      dr_p2, dr_p2, mat_fdm_4(1,1,1))
      end if
!
      dr_p1 = radius_1d_rj_r(3) - radius_1d_rj_r(2)
      dr_n1 = radius_1d_rj_r(2) - radius_1d_rj_r(1)
      dr_p2 = radius_1d_rj_r(4) - radius_1d_rj_r(2)
      call nod_r_4th_fdm_coef_noequi(itwo, dr_p1, dr_n1, dr_p2, dr_p2,  &
     &       mat_fdm_4(1,1,2))
!
!
      do kr = 3, nri-2
        dr_p1 = radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr  )
        dr_n1 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-1)
        dr_p2 = radius_1d_rj_r(kr+2) - radius_1d_rj_r(kr  )
        dr_n2 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-2)
        call nod_r_4th_fdm_coef_noequi(kr, dr_p1, dr_n1, dr_p2, dr_n2,  &
     &       mat_fdm_4(1,1,kr))
      end do
!
      kr = nri-1
      dr_p1 = radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr  )
      dr_n1 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-1)
      dr_n2 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-2)
      call nod_r_4th_fdm_coef_noequi                                    &
     &      (kr, dr_p1, dr_n1, dr_n2, dr_n2, mat_fdm_4(1,1,kr))
!
      kr = nri
      dr_n1 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-1)
      dr_n2 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-2)
      call nod_r_4th_fdm_coef_noequi(kr, dr_n1, dr_n1, dr_n2, dr_n2,    &
     &    mat_fdm_4(1,1,kr))
!
      end subroutine nod_r_4th_fdm_coefs_nonequi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine nod_r_4th_fdm_coef_noequi(kr,                          &
     &          dr_p1, dr_n1, dr_p2, dr_n2, mat_fdm)
!
      use cal_inverse_small_matrix
!
      integer(kind = kint), intent(in) :: kr
      real(kind = kreal), intent(in) :: dr_p1, dr_n1
      real(kind = kreal), intent(in) :: dr_p2, dr_n2
      real(kind = kreal), intent(inout) :: mat_fdm(5,5)
!
      real(kind = kreal) :: mat_taylor_r5(5,5)
      integer(kind = kint) :: ierr
!
!
      mat_taylor_r5(1,1) = one
      mat_taylor_r5(1,2) = zero
      mat_taylor_r5(1,3) = zero
      mat_taylor_r5(1,4) = zero
      mat_taylor_r5(1,5) = zero
!
      mat_taylor_r5(2,1) = one
      mat_taylor_r5(2,2) = dr_p1
      mat_taylor_r5(2,3) = half * dr_p1*dr_p1
      mat_taylor_r5(2,4) = (1.0d0/6.0d0) *  dr_p1**3
      mat_taylor_r5(2,5) = (1.0d0/24.0d0) * dr_p1**4
!
      mat_taylor_r5(3,1) = one
      mat_taylor_r5(3,2) =-dr_n1
      mat_taylor_r5(3,3) = half * dr_n1*dr_n1
      mat_taylor_r5(3,4) =-(1.0d0/6.0d0) *  dr_n1**3
      mat_taylor_r5(3,5) = (1.0d0/24.0d0) * dr_n1**4
!
      mat_taylor_r5(4,1) = one
      mat_taylor_r5(4,2) = dr_p2
      mat_taylor_r5(4,3) = half * dr_p2*dr_p2
      mat_taylor_r5(4,4) = (1.0d0/6.0d0) *  dr_p2**3
      mat_taylor_r5(4,5) = (1.0d0/24.0d0) * dr_p2**4
!
      mat_taylor_r5(5,1) = one
      mat_taylor_r5(5,2) =-dr_n2
      mat_taylor_r5(5,3) = half * dr_n2*dr_n2
      mat_taylor_r5(5,4) =-(1.0d0/6.0d0) *  dr_n2**3
      mat_taylor_r5(5,5) = (1.0d0/24.0d0) * dr_n2**4
!
      call cal_inverse_nn_matrix(ifive, mat_taylor_r5, mat_fdm, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix at nod_r_4th_fdm_coef_noequi', kr
      end if
!
      end subroutine nod_r_4th_fdm_coef_noequi
!
! -----------------------------------------------------------------------
!
      end module const_radial_4th_fdm_noequi
