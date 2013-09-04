!
!      module set_radius_func_noequi
!
!      Programmed by H. Matsui on June., 1994
!      modified by H. Matsui on Apr., 2009
!
!      subroutine set_dr_for_equidist
!      subroutine set_dr_for_nonequi
!
!      subroutine nod_r_2nd_fdm_coefs_equi
!      subroutine nod_r_2nd_fdm_coefs_nonequi
!
!      subroutine nod_r_2nd_fdm_coef_noequi(kr, dr_p1, dr_n1, mat_fdm)
!*
!***********************************************************************
!
!    delta r for eqidistance grid
!*        dr_1d_rj(k,0) = r(k+1) - r(k)
!*        dr_1d_rj(k,1) = r(k+1) - r(k)
!*        dr_1d_rj(k,2) = 1 / (r(k+1) - r(k)) 
!*
!***********************************************************************
!*
!***********************************************************************
!
!    delta r for non_eqidistance grid
!*        dr_1d_rj(k,0) = r(k+1) - r(k)
!*        dr_1d_rj(k,1) = r(k) - r(k-1)
!*        dr_1d_rj(k,2) = 1 / ( (r(k+1) - r(k)) * (r(k) - r(k-1))
!*                             *(r(k+1) - r(k-1)) ) 
!*
!***********************************************************************
!*
      module set_radius_func_noequi
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_dr_for_equidist
!
      use boundary_radius_func
!
      real(kind = kreal) ::  shell
      integer(kind = kint) :: k, nri
!
!* ---------- whole domain --------
!*
      nri = nlayer_CMB - nlayer_ICB
      shell = radius_1d_rj_r(nlayer_CMB) - radius_1d_rj_r(nlayer_ICB)
!
      do k = 1, nidx_rj(1)
        dr_1d_rj(k,0) = shell / dble(nri)
        dr_1d_rj(k,1) = dr_1d_rj(k,0)
        dr_1d_rj(k,2) = dble(nri)
      end do
!*
!* ----------  inner boundary --------
!*
      if (nlayer_ICB .gt. 1) call set_non_equi_dr_center
!
      end subroutine set_dr_for_equidist
!
!  -------------------------------------------------------------------
!
      subroutine set_dr_for_nonequi
!
      use boundary_radius_func
!
      integer(kind = kint) :: k
!
!* ---------- whole domain --------
!*
      do k = 2, nidx_rj(1)-1
        dr_1d_rj(k,0) = radius_1d_rj_r(k+1) - radius_1d_rj_r(k)
        dr_1d_rj(k,1) = radius_1d_rj_r(k) - radius_1d_rj_r(k-1)
        dr_1d_rj(k,2) =  (radius_1d_rj_r(k+1)-radius_1d_rj_r(k))        &
     &                 * (radius_1d_rj_r(k)-radius_1d_rj_r(k-1))        &
     &                 * (radius_1d_rj_r(k+1)-radius_1d_rj_r(k-1))
        dr_1d_rj(k,2) = one /dr_1d_rj(k,2)
      end do
!*
!* ----------  outer boundary --------
!*
      if (nlayer_CMB .eq. nidx_rj(1)) then
        call set_equi_dr_CMB
      else
        call set_equi_dr_outside
      end if
!*
!* ----------  inner boundary --------
!*
      if (nlayer_ICB .eq. 1) then
        call set_equi_dr_ICB
      else
        call set_non_equi_dr_center
      end if
!
      end subroutine set_dr_for_nonequi
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine nod_r_2nd_fdm_coefs_equi
!
      use m_spheric_parameter
      use m_fdm_coefs
!
      integer(kind = kint) :: kr
!
!
      if(nlayer_ICB .gt. 1) then
        call nod_r_2nd_fdm_coef_noequi(ione, dr_1d_rj(1,0),             &
     &      radius_1d_rj_r(1), mat_fdm_2(1,1,1))
      else
        call nod_r_2nd_fdm_coef_noequi                                  &
     &      (ione, dr_1d_rj(1,0), dr_1d_rj(1,0), mat_fdm_2(1,1,1))
      end if
!
      do kr = 2, nidx_rj(1)
        call nod_r_2nd_fdm_coef_noequi                                  &
     &      (kr, dr_1d_rj(kr,0), dr_1d_rj(kr,0), mat_fdm_2(1,1,kr))
      end do
!
      end subroutine nod_r_2nd_fdm_coefs_equi
!
! -----------------------------------------------------------------------
!
      subroutine nod_r_2nd_fdm_coefs_nonequi
!
      use m_fdm_coefs
!
      integer(kind = kint) :: kr
!
!
      if(nlayer_ICB .gt. 1) then
        call nod_r_2nd_fdm_coef_noequi(ione, dr_1d_rj(1,0),             &
     &      radius_1d_rj_r(1), mat_fdm_2(1,1,1))
      else
        call nod_r_2nd_fdm_coef_noequi                                  &
     &     (ione, dr_1d_rj(1,0), dr_1d_rj(1,0), mat_fdm_2(1,1,1))
      end if
!
      do kr = 2, nidx_rj(1)-1
        call nod_r_2nd_fdm_coef_noequi                                  &
     &      (kr, dr_1d_rj(kr,0), dr_1d_rj(kr,1), mat_fdm_2(1,1,kr))
      end do
!
      kr = nidx_rj(1)
      call nod_r_2nd_fdm_coef_noequi                                    &
     &   (kr, dr_1d_rj(kr,0), dr_1d_rj(kr,0), mat_fdm_2(1,1,kr))
!
      end subroutine nod_r_2nd_fdm_coefs_nonequi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine nod_r_2nd_fdm_coef_noequi(kr, dr_p1, dr_n1, mat_fdm)
!
      use cal_inverse_small_matrix
!
      integer(kind = kint), intent(in) :: kr
      real(kind = kreal), intent(in) :: dr_p1, dr_n1
      real(kind = kreal), intent(inout) :: mat_fdm(3,3)
!
      real(kind = kreal) :: mat_taylor_r3(3,3)
      integer(kind = kint) :: ierr
!
!
      mat_taylor_r3(1,1) = one
      mat_taylor_r3(1,2) = zero
      mat_taylor_r3(1,3) = zero
!
      mat_taylor_r3(2,1) = one
      mat_taylor_r3(2,2) = dr_p1
      mat_taylor_r3(2,3) = half * dr_p1*dr_p1
!
      mat_taylor_r3(3,1) = one
      mat_taylor_r3(3,2) =-dr_n1
      mat_taylor_r3(3,3) = half * dr_n1*dr_n1
!
      call cal_inverse_33_matrix(mat_taylor_r3, mat_fdm, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix at nod_r_2nd_fdm_coef_noequi', kr
      end if
!
      end subroutine nod_r_2nd_fdm_coef_noequi
!
! -----------------------------------------------------------------------
!
      end module set_radius_func_noequi
