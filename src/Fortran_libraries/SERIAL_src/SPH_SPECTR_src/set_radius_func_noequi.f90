!>@file   set_radius_func_noequi.f90
!!@brief  module set_radius_func_noequi
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@date Modified in Apr., 2009
!
!!@brief  Set radial informations for non-equidistance grid
!!
!!@verbatim
!!      subroutine allocate_dr_rj_noequi(nri)
!!      subroutine deallocate_dr_rj_noequi
!!
!!      subroutine set_dr_for_equidist                                  &
!!     &         (nlayer_ICB, nlayer_CMB, nri, radius_1d_rj_r)
!!      subroutine set_dr_for_nonequi(nlayer_CMB, nri, radius_1d_rj_r)
!!
!!      subroutine nod_r_2nd_fdm_coefs_equi                             &
!!     &         (nlayer_ICB, nri, radius_1d_rj_r, mat_fdm_2)
!!      subroutine nod_r_2nd_fdm_coefs_nonequi                          &
!!     &         (nlayer_ICB, nri, radius_1d_rj_r, mat_fdm_2)
!!
!!      subroutine nod_r_2nd_fdm_coef_noequi(kr, dr_p1, dr_n1, mat_fdm)
!!*
!!***********************************************************************
!!
!!    delta r for eqidistance grid
!!*        dr_1d_rj(kr) = r(kr+1) - r(kr)
!!*
!!***********************************************************************
!!*
!!***********************************************************************
!!
!!    delta r for non_eqidistance grid
!!*        dr_1d_rj(kr,0) = r(kr+1) - r(kr)
!!*
!!***********************************************************************
!!@endverbatim
!
      module set_radius_func_noequi
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!>      1d @f$ \Delta r @f$ for @f$ f(r,j) @f$
!!@n@see  set_radius_func_cheby or set_radius_func_cheby
      real(kind = kreal), allocatable :: dr_1d_rj(:)
!
      private :: dr_1d_rj
      private :: check_radial_func_rj
!
!  -------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_dr_rj_noequi(nri)
!
      integer(kind = kint), intent(in) :: nri
!
!
      allocate(dr_1d_rj(nri))
      if(nri .gt. 0) dr_1d_rj = 0.0d0
!
      end subroutine allocate_dr_rj_noequi
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_dr_rj_noequi
!
!
      deallocate(dr_1d_rj)
!
      end subroutine deallocate_dr_rj_noequi
!
! ----------------------------------------------------------------------
!
      subroutine check_radial_func_rj(nri, radius_1d_rj_r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!
      integer(kind = kint) :: k
!
!
      write(*,*) 'k, r, dr_1d_rj'
      do k = 1, nri
        write(*,'(i16, 1p4e20.12)') k, radius_1d_rj_r(k), dr_1d_rj(k)
      end do
!
      end subroutine check_radial_func_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_dr_for_equidist                                    &
     &         (nlayer_ICB, nlayer_CMB, nri, radius_1d_rj_r)
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB, nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!
      real(kind = kreal) ::  shell
      integer(kind = kint) :: kr, nele_shell
!
!
!* ---------- whole domain --------
!*
      nele_shell = nlayer_CMB - nlayer_ICB
      shell = radius_1d_rj_r(nlayer_CMB) - radius_1d_rj_r(nlayer_ICB)
!
      do kr = 1, nri
        dr_1d_rj(kr) = shell / dble(nele_shell)
      end do
!*
!* ----------  inner boundary --------
!*
      if (nlayer_ICB .gt. 1) then
        dr_1d_rj(1) = radius_1d_rj_r(2) - radius_1d_rj_r(1)
      end if      
!
      end subroutine set_dr_for_equidist
!
!  -------------------------------------------------------------------
!
      subroutine set_dr_for_nonequi(nlayer_CMB, nri, radius_1d_rj_r)
!
      integer(kind = kint), intent(in) :: nlayer_CMB, nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!
      integer(kind = kint) :: kr
!
!* ---------- whole domain --------
!*
      do kr = 2, nri-1
        dr_1d_rj(kr) = radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr)
      end do
!*
!* ----------  outer boundary --------
!*
      if (nlayer_CMB .eq. nri) then
        dr_1d_rj(nlayer_CMB) =  radius_1d_rj_r(nlayer_CMB)              &
     &                        - radius_1d_rj_r(nlayer_CMB-1)
      else
        kr = nri
        dr_1d_rj(kr) = radius_1d_rj_r(kr) - radius_1d_rj_r(kr-1)
      end if
!*
!* ----------  inner boundary --------
!*
      dr_1d_rj(1) = radius_1d_rj_r(2) - radius_1d_rj_r(1)
!
      if(iflag_debug .eq. iflag_full_msg) then
        call check_radial_func_rj(nri, radius_1d_rj_r)
      end if
!
      end subroutine set_dr_for_nonequi
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine nod_r_2nd_fdm_coefs_equi                               &
     &         (nlayer_ICB, nri, radius_1d_rj_r, mat_fdm_2)
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
      real(kind = kreal), intent(inout) :: mat_fdm_2(3,3,nri)
!
      integer(kind = kint) :: kr
!
!
      if(nlayer_ICB .gt. 1) then
        call nod_r_2nd_fdm_coef_noequi(ione, dr_1d_rj(1),               &
     &      radius_1d_rj_r(1), mat_fdm_2(1,1,1))
      else
        call nod_r_2nd_fdm_coef_noequi                                  &
     &      (ione, dr_1d_rj(1), dr_1d_rj(1), mat_fdm_2(1,1,1))
      end if
!
      do kr = 2, nri
        call nod_r_2nd_fdm_coef_noequi                                  &
     &      (kr, dr_1d_rj(kr), dr_1d_rj(kr), mat_fdm_2(1,1,kr))
      end do
!
      end subroutine nod_r_2nd_fdm_coefs_equi
!
! -----------------------------------------------------------------------
!
      subroutine nod_r_2nd_fdm_coefs_nonequi                            &
     &         (nlayer_ICB, nri, radius_1d_rj_r, mat_fdm_2)
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!
      real(kind = kreal), intent(inout) :: mat_fdm_2(3,3,nri)
!
      integer(kind = kint) :: kr
!
!
      if(nlayer_ICB .gt. 1) then
        call nod_r_2nd_fdm_coef_noequi(ione, dr_1d_rj(1),               &
     &      radius_1d_rj_r(1), mat_fdm_2(1,1,1))
      else
        call nod_r_2nd_fdm_coef_noequi                                  &
     &     (ione, dr_1d_rj(1), dr_1d_rj(1), mat_fdm_2(1,1,1))
      end if
!
      do kr = 2, nri-1
        call nod_r_2nd_fdm_coef_noequi                                  &
     &      (kr, dr_1d_rj(kr), dr_1d_rj(kr-1), mat_fdm_2(1,1,kr))
      end do
!
      kr = nri
      call nod_r_2nd_fdm_coef_noequi                                    &
     &   (kr, dr_1d_rj(kr), dr_1d_rj(kr), mat_fdm_2(1,1,kr))
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
