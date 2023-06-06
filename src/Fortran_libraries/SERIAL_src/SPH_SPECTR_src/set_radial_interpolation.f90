!>@file   set_radial_interpolation.f90
!!@brief      module set_radial_interpolation
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2022
!
!> @brief Pick spectr data to output
!!
!!@verbatim
!!      subroutine s_set_radial_interpolation(nri, radius_1d_rj_r,      &
!!     &          r_ref, kr_st, kr_in, kr_out, coef_in)
!!        integer(kind = kint), intent(in) ::  nri
!!        real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!!        real(kind = kreal), intent(in) :: r_ref
!!        integer(kind = kint), intent(inout) :: kr_in, kr_out, kr_st
!!        real(kind = kreal), intent(inout) :: coef_in
!!@endverbatim
!
      module set_radial_interpolation
!
      use m_precision
      use m_constants
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_radial_interpolation(nri, radius_1d_rj_r,        &
     &          r_ref, kr_st, kr_in, kr_out, coef_in)
!
      integer(kind = kint), intent(in) ::  nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
      real(kind = kreal), intent(in) :: r_ref
!
      integer(kind = kint), intent(inout) :: kr_in, kr_out, kr_st
      real(kind = kreal), intent(inout) :: coef_in
!
      integer(kind = kint) :: kr
!
      if(r_ref .le. radius_1d_rj_r(1)) then
        kr_in =  0
        kr_out = 1
        coef_in = (radius_1d_rj_r(1) - r_ref) / (radius_1d_rj_r(1))
        return
      else if(r_ref .gt. radius_1d_rj_r(nri)) then
        kr_in =  nri+1
        kr_out = nri+1
        coef_in = (radius_1d_rj_r(1) - r_ref) / (radius_1d_rj_r(1))
        return
      end if
!
      if(r_ref .lt. radius_1d_rj_r(kr_st)) kr_st = 1
      kr_in =  izero
      kr_out = izero
      do kr = 1, nri - 1
        if(radius_1d_rj_r(kr) .eq. r_ref) then
          kr_in =  kr
          kr_out = kr
          coef_in =  one
          exit
        end if
        if(radius_1d_rj_r(kr) .lt. r_ref                                &
     &      .and. radius_1d_rj_r(kr+1) .gt. r_ref) then
          kr_in =  kr
          kr_out = kr + 1
          coef_in = (radius_1d_rj_r(kr+1) - r_ref)                      &
     &             / (radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr))
          exit
        end if
      end do
      kr_st = kr_in
!
      end subroutine s_set_radial_interpolation
!
! ----------------------------------------------------------------------
!
      end module set_radial_interpolation
