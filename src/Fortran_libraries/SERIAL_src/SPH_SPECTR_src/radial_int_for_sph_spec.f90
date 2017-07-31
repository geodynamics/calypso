!>@file   radial_int_for_sph_spec.f90
!!@brief  module radial_int_for_sph_spec
!!
!!@author H. Matsui
!!@date Programmed on  Feb., 2008
!
!>@brief  Evaluate radial integration
!!
!!@verbatim
!!      subroutine radial_integration(kg_st, kg_ed, nri, radius,        &
!!     &         , ntot_comp, f_org, f_int)
!!  Evaluate radial integration f_int =  \int f_org r^{2} dr
!!
!!      subroutine radial_int_matrix_by_simpson                         &
!!     &         (nri, kg_st, kg_ed, radius, a_int, a_ctr)
!!      subroutine radial_int_by_trapezoid                              &
!!     &         (nri, kg_st, kg_ed, radius, a_ctr, a_int)
!!
!!      subroutine radial_int_matrix_by_trapezoid                       &
!!     &         (nri, kg_st, kg_ed, radius, a_int, a_ctr)
!!      subroutine radial_int_matrix_by_simpson                         &
!!     &         (nri, kg_st, kg_ed, radius, a_int, a_ctr)
!!@endverbatim
!!
!!@n @param  ltr      Truncation of spherical harmonics
!
      module radial_int_for_sph_spec
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: radial_int_by_trapezoid, radial_int_by_simpson
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine radial_integration(kg_st, kg_ed, nri, radius,          &
     &          ntot_comp, f_org, f_int)
!
      integer(kind = kint),  intent(in) :: nri, kg_st, kg_ed
      real(kind = kreal), intent(in) :: radius(nri)
      integer(kind = kint),  intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: f_org(0:nri,ntot_comp)
!
      real(kind = kreal), intent(inout) :: f_int(ntot_comp)
!
      integer(kind = kint) :: icomp
!
!
!      if( mod( (kg_ed-kg_st),2) .eq. 0) then
!!$omp parallel do private(icomp)
!        do icomp = 1, ntot_comp
!          call radial_int_by_simpson(nri, kg_st, kg_ed, radius,        &
!     &          f_org(1,icomp), f_org(0,icomp), f_int(1,icomp) )
!        end do
!!$omp end parallel do
!      else
!$omp parallel do private(icomp)
        do icomp = 1, ntot_comp
          call radial_int_by_trapezoid(nri, kg_st, kg_ed, radius,       &
     &        f_org(1,icomp), f_org(0,icomp), f_int(icomp) )
        end do
!$omp end parallel do
!      end if
!
      end subroutine radial_integration
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine radial_int_by_trapezoid(nri, kg_st, kg_ed, radius,     &
     &          f_org, f_ctr, f_int)
!
      integer(kind = kint),  intent(in) :: nri, kg_st, kg_ed
      real(kind = kreal), intent(in) :: radius(nri)
      real(kind = kreal), intent(in) :: f_org(nri)
      real(kind = kreal), intent(in) :: f_ctr
!
      real(kind = kreal), intent(inout) :: f_int
!
      integer(kind = kint) :: kr, kst
      real(kind = kreal) :: dr1
!
!
      if(kg_st .eq. 0) then
        kst = 1
        dr1 = radius(1)
        f_int = half * dr1 * (f_ctr + f_org(1))
      else
        kst = kg_st
        f_int = zero
      end if
!
      do kr = kst, kg_ed-1
        dr1 = radius(kr+1) - radius(kr)
        f_int = f_int + half * dr1 * (f_org(kr) + f_org(kr+1))
      end do
!
      end subroutine radial_int_by_trapezoid
!
! -----------------------------------------------------------------------
!
      subroutine radial_int_by_simpson(nri, kg_st, kg_ed, radius,       &
     &          f_org, f_ctr, f_int)
!
      integer(kind = kint),  intent(in) :: nri, kg_st, kg_ed
      real(kind = kreal), intent(in) :: radius(nri)
      real(kind = kreal), intent(in) :: f_org(nri)
      real(kind = kreal), intent(in) :: f_ctr
!
      real(kind = kreal), intent(inout) :: f_int
!
      integer(kind = kint) :: kr, kst
      real(kind = kreal) :: dr1, dr2, drs, coef
!
!
      if(kg_st .eq. 0) then
        kst = 2
        dr1 = radius(1)
        dr2 = radius(2) - radius(1)
        drs = radius(2)
        coef = drs*drs / (6.0d0*dr1*dr2)
        f_int = half*drs * (f_ctr + f_org(2))                           &
     &                 + coef * (f_org(1)*drs                           &
     &                 - (f_org(2)*dr1 + f_ctr*dr2))
      else
        kst = kg_st
        f_int = zero
      end if
!
      do kr = kst, kg_ed-2, 2
        dr1 = radius(kr+1) - radius(kr)
        dr2 = radius(kr+2) - radius(kr+1)
        drs = radius(kr+2) - radius(kr)
        coef = drs*drs / (6.0d0*dr1*dr2)
        f_int = f_int + half*drs * (f_org(kr) + f_org(kr+2))            &
     &                 + coef * (f_org(kr+1)*drs                        &
     &                 - (f_org(kr+2)*dr1 + f_org(kr)*dr2))
      end do
!
      end subroutine radial_int_by_simpson
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine radial_int_matrix_by_simpson                           &
     &         (nri, kg_st, kg_ed, radius, a_int, a_ctr)
!
      integer(kind = kint),  intent(in) :: nri, kg_st, kg_ed
      real(kind = kreal), intent(in) :: radius(nri)
!
      real(kind = kreal), intent(inout) :: a_ctr
      real(kind = kreal), intent(inout) :: a_int(nri)
!
      integer(kind = kint) :: kr, kst
      real(kind = kreal) :: dr1, dr2, drs, coef
!
      if(kg_st .eq. 0) then
        kst = 2
        dr1 = radius(1)
        dr2 = radius(2) - radius(1)
        drs = radius(2)
        coef = drs*drs / (6.0d0*dr1*dr2)
!
        a_ctr =     half*drs - coef*dr2
        a_int(1) =  coef*drs
        a_int(2) =  half*drs - coef*dr1
      else
        kst = kg_st
        a_int(kst) = zero
      end if
!
      do kr = kst, kg_ed-2, 2
        dr1 = radius(kr+1) - radius(kr)
        dr2 = radius(kr+2) - radius(kr+1)
        drs = radius(kr+2) - radius(kr)
        coef = drs*drs / (6.0d0*dr1*dr2)
        a_int(kr  ) = half*drs -  coef*dr2 + a_int(kr  )
        a_int(kr+1) = coef*drs
        a_int(kr+2) = (half*drs - coef*dr1)
      end do
!
      end subroutine radial_int_matrix_by_simpson
!
! -----------------------------------------------------------------------
!
      subroutine radial_int_matrix_by_trapezoid                         &
     &         (nri, kg_st, kg_ed, radius, a_int, a_ctr)
!
      integer(kind = kint),  intent(in) :: nri, kg_st, kg_ed
      real(kind = kreal), intent(in) :: radius(nri)
!
      real(kind = kreal), intent(inout) :: a_ctr
      real(kind = kreal), intent(inout) :: a_int(nri)
!
      integer(kind = kint) :: kr, kst
      real(kind = kreal) :: dr1
!
!
      if(kg_st .eq. 0) then
        kst = 1
        a_ctr =    half * radius(1)
        a_int(1) = half * radius(1)
      else
        kst = kg_st
        a_int(kg_st) = zero
      end if
!
      do kr = kst, kg_ed-1
        dr1 = radius(kr+1) - radius(kr)
        a_int(kr  ) = half * dr1 + a_int(kr  )
        a_int(kr+1) = half * dr1
      end do
!
      end subroutine radial_int_matrix_by_trapezoid
!
! -----------------------------------------------------------------------
!
      end module radial_int_for_sph_spec
