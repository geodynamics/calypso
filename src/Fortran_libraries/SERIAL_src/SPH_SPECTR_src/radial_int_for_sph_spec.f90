!>@file   radial_int_for_sph_spec.f90
!!@brief  module radial_int_for_sph_spec
!!
!!@author H. Matsui
!!@date Programmed on  Feb., 2008
!
!>@brief  Evaluate radial integration
!!
!!@verbatim
!!      subroutine radial_integration(jmax, nri, kg_st, kg_ed, radius,  &
!!     &          ntot_comp, f_org, f_int)
!!  Evaluate radial integration f_int =  \int f_org r^{2} dr
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
      subroutine radial_integration(jmax, nri, kg_st, kg_ed, radius,    &
     &          ntot_comp, f_org, f_int)
!
      integer(kind = kint),  intent(in) :: jmax,nri, kg_st, kg_ed
      real(kind = kreal), intent(in) :: radius(nri)
      integer(kind = kint),  intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: f_org(jmax,nri,ntot_comp)
!
      real(kind = kreal), intent(inout) :: f_int(jmax,ntot_comp)
!
!
!      if( mod( (kg_ed-kg_st),2) .eq. 0) then
!        call radial_int_by_simpson(jmax, nri, kg_st, kg_ed, radius,    &
!     &      ntot_comp, f_org, f_int)
!      else
        call radial_int_by_trapezoid(jmax, nri, kg_st, kg_ed, radius,   &
     &      ntot_comp, f_org, f_int)
!      end if
!
      end subroutine radial_integration
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine radial_int_by_trap_old(nri, kg_st, kg_ed, radius,     &
     &          ntot_comp, f_org, f_int)
!
      integer(kind = kint),  intent(in) :: nri, kg_st, kg_ed
      real(kind = kreal), intent(in) :: radius(nri)
      integer(kind = kint),  intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: f_org(ntot_comp,nri)
!
      real(kind = kreal), intent(inout) :: f_int(ntot_comp)
!
      integer(kind = kint) :: icomp, kr, kst
      real(kind = kreal) :: dr1
!
!
      if(kg_st .eq. 0) then
        kst = 1
        dr1 = radius(1)
!$omp parallel do private(icomp)
        do icomp = 1, ntot_comp
          f_int(icomp) = f_org(icomp,1) * dr1 * half
        end do
!$omp end parallel do
      else
        kst = kg_st
!$omp parallel do private(icomp)
        do icomp = 1, ntot_comp
          f_int(icomp) = zero
        end do
!$omp end parallel do
      end if
!
!$omp parallel private(dr1)
      do kr = kst, kg_ed-1
        dr1 = radius(kr+1) - radius(kr)
!$omp do private(icomp)
        do icomp = 1, ntot_comp
          f_int(icomp) = f_int(icomp) + half * dr1                      &
     &                  * (f_org(icomp,kr  ) + f_org(icomp,kr+1))
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine radial_int_by_trap_old
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine radial_int_by_trapezoid(jmax, nri, kg_st, kg_ed,       &
     &          radius, ntot_comp, f_org, f_int)
!
      integer(kind = kint),  intent(in) :: jmax, nri, kg_st, kg_ed
      real(kind = kreal), intent(in) :: radius(nri)
      integer(kind = kint),  intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: f_org(jmax,nri,ntot_comp)
!
      real(kind = kreal), intent(inout) :: f_int(jmax,ntot_comp)
!
      integer(kind = kint) :: icomp, kr, kst, j
      real(kind = kreal) :: dr1
!
!
      if(kg_st .eq. 0) then
        kst = 1
        dr1 = radius(1)
!$omp parallel do private(icomp,j)
        do icomp = 1, ntot_comp
          do j = 1, jmax
            f_int(j,icomp) = f_org(j,1,icomp) * dr1 * half
          end do
        end do
!$omp end parallel do
      else
        kst = kg_st
!$omp parallel do private(icomp,j)
        do icomp = 1, ntot_comp
          do j = 1, jmax
            f_int(j,icomp) = zero
          end do
        end do
!$omp end parallel do
      end if
!
!$omp parallel do private(icomp,j,kr,dr1)
      do icomp = 1, ntot_comp
        do j = 1, jmax
          do kr = kst, kg_ed-1
            dr1 = radius(kr+1) - radius(kr)
            f_int(j,icomp) = f_int(j,icomp) + half * dr1                &
     &                  * (f_org(j,kr,icomp) + f_org(j,kr+1,icomp))
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine radial_int_by_trapezoid
!
! -----------------------------------------------------------------------
!
      subroutine radial_int_by_simpson(jmax, nri, kg_st, kg_ed, radius, &
     &          ntot_comp, f_org, f_int)
!
      integer(kind = kint),  intent(in) :: jmax, nri, kg_st, kg_ed
      real(kind = kreal), intent(in) :: radius(nri)
      integer(kind = kint),  intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: f_org(jmax,nri,ntot_comp)
!
      real(kind = kreal), intent(inout) :: f_int(jmax,ntot_comp)
!
      integer(kind = kint) :: icomp, kr, kst, j
      real(kind = kreal) :: dr1, dr2, drs, coef
!
!
      if(kg_st .eq. 0) then
        kst = 2
        dr1 = radius(1)
        dr2 = radius(2) - radius(1)
        drs = radius(2)
        coef = drs*drs / (6.0d0*dr1*dr2)
!$omp parallel do private(icomp,j)
        do icomp = 1, ntot_comp
          do j = 1, jmax
            f_int(j,icomp) = half * drs * (f_org(j,2,icomp))            &
     &          + coef * (f_org(j,1,icomp)*drs - f_org(j,2,icomp)*dr1 )
          end do
        end do
!$omp end parallel do
      else
        kst = kg_st
!$omp parallel do private(icomp)
        do icomp = 1, ntot_comp
          do j = 1, jmax
            f_int(j,icomp) = zero
          end do
        end do
!$omp end parallel do
      end if
!
!$omp parallel do private(icomp,dr1,dr2,drs,coef,j)
      do icomp = 1, ntot_comp
        do j = 1, jmax
          do kr = kst, kg_ed-2, 2
            dr1 = radius(kr+1) - radius(kr)
            dr2 = radius(kr+2) - radius(kr+1)
            drs = radius(kr+2) - radius(kr)
            coef = drs*drs / (6.0d0*dr1*dr2)
            f_int(j,icomp) = f_int(j,icomp)                             &
     &          + half*drs * (f_org(j,kr, icomp) + f_org(j,kr+2,icomp)) &
     &          + coef * (f_org(j,kr+1,icomp)*drs                       &
     &            - (f_org(j,kr+2,icomp)*dr1 + f_org(j,kr,icomp)*dr2))
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine radial_int_by_simpson
!
! -----------------------------------------------------------------------
!
      end module radial_int_for_sph_spec
