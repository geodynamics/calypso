!>@file   sources_from_boundary_flux.f90
!!@brief  module sources_from_boundary_flux
!!
!!@author H. Matsui
!!@date Programmed in June, 2013
!
!> @brief Obtain homogeneous sources from boundary flux
!!
!!@verbatim
!!   Get homogeneous heat flux from heat flux at ICB and CMB
!!      real(kind = kreal) function source_by_both_fluxes               &
!!     &                          (r_ICB, r_CMB, flux_ICB, flux_CMB)
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: flux_ICB, flux_CMB
!!      flux_total = 4*pi * (flux_ICB * r_ICB^2 + flux_CMB * r_CMB^2)
!!      source = - flux_total / (4/3 * pi * (r_CMB^3 - r_ICB^3))
!!
!!   Get homogeneous heat flux in INNER core from CMB heat flux
!!      real(kind = kreal) function source_from_inner_core(r_ICB, r_CMB,&
!!     &                                                   flux_CMB)
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: flux_CMB
!!           source_IC = - 3 * r_CMB^2 / r_ICB^3
!!
!!      real(kind = kreal) function inner_core_source_w_OC_sorce        &
!!     &                          (r_ICB, r_CMB, flux_CMB, source_OC)
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: source_OC
!!        real(kind = kreal), intent(in) :: flux_CMB
!!           source_IC = - 3 * r_CMB^2 / r_ICB^3
!!                      - source_OC * ((r_CMB/r_ICB)^3 - one)
!!
!!   Get homogeneous heat flux in whole core from CMB heat flux
!!      real(kind = kreal) function source_at_whole_core(r_CMB,         &
!!     &                                                 flux_CMB)
!!        real(kind = kreal), intent(in) :: r_CMB
!!        real(kind = kreal), intent(in) :: flux_CMB
!!      flux_CMB = 4*pi * flux_CMB * r_CMB^2
!!      source = - flux_CMB / (4/3 * pi * r_CMB^3)
!!@endverbatim
!
      module sources_from_boundary_flux
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function source_by_both_fluxes                 &
     &                          (r_ICB, r_CMB, flux_ICB, flux_CMB)
!
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: flux_ICB, flux_CMB
!
      real(kind = kreal) :: q
!
      q = flux_ICB * r_ICB**2 + flux_CMB * r_CMB**2
      source_by_both_fluxes = - three * q / (r_CMB**3 - r_ICB**3)
!
      end function source_by_both_fluxes
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function source_from_inner_core(r_ICB, r_CMB,  &
     &                                                   flux_CMB)
!
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: flux_CMB
!
      source_from_inner_core = - three * flux_CMB * r_CMB**2 / r_ICB**3
!
      end function source_from_inner_core
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function inner_core_source_w_OC_sorce          &
     &                          (r_ICB, r_CMB, flux_CMB, source_OC)
!
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind = kreal), intent(in) :: source_OC
      real(kind = kreal), intent(in) :: flux_CMB
!
      inner_core_source_w_OC_sorce                                      &
     &     = source_from_inner_core(r_ICB, r_CMB, flux_CMB)             &
     &      - source_OC * ((r_CMB/r_ICB)**3 - one)
!
      end function inner_core_source_w_OC_sorce
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function source_at_whole_core(r_CMB,           &
     &                                                 flux_CMB)
!
      real(kind = kreal), intent(in) :: r_CMB
      real(kind = kreal), intent(in) :: flux_CMB
!
      source_at_whole_core = - three * flux_CMB / r_CMB
!
      end function source_at_whole_core
!
!-----------------------------------------------------------------------
!
      end module sources_from_boundary_flux

