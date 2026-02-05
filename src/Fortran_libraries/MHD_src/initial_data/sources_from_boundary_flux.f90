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
!!     &                          (idx_degree_zero, sph_bc_T, bcs_T)
!!        integer(kind = kint), intent(in) :: idx_degree_zero
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!      flux_total = 4*pi * (flux_ICB * r_ICB^2 + flux_CMB * r_CMB^2)
!!      source = - flux_total / (4/3 * pi * (r_CMB^3 - r_ICB^3))
!!
!!   Get homogeneous heat flux in INNER core from CMB heat flux
!!      real(kind = kreal) function source_from_inner_core              &
!!     &                          (idx_degree_zero, sph_bc_T, bcs_T)
!!        integer(kind = kint), intent(in) :: idx_degree_zero
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!      flux_CMB = 4*pi * flux_CMB * r_CMB^2
!!      source = - flux_CMB / (4/3 * pi * r_ICB^3)
!!
!!   Get homogeneous heat flux in whole core from CMB heat flux
!!      real(kind = kreal) function source_at_whole_core(sph_bc_T,      &
!!     &                                                 bcs_T)
!!        integer(kind = kint), intent(in) :: idx_degree_zero
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!      flux_CMB = 4*pi * flux_CMB * r_CMB^2
!!      source = - flux_CMB / (4/3 * pi * r_CMB^3)
!!@endverbatim
!
      module sources_from_boundary_flux
!
      use m_precision
      use m_constants
!
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
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
     &                          (idx_degree_zero, sph_bc_T, bcs_T)
!
      use sph_boundary_data_picker
!
      integer(kind = kint), intent(in) :: idx_degree_zero
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
!
      real(kind = kreal) :: r_in, r_out
      real(kind = kreal) :: flux_ICB, flux_CMB
      real(kind = kreal) :: q
!
!
      r_in =     sph_inner_boundary_radius(sph_bc_T)
      r_out =    sph_outer_boundary_radius(sph_bc_T)
      flux_ICB = sph_inner_boundary_scalar_coef(bcs_T, idx_degree_zero)
      flux_CMB = sph_outer_boundary_scalar_coef(bcs_T, idx_degree_zero)
!
      q = flux_ICB * r_in**2 + flux_CMB * r_out**2
      source_by_both_fluxes = - q * (three / (r_out**3 - r_in**3))
!
      end function source_by_both_fluxes
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function source_from_inner_core                &
     &                          (idx_degree_zero, sph_bc_T, bcs_T)
!
      use sph_boundary_data_picker
!
      integer(kind = kint), intent(in) :: idx_degree_zero
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
!
      real(kind = kreal) :: r_in, r_out, flux_CMB
      real(kind = kreal) :: q
!
!
      r_in =     sph_inner_boundary_radius(sph_bc_T)
      r_out =    sph_outer_boundary_radius(sph_bc_T)
      flux_CMB = sph_outer_boundary_scalar_coef(bcs_T, idx_degree_zero)
!
      q = flux_CMB * r_out**2
      source_from_inner_core = - q * (three / r_in**3)
!
      end function source_from_inner_core
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function source_at_whole_core                  &
     &                          (idx_degree_zero, sph_bc_T, bcs_T)
!
      use sph_boundary_data_picker
!
      integer(kind = kint), intent(in) :: idx_degree_zero
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
!
      real(kind = kreal) :: r_out, flux_CMB
!
      r_out =    sph_outer_boundary_radius(sph_bc_T)
      flux_CMB = sph_outer_boundary_scalar_coef(bcs_T, idx_degree_zero)
!
      source_at_whole_core = - three * flux_CMB / r_out
!
      end function source_at_whole_core
!
!-----------------------------------------------------------------------
!
      end module sources_from_boundary_flux

