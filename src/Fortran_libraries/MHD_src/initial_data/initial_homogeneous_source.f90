!>@file   initial_homogeneous_source.f90
!!@brief  module initial_homogeneous_source
!!
!!@author H. Matsui
!!@date Programmed in June, 2013
!
!> @brief Obtain homogeneous sources from boundary flux
!!
!!@verbatim
!!      subroutine set_outer_core_heat_source(sph, sph_bc_T, source,    &
!!     &                                      n_point, souce_rj)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: souce_rj(n_point)
!!   Set homogeneous heat flux in outer core
!!
!!      subroutine add_heat_source_from_centre(sph, kr_end, source,     &
!!     &                                       n_point, souce_rj)
!!        type(sph_grids), intent(in) :: sph
!!        integer(kind = kint), intent(in)  :: kr_end
!!        real(kind = kreal), intent(in) :: source
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: souce_rj(n_point)
!!   Set homogeneous heat flux to radial grid kr_end
!!
!!      subroutine add_inner_core_temp_w_source(sph, sph_bc_T, source,  &
!!     &                                        n_point, temp_rj)
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_grids), intent(in) :: sph
!!        real(kind = kreal), intent(in) :: source
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: temp_rj(n_point)
!!   Temperature with homogeneous heat flux in inner core
!!          T(r) = T_ICB + (2/3) * source * (r_ICB**2 - rr**2)
!!@endverbatim
!
      module initial_homogeneous_source
!
      use m_precision
      use m_constants
!
      use t_boundary_params_sph_MHD
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_outer_core_heat_source(sph, sph_bc_T, source,      &
     &                                      n_point, souce_rj)
!
      use spherical_indices_picker
      use sph_boundary_data_picker
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_T
      real(kind = kreal), intent(in) :: source
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: souce_rj(n_point)
!
      integer(kind = kint) :: jj, k, inod, kr_in, kr_out
!
!
!$omp parallel workshare
      souce_rj(1:n_point) = zero
!$omp end parallel workshare
!
!    Find address for l = m = 0
      kr_in =  sph_inner_boundary_r_grid(sph_bc_T)
      kr_out = sph_outer_boundary_r_grid(sph_bc_T)
      jj =  idx_rj_degree_zero(sph)
      if(jj .le. 0) return
!
!   Substitute initial heat source
      do k = kr_in, kr_out
        inod = local_sph_data_address(sph, k, jj)
        souce_rj(inod)  = source
      end do
!
      end subroutine set_outer_core_heat_source
!
!-----------------------------------------------------------------------
!
      subroutine add_heat_source_from_centre(sph, kr_end, source,       &
     &                                       n_point, souce_rj)
!
      use spherical_indices_picker
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in)  :: kr_end
      real(kind = kreal), intent(in) :: source
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: souce_rj(n_point)
!
      integer(kind = kint) :: inod
      integer :: jj, k
!
!
!    Find address for l = m = 0
      jj = idx_rj_degree_zero(sph)
      if(jj .eq. 0) return
!
!   Substitute initial heat source
      do k = 1, kr_end
        inod = local_sph_data_address(sph, k, jj)
        souce_rj(inod) = source
      end do
!
!    Center
      inod = inod_rj_center(sph)
      if(inod .gt. 0) souce_rj(inod) = source
!
      end subroutine add_heat_source_from_centre
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_inner_core_temp_w_source(sph, sph_bc_T, source,    &
     &                                        n_point, temp_rj)
!
      use spherical_indices_picker
      use sph_boundary_data_picker
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_grids), intent(in) :: sph
      real(kind = kreal), intent(in) :: source
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: temp_rj(n_point)
!
      real (kind = kreal) :: rr, radius_in, T_ICB
      integer(kind = kint) :: inod, jj, k, kr_in
!
!    Find address for l = m = 0
      jj =  idx_rj_degree_zero(sph)
      if(jj .eq. 0) return
!
      radius_in = sph_inner_boundary_radius(sph_bc_T)
      kr_in =     sph_inner_boundary_r_grid(sph_bc_T)
      inod = local_sph_data_address(sph, kr_in, jj)
      T_ICB = temp_rj(inod)
!
!   Fill inner core temperature
      do k = 1, kr_in
        inod = local_sph_data_address(sph, k, jj)
        rr = radius_1d_rj_r(sph, k)
        temp_rj(inod) = T_ICB + (half / three) * source                 &
     &                         * (radius_in**2 - rr**2)
      end do
!
!    Center
      inod = inod_rj_center(sph)
      if(inod .le. 0) return
      temp_rj(inod) = T_ICB + (half / three) * source * radius_in**2 
!
      end subroutine add_inner_core_temp_w_source
!
!-----------------------------------------------------------------------
!
      end module initial_homogeneous_source

