!>@file   initial_magne_sph_mhd.f90
!!@brief  module initial_magne_sph_mhd
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial magnetic field as dipole and toroidal
!!
!!@verbatim
!!      subroutine initial_magne_shell_dipole(sph, sph_bc_B, l, m,      &
!!     &          n_point, d_rj_magne, d_rj_current)
!!      subroutine initial_magne_shell_toroidal(sph, sph_bc_B, l, m,    &
!!     &          n_point, d_rj_magne, d_rj_current)
!!      subroutine initial_magne_sphere_dipole(sph, sph_bc_B, l, m,     &
!!     &          n_point, d_rj_magne, d_rj_current)
!!      subroutine initial_magne_sphere_toroidal(sph, sph_bc_B, l, m,   &
!!     &          n_point, d_rj_magne, d_rj_current)
!!      subroutine initial_magne_qvc_dipole(sph, sph_bc_B, l, m,        &
!!     &          n_point, d_rj_magne, d_rj_current)
!!      subroutine initial_magne_qvc_toroidal(sph, sph_bc_B, l, m,      &
!!     &          n_point, d_rj_magne, d_rj_current)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        integer(kind = kint), intent(in) :: l, m
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!!@endverbatim
!
      module initial_magne_sph_mhd
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
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
      subroutine initial_magne_shell_dipole(sph, sph_bc_B, l, m,        &
     &          n_point, d_rj_magne, d_rj_current)
!
      use spherical_indices_picker
      use sph_boundary_data_picker
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: l, m
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!
      real(kind = kreal) :: rr, r_in, r_out
      integer(kind = kint) :: inod, k, js
!
!!!!!     Y_{1}^{m} component of poloidal magnetic field
      r_in =   sph_inner_boundary_radius(sph_bc_B)
      r_out =  sph_outer_boundary_radius(sph_bc_B)
      js = find_local_sph_mode_address(sph, l, m)
      if(js .eq. 0) return
!$omp parallel do private(k,inod,rr)
        do k = sph_inner_boundary_r_grid(sph_bc_B),                     &
     &            sph_outer_boundary_r_grid(sph_bc_B)
          inod = local_sph_data_address(sph, k, js)
          rr = radius_1d_rj_r(sph, k)
!
          d_rj_magne(inod,1) = (five / eight)                           &
     &        * (-three * rr**3 + four * r_out * rr**2 - r_in**4/rr)
          d_rj_magne(inod,2) = (five / eight)                           &
     &        * (-dnine * rr**2 + eight * r_out * rr + r_in**4/rr**2)
!
          d_rj_current(inod,3) = (five*three / two) * rr
        end do
!$omp end parallel do
!
      end subroutine initial_magne_shell_dipole
!
!-----------------------------------------------------------------------
!
      subroutine initial_magne_shell_toroidal(sph, sph_bc_B, l, m,      &
     &          n_point, d_rj_magne, d_rj_current)
!
      use spherical_indices_picker
      use sph_boundary_data_picker
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: l, m
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!
      real(kind = kreal) :: pi, rr, r_in
      integer(kind = kint) :: inod, k, jt
!
!!!!!     Y_{2}^{m} component of toroidal magnetic field
      pi = four * atan(one)
      r_in =   sph_inner_boundary_radius(sph_bc_B)
      jt = find_local_sph_mode_address(sph, l, m)
      if (jt .eq. 0) return
!$omp parallel do private(k,inod,rr)
        do k = sph_inner_boundary_r_grid(sph_bc_B),                     &
     &            sph_outer_boundary_r_grid(sph_bc_B)
          inod = local_sph_data_address(sph, k, jt)
          rr = radius_1d_rj_r(sph, k)
          d_rj_magne(inod,3) =  (ten/three) * rr * sin(pi*(rr-r_in))
!
          d_rj_current(inod,1) = d_rj_magne(inod,3)
          d_rj_current(inod,2) = (ten / three) * sin(pi*(rr-r_in))      &
     &                  + (ten / three) * pi * rr * cos(pi*(rr-r_in))
        end do
!$omp end parallel do
!
      end subroutine initial_magne_shell_toroidal
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine initial_magne_sphere_dipole(sph, sph_bc_B, l, m,       &
     &          n_point, d_rj_magne, d_rj_current)
!
      use spherical_indices_picker
      use sph_boundary_data_picker
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: l, m
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!
      real (kind = kreal) :: rr, r_out
      integer(kind = kint) :: inod, k, js
!
!!!!!     Y_{1}^{m} component of poloidal magnetic field
      r_out =  sph_outer_boundary_radius(sph_bc_B)
      js = find_local_sph_mode_address(sph, l, m)
      if(js .eq. 0) return
!$omp parallel do private(k,inod,rr)
        do k = 1, sph_outer_boundary_r_grid(sph_bc_B)
          inod = local_sph_data_address(sph, k, js)
          rr = radius_1d_rj_r(sph, k)
          d_rj_magne(inod,1) = (five / two) * rr**2                     &
     &                      * (four*r_out - three*rr) / (r_out+three)
          d_rj_magne(inod,2) = (five / two) * rr                        &
     &                      * (eight*r_out - dnine*rr) / (r_out+three)
!
          d_rj_current(inod,3) = five*six * rr / (three + r_out)
        end do
!$omp end parallel do
!
      end subroutine initial_magne_sphere_dipole
!
!-----------------------------------------------------------------------
!
      subroutine initial_magne_sphere_toroidal(sph, sph_bc_B, l, m,     &
     &          n_point, d_rj_magne, d_rj_current)
!
      use spherical_indices_picker
      use sph_boundary_data_picker
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: l, m
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!
      real (kind = kreal) :: pi, rr, r_out
      integer(kind = kint) :: inod, k, jt
!
!!!!!     Y_{2}^{m} component of toroidal magnetic field
      pi = four * atan(one)
      r_out =  sph_outer_boundary_radius(sph_bc_B)
      jt = find_local_sph_mode_address(sph, l, m)
      if(jt .eq. 0) return
!$omp parallel do private(k,inod,rr)
        do k = 1, sph_outer_boundary_r_grid(sph_bc_B)
          inod = local_sph_data_address(sph, k, jt)
          rr = radius_1d_rj_r(sph, k)
!
          d_rj_magne(inod,3) = (ten / three) * rr * sin(pi*rr/r_out)
!
          d_rj_current(inod,1) = d_rj_magne(inod,3)
          d_rj_current(inod,2) = (ten / three) * sin(pi*rr/r_out)       &
     &       + (ten / three) * (pi/r_out) * rr * cos(pi*rr/r_out)
        end do
!$omp end parallel do
!
      end subroutine initial_magne_sphere_toroidal
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine initial_magne_qvc_dipole(sph, sph_bc_B, l, m,          &
     &          n_point, d_rj_magne, d_rj_current)
!
      use spherical_indices_picker
      use sph_boundary_data_picker
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: l, m
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!
      real(kind = kreal) :: rr, r_in, r_out
      integer(kind = kint) :: inod, k, js
!
!!!!!     Y_{1}^{m} component of poloidal magnetic field
      r_in =   sph_inner_boundary_radius(sph_bc_B)
      r_out =  sph_outer_boundary_radius(sph_bc_B)
      js = find_local_sph_mode_address(sph, l, m)
      if(js .eq. 0) return
!$omp parallel do private(k,inod,rr)
        do k = sph_inner_boundary_r_grid(sph_bc_B),                     &
     &            sph_outer_boundary_r_grid(sph_bc_B)
          inod = local_sph_data_address(sph, k, js)
          rr = radius_1d_rj_r(sph, k)
!
          d_rj_magne(inod,1) =  (five / eight) * (dnine*half * rr**4    &
     &        - (three*r_in + three*r_out + four) * two * rr**3         &
     &        + (four*r_in + four*r_out + three*r_in*r_out)             &
     &         * three * rr**2                                          &
     &        - four*six * r_in*r_out * rr)
          d_rj_magne(inod,2) =  (five / eight) * (two*dnine * rr**3     &
     &        - (three*r_in + three*r_out + four) * six * rr**2         &
     &        + (four*r_in + four*r_out + three*r_in*r_out)*six * rr    &
     &        - four*six * r_in*r_out)
!
          d_rj_current(inod,3) = (five / eight) * (-four*dnine * rr**2  &
     &             + (three*r_in + three*r_out + four) * eight * rr     &
     &                - eight*six * r_in*r_out / rr)
        end do
!$omp end parallel do
!
      end subroutine initial_magne_qvc_dipole
!
!-----------------------------------------------------------------------
!
      subroutine initial_magne_qvc_toroidal(sph, sph_bc_B, l, m,       &
     &          n_point, d_rj_magne, d_rj_current)
!
      use spherical_indices_picker
      use sph_boundary_data_picker
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: l, m
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!
      real(kind = kreal) :: pi, rr, r_in
      integer(kind = kint) :: inod, k, jt
!
!
!!!!!     Y_{2}^{m} component of toroidal magnetic field
      pi = four * atan(one)
      r_in =   sph_inner_boundary_radius(sph_bc_B)
      jt = find_local_sph_mode_address(sph, l, m)
      if (jt .eq. 0) return
!$omp parallel do private(k,inod,rr)
        do k = sph_inner_boundary_r_grid(sph_bc_B),                     &
     &            sph_outer_boundary_r_grid(sph_bc_B)
          inod = local_sph_data_address(sph, k, jt)
          rr = radius_1d_rj_r(sph, k)
          d_rj_magne(inod,3) = (ten/eight) * rr * sin(pi*(rr-r_in))
!
          d_rj_current(inod,1) = d_rj_magne(inod,3)
          d_rj_current(inod,2) = (ten / eight) * (sin(pi*(rr-r_in))     &
     &                                + pi * rr * cos(pi*(rr-r_in)))
        end do
!$omp end parallel do
!
      end subroutine initial_magne_qvc_toroidal
!
!-----------------------------------------------------------------------
!
      end module initial_magne_sph_mhd
