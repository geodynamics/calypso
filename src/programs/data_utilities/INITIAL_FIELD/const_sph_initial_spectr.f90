!>@file   const_sph_initial_spectr.f90
!!@brief  module const_sph_initial_spectr
!!
!!@author H. Matsui
!!@date Programmed in June, 2013
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine sph_initial_spectrum                                 &
!!     &         (fst_file_IO, sph_MHD_bc, ipol, itor, rj_fld, rst_step)
!!        type(field_IO_params), intent(in) :: fst_file_IO
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol, itor
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(IO_step_param), intent(inout) :: rst_step
!!
!!       Sample program to generate initial field
!!       This program generates initial condition
!!        for dynamo benchmark case 1
!!
!!       j_lc = find_local_sph_mode_address(l, m)
!!         Return local spherical harmonics mode address j_lc for Y(l,m)
!!         If requested mode does not exist in the process, 0 is set
!!       inod = local_sph_data_address(k, j_lc)
!!         Return address of sphectrum data
!!       inod = inod_rj_center()
!!         If spectrum data have center, inod_rj_center 
!!         returns this address.
!!
!!       nidx_rj(1) :: Number of radial grids
!!       rr = radius_1d_rj_r(k)
!!         Return radius at global grid address k
!!
!!       Temperature :: d_rj(:,ipol%i_temp)
!!       Composition :: d_rj(:,ipol%i_light)
!!
!!       Poloidal velocity ::       d_rj(:,ipol%i_velo)
!!       Toroidal velocity ::       d_rj(:,itor%i_velo)
!!       Poloidal magnetic field :: d_rj(:,ipol%i_magne)
!!       Toroidal magnetic field :: d_rj(:,itor%i_magne)
!!
!!       Heat source ::          d_rj(:,ipol%i_heat_source)
!!       Light element source :: d_rj(:,ipol%i_light_source)
!!
!!       nlayer_ICB() :: radial ID for ICB
!!       nlayer_CMB() :: radial ID for CMB
!!        r_ICB() :: ICB radius
!!        r_CMB() :: CMB radius
!!
!!       nidx_rj(1) :: Number of radial grids
!!       sph_bc_*%kr_in :: radial ID for inner boundary of each evolution
!!       sph_bc_*%kr_out :: radial ID for outer boundary
!!       sph_bc_*%r_ICB(0) :: radius for inner boundary of each evolution
!!       sph_bc_*%r_CMB(0) :: radius for outer boundary
!!
!!
!!      subroutine adjust_by_CMB_temp                                   &
!!     &         (sph_bc_T, ipol, n_point, ntot_phys_rj, d_rj)
!!
!!      subroutine add_outer_core_heat_source                           &
!!     &         (sph_bc_T, ipol, n_point, ntot_phys_rj, d_rj)
!!       Set homogenious heat source at outer core
!!       by CMB and ICB heat flux
!!
!!      subroutine add_inner_core_heat_source                           &
!!     &         (sph_bc_T, ipol, n_point, ntot_phys_rj, d_rj)
!!       Set homogenious heat source and temperature at inner core
!!       by CMB heat flux
!!         f_CMB = (dT/dr)_CMB
!!         f_ICB = -(dT/dr)_ICB
!!               = - f_CMB * (r_CMB / r_ICB)**2
!!             Q = - 3.0 * f_ICB / r_ICB
!!          T(r) = T_ICB + 0.5 * (f_ICB / r_ICB) * (r_ICB**2 - r**2)
!!
!!      subroutine add_whole_core_heat_source                           &
!!     &          (sph_bc_T, ipol, n_point, ntot_phys_rj, d_rj)
!!       Set homogenious heat source for whole core
!!       and temperature at inner core by CMB heat flux
!!         f_CMB = (dT/dr)_CMB
!!             Q = - 3.0 * f_CMB / r_CMB
!!         f_ICB = -(dT/dr)_ICB
!!               = - f_CMB * (r_ICB / r_CMB)
!!          T(r) = T_ICB + 0.5 * (f_ICB / r_ICB) * (r_ICB**2 - r**2)
!!@endverbatim
!
!
      module const_sph_initial_spectr
!
      use m_precision
      use m_constants
!
      use m_spheric_parameter
      use t_phys_address
      use t_phys_data
      use t_file_IO_parameter
      use t_boundary_data_sph_MHD
!
      implicit none
!
      private :: set_initial_velocity
      private :: set_initial_temperature
      private :: set_initial_composition
      private :: set_initial_magne_sph
      private :: set_initial_heat_source_sph
      private :: set_initial_light_source_sph
!
      private :: add_inner_core_heat_source
      private :: add_outer_core_heat_source
      private :: add_whole_core_heat_source
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_spectrum                                   &
     &         (fst_file_IO, sph_MHD_bc, ipol, itor, rj_fld, rst_step)
!
      use m_initial_field_control
      use m_MHD_step_parameter
      use t_IO_step_parameter
!
      use sph_mhd_rst_IO_control
      use set_sph_restart_IO
!
      type(field_IO_params), intent(in) :: fst_file_IO
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
      type(IO_step_param), intent(inout) :: rst_step
!
      integer(kind = kint) :: iflag
!
!  Set initial velocity if velocity is exist
      if(ipol%i_velo .gt. izero) then
        call  set_initial_velocity(sph_MHD_bc%sph_bc_U,                 &
     &      ipol, itor, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
!  Set initial temperature if temperature is exist
      if(ipol%i_temp .gt. izero) then
        call  set_initial_temperature(sph_MHD_bc%sph_bc_T,              &
     &      ipol, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
!  Set initial composition if composition is exist
      if(ipol%i_light .gt. izero) then
        call set_initial_composition(sph_MHD_bc%sph_bc_C,               &
     &      ipol, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
!  Set initial magnetic field if magnetic field is exist
      if(ipol%i_magne .gt. izero) then
        call set_initial_magne_sph(sph_MHD_bc%sph_bc_B,                 &
     &      ipol, itor, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
!  Set heat source if  heat source is exist
      if(ipol%i_heat_source .gt. izero) then
        call set_initial_heat_source_sph(sph_MHD_bc%sph_bc_T,           &
     &      ipol, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!      if(ipol%i_heat_source*ipol%i_temp .gt. izero) then
!        call add_inner_core_heat_source(sph_MHD_bc%sph_bc_T,           &
!     &      ipol, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!      end if
!  Set light element source if light element is exist
      if(ipol%i_light_source .gt. izero) then
        call set_initial_light_source_sph(sph_MHD_bc%sph_bc_C,          &
     &      ipol, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
!  Copy initial field to restart IO data
      call copy_time_step_data(MHD_step1%init_d, MHD_step1%time_d)
      call init_output_sph_restart_file(rj_fld)
!
!
      if(MHD_step1%init_d%i_time_step .eq. -1) then
        rst_step%istep_file = MHD_step1%init_d%i_time_step
      else
        iflag = set_IO_step_flag(MHD_step1%time_d%i_time_step,rst_step)
      end if
      call output_sph_restart_control                                   &
     &   (fst_file_IO, MHD_step1%time_d, rj_fld, rst_step)
!
      end subroutine sph_initial_spectrum
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_velocity                                   &
     &         (sph_bc_U, ipol, itor, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer ( kind = kint) :: inod, jj, k
!      real (kind = kreal) :: rr
      real (kind = kreal) :: pi, xr, shell
!      real(kind = kreal), parameter :: A_light = 0.1d0
!
      pi = four * atan(one)
      shell = sph_bc_U%r_CMB(0) - sph_bc_U%r_ICB(0)
!
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,ipol%i_velo) = zero
        d_rj(inod,itor%i_velo) = zero
      end do
!$omp end parallel do
!
!      jj = find_local_sph_mode_address(1, 0)
!      if (jj .gt. 0) then
!        do k = sph_bc_U%kr_in+1, sph_bc_U%kr_out
!          rr = radius_1d_rj_r(k)
!          inod = local_sph_data_address(k,jj)
!          d_rj(inod,itor%i_velo) = half * rr*rr
!        end do
!      end if
!
!      jj =  find_local_sph_mode_address(2, 1)
!
!      if (jj .gt. 0) then
!        do k = sph_bc_U%kr_in, sph_bc_U%kr_out
!          inod = local_sph_data_address(k,jj)
!          xr = two * radius_1d_rj_r(k)                                 &
!    &         - one * (sph_bc_U%r_CMB(0) + sph_bc_U%r_ICB(0)) / shell
!          d_rj(inod,itor%i_velo) = (one-three*xr**2+three*xr**4-xr**6) &
!    &                            * A_light * three / (sqrt(two*pi))
!        end do
!      end if
!
      end subroutine set_initial_velocity
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_temperature                                &
     &         (sph_bc_T, ipol, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer ( kind = kint) :: inod, k, jj
      real (kind = kreal) :: pi, rr, xr, shell
      real(kind = kreal), parameter :: A_temp = 0.1d0
!
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,ipol%i_temp) = zero
      end do
!$omp end parallel do
!
      pi = four * atan(one)
      shell = sph_bc_T%r_CMB(0) - sph_bc_T%r_ICB(0)
!
!   search address for (l = m = 0)
      jj = find_local_sph_mode_address(0, 0)
!
!   set reference temperature if (l = m = 0) mode is there
      if (jj .gt. 0) then
        do k = 1, sph_bc_T%kr_in-1
          inod = local_sph_data_address(k,jj)
          d_rj(inod,ipol%i_temp) = 1.0d0
        end do
        do k = sph_bc_T%kr_in, sph_bc_T%kr_out
          inod = local_sph_data_address(k,jj)
          rr = radius_1d_rj_r(k)
          d_rj(inod,ipol%i_temp) = ((20.d0/13.0d0) / rr  - 1.0d0 )      &
     &                            * 7.0d0 / 13.0d0
        end do
      end if
!
!
!    Find local addrtess for (l,m) = (4,4)
      jj =  find_local_sph_mode_address(4, 4)
!      jj =  find_local_sph_mode_address(5, 5)
!
!    If data for (l,m) = (4,4) is there, set initial temperature
      if (jj .gt. 0) then
!    Set initial field from ICB to CMB
        do k = sph_bc_T%kr_in, sph_bc_T%kr_out
!
!    Set radius data
          rr = radius_1d_rj_r(k)
!    Set 1d address to substitute at (Nr, j)
          inod = local_sph_data_address(k,jj)
!
!    set initial temperature
          xr = two * rr                                                 &
     &        - one * (sph_bc_T%r_CMB(0) + sph_bc_T%r_ICB(0)) / shell
          d_rj(inod,ipol%i_temp) = (one-three*xr**2+three*xr**4-xr**6)  &
     &                            * A_temp * three / (sqrt(two*pi))
        end do
      end if
!
!    Center
      if(inod_rj_center() .gt. 0) then
        jj = find_local_sph_mode_address(0, 0)
        inod = local_sph_data_address(1,jj)
        d_rj(inod_rj_center(),ipol%i_temp) = d_rj(inod,ipol%i_temp)
      end if
!
      end subroutine set_initial_temperature
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_composition                                &
     &         (sph_bc_C, ipol, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_C
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) ::  n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer (kind = kint) :: inod, k, jj
      real (kind = kreal) :: pi, rr, xr, shell
      real(kind = kreal), parameter :: A_light = 0.1d0
!
!
      pi = four * atan(one)
      shell = sph_bc_C%r_CMB(0) - sph_bc_C%r_ICB(0)
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,ipol%i_light) = zero
      end do
!$omp end parallel do
!
!   search address for (l = m = 0)
      jj = find_local_sph_mode_address(0, 0)
!
!   set reference temperature if (l = m = 0) mode is there
!
      if (jj .gt. 0) then
        do k = 1, sph_bc_C%kr_in-1
          inod = local_sph_data_address(k,jj)
          d_rj(inod,ipol%i_light) = 1.0d0
        end do
        do k = sph_bc_C%kr_in, sph_bc_C%kr_out
          inod = local_sph_data_address(k,jj)
          rr = radius_1d_rj_r(k)
          d_rj(inod,ipol%i_light) = ((20.d0/13.0d0) / rr  - 1.0d0 )     &
     &                             * 7.0d0 / 13.0d0
        end do
      end if
!
!
!    Find local addrtess for (l,m) = (4,4)
      jj =  find_local_sph_mode_address(4, 4)
!
      if (jj .gt. 0) then
        do k = sph_bc_C%kr_in, sph_bc_C%kr_out
          inod = local_sph_data_address(k,jj)
          xr = two * radius_1d_rj_r(k)                                  &
     &        - one * (sph_bc_C%r_CMB(0) + sph_bc_C%r_ICB(0)) / shell
          d_rj(inod,ipol%i_light) = (one-three*xr**2+three*xr**4-xr**6) &
     &                            * A_light * three / (sqrt(two*pi))
        end do
      end if
!
!    Center
      if(inod_rj_center() .gt. 0) then
        jj = find_local_sph_mode_address(0, 0)
        inod = local_sph_data_address(1,jj)
        d_rj(inod_rj_center(),ipol%i_light)  = d_rj(inod,ipol%i_light)
      end if
!
      end subroutine set_initial_composition
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_magne_sph                                  &
     &         (sph_bc_B, ipol, itor, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(phys_address), intent(in) :: ipol, itor
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: is, it, k, js, jt, is_ICB, is_CMB
!
!
      pi = four * atan(one)
!
!$omp parallel do
      do is = 1, n_point
        d_rj(is,ipol%i_magne) = zero
        d_rj(is,itor%i_magne) = zero
      end do
!$omp end parallel do
!
!
!    Find local addrtess for (l,m) = (1,0)
      js =  find_local_sph_mode_address(1,0)
!
      if (js .gt. 0) then
        do k = sph_bc_B%kr_in, sph_bc_B%kr_out
          is = local_sph_data_address(k,js)
          rr = radius_1d_rj_r(k)
!   Substitute poloidal mangetic field
          d_rj(is,ipol%i_magne) =  (5.0d0/8.0d0)                        &
     &                           * (-3.0d0 * rr**3                      &
     &                             + 4.0d0 * sph_bc_B%r_CMB(0) * rr**2  &
     &                             - sph_bc_B%r_ICB(0)**4 / rr)
        end do
!
!   Fill potential field if inner core exist
        is_ICB = local_sph_data_address(sph_bc_B%kr_in,js)
        do k = 1, sph_bc_B%kr_in-1
          is = local_sph_data_address(k,js)
          rr = radius_1d_rj_r(k) / sph_bc_B%r_ICB(0)
!   Substitute poloidal mangetic field
          d_rj(is,ipol%i_magne) =  d_rj(is_ICB,ipol%i_magne)            &
     &                            * rr**(ione+1)
        end do
!
!   Fill potential field if external of the core exist
        is_CMB = local_sph_data_address(sph_bc_B%kr_out,js)
        do k = sph_bc_B%kr_out+1, nidx_rj(1)
          is = local_sph_data_address(k,js)
          rr = radius_1d_rj_r(k) / sph_bc_B%r_CMB(0)
!   Substitute poloidal mangetic field
          d_rj(is,ipol%i_magne) =  d_rj(is_ICB,ipol%i_magne)            &
     &                            * rr**(-ione)
        end do
      end if
!
!
!    Find local addrtess for (l,m) = (2,0)
      jt =  find_local_sph_mode_address(1,-1)
!
      if (jt .gt. 0) then
        do k = 1, sph_bc_B%kr_out
          it = local_sph_data_address(k,jt)
          rr = radius_1d_rj_r(k)
!   Substitute totoidal mangetic field
          d_rj(it,itor%i_magne) = (10.0d0/3.0d0) * rr                   &
     &                           * sin(pi*(rr - sph_bc_B%r_ICB(0)))
        end do
      end if
!
      end subroutine set_initial_magne_sph
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_heat_source_sph                            &
     &         (sph_bc_T, ipol, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) ::  n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real (kind = kreal) :: rr, q
      integer(kind = kint) :: inod, k, jj
!
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,ipol%i_heat_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(0, 0)
!
      if (jj .gt. 0) then
        q = (three / (sph_bc_T%r_CMB(0)**3 - sph_bc_T%r_ICB(0)**3))     &
     &     * (-sph_bc_T%CMB_flux(jj) * sph_bc_T%r_CMB(0)**2             &
     &        - sph_bc_T%ICB_flux(jj) * sph_bc_T%r_ICB(0)**2)
!
        do k = sph_bc_T%kr_in, sph_bc_T%kr_out
          inod = local_sph_data_address(k,jj)
          rr = radius_1d_rj_r(k)
!   Substitute initial heat source
          d_rj(inod,ipol%i_heat_source)  = q
        end do
      end if
!    Center
      if(inod_rj_center() .gt. 0) then
        d_rj(inod_rj_center(),ipol%i_heat_source) = q
      end if
!
      end subroutine set_initial_heat_source_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_light_source_sph                           &
     &         (sph_bc_C, ipol, n_point, ntot_phys_rj, d_rj)
!
      use calypso_mpi
!
      type(sph_boundary_type), intent(in) :: sph_bc_C
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, jj, inod
!
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,ipol%i_light_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(0, 0)
!
      if (jj .gt. 0) then
        do k = 1, sph_bc_C%kr_in-1
          inod = local_sph_data_address(k,jj)
          d_rj(inod,ipol%i_light) = 1.0d0
        end do
        do k = sph_bc_C%kr_in, sph_bc_C%kr_out
          inod = local_sph_data_address(k,jj)
!          rr = radius_1d_rj_r(k)
!    Substitute initial heat source
          d_rj(inod,ipol%i_light_source) = 1.0d0
        end do
      end if
!
!    Center
      if(inod_rj_center() .gt. 0) then
        d_rj(inod_rj_center(),ipol%i_light_source) = 1.0d0
      end if
!
      end subroutine set_initial_light_source_sph
!
!-----------------------------------------------------------------------
!
      subroutine adjust_by_CMB_temp                                     &
     &         (sph_bc_T, ipol, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer ( kind = kint) :: inod, k, jj
      real (kind = kreal) :: temp_CMB = 0.0d0
!
!
!   search address for (l = m = 0)
      jj = find_local_sph_mode_address(0, 0)
!
!   set reference temperature if (l = m = 0) mode is there
      if (jj .gt. 0) then
        inod = local_sph_data_address(sph_bc_T%kr_out,jj)
        temp_CMB = d_rj(inod,ipol%i_temp)
!
        do k = 1, nidx_rj(1)
          inod = local_sph_data_address(k,jj)
          d_rj(inod,ipol%i_temp) = d_rj(inod,ipol%i_temp) - temp_CMB
        end do
      end if
!
!    Center
      if(inod_rj_center() .gt. 0) then
        jj = find_local_sph_mode_address(0, 0)
        inod = local_sph_data_address(1,jj)
        d_rj(inod_rj_center(),ipol%i_temp)                              &
     &              = d_rj(inod,ipol%i_temp) - temp_CMB
      end if
!
      end subroutine adjust_by_CMB_temp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_outer_core_heat_source                             &
     &         (sph_bc_T, ipol, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) ::  n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real (kind = kreal) :: rr, q
      integer(kind = kint) :: inod, k, jj
!
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,ipol%i_heat_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(0, 0)
!
      if (jj .gt. 0) then
        q = (three / (sph_bc_T%r_CMB(0)**3 - sph_bc_T%r_ICB(0)**3))     &
     &     * (-sph_bc_T%CMB_flux(jj) * sph_bc_T%r_CMB(0)**2             &
     &       - sph_bc_T%ICB_flux(jj) * sph_bc_T%r_ICB(0)**2)
!
!
        do k = sph_bc_T%kr_in, sph_bc_T%kr_out
          inod = local_sph_data_address(k,jj)
          rr = radius_1d_rj_r(k)
!   Substitute initial heat source
          d_rj(inod,ipol%i_heat_source)  = q
        end do
      end if
!
      end subroutine add_outer_core_heat_source
!
!-----------------------------------------------------------------------
!
      subroutine add_inner_core_heat_source                             &
     &         (sph_bc_T, ipol, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) ::  n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real (kind = kreal) :: rr, q, T_ICB, f_ICB
      integer(kind = kint) :: inod, k, jj
!
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,ipol%i_heat_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(0, 0)
!
      if (jj .gt. 0) then
        f_ICB = -sph_bc_T%CMB_flux(jj)                                  &
     &         * (sph_bc_T%r_CMB(0) / r_ICB())**2
        q = three * f_ICB / r_ICB()
!
        inod = local_sph_data_address(nlayer_ICB(),jj)
        T_ICB = d_rj(inod,ipol%i_temp)
!
        do k = 1, nlayer_ICB()
          inod = local_sph_data_address(k,jj)
          rr = radius_1d_rj_r(k)
!   Substitute initial heat source
          d_rj(inod,ipol%i_heat_source) = q
!   Fill inner core temperature
          d_rj(inod,ipol%i_temp) = T_ICB                                &
     &       + half * f_ICB * (r_ICB()**2 - rr**2) / r_ICB()
        end do
      end if
!
!    Center
      if(inod_rj_center() .gt. 0) then
        inod = inod_rj_center()
        d_rj(inod,ipol%i_heat_source) = q
        d_rj(inod,ipol%i_temp) = T_ICB + half * f_ICB * r_ICB()
      end if
!
      end subroutine add_inner_core_heat_source
!
!-----------------------------------------------------------------------
!
      subroutine add_whole_core_heat_source                             &
     &         (sph_bc_T, ipol, n_point, ntot_phys_rj, d_rj)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) ::  n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real (kind = kreal) :: rr, q, T_ICB, f_ICB
      integer(kind = kint) :: inod, k, jj
!
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,ipol%i_heat_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(0, 0)
!
      if (jj .gt. 0) then
        q = - three * sph_bc_T%CMB_flux(jj) / sph_bc_T%r_CMB(0)
        f_ICB = -sph_bc_T%CMB_flux(jj)                                  &
     &         * (r_ICB() / sph_bc_T%r_CMB(0))
        write(*,*) 'q', q
        write(*,*) 'flux_ICB', f_ICB
!
        inod = local_sph_data_address(nlayer_ICB(),jj)
        T_ICB = d_rj(inod,ipol%i_temp)
!
        do k = 1, sph_bc_T%kr_out
          inod = local_sph_data_address(k,jj)
          rr = radius_1d_rj_r(k)
!   Substitute initial heat source
          d_rj(inod,ipol%i_heat_source) = q
        end do
!
        do k = 1, nlayer_ICB()
          inod = local_sph_data_address(k,jj)
          rr = radius_1d_rj_r(k)
!   Fill inner core temperature
          d_rj(inod,ipol%i_temp) = T_ICB                                &
     &       + half * f_ICB * (r_ICB()**2 - rr**2) / r_ICB()
        end do
      end if
!
!    Center
      if(inod_rj_center() .gt. 0) then
        inod = inod_rj_center()
        d_rj(inod,ipol%i_heat_source) = q
        d_rj(inod,ipol%i_temp) = T_ICB + half * f_ICB * r_ICB()
      end if
!
      end subroutine add_whole_core_heat_source
!
!-----------------------------------------------------------------------
!
      end module const_sph_initial_spectr
