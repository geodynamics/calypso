!>@file   const_sph_initial_spectr.f90
!!@brief  module const_sph_initial_spectr
!!
!!@author H. Matsui
!!@date Programmed in June, 2013
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine sph_initial_spectrum
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
!!       nidx_rj(1) :: Number of radial grids
!!       nlayer_ICB :: radial ID for ICB
!!       nlayer_CMB :: radial ID for CMB
!!       r_ICB :: ICB radius
!!       r_CMB :: CMB radius
!!@endverbatim
!
!
      module const_sph_initial_spectr
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_phys_address
!
      implicit none
!
      private :: find_local_sph_mode_address
      private :: set_initial_velocity
      private :: set_initial_temperature
      private :: set_initial_composition
      private :: set_initial_magne_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_spectrum
!
      use m_initial_field_control
      use m_t_int_parameter
      use m_t_step_parameter
      use m_field_data_IO
      use sph_mhd_rst_IO_control
      use set_sph_restart_IO
!
!
      i_step_MHD = i_step_init
      time   =     time_init
!
!  Set initial velocity if velocity is exist
      if(ipol%i_velo .gt. izero) call  set_initial_velocity
!
!  Set initial temperature if temperature is exist
      if(ipol%i_temp .gt. izero) call  set_initial_temperature
!
!  Set initial composition if composition is exist
      if(ipol%i_light .gt. izero) call set_initial_composition
!
!  Set initial magnetic field if magnetic field is exist
      if(ipol%i_magne .gt. izero) call set_initial_magne_sph
!
!  Set heat source if  heat source is exist
      if(ipol%i_heat_source .gt. izero) then
        call set_initial_heat_source_sph
      end if
!  Set light element source if light element is exist
      if(ipol%i_light_source .gt. izero) then
        call set_initial_light_source_sph
      end if
!
!  Copy initial field to restart IO data
      call set_sph_restart_num_to_IO
!
      call output_sph_restart_control
!
      end subroutine sph_initial_spectrum
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_velocity
!
      use m_sph_spectr_data
!
      integer ( kind = kint) :: inod, jj, k
      real (kind = kreal) :: rr
!
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_velo) = zero
        d_rj(inod,itor%i_velo) = zero
      end do
!$omp end parallel do
!
      jj = find_local_sph_mode_address(1, 0)
      if (jj .gt. 0) then
        do k = nlayer_ICB+1, nlayer_CMB
          rr = radius_1d_rj_r(k)
          inod = local_sph_data_address(k,jj)
          d_rj(inod,itor%i_velo) = half * rr*rr
        end do
      end if
!
      end subroutine set_initial_velocity
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_temperature
!
      use m_sph_spectr_data
!
      integer ( kind = kint) :: inod, k, jj
      real (kind = kreal) :: pi, rr, xr, shell
      real(kind = kreal), parameter :: A_temp = 0.1d0
!
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_temp) = zero
      end do
!$omp end parallel do
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
!   search address for (l = m = 0)
      jj = find_local_sph_mode_address(0, 0)
!
!   set reference temperature if (l = m = 0) mode is there
      if (jj .gt. 0) then
        do k = 1, nidx_rj(1)
          inod = local_sph_data_address(k,jj)
          d_rj(inod,ipol%i_temp) = (ar_1d_rj(k,1) * 20.d0/13.0d0        &
     &                              - 1.0d0 ) * 7.0d0 / 13.0d0
        end do
      end if
!
!
!    Find local addrtess for (l,m) = (4,4)
!      jj =  find_local_sph_mode_address(4, 4)
      jj =  find_local_sph_mode_address(5, 5)
!
!    If data for (l,m) = (4,4) is there, set initial temperature
      if (jj .gt. 0) then
!    Set initial field from ICB to CMB
        do k = nlayer_ICB, nlayer_CMB
!
!    Set radius data
          rr = radius_1d_rj_r(k)
!    Set 1d address to substitute at (Nr, j)
          inod = local_sph_data_address(k,jj)
!
!    set initial temperature
          xr = two * rr - one * (r_CMB+r_ICB) / shell
          d_rj(inod,ipol%i_temp) = (one-three*xr**2+three*xr**4-xr**6)  &
     &                            * A_temp * three / (sqrt(two*pi))
        end do
      end if
!
      end subroutine set_initial_temperature
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_composition
!
      use m_sph_spectr_data
!
      integer (kind = kint) :: inod, k, jj
      real (kind = kreal) :: pi, xr, shell
      real(kind = kreal), parameter :: A_light = 0.1d0
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
!$omp parallel do
      do inod = 1, nnod_rj
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
        do k = 1, nidx_rj(1)
          inod = local_sph_data_address(k,jj)
          d_rj(inod,ipol%i_light) = (ar_1d_rj(k,1) * 20.d0/13.0d0       &
     &                              - 1.0d0 ) * 7.0d0 / 13.0d0
        end do
      end if
!
!
!    Find local addrtess for (l,m) = (4,4)
      jj =  find_local_sph_mode_address(4, 4)
!
      if (jj .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          inod = local_sph_data_address(k,jj)
          xr = two * radius_1d_rj_r(k) - one * (r_CMB+r_ICB) / shell
          d_rj(inod,ipol%i_light) = (one-three*xr**2+three*xr**4-xr**6) &
     &                            * A_light * three / (sqrt(two*pi))
        end do
      end if
!
      end subroutine set_initial_composition
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_magne_sph
!
      use m_sph_spectr_data
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: is, it, k, js, jt, is_ICB, is_CMB
!
!
      pi = four * atan(one)
!
!$omp parallel do
      do is = 1, nnod_rj
        d_rj(is,ipol%i_magne) = zero
        d_rj(is,itor%i_magne) = zero
      end do
!$omp end parallel do
!
!
!    Find local addrtess for (l,m) = (1,0)
      js =  find_local_sph_mode_address(ione, izero)
!
      if (js .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          is = local_sph_data_address(k,js)
          rr = radius_1d_rj_r(k)
!   Substitute poloidal mangetic field
          d_rj(is,ipol%i_magne) =  (5.0d0/8.0d0) * (-3.0d0 * rr**3      &
     &                     + 4.0d0 * r_CMB * rr**2 - r_ICB**4 / rr)
        end do
!
!   Fill potential field if inner core exist
        is_ICB = local_sph_data_address(nlayer_ICB,js)
        do k = 1, nlayer_ICB-1
          is = local_sph_data_address(k,js)
          rr = radius_1d_rj_r(k) / r_ICB
!   Substitute poloidal mangetic field
          d_rj(is,ipol%i_magne) =  d_rj(is_ICB,ipol%i_magne)            &
     &                            * rr**(ione+1)
        end do
!
!   Fill potential field if external of the core exist
        is_CMB = local_sph_data_address(nlayer_CMB,js)
        do k = nlayer_CMB+1, nidx_rj(1)
          is = local_sph_data_address(k,js)
          rr = radius_1d_rj_r(k) / r_CMB
!   Substitute poloidal mangetic field
          d_rj(is,ipol%i_magne) =  d_rj(is_ICB,ipol%i_magne)            &
     &                            * rr**(-ione)
        end do
      end if
!
!
!    Find local addrtess for (l,m) = (2,0)
      jt =  find_local_sph_mode_address(itwo, izero)
!
      if (jt .gt. 0) then
        do k = 1, nlayer_CMB
          it = local_sph_data_address(k,jt)
          rr = radius_1d_rj_r(k)
!   Substitute totoidal mangetic field
          d_rj(it,itor%i_magne) = (10.0d0/3.0d0) * rr                   &
     &                           * sin(pi*(rr-r_ICB))
        end do
      end if
!
      end subroutine set_initial_magne_sph
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_heat_source_sph
!
      use m_sph_spectr_data
!
      real (kind = kreal) :: rr
      integer(kind = kint) :: ii, k, jj
!
!
!$omp parallel do
      do ii = 1, nnod_rj
        d_rj(ii,ipol%i_heat_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(izero, izero)
!
      if (jj .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          ii = local_sph_data_address(k,jj)
          rr = radius_1d_rj_r(k)
!   Substitute initial heat source
          d_rj(ii,ipol%i_heat_source) = two / rr
        end do
      end if
!
      end subroutine set_initial_heat_source_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_light_source_sph
!
      use m_sph_spectr_data
!
!      real (kind = kreal) :: rr
      integer(kind = kint) :: ii, k, jj
!
!
!$omp parallel do
      do ii = 1, nnod_rj
        d_rj(ii,ipol%i_light_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(izero, izero)
!
      if (jj .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          ii = local_sph_data_address(k,jj)
!          rr = radius_1d_rj_r(k)
!   Substitute initial heat source
          d_rj(ii,ipol%i_light_source) = one
        end do
      end if
!
      end subroutine set_initial_light_source_sph
!
!-----------------------------------------------------------------------
!
      end module const_sph_initial_spectr
