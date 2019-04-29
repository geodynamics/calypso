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
!!     &        (fst_file_IO, sph_MHD_bc, SPH_MHD, rst_step, sph_fst_IO)
!!        type(sph_grids), intent(in) :: sph
!!        type(field_IO_params), intent(in) :: fst_file_IO
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol, itor
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(IO_step_param), intent(inout) :: rst_step
!!        type(field_IO), intent(inout) :: sph_fst_IO
!!
!!       Sample program to generate initial field
!!       This program generates initial condition
!!        for dynamo benchmark case 1
!!
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
!!       sph_bc_*%kr_in :: radial ID for inner boundary of each evolution
!!       sph_bc_*%kr_out :: radial ID for outer boundary
!!       sph_bc_*%r_ICB(0) :: radius for inner boundary of each evolution
!!       sph_bc_*%r_CMB(0) :: radius for outer boundary
!!
!!
!!      subroutine adjust_by_CMB_temp(sph_bc_T, sph, ipol, rj_fld)
!!
!!      subroutine add_outer_core_heat_source                           &
!!     &         (sph_bc_T, sph, ipol, rj_fld)
!!       Set homogenious heat source at outer core
!!       by CMB and ICB heat flux
!!
!!      subroutine add_inner_core_heat_source                           &
!!     &         (sph_bc_T, sph, ipol, rj_fld)
!!       Set homogenious heat source and temperature at inner core
!!       by CMB heat flux
!!         f_CMB = (dT/dr)_CMB
!!         f_ICB = -(dT/dr)_ICB
!!               = - f_CMB * (r_CMB / r_ICB)**2
!!             Q = - 3.0 * f_ICB / r_ICB
!!          T(r) = T_ICB + 0.5 * (f_ICB / r_ICB) * (r_ICB**2 - r**2)
!!
!!      subroutine add_whole_core_heat_source                           &
!!     &         (sph_bc_T, sph, ipol, rj_fld)
!!       Set homogenious heat source for whole core
!!       and temperature at inner core by CMB heat flux
!!         f_CMB = (dT/dr)_CMB
!!             Q = - 3.0 * f_CMB / r_CMB
!!         f_ICB = -(dT/dr)_ICB
!!               = - f_CMB * (r_ICB / r_CMB)
!!          T(r) = T_ICB + 0.5 * (f_ICB / r_ICB) * (r_ICB**2 - r**2)
!!
!!
!!   Function to obtain local addresses from global mode information
!!     Informations are stored in structure 'sph'
!!
!!      j_lc = find_local_sph_mode_address(sph, l, m) ::
!!         Return local spherical harmonics mode address j_lc for Y(l,m)
!!         If requested mode does not exist in the process, 0 is set
!!
!!     inod = local_sph_data_address(sph, k, j_lc) :: 
!!         Return address of sphectrum data
!!     radius_1d_rj_r(sph, k) :: Radius at global grid address k
!!     nlayer_ICB(sph) :: radial ID for ICB
!!     nlayer_CMB(sph) :: radial ID for CMB
!!     r_ICB(sph) :: ICB radius in grid data
!!     r_CMB(sph) :: CMB radius in grid data
!!     inod_rj_center(sph) :: Local data ID for center data
!!         If spectrum data does not have center
!!              inod_rj_center(sph) = 0
!!     nidx_rj(sph,1) :: Number of radial grids
!!     nidx_rj(sph,2) :: Number of modes in each process
!!     nnod_rj(sph) :: Number of local data points
!!@endverbatim
!
!
      module const_sph_initial_spectr
!
      use m_precision
      use m_constants
!
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_file_IO_parameter
      use t_boundary_data_sph_MHD
      use t_field_data_IO
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
      private :: find_local_sph_mode_address
      private :: local_sph_data_address
      private :: radius_1d_rj_r
      private :: r_ICB, nlayer_ICB, nlayer_CMB
      private :: inod_rj_center, nidx_rj, nnod_rj
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_spectrum                                   &
     &        (fst_file_IO, sph_MHD_bc, SPH_MHD, rst_step, sph_fst_IO)
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
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(IO_step_param), intent(inout) :: rst_step
      type(field_IO), intent(inout) :: sph_fst_IO
!
      integer(kind = kint) :: i_step
!
!  Set initial velocity if velocity is exist
      call set_initial_velocity(sph_MHD_bc%sph_bc_U,                    &
     &    SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
!
!  Set initial temperature if temperature is exist
      call set_initial_temperature                                      &
     &   (sph_MHD_bc%sph_bc_T, SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!
!  Set initial composition if composition is exist
      call set_initial_composition                                      &
     &   (sph_MHD_bc%sph_bc_C, SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!
!  Set initial magnetic field if magnetic field is exist
      call set_initial_magne_sph(sph_MHD_bc%sph_bc_B,                   &
     &    SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
!
!  Set heat source if  heat source is exist
      call set_initial_heat_source_sph                                  &
     &   (sph_MHD_bc%sph_bc_T, SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!      call add_inner_core_heat_source                                  &
!     &   (sph_MHD_bc%sph_bc_T, SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!
!  Set light element source if light element is exist
      call set_initial_light_source_sph                                 &
     &   (sph_MHD_bc%sph_bc_C, SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!
!  Copy initial field to restart IO data
      call copy_time_step_data(MHD_step1%init_d, MHD_step1%time_d)
      call set_sph_restart_num_to_IO(SPH_MHD%fld, sph_fst_IO)
!
!
      if(MHD_step1%init_d%i_time_step .eq. -1) then
        i_step = MHD_step1%init_d%i_time_step
      else
        i_step = MHD_step1%time_d%i_time_step
      end if
      call output_sph_restart_control(i_step, fst_file_IO,              &
     &    MHD_step1%time_d, SPH_MHD%fld, rst_step, sph_fst_IO)
!
      end subroutine sph_initial_spectrum
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_velocity                                   &
     &         (sph_bc_U, sph, ipol, itor, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
      integer :: jj, k
      integer ( kind = kint) :: inod
!      real (kind = kreal) :: rr
      real (kind = kreal) :: pi, xr, shell
!      real(kind = kreal), parameter :: A_light = 0.1d0
!
!
      if(ipol%i_velo .eq. izero) return
!
      pi = four * atan(one)
      shell = sph_bc_U%r_CMB(0) - sph_bc_U%r_ICB(0)
!
!$omp parallel do
      do inod = 1, nnod_rj(sph)
        rj_fld%d_fld(inod,ipol%i_velo) = zero
        rj_fld%d_fld(inod,itor%i_velo) = zero
      end do
!$omp end parallel do
!
!      jj = find_local_sph_mode_address(sph, 1, 0)
!      if (jj .gt. 0) then
!        do k = sph_bc_U%kr_in+1, sph_bc_U%kr_out
!          rr = radius_1d_rj_r(sph, k)
!          inod = local_sph_data_address(sph, k, jj)
!          rj_fld%d_fld(inod,itor%i_velo) = half * rr*rr
!        end do
!      end if
!
!      jj =  find_local_sph_mode_address(sph, 2, 1)
!
!      if (jj .gt. 0) then
!        do k = sph_bc_U%kr_in, sph_bc_U%kr_out
!          inod = local_sph_data_address(sph, k, jj)
!          xr = two * radius_1d_rj_r(sph, k)                            &
!    &         - one * (sph_bc_U%r_CMB(0) + sph_bc_U%r_ICB(0)) / shell
!          rj_fld%d_fld(inod,itor%i_velo)                               &
!    &        = (one-three*xr**2+three*xr**4-xr**6)                     &
!    &         * A_light * three / (sqrt(two*pi))
!        end do
!      end if
!
      end subroutine set_initial_velocity
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_temperature(sph_bc_T, sph, ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      integer :: jj, k
      integer ( kind = kint) :: inod, i_center
      real (kind = kreal) :: pi, rr, xr, shell
      real(kind = kreal), parameter :: A_temp = 0.1d0
!
!
      if(ipol%i_temp .eq. izero) return
!
!$omp parallel do
      do inod = 1, nnod_rj(sph)
        rj_fld%d_fld(inod,ipol%i_temp) = zero
      end do
!$omp end parallel do
!
      pi = four * atan(one)
      shell = sph_bc_T%r_CMB(0) - sph_bc_T%r_ICB(0)
!
!   search address for (l = m = 0)
      jj = find_local_sph_mode_address(sph, 0, 0)
!
!   set reference temperature if (l = m = 0) mode is there
      if (jj .gt. 0) then
        do k = 1, sph_bc_T%kr_in-1
          inod = local_sph_data_address(sph, k, jj)
          rj_fld%d_fld(inod,ipol%i_temp) = 1.0d0
        end do
        do k = sph_bc_T%kr_in, sph_bc_T%kr_out
          inod = local_sph_data_address(sph, k, jj)
          rr = radius_1d_rj_r(sph, k)
          rj_fld%d_fld(inod,ipol%i_temp)                                &
     &         = ((20.d0/13.0d0) / rr  - 1.0d0 ) * 7.0d0 / 13.0d0
        end do
      end if
!
!
!    Find local addrtess for (l,m) = (4,4)
      jj =  find_local_sph_mode_address(sph, 4, 4)
!      jj =  find_local_sph_mode_address(sph, 5, 5)
!
!    If data for (l,m) = (4,4) is there, set initial temperature
      if (jj .gt. 0) then
!    Set initial field from ICB to CMB
        do k = sph_bc_T%kr_in, sph_bc_T%kr_out
!
!    Set radius data
          rr = radius_1d_rj_r(sph, k)
!    Set 1d address to substitute at (Nr, j)
          inod = local_sph_data_address(sph, k, jj)
!
!    set initial temperature
          xr = two * rr                                                 &
     &        - one * (sph_bc_T%r_CMB(0) + sph_bc_T%r_ICB(0)) / shell
          rj_fld%d_fld(inod,ipol%i_temp)                                &
     &       = (one-three*xr**2+three*xr**4-xr**6)                      &
     &        * A_temp * three / (sqrt(two*pi))
        end do
      end if
!
!    Center
      i_center = inod_rj_center(sph)
      if(i_center .gt. 0) then
        jj = find_local_sph_mode_address(sph, 0, 0)
        inod = local_sph_data_address(sph, 1, jj)
        rj_fld%d_fld(i_center,ipol%i_temp)                              &
     &                 = rj_fld%d_fld(inod,ipol%i_temp)
      end if
!
      end subroutine set_initial_temperature
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_composition(sph_bc_C, sph, ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_C
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      integer :: jj, k
      integer (kind = kint) :: inod, i_center
      real (kind = kreal) :: pi, rr, xr, shell
      real(kind = kreal), parameter :: A_light = 0.1d0
!
!
      if(ipol%i_light .eq. izero) return
!
      pi = four * atan(one)
      shell = sph_bc_C%r_CMB(0) - sph_bc_C%r_ICB(0)
!
!$omp parallel do
      do inod = 1, nnod_rj(sph)
        rj_fld%d_fld(inod,ipol%i_light) = zero
      end do
!$omp end parallel do
!
!   search address for (l = m = 0)
      jj = find_local_sph_mode_address(sph, 0, 0)
!
!   set reference temperature if (l = m = 0) mode is there
!
      if (jj .gt. 0) then
        do k = 1, sph_bc_C%kr_in-1
          inod = local_sph_data_address(sph, k, jj)
          rj_fld%d_fld(inod,ipol%i_light) = 1.0d0
        end do
        do k = sph_bc_C%kr_in, sph_bc_C%kr_out
          inod = local_sph_data_address(sph, k, jj)
          rr = radius_1d_rj_r(sph, k)
          rj_fld%d_fld(inod,ipol%i_light)                               &
     &         = ((20.d0/13.0d0) / rr  - 1.0d0 ) * 7.0d0 / 13.0d0
        end do
      end if
!
!
!    Find local addrtess for (l,m) = (4,4)
      jj =  find_local_sph_mode_address(sph, 4, 4)
!
      if (jj .gt. 0) then
        do k = sph_bc_C%kr_in, sph_bc_C%kr_out
          inod = local_sph_data_address(sph, k, jj)
          xr = two * radius_1d_rj_r(sph, k)                             &
     &        - one * (sph_bc_C%r_CMB(0) + sph_bc_C%r_ICB(0)) / shell
          rj_fld%d_fld(inod,ipol%i_light)                               &
     &       = (one-three*xr**2+three*xr**4-xr**6)                      &
     &        * A_light * three / (sqrt(two*pi))
        end do
      end if
!
!    Center
      i_center = inod_rj_center(sph)
      if(i_center .gt. 0) then
        jj = find_local_sph_mode_address(sph, 0, 0)
        inod = local_sph_data_address(sph, 1, jj)
        rj_fld%d_fld(i_center,ipol%i_light)                             &
     &              = rj_fld%d_fld(inod,ipol%i_light)
      end if
!
      end subroutine set_initial_composition
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_magne_sph                                  &
     &         (sph_bc_B, sph, ipol, itor, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: is, it, is_ICB, is_CMB
      integer :: js, jt, k
!
!
      if(ipol%i_magne .eq. izero) return
      pi = four * atan(one)
!
!$omp parallel do
      do is = 1, nnod_rj(sph)
        rj_fld%d_fld(is,ipol%i_magne) = zero
        rj_fld%d_fld(is,itor%i_magne) = zero
      end do
!$omp end parallel do
!
!
!    Find local addrtess for (l,m) = (1,0)
      js =  find_local_sph_mode_address(sph, 1, 0)
!
      if (js .gt. 0) then
        do k = sph_bc_B%kr_in, sph_bc_B%kr_out
          is = local_sph_data_address(sph, k, js)
          rr = radius_1d_rj_r(sph, k)
!   Substitute poloidal mangetic field
          rj_fld%d_fld(is,ipol%i_magne)                                 &
     &                           =  (5.0d0/8.0d0) * (-3.0d0 * rr**3     &
     &                             + 4.0d0 * sph_bc_B%r_CMB(0) * rr**2  &
     &                             - sph_bc_B%r_ICB(0)**4 / rr)
        end do
!
!   Fill potential field if inner core exist
        is_ICB = local_sph_data_address(sph, int(sph_bc_B%kr_in), js)
        do k = 1, sph_bc_B%kr_in-1
          is = local_sph_data_address(sph, k, js)
          rr = radius_1d_rj_r(sph, k) / sph_bc_B%r_ICB(0)
!   Substitute poloidal mangetic field
          rj_fld%d_fld(is,ipol%i_magne)                                 &
     &       =  rj_fld%d_fld(is_ICB,ipol%i_magne) * rr**(ione+1)
        end do
!
!   Fill potential field if external of the core exist
        is_CMB = local_sph_data_address(sph, int(sph_bc_B%kr_out), js)
        do k = sph_bc_B%kr_out+1, nidx_rj(sph,1)
          is = local_sph_data_address(sph, k, js)
          rr = radius_1d_rj_r(sph, k) / sph_bc_B%r_CMB(0)
!   Substitute poloidal mangetic field
          rj_fld%d_fld(is,ipol%i_magne)                                 &
     &       =  rj_fld%d_fld(is_ICB,ipol%i_magne) * rr**(-ione)
        end do
      end if
!
!
!    Find local addrtess for (l,m) = (2,0)
      jt =  find_local_sph_mode_address(sph, 1, -1)
!
      if (jt .gt. 0) then
        do k = 1, sph_bc_B%kr_out
          it = local_sph_data_address(sph, k, jt)
          rr = radius_1d_rj_r(sph, k)
!   Substitute totoidal mangetic field
          rj_fld%d_fld(it,itor%i_magne)                                 &
     &       = (10.0d0/3.0d0) * rr * sin(pi*(rr - sph_bc_B%r_ICB(0)))
        end do
      end if
!
      end subroutine set_initial_magne_sph
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_heat_source_sph                            &
     &         (sph_bc_T, sph, ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      real (kind = kreal) :: rr, q
      integer(kind = kint) :: inod, i_center
      integer :: jj, k
!
!
      if(ipol%i_heat_source .eq. izero) return
!
!$omp parallel do
      do inod = 1, nnod_rj(sph)
        rj_fld%d_fld(inod,ipol%i_heat_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(sph, 0, 0)
!
      if (jj .gt. 0) then
        q = (three / (sph_bc_T%r_CMB(0)**3 - sph_bc_T%r_ICB(0)**3))     &
     &     * (-sph_bc_T%CMB_flux(jj) * sph_bc_T%r_CMB(0)**2             &
     &        - sph_bc_T%ICB_flux(jj) * sph_bc_T%r_ICB(0)**2)
!
        do k = sph_bc_T%kr_in, sph_bc_T%kr_out
          inod = local_sph_data_address(sph, k, jj)
          rr = radius_1d_rj_r(sph, k)
!   Substitute initial heat source
          rj_fld%d_fld(inod,ipol%i_heat_source)  = q
        end do
      end if
!    Center
      i_center = inod_rj_center(sph)
      if(i_center .gt. 0) then
        rj_fld%d_fld(i_center,ipol%i_heat_source) = q
      end if
!
      end subroutine set_initial_heat_source_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_light_source_sph                           &
     &         (sph_bc_C, sph, ipol, rj_fld)
!
      use calypso_mpi
!
      type(sph_boundary_type), intent(in) :: sph_bc_C
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: inod, i_center
      integer :: jj, k
!
!
      if(ipol%i_light_source .eq. izero) return
!
!$omp parallel do
      do inod = 1, nnod_rj(sph)
        rj_fld%d_fld(inod,ipol%i_light_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(sph, 0, 0)
!
      if (jj .gt. 0) then
        do k = 1, sph_bc_C%kr_in-1
          inod = local_sph_data_address(sph, k, jj)
          rj_fld%d_fld(inod,ipol%i_light) = 1.0d0
        end do
        do k = sph_bc_C%kr_in, sph_bc_C%kr_out
          inod = local_sph_data_address(sph, k, jj)
!          rr = radius_1d_rj_r(sph, k)
!    Substitute initial heat source
          rj_fld%d_fld(inod,ipol%i_light_source) = 1.0d0
        end do
      end if
!
!    Center
      i_center = inod_rj_center(sph)
      if(i_center .gt. 0) then
        rj_fld%d_fld(i_center,ipol%i_light_source) = 1.0d0
      end if
!
      end subroutine set_initial_light_source_sph
!
!-----------------------------------------------------------------------
!
      subroutine adjust_by_CMB_temp(sph_bc_T, sph, ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      integer ( kind = kint) :: inod, i_center
      integer :: jj, k
      real (kind = kreal) :: temp_CMB = 0.0d0
!
!
      if(ipol%i_temp .eq. izero) return
!
!   search address for (l = m = 0)
      jj = find_local_sph_mode_address(sph, 0, 0)
!
!   set reference temperature if (l = m = 0) mode is there
      if (jj .gt. 0) then
        inod = local_sph_data_address(sph, int(sph_bc_T%kr_out), jj)
        temp_CMB = rj_fld%d_fld(inod,ipol%i_temp)
!
        do k = 1, nidx_rj(sph,1)
          inod = local_sph_data_address(sph, k, jj)
          rj_fld%d_fld(inod,ipol%i_temp)                                &
     &            = rj_fld%d_fld(inod,ipol%i_temp) - temp_CMB
        end do
      end if
!
!    Center
      i_center = inod_rj_center(sph)
      if(i_center .gt. 0) then
        jj = find_local_sph_mode_address(sph, 0, 0)
        inod = local_sph_data_address(sph, 1, jj)
        rj_fld%d_fld(i_center,ipol%i_temp)                             &
     &              = rj_fld%d_fld(inod,ipol%i_temp) - temp_CMB
      end if
!
      end subroutine adjust_by_CMB_temp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_outer_core_heat_source                             &
     &         (sph_bc_T, sph, ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      real (kind = kreal) :: rr, q
      integer(kind = kint) :: inod
      integer :: jj, k
!
!
      if(ipol%i_heat_source .eq. izero) return
!
!$omp parallel do
      do inod = 1, nnod_rj(sph)
        rj_fld%d_fld(inod,ipol%i_heat_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(sph, 0, 0)
!
      if (jj .gt. 0) then
        q = (three / (sph_bc_T%r_CMB(0)**3 - sph_bc_T%r_ICB(0)**3))     &
     &     * (-sph_bc_T%CMB_flux(jj) * sph_bc_T%r_CMB(0)**2             &
     &       - sph_bc_T%ICB_flux(jj) * sph_bc_T%r_ICB(0)**2)
!
!
        do k = sph_bc_T%kr_in, sph_bc_T%kr_out
          inod = local_sph_data_address(sph, k, jj)
          rr = radius_1d_rj_r(sph, k)
!   Substitute initial heat source
          rj_fld%d_fld(inod,ipol%i_heat_source)  = q
        end do
      end if
!
      end subroutine add_outer_core_heat_source
!
!-----------------------------------------------------------------------
!
      subroutine add_inner_core_heat_source                             &
     &         (sph_bc_T, sph, ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      real (kind = kreal) :: rr, q, T_ICB, f_ICB
      integer(kind = kint) :: inod, i_center
      integer :: jj, k
!
!
      if(ipol%i_heat_source*ipol%i_temp .eq. izero) return
!
!$omp parallel do
      do inod = 1, nnod_rj(sph)
        rj_fld%d_fld(inod,ipol%i_heat_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(sph, 0, 0)
!
      if (jj .gt. 0) then
        f_ICB = -sph_bc_T%CMB_flux(jj)                                  &
     &         * (sph_bc_T%r_CMB(0) / r_ICB(sph))**2
        q = three * f_ICB / r_ICB(sph)
!
        inod = local_sph_data_address(sph, nlayer_ICB(sph), jj)
        T_ICB = rj_fld%d_fld(inod,ipol%i_temp)
!
        do k = 1, nlayer_ICB(sph)
          inod = local_sph_data_address(sph, k, jj)
          rr = radius_1d_rj_r(sph, k)
!   Substitute initial heat source
          rj_fld%d_fld(inod,ipol%i_heat_source) = q
!   Fill inner core temperature
          rj_fld%d_fld(inod,ipol%i_temp) = T_ICB                        &
     &       + half * f_ICB * (r_ICB(sph)**2 - rr**2) / r_ICB(sph)
        end do
      end if
!
!    Center
      i_center = inod_rj_center(sph)
      if(i_center .gt. 0) then
        rj_fld%d_fld(i_center,ipol%i_heat_source) = q
        rj_fld%d_fld(i_center,ipol%i_temp)                              &
     &         = T_ICB + half * f_ICB * r_ICB(sph)
      end if
!
      end subroutine add_inner_core_heat_source
!
!-----------------------------------------------------------------------
!
      subroutine add_whole_core_heat_source                             &
     &         (sph_bc_T, sph, ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      real (kind = kreal) :: rr, q, T_ICB, f_ICB
      integer(kind = kint) :: inod, i_center
      integer :: jj, k
!
!
      if(ipol%i_heat_source .eq. izero) return
!
!$omp parallel do
      do inod = 1, nnod_rj(sph)
        rj_fld%d_fld(inod,ipol%i_heat_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(sph, 0, 0)
!
      if (jj .gt. 0) then
        q = - three * sph_bc_T%CMB_flux(jj) / sph_bc_T%r_CMB(0)
        f_ICB = -sph_bc_T%CMB_flux(jj)                                  &
     &         * (r_ICB(sph) / sph_bc_T%r_CMB(0))
        write(*,*) 'q', q
        write(*,*) 'flux_ICB', f_ICB
!
        inod = local_sph_data_address(sph, nlayer_ICB(sph), jj)
        T_ICB = rj_fld%d_fld(inod,ipol%i_temp)
!
        do k = 1, sph_bc_T%kr_out
          inod = local_sph_data_address(sph, k, jj)
          rr = radius_1d_rj_r(sph, k)
!   Substitute initial heat source
          rj_fld%d_fld(inod,ipol%i_heat_source) = q
        end do
!
        do k = 1, nlayer_ICB(sph)
          inod = local_sph_data_address(sph, k, jj)
          rr = radius_1d_rj_r(sph, k)
!   Fill inner core temperature
          rj_fld%d_fld(inod,ipol%i_temp) = T_ICB                        &
     &       + half * f_ICB * (r_ICB(sph)**2 - rr**2) / r_ICB(sph)
        end do
      end if
!
!    Center
      i_center = inod_rj_center(sph)
      if(i_center .gt. 0) then
        rj_fld%d_fld(i_center,ipol%i_heat_source) = q
        rj_fld%d_fld(i_center,ipol%i_temp)                              &
     &         = T_ICB + half * f_ICB * r_ICB(sph)
      end if
!
      end subroutine add_whole_core_heat_source
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!!   Wrapper routines from mesh strucutres. Do not edit.
!-----------------------------------------------------------------------
!
      integer function find_local_sph_mode_address(sph, l, m)
!
      type(sph_grids), intent(in) :: sph
      integer, intent(in) :: l, m
!
!
      find_local_sph_mode_address                                       &
     &      = find_local_sph_address(sph%sph_rj, l, m)
!
      end function find_local_sph_mode_address
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function local_sph_data_address              &
     &                            (sph, kr, j_lc)
!
      type(sph_grids), intent(in) :: sph
      integer :: kr, j_lc
!
!
      local_sph_data_address                                            &
     &      = local_sph_node_address(sph%sph_rj, kr, j_lc)
!
      end function local_sph_data_address
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function radius_1d_rj_r(sph, kr)
!
      type(sph_grids), intent(in) :: sph
      integer, intent(in) :: kr
!
      radius_1d_rj_r = sph%sph_rj%radius_1d_rj_r(kr)
!
      end function radius_1d_rj_r
!
!-----------------------------------------------------------------------
!
      real function r_CMB(sph)
!
      type(sph_grids), intent(in) :: sph
!
      r_CMB = int(sph%sph_params%radius_CMB)
!
      end function r_CMB
!
!-----------------------------------------------------------------------
!
      real function r_ICB(sph)
!
      type(sph_grids), intent(in) :: sph
!
      r_ICB = int(sph%sph_params%radius_ICB)
!
      end function r_ICB
!
!-----------------------------------------------------------------------
!
      integer function nlayer_CMB(sph)
!
      type(sph_grids), intent(in) :: sph
!
      nlayer_CMB = int(sph%sph_params%nlayer_CMB)
!
      end function nlayer_CMB
!
!-----------------------------------------------------------------------
!
      integer function nlayer_ICB(sph)
!
      type(sph_grids), intent(in) :: sph
!
      nlayer_ICB = int(sph%sph_params%nlayer_ICB)
!
      end function nlayer_ICB
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function inod_rj_center(sph)
!
      type(sph_grids), intent(in) :: sph
!
      inod_rj_center = sph%sph_rj%inod_rj_center
!
      end function inod_rj_center
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function nidx_rj(sph, nd)
!
      type(sph_grids), intent(in) :: sph
      integer, intent(in) :: nd
!
      nidx_rj = sph%sph_rj%nidx_rj(nd)
!
      end function nidx_rj
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function nnod_rj(sph)
!
      type(sph_grids), intent(in) :: sph
!
      nnod_rj = sph%sph_rj%nnod_rj
!
      end function nnod_rj
!
!-----------------------------------------------------------------------
!
!
      end module const_sph_initial_spectr
