!>@file   initial_magne_sph_dynamo.f90
!!@brief  module initial_magne_sph_dynamo
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine sph_initial_magne_benchmarks                         &
!!     &         (iflag_restart_mode, sph_params, sph_rj, ipol, rj_fld)
!!      subroutine initial_sph_seed_magne(sph_params, sph_rj,           &
!!     &                                  sph_bc_B, ipol, rj_fld)
!!        integer(kind = kint), intent(in) :: iflag_restart_mode
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module initial_magne_sph_dynamo
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_spheric_parameter
      use t_phys_address
      use t_phys_data
      use t_boundary_params_sph_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_magne_benchmarks                           &
     &         (iflag_restart_mode, sph_params, sph_rj, ipol, rj_fld)
!
      use m_machine_parameter
      use m_initial_field_control
!
      use t_reference_scalar_param
      use t_spheric_parameter
      use t_phys_data
!
      use initial_magne_dynamobench
      use copy_nodal_fields
!
      integer(kind = kint), intent(in) :: iflag_restart_mode
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call calypso_mpi_barrier
      if(ipol%base%i_magne .le. 0) return
!
      call clear_field_data(rj_fld, n_vector, ipol%base%i_magne)
      call clear_field_data(rj_fld, n_vector, ipol%base%i_current)
!
      if(iflag_restart_mode .eq. i_rst_dbench1) then
        call initial_magne_sph_dbench_case1(sph_rj,                     &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      sph_params%radius_ICB, sph_params%radius_CMB,               &
     &      ipol%base%i_magne, ipol%base%i_current,                     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(iflag_restart_mode .eq. i_rst_dbench2) then
        call initial_magne_sph_dbench_case2(sph_rj,                     &
     &      sph_params%nlayer_CMB, sph_params%radius_CMB,               &
     &      ipol%base%i_magne, ipol%base%i_current,                     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(iflag_restart_mode .eq. i_rst_dbench_qcv) then
        call initial_magne_sph_dbench_qcv(sph_rj,                       &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      sph_params%radius_ICB, sph_params%radius_CMB,               &
     &      ipol%base%i_magne, ipol%base%i_current,                     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sph_initial_magne_benchmarks
!
!-----------------------------------------------------------------------
!
      subroutine initial_sph_seed_magne(sph_params, sph_rj,             &
     &                                  sph_bc_B, ipol, rj_fld)
!
      use initial_magne_dynamobench
      use copy_nodal_fields
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      real(kind = kreal), parameter :: reduce_ratio = 1.0d-3
!
!
      call calypso_mpi_barrier
      if(ipol%base%i_magne .le. 0) return
!
      call clear_field_data(rj_fld, n_vector, ipol%base%i_magne)
      call clear_field_data(rj_fld, n_vector, ipol%base%i_current)
!
      if (sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        call initial_magne_sph_dbench_case2(sph_rj,                     &
     &      sph_params%nlayer_CMB, sph_params%radius_CMB,               &
     &      ipol%base%i_magne, ipol%base%i_current,                     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call initial_magne_sph_dbench_case1(sph_rj,                     &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      sph_params%radius_ICB, sph_params%radius_CMB,               &
     &      ipol%base%i_magne, ipol%base%i_current,                     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      call reduce_initial_magne_sph(reduce_ratio,                       &
     &    ipol%base%i_magne, ipol%base%i_current,                       &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine initial_sph_seed_magne
!
!-----------------------------------------------------------------------
!
      end module initial_magne_sph_dynamo
