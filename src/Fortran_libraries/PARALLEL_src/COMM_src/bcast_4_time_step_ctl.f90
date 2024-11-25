!>@file   bcast_4_time_step_ctl.f90
!!@brief  module bcast_4_time_step_ctl
!!
!!@author H. Matsui
!!@date Programmed in June, 2016
!
!> @brief Bloardcast for time step parameters
!!
!!@verbatim
!!      subroutine bcast_ctl_data_4_time_step(tctl)
!!        type(time_data_control), intent(inout) :: tctl
!! ------------------------------------------------------------------
!!      Example of control parameters for flexible time step
!!
!!    begin time_step_ctl
!!      elapsed_time_ctl      42500.
!!
!!      flexible_step_ctl        ON
!!      dt_ctl                   5.0e-5
!!      min_delta_t_ctl          1.0e-6
!!      max_delta_t_ctl          1.0e-5
!!      max_eps_to_shrink_ctl    1.0e-1
!!      min_eps_to_expand_ctl    1.0e-1
!!
!!      ratio_to_CFL_ctl    0.3
!!
!!      start_rst_step_ctl    10
!!      end_rst_step_ctl      20
!!
!!      delta_t_check_ctl            2.0e-5
!!      delta_t_rst_ctl              1.0e-2
!!      delta_t_sectioning_ctl       1.0e-3
!!      delta_t_isosurface_ctl       1.0e-3
!!      delta_t_map_projection_ctl   1.0e-3
!!      delta_t_pvr_ctl              1.0e-2
!!      delta_t_fline_ctl            1.0e-1    
!!      delta_t_LIC_ctl              1.0e-1
!!      delta_t_field_ctl            1.0e-3
!!      delta_t_monitor_ctl          1.0e-4
!!      delta_t_sgs_coefs_ctl        2.0e-5
!!      delta_t_boundary_ctl         1.0e-4
!!    end time_step_ctl
!!
!! ------------------------------------------------------------------
!!
!!      Example of control parameters for fixed time step
!!
!!    begin time_step_ctl
!!      elapsed_time_ctl      42500.
!!
!!      flexible_step_ctl     OFF
!!
!!      i_step_init_ctl       0
!!      i_step_finish_ctl     2000
!!      i_step_number_ctl     2000
!!
!      i_step_check_ctl             40
!!      i_step_rst_ctl              800
!!      i_step_sectioning_ctl       400
!!      i_step_isosurface_ctl       400
!!      i_step_map_projection_ctl   400
!!      i_step_pvr_ctl              400
!!      i_step_fline_ctl            400
!!      i_step_LIC_ctl              400
!!      i_step_snapshot_ctl         800
!!      i_step_field_ctl            800
!!      i_step_monitor_ctl           40
!!      i_step_sgs_coefs_ctl       2000
!!      i_step_boundary_ctl          40
!!
!!      dt_ctl              5.0e-5
!!      time_init_ctl       0.0e-8
!!    end time_step_ctl
!!
!! ------------------------------------------------------------------
!!@endverbatim
!
      module bcast_4_time_step_ctl
!
      use m_precision
      use calypso_mpi
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_4_time_step(tctl)
!
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use t_ctl_data_4_time_steps
      use bcast_control_arrays
!
      type(time_data_control), intent(inout) :: tctl
!
!
      call bcast_ctl_type_r1(tctl%elapsed_time_ctl)
!
      call bcast_ctl_type_r1(tctl%dt_ctl)
      call bcast_ctl_type_r1(tctl%time_init_ctl)
!
      call bcast_ctl_type_r1(tctl%min_delta_t_ctl)
      call bcast_ctl_type_r1(tctl%max_delta_t_ctl)
      call bcast_ctl_type_r1(tctl%max_eps_to_shrink_ctl)
      call bcast_ctl_type_r1(tctl%min_eps_to_expand_ctl)
!
      call bcast_ctl_type_r1(tctl%delta_t_check_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_rst_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_psf_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_iso_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_map_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_pvr_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_fline_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_lic_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_tracer_output_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_field_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_monitor_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_sgs_coefs_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_boundary_ctl)
!
!
      call bcast_ctl_type_i1(tctl%i_step_init_ctl)
      call bcast_ctl_type_i1(tctl%i_step_number_ctl)
      call bcast_ctl_type_i1(tctl%i_step_number_ctl)
!
      call bcast_ctl_type_i1(tctl%i_step_check_ctl)
      call bcast_ctl_type_i1(tctl%i_step_rst_ctl)
!
      call bcast_ctl_type_i1(tctl%i_step_psf_ctl)
      call bcast_ctl_type_i1(tctl%i_step_iso_ctl)
      call bcast_ctl_type_i1(tctl%i_step_map_ctl)
      call bcast_ctl_type_i1(tctl%i_step_pvr_ctl)
      call bcast_ctl_type_i1(tctl%i_step_lic_ctl)
      call bcast_ctl_type_i1(tctl%i_step_tracer_output_ctl)
      call bcast_ctl_type_i1(tctl%i_step_fline_ctl)
!
      call bcast_ctl_type_i1(tctl%i_step_ucd_ctl)
      call bcast_ctl_type_i1(tctl%i_step_monitor_ctl)
!
      call bcast_ctl_type_i1(tctl%i_step_sgs_coefs_ctl)
      call bcast_ctl_type_i1(tctl%i_step_boundary_ctl)
!
      call bcast_ctl_type_i1(tctl%i_diff_steps_ctl)
!
      call bcast_ctl_type_i1(tctl%start_rst_step_ctl)
      call bcast_ctl_type_i1(tctl%end_rst_step_ctl)
!
!
      call bcast_ctl_type_c1(tctl%flexible_step_ctl)
!
      call calypso_mpi_bcast_character(tctl%block_name,                 &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(tctl%i_tstep, 0)
!
      end subroutine bcast_ctl_data_4_time_step
!
!   --------------------------------------------------------------------
!
      end module bcast_4_time_step_ctl
