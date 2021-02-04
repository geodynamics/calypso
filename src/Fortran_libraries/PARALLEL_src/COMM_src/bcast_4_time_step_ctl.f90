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
!!      subroutine reset_ctl_data_4_time_step(tctl)
!!        type(time_data_control), intent(inout) :: tctl
!!@endverbatim
!
      module bcast_4_time_step_ctl
!
      use m_precision
      use t_ctl_data_4_time_steps
!
      implicit  none
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_4_time_step(tctl)
!
      use calypso_mpi_int
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
      call bcast_ctl_type_r1(tctl%delta_t_pvr_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_fline_ctl)
      call bcast_ctl_type_r1(tctl%delta_t_lic_ctl)
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
      call bcast_ctl_type_i1(tctl%i_step_pvr_ctl)
      call bcast_ctl_type_i1(tctl%i_step_lic_ctl)
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
      call calypso_mpi_bcast_one_int(tctl%i_tstep, 0)
!
      end subroutine bcast_ctl_data_4_time_step
!
!   --------------------------------------------------------------------
!
      subroutine reset_ctl_data_4_time_step(tctl)
!
      type(time_data_control), intent(inout) :: tctl
!
!
      tctl%elapsed_time_ctl%iflag = 0
!
      tctl%dt_ctl%iflag = 0
      tctl%time_init_ctl%iflag = 0
!
      tctl%min_delta_t_ctl%iflag = 0
      tctl%max_delta_t_ctl%iflag = 0
      tctl%max_eps_to_shrink_ctl%iflag = 0
      tctl%min_eps_to_expand_ctl%iflag = 0
!
      tctl%delta_t_check_ctl%iflag = 0
      tctl%delta_t_rst_ctl%iflag = 0
      tctl%delta_t_psf_ctl%iflag = 0
      tctl%delta_t_iso_ctl%iflag = 0
      tctl%delta_t_pvr_ctl%iflag = 0
      tctl%delta_t_fline_ctl%iflag = 0
      tctl%delta_t_lic_ctl%iflag = 0
      tctl%delta_t_field_ctl%iflag = 0
      tctl%delta_t_monitor_ctl%iflag = 0
      tctl%delta_t_sgs_coefs_ctl%iflag = 0
      tctl%delta_t_boundary_ctl%iflag = 0
!
!
      tctl%i_step_init_ctl%iflag = 0
      tctl%i_step_number_ctl%iflag = 0
      tctl%i_step_number_ctl%iflag = 0
!
      tctl%i_step_check_ctl%iflag = 0
      tctl%i_step_rst_ctl%iflag = 0
!
      tctl%i_step_psf_ctl%iflag = 0
      tctl%i_step_iso_ctl%iflag = 0
      tctl%i_step_pvr_ctl%iflag = 0
      tctl%i_step_lic_ctl%iflag = 0
      tctl%i_step_fline_ctl%iflag = 0
!
      tctl%i_step_ucd_ctl%iflag = 0
      tctl%i_step_monitor_ctl%iflag = 0
!
      tctl%i_step_sgs_coefs_ctl%iflag = 0
      tctl%i_step_boundary_ctl%iflag = 0
!
      tctl%i_diff_steps_ctl%iflag = 0
!
      tctl%start_rst_step_ctl%iflag = 0
      tctl%end_rst_step_ctl%iflag = 0
!
!
      tctl%flexible_step_ctl%iflag = 0
!
      tctl%i_tstep = 0
!
      end subroutine reset_ctl_data_4_time_step
!
!   --------------------------------------------------------------------
!
      end module bcast_4_time_step_ctl
