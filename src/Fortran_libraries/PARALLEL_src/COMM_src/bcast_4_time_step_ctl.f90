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
      call bcast_ctl_type_i1(tctl%i_step_psf_ctl)
      call bcast_ctl_type_i1(tctl%i_step_iso_ctl)
      call bcast_ctl_type_i1(tctl%i_step_psf_ctl)
      call bcast_ctl_type_i1(tctl%i_step_iso_ctl)
      call bcast_ctl_type_i1(tctl%i_step_pvr_ctl)
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
      end subroutine bcast_ctl_data_4_time_step
!
!   --------------------------------------------------------------------
!
      end module bcast_4_time_step_ctl
