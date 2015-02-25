!>@file   m_ctl_data_4_time_steps.f90
!!@brief  module m_ctl_data_4_time_steps
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!!@n    Modified in Nov., 2006
!
!> @brief Control input routine for time step parameters
!!
!!@verbatim
!!
!!      subroutine read_time_step_ctl
!!
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
!!      start_rst_step_ctl    10
!!      end_rst_step_ctl      20
!!
!!      delta_t_check_ctl        2.0e-5
!!      delta_t_rst_ctl          1.0e-2
!!      delta_t_psf_ctl          1.0e-3
!!      delta_t_pvr_ctl          1.0e-2
!!      delta_t_fline_ctl        1.0e-1
!!      delta_t_field_ctl        1.0e-3
!!      delta_t_monitor_ctl      1.0e-4
!!      delta_t_sgs_coefs_ctl    2.0e-5
!!      delta_t_boundary_ctl     1.0e-4
!!    end
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
!!      i_step_check_ctl         40
!!      i_step_rst_ctl          800
!!      i_step_psf_ctl          400
!!      i_step_pvr_ctl          400
!!      i_step_fline_ctl        400
!!      i_step_snapshot_ctl     800
!!      i_step_field_ctl        800
!!      i_step_monitor_ctl       40
!!      i_step_sgs_coefs_ctl   2000
!!      i_step_boundary_ctl      40
!!
!!      dt_ctl              5.0e-5
!!      time_init_ctl       0.0e-8
!!    end
!!
!! ------------------------------------------------------------------
!!@endverbatim
!>@n
!>@n@param      elapsed_time_ctl
!>                Simulation time on wall clock (sec.)
!
!>@n@param      flexible_step_ctl
!>                Flexible time step switch ('On' or 'Off')
!>@n@param      dt_ctl
!>                time step
!
!>@n@param      min_delta_t_ctl
!>                minimum time step length
!>@n@param      max_delta_t_ctl
!>                maximum time step length
!>@n@param      max_eps_to_shrink_ctl
!>                maximum threshold to shrink time step
!>@n@param      min_eps_to_expand_ctl
!>                minimum threshold to expand time step
!
!>@n@param      start_rst_step_ctl
!>                Increment time for volume integrated data output
!>@n@param      end_rst_step_ctl
!>                Increment time for restart data output
!
!>@n@param      delta_t_check_ctl
!>                Increment time for volume integrated data output
!>@n@param      delta_t_rst_ctl
!>                Increment time for restart data output
!>@n@param      delta_t_psf_ctl
!>                Increment time for surface rendering data output
!>@n@param      delta_t_pvr_ctl
!>                Increment time for volume rendering data output
!>@n@param      delta_t_fline_ctl
!>                Increment time for field line data output
!>@n@param      delta_t_field_ctl
!>                Increment time for whole field data output
!>@n@param      delta_t_monitor_ctl
!>                Increment time for monotoring on nodes data output
!>@n@param      delta_t_sgs_coefs_ctl
!>                Increment time for SGS model parameters data output
!>@n@param      delta_t_boundary_ctl
!>                Increment time for boundary data output
!
!
!>@n@param      i_step_init_ctl
!>                start time step
!>@n@param      i_step_number_ctl or i_step_finish_ctl
!>                end time step
!
!>@n@param      i_step_check_ctl
!>                Increment step for volume integrated data output
!>@n@param      i_step_rst_ctl
!>                Increment step for restart data output
!>@n@param      i_step_psf_ctl
!>                Increment step for surface rendering data output
!>@n@param      i_step_pvr_ctl
!>                Increment step for volume rendering data output
!>@n@param      i_step_fline_ctl
!>                Increment step for field line data output
!>@n@param      i_step_field_ctl or i_step_snapshot_ctl
!>                Increment step for whole field data output
!>@n@param      i_step_monitor_ctl
!>                Increment step for monotoring on nodes data output
!>@n@param      i_step_sgs_coefs_ctl
!>                Increment step for SGS model parameters data output
!>@n@param      i_step_boundary_ctl
!>                Increment step for boundary data output
!
!>@n@param      time_init_ctl
!>                Initial time
!
      module m_ctl_data_4_time_steps
!
      use m_precision
      use t_control_elements
!
      implicit  none
!
!
      type(read_integer_item), save :: i_step_init_ctl
!   First step
      type(read_integer_item), save :: i_step_number_ctl
!   End step
      type(read_real_item), save :: elapsed_time_ctl
!   Elapsed time for finish
! 
      type(read_integer_item), save :: i_step_check_ctl
!   Monitoring interval
      type(read_integer_item), save :: i_step_rst_ctl
!   Interval for restart
      type(read_integer_item), save :: i_step_pvr_ctl
      type(read_integer_item), save :: i_step_psf_ctl
      type(read_integer_item), save :: i_step_iso_ctl
      type(read_integer_item), save :: i_step_ucd_ctl
      type(read_integer_item), save :: i_step_fline_ctl
      type(read_integer_item), save :: i_step_monitor_ctl
!
      type(read_integer_item), save :: i_step_sgs_coefs_ctl
!   Interval to evaluate model coefficients
      type(read_integer_item), save :: i_step_boundary_ctl
! 
      type(read_real_item), save :: dt_ctl
!   Delta t
      type(read_real_item), save :: time_init_ctl
!   Initial time (If there is no initial field data)
!
      type(read_integer_item), save :: i_diff_steps_ctl
!
!
      type(read_character_item), save :: flexible_step_ctl
!
      type(read_integer_item), save :: start_rst_step_ctl
      type(read_integer_item), save :: end_rst_step_ctl
!
      type(read_real_item), save :: min_delta_t_ctl
      type(read_real_item), save :: max_delta_t_ctl
      type(read_real_item), save :: max_eps_to_shrink_ctl
      type(read_real_item), save :: min_eps_to_expand_ctl
!
      type(read_real_item), save :: delta_t_check_ctl
      type(read_real_item), save :: delta_t_rst_ctl
      type(read_real_item), save :: delta_t_field_ctl
      type(read_real_item), save :: delta_t_psf_ctl
      type(read_real_item), save :: delta_t_iso_ctl
      type(read_real_item), save :: delta_t_pvr_ctl
      type(read_real_item), save :: delta_t_fline_ctl
      type(read_real_item), save :: delta_t_monitor_ctl
      type(read_real_item), save :: delta_t_sgs_coefs_ctl
      type(read_real_item), save :: delta_t_boundary_ctl
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
      integer (kind=kint) :: i_time_step =      0
!
!   4th level for time steps
!
      character(len=kchara), parameter                                  &
     &       :: hd_elapsed_time =     'elapsed_time_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_flexible_step =     'flexible_step_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_min_delta_t =       'min_delta_t_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_max_delta_t =       'max_delta_t_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_max_eps_to_shrink = 'max_eps_to_shrink_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_min_eps_to_expand = 'min_eps_to_expand_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_start_rst_step =    'start_rst_step_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_end_rst_step =      'end_rst_step_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_delta_t_check =     'delta_t_check_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_delta_t_rst =       'delta_t_rst_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_delta_t_psf =       'delta_t_psf_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_delta_t_iso =       'delta_t_iso_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_delta_t_pvr =       'delta_t_pvr_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_delta_t_fline =     'delta_t_fline_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_delta_t_ucd =       'delta_t_field_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_delta_t_monitor =   'delta_t_monitor_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_delta_t_sgs_coefs = 'delta_t_sgs_coefs_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_delta_t_boundary =  'delta_t_boundary_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_init =      'i_step_init_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_number =    'i_step_number_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_finish_number =  'i_step_finish_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_check =     'i_step_check_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_rst =       'i_step_rst_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_psf =       'i_step_psf_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_iso =       'i_step_iso_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_pvr =       'i_step_pvr_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_fline =     'i_step_fline_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_ucd =       'i_step_field_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_snap =      'i_step_snapshot_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_monitor =   'i_step_monitor_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_sgs_coefs = 'i_step_sgs_coefs_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_step_boundary =  'i_step_boundary_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_i_diff_steps =     'i_diff_steps_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_dt =               'dt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_time_init =        'time_init_ctl'
!
      private :: hd_time_step, i_time_step
      private :: hd_flexible_step, hd_min_delta_t, hd_max_delta_t
      private :: hd_max_eps_to_shrink, hd_min_eps_to_expand
      private :: hd_start_rst_step, hd_end_rst_step
      private :: hd_delta_t_check, hd_delta_t_rst, hd_delta_t_ucd
      private :: hd_delta_t_psf, hd_delta_t_iso
      private :: hd_delta_t_pvr, hd_delta_t_fline
      private :: hd_delta_t_monitor, hd_delta_t_sgs_coefs
      private :: hd_delta_t_boundary
!
      private :: hd_elapsed_time, hd_i_step_init
      private :: hd_i_step_number, hd_i_step_check, hd_i_step_rst
      private :: hd_i_step_psf, hd_i_step_iso, hd_i_finish_number
      private :: hd_i_step_pvr, hd_i_step_fline, hd_i_step_snap
      private :: hd_i_step_ucd, hd_i_step_monitor, hd_i_step_sgs_coefs
      private :: hd_i_step_boundary, hd_dt, hd_time_init
      private :: hd_i_diff_steps
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_time_step_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_time_step) .eq. 0) return
      if (i_time_step .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_time_step, i_time_step)
        if(i_time_step .gt. 0) exit
!
!
        call read_real_ctl_type(hd_elapsed_time, elapsed_time_ctl)
!
        call read_real_ctl_type(hd_dt, dt_ctl)
        call read_real_ctl_type(hd_time_init, time_init_ctl)
!
        call read_real_ctl_type(hd_min_delta_t, min_delta_t_ctl)
        call read_real_ctl_type(hd_max_delta_t, max_delta_t_ctl)
        call read_real_ctl_type(hd_max_eps_to_shrink,                   &
     &      max_eps_to_shrink_ctl)
        call read_real_ctl_type(hd_min_eps_to_expand,                   &
     &      min_eps_to_expand_ctl)
!
        call read_real_ctl_type(hd_delta_t_check, delta_t_check_ctl)
        call read_real_ctl_type(hd_delta_t_rst, delta_t_rst_ctl)
        call read_real_ctl_type(hd_delta_t_psf, delta_t_psf_ctl)
        call read_real_ctl_type(hd_delta_t_iso, delta_t_iso_ctl)
        call read_real_ctl_type(hd_delta_t_pvr, delta_t_pvr_ctl)
        call read_real_ctl_type(hd_delta_t_fline, delta_t_fline_ctl)
        call read_real_ctl_type(hd_delta_t_ucd, delta_t_field_ctl)
        call read_real_ctl_type(hd_delta_t_monitor,                     &
     &      delta_t_monitor_ctl)
        call read_real_ctl_type(hd_delta_t_sgs_coefs,                   &
     &      delta_t_sgs_coefs_ctl)
        call read_real_ctl_type(hd_delta_t_boundary,                    &
     &      delta_t_boundary_ctl)
!
!
        call read_integer_ctl_type(hd_i_step_init, i_step_init_ctl)
        call read_integer_ctl_type(hd_i_step_number,                    &
     &      i_step_number_ctl)
        call read_integer_ctl_type(hd_i_finish_number,                  &
     &      i_step_number_ctl)
!
        call read_integer_ctl_type(hd_i_step_check, i_step_check_ctl)
        call read_integer_ctl_type(hd_i_step_rst, i_step_rst_ctl)
        call read_integer_ctl_type(hd_i_step_psf, i_step_psf_ctl)
        call read_integer_ctl_type(hd_i_step_iso, i_step_iso_ctl)
        call read_integer_ctl_type(hd_i_step_pvr, i_step_pvr_ctl)
        call read_integer_ctl_type(hd_i_step_fline, i_step_fline_ctl)
!
        call read_integer_ctl_type(hd_i_step_ucd, i_step_ucd_ctl)
        call read_integer_ctl_type(hd_i_step_monitor,                   &
     &      i_step_monitor_ctl)
!
        call read_integer_ctl_type(hd_i_step_sgs_coefs,                 &
     &      i_step_sgs_coefs_ctl)
        call read_integer_ctl_type(hd_i_step_boundary,                  &
     &      i_step_boundary_ctl)
!
        call read_integer_ctl_type(hd_i_diff_steps, i_diff_steps_ctl)
!
        call read_integer_ctl_type(hd_start_rst_step,                   &
     &      start_rst_step_ctl)
        call read_integer_ctl_type(hd_end_rst_step, end_rst_step_ctl)
!
!
        call read_chara_ctl_type(hd_flexible_step, flexible_step_ctl)
      end do
!
      end subroutine read_time_step_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_4_time_steps
