!>@file   t_ctl_data_4_time_steps.f90
!!        module t_ctl_data_4_time_steps
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!!@n    Modified in Nov., 2006
!!
!> @brief Control input routine for time step parameters
!!
!!@verbatim
!!      subroutine reset_ctl_data_4_time_step(tctl)
!!        type(time_data_control), intent(inout) :: tctl
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
!!      i_step_check_ctl             40
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
!>@n@param      ratio_to_CFL_ctl
!>                Ratio to CFL condition
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
!>@n@param      delta_t_iso_ctl
!>                Increment time for isosurface rendering data output
!>@n@param      delta_t_map_ctl
!>                Increment time for map projection data output
!>@n@param      delta_t_pvr_ctl
!>                Increment time for volume rendering data output
!>@n@param      delta_t_fline_ctl
!>                Increment time for field line data output
!>@n@param      delta_t_lic_ctl
!>                Increment time for LIC rendering output
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
!>@n@param      i_step_sectioning_ctl
!>                Increment step for surface rendering data output
!>@n@param      i_step_map_projection_ctl
!>                Increment step for map projection data output
!>@n@param      i_step_pvr_ctl
!>                Increment step for volume rendering data output
!>@n@param      i_step_fline_ctl
!>                Increment step for field line data output
!>@n@param      i_step_lic_ctl
!>                Increment step for LIC rendering data output
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
      module t_ctl_data_4_time_steps
!
      use m_precision
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_read_control_elements
!
      implicit  none
!
!
!>   Structure of time stepping controls
      type time_data_control
!>        Block name
        character(len=kchara) :: block_name = 'time_step_ctl'
!>   First step
        type(read_integer_item) :: i_step_init_ctl
!>   End step
        type(read_integer_item) :: i_step_number_ctl
!>   Elapsed time for finish
        type(read_real_item) :: elapsed_time_ctl
! 
!>   Monitoring inrement
        type(read_integer_item) :: i_step_check_ctl
!>   Increment for restart
        type(read_integer_item) :: i_step_rst_ctl
!
!>   Increment for volume rendering
        type(read_integer_item) :: i_step_pvr_ctl
!>   Increment for sectioning
        type(read_integer_item) :: i_step_psf_ctl
!>   Increment for map projection
        type(read_integer_item) :: i_step_map_ctl
!>   Increment for isosurface
        type(read_integer_item) :: i_step_iso_ctl
!>   Increment for LIC rendering
        type(read_integer_item) :: i_step_lic_ctl
!>   Increment for field line
        type(read_integer_item) :: i_step_fline_ctl
!>   Increment for field data output
        type(read_integer_item) :: i_step_ucd_ctl
!
!>   Increment for monitoring point data
        type(read_integer_item) :: i_step_monitor_ctl
!
!>   Increment to evaluate model coefficients
        type(read_integer_item) :: i_step_sgs_coefs_ctl
!>   Increment to output boundary data
        type(read_integer_item) :: i_step_boundary_ctl
! 
!>   Delta t
        type(read_real_item) :: dt_ctl
!>   Initial time (If there is no initial field data)
        type(read_real_item) :: time_init_ctl
!
        type(read_integer_item) :: i_diff_steps_ctl
!
!>     Flexible time step flag
        type(read_character_item) :: flexible_step_ctl
!
!>     Ratio to CFL condition
        type(read_real_item) :: ratio_to_cfl_ctl
!
!>     Start time
        type(read_integer_item) :: start_rst_step_ctl
!>     End  time
        type(read_integer_item) :: end_rst_step_ctl
!
!>     Minimum delta t
        type(read_real_item) :: min_delta_t_ctl
!>     Maximum delta t
        type(read_real_item) :: max_delta_t_ctl
!>     Maximum delta t
        type(read_real_item) :: max_eps_to_shrink_ctl
!>     Minimum delta t
        type(read_real_item) :: min_eps_to_expand_ctl
!
!>   Monitoring inrement
        type(read_real_item) :: delta_t_check_ctl
!>   time interval for restart
        type(read_real_item) :: delta_t_rst_ctl
!
!>   time interval for sectioning
        type(read_real_item) :: delta_t_psf_ctl
!>   time interval for isosurface
        type(read_real_item) :: delta_t_iso_ctl
!>   time interval for map projection
        type(read_real_item) :: delta_t_map_ctl
!>   time interval for volume rendering
        type(read_real_item) :: delta_t_pvr_ctl
!>   time interval for field line
        type(read_real_item) :: delta_t_fline_ctl
!>   time interval for LIC rendering
        type(read_real_item) :: delta_t_lic_ctl
!
!>   time interval for field data output
        type(read_real_item) :: delta_t_field_ctl
!>   time interval for monitoring point data
        type(read_real_item) :: delta_t_monitor_ctl
!>   time interval to evaluate model coefficients
        type(read_real_item) :: delta_t_sgs_coefs_ctl
!>   time interval to output boundary data
        type(read_real_item) :: delta_t_boundary_ctl
!
        integer (kind=kint) :: i_tstep =      0
      end type time_data_control
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
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
      tctl%delta_t_map_ctl%iflag = 0
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
      tctl%i_step_map_ctl%iflag = 0
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
      end module t_ctl_data_4_time_steps
