!>@file   t_MHD_step_parameter.f90
!!@brief  module t_MHD_step_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for time steppings
!
      module  t_MHD_step_parameter
!
!
      use m_precision
      use t_time_data
      use t_IO_step_parameter
      use t_VIZ_step_parameter
      use t_flex_delta_t_data
!
      implicit  none
!
!
!
      type MHD_step_param
!>        Structure for time data
        type(time_data) :: time_d
!>        Structure for initial time data
        type(time_data) :: init_d
!>        Structure for end time data
        type(finish_data) :: finish_d
!
!>        Flag for initial step to use Euler scheme
!!        insted of Adams-BAshforth
        integer(kind=kint) :: iflag_initial_step = 0
!
!>        Increment for mean restart data
        type(IO_step_param) :: rst_step
!>        Increment for mean field data
        type(IO_step_param) :: ucd_step
!>        Increment for mean square output
        type(IO_step_param) :: rms_step
!>        Increment for nodal monitor
        type(IO_step_param) :: point_step
!>        Increment for boundary condition file
        type(IO_step_param) :: boundary_step
!>        Increment for model coefficient file
        type(IO_step_param) :: sgs_IO_step
!
!>        Increment for visualizations
        type(VIZ_step_params) :: viz_step
!
!>        Flexible step information
        type(flexible_stepping_parameter) :: flex_p
      end type MHD_step_param
!
      end module  t_MHD_step_parameter
