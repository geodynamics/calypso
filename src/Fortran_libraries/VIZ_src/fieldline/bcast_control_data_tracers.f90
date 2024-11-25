!>@file   bcast_control_data_tracers.f90
!!@brief  module bcast_control_data_tracers
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine bcast_tracer_controls(viz_ctls)
!!        type(tracers_control), intent(inout) :: tracer_ctls
!!@endverbatim
      module bcast_control_data_tracers
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_tracers
      use calypso_mpi
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_tracer_controls(tracer_ctls)
!
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
      use bcast_ctl_data_field_line
!
      type(tracers_control), intent(inout) :: tracer_ctls
!
!
      call bcast_files_4_fline_ctl(tracer_ctls%tracer_controls)
!
      call bcast_ctl_type_r1(tracer_ctls%delta_t_tracer_out_ctl)
      call bcast_ctl_type_i1(tracer_ctls%i_step_tracer_out_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (tracer_ctls%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(tracer_ctls%i_tracers_control, 0)
!
      end subroutine bcast_tracer_controls
!
!  ---------------------------------------------------------------------
!
      end module bcast_control_data_tracers
