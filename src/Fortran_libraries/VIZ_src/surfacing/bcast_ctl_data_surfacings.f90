!>@file   bcast_ctl_data_surfacings.f90
!!@brief  module bcast_ctl_data_surfacings
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine bcast_surfacing_controls(surfacing_ctls)
!!       type(surfacing_controls), intent(inout) :: surfacing_ctls
!!@endverbatim
!
      module bcast_ctl_data_surfacings
!
      use m_precision
      use m_machine_parameter
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
      subroutine bcast_surfacing_controls(surfacing_ctls)
!
      use t_control_data_surfacings
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
      use bcast_section_control_data
!
      type(surfacing_controls), intent(inout) :: surfacing_ctls
!
!
      call bcast_files_4_psf_ctl(surfacing_ctls%psf_s_ctls)
      call bcast_files_4_iso_ctl(surfacing_ctls%iso_s_ctls)
!
      call bcast_ctl_type_r1(surfacing_ctls%delta_t_psf_s_ctl)
      call bcast_ctl_type_r1(surfacing_ctls%delta_t_iso_s_ctl)
      call bcast_ctl_type_r1(surfacing_ctls%delta_t_ucd_s_ctl)
!
      call bcast_ctl_type_i1(surfacing_ctls%i_step_psf_s_ctl)
      call bcast_ctl_type_i1(surfacing_ctls%i_step_iso_s_ctl)
      call bcast_ctl_type_i1(surfacing_ctls%i_step_ucd_s_ctl)
!
      call bcast_ctl_type_c1(surfacing_ctls%output_ucd_fmt_s_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (surfacing_ctls%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int                                    &
     &   (surfacing_ctls%i_surfacing_control, 0)
!
      end subroutine bcast_surfacing_controls
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_data_surfacings
