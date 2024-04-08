!>@file   bcast_ctl_data_viz3.f90
!!@brief  module bcast_ctl_data_viz3
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!
!>@brief Control data for four visualizations
!!
!!@verbatim
!!      subroutine bcast_viz3_controls(viz3_ctls)
!!       type(vis3_controls), intent(inout) :: viz3_ctls
!!@endverbatim
!
      module bcast_ctl_data_viz3
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi_int
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_viz3_controls(viz3_ctls)
!
      use t_control_data_viz3
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
      use bcast_section_control_data
      use bcast_maps_control_data
      use bcast_ctl_data_field_line
      use bcast_control_data_pvrs
!
      type(vis3_controls), intent(inout) :: viz3_ctls
!
!
      call bcast_files_4_psf_ctl(viz3_ctls%psf_ctls)
      call bcast_files_4_iso_ctl(viz3_ctls%iso_ctls)
      call bcast_files_4_map_ctl(viz3_ctls%map_ctls)
      call bcast_files_4_pvr_ctl(viz3_ctls%pvr_ctls)
!
      call bcast_ctl_type_r1(viz3_ctls%delta_t_psf_v_ctl)
      call bcast_ctl_type_r1(viz3_ctls%delta_t_iso_v_ctl)
      call bcast_ctl_type_r1(viz3_ctls%delta_t_map_v_ctl)
!
      call bcast_ctl_type_r1(viz3_ctls%delta_t_pvr_v_ctl)
      call bcast_ctl_type_r1(viz3_ctls%delta_t_ucd_v_ctl)
!
      call bcast_ctl_type_i1(viz3_ctls%i_step_psf_v_ctl)
      call bcast_ctl_type_i1(viz3_ctls%i_step_iso_v_ctl)
      call bcast_ctl_type_i1(viz3_ctls%i_step_map_v_ctl)
!
      call bcast_ctl_type_i1(viz3_ctls%i_step_pvr_v_ctl)
      call bcast_ctl_type_i1(viz3_ctls%i_step_ucd_v_ctl)
!
      call bcast_ctl_type_c1(viz3_ctls%output_field_file_fmt_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (viz3_ctls%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(viz3_ctls%i_viz_control, 0)
!
      end subroutine bcast_viz3_controls
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_data_viz3
