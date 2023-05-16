!>@file   bcast_ctl_data_viz4.f90
!!@brief  module bcast_ctl_data_viz4
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!
!>@brief Control data for four visualizations
!!
!!@verbatim
!!      subroutine bcast_viz4_controls(viz_ctls)
!!       type(vis4_controls), intent(inout) :: viz_ctls
!!@endverbatim
!
      module bcast_ctl_data_viz4
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
      subroutine bcast_viz4_controls(viz_ctls)
!
      use t_control_data_viz4
      use calypso_mpi_int
      use bcast_control_arrays
      use bcast_section_control_data
      use bcast_ctl_data_field_line
      use bcast_control_data_pvrs
!
      type(vis4_controls), intent(inout) :: viz_ctls
!
!
      call bcast_files_4_psf_ctl(viz_ctls%psf_ctls)
      call bcast_files_4_iso_ctl(viz_ctls%iso_ctls)
      call bcast_files_4_pvr_ctl(viz_ctls%pvr_ctls)
      call bcast_files_4_fline_ctl(viz_ctls%fline_ctls)
!
      call bcast_files_4_pvr_ctl(viz_ctls%pvr_anaglyph_ctls)
!
      call bcast_ctl_type_r1(viz_ctls%delta_t_psf_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_iso_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_pvr_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_fline_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_ucd_v_ctl)
!
      call bcast_ctl_type_i1(viz_ctls%i_step_psf_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_iso_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_pvr_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_fline_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_ucd_v_ctl)
!
      call bcast_ctl_type_c1(viz_ctls%output_field_file_fmt_ctl)
!
      call calypso_mpi_bcast_one_int(viz_ctls%i_viz_control, 0)
!
      end subroutine bcast_viz4_controls
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_data_viz4
