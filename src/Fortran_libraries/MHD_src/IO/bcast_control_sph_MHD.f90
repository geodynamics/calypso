!>@file   bcast_control_sph_MHD.f90
!!@brief  module bcast_control_sph_MHD
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!@n        Modified by H. Matsui on Apr., 2023
!!
!!@verbatim
!!      subroutine bcast_sph_mhd_control_data(MHD_ctl)
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
!!
!!      subroutine bcast_sph_mhd_control(smctl_ctl)
!!        type(sph_mhd_control_control), intent(inout) :: smctl_ctl
!!      subroutine bcast_crustal_filtering_ctl(crust_filter_c)
!!        type(clust_filtering_ctl), intent(inout) :: crust_filter_c
!!@endverbatim
!
      module bcast_control_sph_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_psf
!
      implicit none
!
      private :: bcast_sph_mhd_control
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_control_data(MHD_ctl)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
      use bcast_ctl_MHD_model
      use bcast_monitor_data_ctl
!
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
!
!
      call bcast_ctl_data_4_platform(MHD_ctl%plt)
      call bcast_ctl_data_4_platform(MHD_ctl%org_plt)
      call bcast_ctl_data_4_platform(MHD_ctl%new_plt)
!
      call bcast_parallel_shell_ctl(MHD_ctl%psph_ctl)
!
      call bcast_ctl_data_mhd_model(MHD_ctl%model_ctl)
      call bcast_sph_mhd_control(MHD_ctl%smctl_ctl)
!
      call bcast_node_monitor_data_ctl(MHD_ctl%nmtr_ctl)
      call bcast_sph_monitoring_ctl(MHD_ctl%smonitor_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (MHD_ctl%fname_psph, cast_long(kchara), 0)
!
      call calypso_mpi_bcast_character                                  &
     &   (MHD_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(MHD_ctl%i_mhd_ctl, 0)
!
      end subroutine bcast_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_control(smctl_ctl)
!
      use t_ctl_data_SPH_MHD_control
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_4_time_step_ctl
      use bcast_ctl_data_mhd_time_rst
!
      type(sph_mhd_control_control), intent(inout) :: smctl_ctl
!
!
      call bcast_restart_ctl(smctl_ctl%mrst_ctl)
      call bcast_time_loop_ctl(smctl_ctl%mevo_ctl)
      call bcast_ctl_data_4_time_step(smctl_ctl%tctl)
!
      call calypso_mpi_bcast_character(smctl_ctl%block_name,            &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(smctl_ctl%i_control, 0)
!
      end subroutine bcast_sph_mhd_control
!
!   --------------------------------------------------------------------
!
      subroutine bcast_crustal_filtering_ctl(crust_filter_c)
!
      use t_ctl_data_crust_filter
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(clust_filtering_ctl), intent(inout) :: crust_filter_c
!
!
      call bcast_ctl_type_i1(crust_filter_c%crust_truncation_ctl)
!
      call calypso_mpi_bcast_character(crust_filter_c%block_name,       &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int                                    &
     &   (crust_filter_c%i_crustal_filtering, 0)
!
      end subroutine bcast_crustal_filtering_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_control_sph_MHD
