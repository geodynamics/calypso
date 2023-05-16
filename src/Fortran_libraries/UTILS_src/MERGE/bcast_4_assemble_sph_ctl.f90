!>@file   bcast_4_assemble_sph_ctl.f90
!!@brief  module bcast_4_assemble_sph_ctl
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2007
!
!>@brief  Control data for merge program
!!
!!@verbatim
!!       subroutine bcast_merge_control_data(mgd_ctl)
!!         type(control_data_4_merge), intent(inout) :: mgd_ctl
!!@endverbatim
!
      module bcast_4_assemble_sph_ctl
!
      use m_precision
      use calypso_mpi
      use t_control_data_4_merge
!
      implicit none
!
      private :: bcast_merge_field_data, bcast_merge_step_data
      private :: bcast_newrst_control
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine bcast_merge_control_data(mgd_ctl)
!
      use m_machine_parameter
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use t_ctl_data_gen_sph_shell
      use bcast_4_platform_ctl
      use bcast_4_sphere_ctl
      use bcast_control_arrays
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      call bcast_ctl_data_4_platform(mgd_ctl%source_plt)
      call bcast_ctl_data_4_platform(mgd_ctl%assemble_plt)
!
      call bcast_parallel_shell_ctl(mgd_ctl%src_psph_ctl)
      call bcast_parallel_shell_ctl(mgd_ctl%asbl_psph_ctl)
!
      call bcast_merge_field_data(mgd_ctl)
      call bcast_merge_step_data(mgd_ctl)
      call bcast_newrst_control(mgd_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (mgd_ctl%fname_src_psph_ctl, cast_long(kchara), 0)
      call calypso_mpi_bcast_character                                  &
     &   (mgd_ctl%fname_asbl_psph_ctl, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(mgd_ctl%i_assemble, 0)
!
      end subroutine bcast_merge_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine bcast_merge_field_data(mgd_ctl)
!
      use calypso_mpi_int
      use bcast_4_field_ctl
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      call bcast_phys_data_ctl(mgd_ctl%fld_mge_ctl)
!
      call calypso_mpi_bcast_one_int(mgd_ctl%i_model, 0)
!
      end subroutine bcast_merge_field_data
!
! -----------------------------------------------------------------------
!
       subroutine bcast_merge_step_data(mgd_ctl)
!
      use calypso_mpi_int
      use bcast_4_time_step_ctl
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      call bcast_ctl_data_4_time_step(mgd_ctl%t_mge_ctl)
      call bcast_ctl_data_4_time_step(mgd_ctl%t2_mge_ctl)
!
      call calypso_mpi_bcast_one_int(mgd_ctl%i_control, 0)
!
      end subroutine bcast_merge_step_data
!
! -----------------------------------------------------------------------
!
      subroutine bcast_newrst_control(mgd_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      call bcast_ctl_type_r1(mgd_ctl%magnetic_ratio_ctl)
!
      call calypso_mpi_bcast_one_int(mgd_ctl%i_newrst_magne, 0)
!
      end subroutine bcast_newrst_control
!
! -----------------------------------------------------------------------
!
      end module bcast_4_assemble_sph_ctl
