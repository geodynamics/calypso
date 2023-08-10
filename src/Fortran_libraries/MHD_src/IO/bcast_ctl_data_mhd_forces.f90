!>@file   bcast_ctl_data_mhd_forces.f90
!!@brief  module bcast_ctl_data_mhd_forces
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!!@date Modified in July, 2013
!
!>@brief  Distribute control data for normalization
!!
!!@verbatim
!!      subroutine bcast_forces_ctl(frc_ctl)
!!        type(forces_control), intent(inout) :: frc_ctl
!!      subroutine bcast_gravity_ctl(g_ctl)
!!        type(forces_control), intent(inout) :: g_ctl
!!      subroutine bcast_coriolis_ctl(cor_ctl)
!!        type(coriolis_control), intent(inout) :: cor_ctl
!!      subroutine bcast_magneto_ctl(mcv_ctl)
!!        type(magneto_convection_control), intent(inout) :: mcv_ctl
!!      subroutine bcast_magnetic_scale_ctl(bscale_ctl)
!!        type(magnetic_field_scale_control), intent(inout) :: bscale_ctl
!!      subroutine reset_ref_scalar_ctl(refs_ctl)
!!        type(reference_temperature_ctl), intent(inout) :: refs_ctl
!!      subroutine bcast_ref_value_ctl(ref_ctl)
!!        type(reference_point_control), intent(inout) :: ref_ctl
!!      subroutine bcast_takepiro_ctl(takepiro_ctl)
!!        type(takepiro_model_control), intent(inout) :: takepiro_ctl
!!@endverbatim
!
      module bcast_ctl_data_mhd_forces
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_takepiro_ctl, bcast_ref_value_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine bcast_forces_ctl(frc_ctl)
!
      use t_ctl_data_mhd_forces
      use transfer_to_long_integers
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
!
      type(forces_control), intent(inout) :: frc_ctl
!
      call bcast_ctl_array_c1(frc_ctl%force_names)
!
      call calypso_mpi_bcast_character                                  &
     &   (frc_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(frc_ctl%i_forces_ctl, 0)
!
      end subroutine bcast_forces_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_gravity_ctl(g_ctl)
!
      use t_ctl_data_gravity
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
!
      type(gravity_control), intent(inout) :: g_ctl
!
!
      call bcast_ctl_array_cr(g_ctl%gravity_vector)
      call bcast_ctl_type_c1(g_ctl%gravity)
!
      call calypso_mpi_bcast_character                                  &
     &   (g_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(g_ctl%i_gravity_ctl, 0)
!
      end subroutine bcast_gravity_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_coriolis_ctl(cor_ctl)
!
      use t_ctl_data_coriolis_force
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
!
      type(coriolis_control), intent(inout) :: cor_ctl
!
      call bcast_ctl_array_cr(cor_ctl%system_rotation)
!
      call calypso_mpi_bcast_character                                  &
     &   (cor_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(cor_ctl%i_coriolis_ctl, 0)
!
      end subroutine bcast_coriolis_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_magneto_ctl(mcv_ctl)
!
      use t_ctl_data_mhd_magne
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(magneto_convection_control), intent(inout) :: mcv_ctl
!
!
      call bcast_ctl_array_cr(mcv_ctl%ext_magne)
      call bcast_ctl_type_c1(mcv_ctl%magneto_cv)
      call bcast_ctl_type_c1(mcv_ctl%filterd_induction_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (mcv_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(mcv_ctl%i_magneto_ctl, 0)
!
      end subroutine bcast_magneto_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_magnetic_scale_ctl(bscale_ctl)
!
      use t_ctl_data_magnetic_scale
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(magnetic_field_scale_control), intent(inout) :: bscale_ctl
!
!
      call bcast_ctl_array_cr(bscale_ctl%mag_to_kin_energy_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (bscale_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(bscale_ctl%i_bscale_ctl, 0)
!
      end subroutine bcast_magnetic_scale_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_ref_scalar_ctl(refs_ctl)
!
      use t_ctl_data_temp_model
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
!
      type(reference_temperature_ctl), intent(inout) :: refs_ctl
!
!
      call bcast_ref_value_ctl(refs_ctl%low_ctl)
      call bcast_ref_value_ctl(refs_ctl%high_ctl)
      call bcast_takepiro_ctl(refs_ctl%takepiro_ctl)
!
      call bcast_ctl_type_c1(refs_ctl%filterd_advect_ctl)
      call bcast_ctl_type_c1(refs_ctl%reference_ctl)
      call bcast_ctl_type_c1(refs_ctl%ref_file_ctl)
      call bcast_ctl_type_c1(refs_ctl%stratified_ctl)
      call bcast_ctl_type_r1(refs_ctl%ICB_diffuse_reduction_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (refs_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(refs_ctl%i_temp_def, 0)
!
      end subroutine bcast_ref_scalar_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_ref_value_ctl(ref_ctl)
!
      use t_ctl_data_temp_model
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(reference_point_control), intent(inout) :: ref_ctl
!
      call bcast_ctl_type_r1(ref_ctl%depth)
      call bcast_ctl_type_r1(ref_ctl%value)
!
      call calypso_mpi_bcast_character                                  &
     &   (ref_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(ref_ctl%i_referenced, 0)
!
      end subroutine bcast_ref_value_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_takepiro_ctl(takepiro_ctl)
!
      use t_ctl_data_stratified_model
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(takepiro_model_control), intent(inout) :: takepiro_ctl
!
      call bcast_ctl_type_r1(takepiro_ctl%stratified_sigma_ctl)
      call bcast_ctl_type_r1(takepiro_ctl%stratified_width_ctl)
      call bcast_ctl_type_r1(takepiro_ctl%stratified_outer_r_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (takepiro_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(takepiro_ctl%i_takepiro_t_ctl, 0)
!
      end subroutine bcast_takepiro_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_data_mhd_forces
