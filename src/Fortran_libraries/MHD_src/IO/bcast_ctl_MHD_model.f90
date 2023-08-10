!>@file   bcast_ctl_MHD_model.f90
!!@brief  module bcast_ctl_MHD_model
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine bcast_ctl_data_mhd_model(model_ctl)
!!        type(mhd_model_control), intent(inout) :: model_ctl
!!
!!      subroutine bcast_dimless_ctl(dless_ctl)
!!        type(dimless_control), intent(inout) :: dless_ctl
!!      subroutine bcast_coef_term_ctl(eqs_ctl)
!!        type(equations_control), intent(in) :: eqs_ctl
!!
!!      subroutine bcast_thermal_ctl(heat_ctl)
!!        type(heat_equation_control), intent(inout) :: heat_ctl
!!      subroutine bcast_momentum_ctl(mom_ctl)
!!        type(momentum_equation_control), intent(inout) :: mom_ctl
!!      subroutine bcast_induction_ctl(induct_ctl)
!!        type(induction_equation_control), intent(inout) :: induct_ctl
!!@endverbatim
!
      module bcast_ctl_MHD_model
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
      private :: bcast_dimless_ctl, bcast_coef_term_ctl
      private :: bcast_thermal_ctl, bcast_momentum_ctl
      private :: bcast_induction_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_mhd_model(model_ctl)
!
      use t_ctl_data_MHD_model
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_4_field_ctl
      use bcast_ctl_data_mhd_evo
      use bcast_ctl_data_mhd_forces
      use transfer_to_long_integers
!
      type(mhd_model_control), intent(inout) :: model_ctl
!
!
      call bcast_phys_data_ctl(model_ctl%fld_ctl)
      call bcast_mhd_time_evo_ctl(model_ctl%evo_ctl)
      call bcast_mhd_layer_ctl(model_ctl%earea_ctl)
!
      call bcast_bc_4_node_ctl(model_ctl%nbc_ctl)
      call bcast_bc_4_surf_ctl(model_ctl%sbc_ctl)
!
      call bcast_dimless_ctl(model_ctl%dless_ctl)
      call bcast_coef_term_ctl(model_ctl%eqs_ctl)
      call bcast_forces_ctl(model_ctl%frc_ctl)
      call bcast_gravity_ctl(model_ctl%g_ctl)
      call bcast_coriolis_ctl(model_ctl%cor_ctl)
      call bcast_magneto_ctl(model_ctl%mcv_ctl)
      call bcast_magnetic_scale_ctl(model_ctl%bscale_ctl)
      call bcast_ref_scalar_ctl(model_ctl%reft_ctl)
      call bcast_ref_scalar_ctl(model_ctl%refc_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (model_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(model_ctl%i_model, 0)
!
      end subroutine bcast_ctl_data_mhd_model
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_dimless_ctl(dless_ctl)
!
      use t_ctl_data_dimless_numbers
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(dimless_control), intent(inout) :: dless_ctl
!
      call bcast_ctl_array_cr(dless_ctl%dimless)
!
      call calypso_mpi_bcast_character                                  &
     &   (dless_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(dless_ctl%i_dimless_ctl, 0)
!
      end subroutine bcast_dimless_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_coef_term_ctl(eqs_ctl)
!
      use t_ctl_data_mhd_normalize
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(equations_control), intent(inout) :: eqs_ctl
!
!
      call bcast_thermal_ctl(eqs_ctl%heat_ctl)
      call bcast_momentum_ctl(eqs_ctl%mom_ctl)
      call bcast_induction_ctl(eqs_ctl%induct_ctl)
      call bcast_thermal_ctl(eqs_ctl%comp_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (eqs_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(eqs_ctl%i_coef_term_ctl, 0)
!
      end subroutine bcast_coef_term_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_thermal_ctl(heat_ctl)
!
      use t_ctl_data_termal_norm
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(heat_equation_control), intent(inout) :: heat_ctl
!
      call bcast_ctl_array_cr(heat_ctl%coef_4_adv_flux)
      call bcast_ctl_array_cr(heat_ctl%coef_4_diffuse)
      call bcast_ctl_array_cr(heat_ctl%coef_4_source)
!
      call calypso_mpi_bcast_character                                  &
     &   (heat_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(heat_ctl%i_diff_adv, 0)
!
      end subroutine bcast_thermal_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_momentum_ctl(mom_ctl)
!
      use t_ctl_data_momentum_norm
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(momentum_equation_control), intent(inout) :: mom_ctl
!
!
      call bcast_ctl_array_cr(mom_ctl%coef_4_intertia)
      call bcast_ctl_array_cr(mom_ctl%coef_4_grad_p)
      call bcast_ctl_array_cr(mom_ctl%coef_4_viscous)
!
      call bcast_ctl_array_cr(mom_ctl%coef_4_termal_buo)
      call bcast_ctl_array_cr(mom_ctl%coef_4_comp_buo)
      call bcast_ctl_array_cr(mom_ctl%coef_4_Coriolis)
      call bcast_ctl_array_cr(mom_ctl%coef_4_Lorentz)
!
      call calypso_mpi_bcast_character                                  &
     &   (mom_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(mom_ctl%i_momentum, 0)
!
      end subroutine bcast_momentum_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_induction_ctl(induct_ctl)
!
      use t_ctl_data_induct_norm
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(induction_equation_control), intent(inout) :: induct_ctl
!
      call bcast_ctl_array_cr(induct_ctl%coef_4_magne_evo)
      call bcast_ctl_array_cr(induct_ctl%coef_4_mag_potential)
      call bcast_ctl_array_cr(induct_ctl%coef_4_mag_diffuse)
      call bcast_ctl_array_cr(induct_ctl%coef_4_induction)
!
      call calypso_mpi_bcast_character                                  &
     &   (induct_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(induct_ctl%i_induct_ctl, 0)
!
      end subroutine bcast_induction_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_MHD_model
