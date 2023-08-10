!>@file   bcast_ctl_data_mhd_evo.f90
!!@brief  module bcast_ctl_data_mhd_evo
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!
!>@brief  Control data of time integration flags
!!
!!@verbatim
!!      subroutine bcast_mhd_time_evo_ctl(evo_ctl)
!!        type(mhd_evolution_control), intent(inout) :: evo_ctl
!!      subroutine bcast_mhd_layer_ctl(earea_ctl)
!!        type(mhd_evo_area_control), intent(inout) :: earea_ctl
!!
!!      subroutine bcast_bc_4_node_ctl(nbc_ctl)
!!        type(node_bc_control), intent(inout) :: nbc_ctl
!!      subroutine bcast_bc_4_surf_ctl(sbc_ctl)
!!        type(surf_bc_control), intent(inout) :: sbc_ctl
!!@endverbatim
!
      module bcast_ctl_data_mhd_evo
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine bcast_mhd_time_evo_ctl(evo_ctl)
!
      use t_ctl_data_mhd_evolution
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
!
      type(mhd_evolution_control), intent(inout) :: evo_ctl
!
!
      call bcast_ctl_array_c1(evo_ctl%t_evo_field_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (evo_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(evo_ctl%i_time_evo, 0)
!
      end subroutine bcast_mhd_time_evo_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_mhd_layer_ctl(earea_ctl)
!
      use t_ctl_data_mhd_evo_area
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
!
      type(mhd_evo_area_control), intent(inout) :: earea_ctl
!
!
      call bcast_ctl_array_c1(earea_ctl%evo_fluid_group_ctl)
      call bcast_ctl_array_c1(earea_ctl%evo_conduct_group_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (earea_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(earea_ctl%i_layers_ctl, 0)
!
      end subroutine bcast_mhd_layer_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_bc_4_node_ctl(nbc_ctl)
!
      use t_ctl_data_node_boundary
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
!
      type(node_bc_control), intent(inout) :: nbc_ctl
!
!
      call bcast_ctl_array_c2r(nbc_ctl%node_bc_T_ctl)
      call bcast_ctl_array_c2r(nbc_ctl%node_bc_U_ctl)
      call bcast_ctl_array_c2r(nbc_ctl%node_bc_P_ctl)
      call bcast_ctl_array_c2r(nbc_ctl%node_bc_C_ctl)
      call bcast_ctl_array_c2r(nbc_ctl%node_bc_B_ctl)
      call bcast_ctl_array_c2r(nbc_ctl%node_bc_MP_ctl)
      call bcast_ctl_array_c2r(nbc_ctl%node_bc_A_ctl)
      call bcast_ctl_array_c2r(nbc_ctl%node_bc_J_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (nbc_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(nbc_ctl%i_bc_4_node, 0)
!
      end subroutine bcast_bc_4_node_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_bc_4_surf_ctl(sbc_ctl)
!
      use t_ctl_data_surf_boundary
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
!
      type(surf_bc_control), intent(inout) :: sbc_ctl
!
!
      call bcast_ctl_array_c2r(sbc_ctl%surf_bc_HF_ctl)
      call bcast_ctl_array_c2r(sbc_ctl%surf_bc_ST_ctl)
      call bcast_ctl_array_c2r(sbc_ctl%surf_bc_PN_ctl)
      call bcast_ctl_array_c2r(sbc_ctl%surf_bc_BN_ctl)
      call bcast_ctl_array_c2r(sbc_ctl%surf_bc_JN_ctl)
      call bcast_ctl_array_c2r(sbc_ctl%surf_bc_AN_ctl)
      call bcast_ctl_array_c2r(sbc_ctl%surf_bc_MPN_ctl)
      call bcast_ctl_array_c2r(sbc_ctl%surf_bc_CF_ctl)
      call bcast_ctl_array_c2r(sbc_ctl%surf_bc_INF_ctl)
!
      call calypso_mpi_bcast_character                                  &
     &   (sbc_ctl%block_name, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(sbc_ctl%i_bc_4_surf, 0)
!
      end subroutine bcast_bc_4_surf_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_data_mhd_evo
