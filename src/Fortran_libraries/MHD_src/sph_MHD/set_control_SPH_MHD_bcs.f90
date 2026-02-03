!>@file   set_control_SPH_MHD_bcs.f90
!!@brief  module set_control_SPH_MHD_bcs
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Set control data for spherical transform MHD dynamo simulation
!!
!!@verbatim
!!      subroutine s_set_control_SPH_MHD_bcs(MHD_prop, nbc_ctl,         &
!!     &                                     sbc_ctl, MHD_BC)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(node_bc_control), intent(in) :: nbc_ctl
!!        type(surf_bc_control), intent(in) :: sbc_ctl
!!        type(MHD_BC_lists), intent(inout) :: MHD_BC
!!      subroutine set_ctl_SPH_val_diffusions(model_ctl, MHD_prop)
!!        type(mhd_model_control), intent(in) :: model_ctl
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!@endverbatim
!
      module set_control_SPH_MHD_bcs
!
      use calypso_mpi
      use m_precision
      use m_machine_parameter
!
      use t_control_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_control_SPH_MHD_bcs(MHD_prop, nbc_ctl,           &
     &                                     sbc_ctl, MHD_BC)
!
      use t_bc_data_list
      use t_ctl_data_node_boundary
      use t_ctl_data_surf_boundary
!
      use set_control_4_velo
      use set_control_4_press
      use set_control_4_temp
      use set_control_4_magne
      use set_control_4_composition
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(node_bc_control), intent(in) :: nbc_ctl
      type(surf_bc_control), intent(in) :: sbc_ctl
!
      type(MHD_BC_lists), intent(inout) :: MHD_BC
!
!
!   set boundary conditions for temperature
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_temp'
      call s_set_control_4_temp(MHD_prop%ht_prop,                       &
     &    nbc_ctl%node_bc_T_ctl, sbc_ctl%surf_bc_HF_ctl,                &
     &    MHD_BC%temp_BC%nod_BC, MHD_BC%temp_BC%surf_BC)
!
!
!   set boundary conditions for velocity
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_velo'
      call s_set_control_4_velo(MHD_prop%fl_prop,                       &
     &    nbc_ctl%node_bc_U_ctl, sbc_ctl%surf_bc_ST_ctl,                &
     &    MHD_BC%velo_BC%nod_BC, MHD_BC%velo_BC%surf_BC)
!
!  set boundary conditions for pressure
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_press'
      call s_set_control_4_press(MHD_prop%fl_prop,                      &
     &    nbc_ctl%node_bc_P_ctl, sbc_ctl%surf_bc_PN_ctl,                &
     &    MHD_BC%press_BC%nod_BC, MHD_BC%press_BC%surf_BC)!
!   set boundary conditions for composition variation
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_composition'
      call s_set_control_4_composition(MHD_prop%cp_prop,                &
     &    nbc_ctl%node_bc_C_ctl, sbc_ctl%surf_bc_CF_ctl,                &
     &    MHD_BC%light_BC%nod_BC, MHD_BC%light_BC%surf_BC)
!
!   set boundary_conditons for magnetic field
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_magne'
      call s_set_control_4_magne(MHD_prop%cd_prop,                      &
     &    nbc_ctl%node_bc_B_ctl, sbc_ctl%surf_bc_BN_ctl,                &
     &    MHD_BC%magne_BC%nod_BC, MHD_BC%magne_BC%surf_BC)
!
      end subroutine s_set_control_SPH_MHD_bcs
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_SPH_val_diffusions(model_ctl, MHD_prop)
!
      use t_ctl_data_MHD_model
      use t_bc_data_list
!
      use t_ctl_param_val_density
      use t_ctl_param_val_diffusion
!
      type(mhd_model_control), intent(in) :: model_ctl
      type(MHD_evolution_param), intent(inout) :: MHD_prop
!
!   Set polytrope
      call set_valuable_density_ctl                                     &
     &   (my_rank, model_ctl%polytrope_c, MHD_prop%polytrope_param,     &
     &    MHD_prop%flag_ref_density_valiation)
!
!   Set valuable diffusivities
      call set_valuable_diffusion_ctl                                   &
     &   (my_rank, model_ctl%val_viscous_c,                             &
     &    MHD_prop%val_viscous_param, MHD_prop%flag_viscous_variation)
      call set_valuable_diffusion_ctl                                   &
     &   (my_rank, model_ctl%val_mag_diffuse_c,                         &
     &    MHD_prop%val_mag_diffuse_param,                               &
     &    MHD_prop%flag_mag_diffuse_variation)
      call set_valuable_diffusion_ctl                                   &
     &   (my_rank, model_ctl%reft_ctl%valuable_diffusion_ctl,           &
     &    MHD_prop%val_thermal_diffuse_param,                           &
     &    MHD_prop%flag_term_diffuse_variation)
      call set_valuable_diffusion_ctl                                   &
     &   (my_rank, model_ctl%refc_ctl%valuable_diffusion_ctl,           &
     &    MHD_prop%val_comp_diffuse_param,                              &
     &    MHD_prop%flag_comp_diffuse_variation)
!
      if(iflag_debug .le. 0) return
      call check_polytrope_parameters(MHD_prop%polytrope_param)
!
      call check_val_diffuse_parameters(MHD_prop%val_viscous_param)
      call check_val_diffuse_parameters(MHD_prop%val_mag_diffuse_param)
      call check_val_diffuse_parameters                                 &
     &   (MHD_prop%val_thermal_diffuse_param)
      call check_val_diffuse_parameters                                 &
     &   (MHD_prop%val_comp_diffuse_param)
!
      end subroutine set_ctl_SPH_val_diffusions
!
! ----------------------------------------------------------------------
!
      end module set_control_SPH_MHD_bcs
