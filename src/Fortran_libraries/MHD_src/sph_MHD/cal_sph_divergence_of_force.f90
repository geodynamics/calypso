!>@file   cal_sph_divergence_of_force.f90
!!@brief  module cal_sph_divergence_of_force
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate curl or divergence of forces
!!
!!@verbatim
!!      subroutine cal_div_of_forces_sph_2                              &
!!     &         (sph_rj, r_2nd, MHD_prop, sph_MHD_bc, g_sph_rj,        &
!!     &          ipol_frc, ipol_div_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(base_force_address), intent(in) :: ipol_rot_frc
!!        type(base_force_address), intent(in) :: ipol_div_frc
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine cal_div_of_buoyancies_sph_2                          &
!!     &         (iflag_4_gravity, iflag_4_composit_buo,                &
!!     &          sph_rj, r_2nd, sph_MHD_bc, g_sph_rj,                  &
!!     &          ipol_frc, ipol_div_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(base_force_address), intent(in) :: ipol_div_frc
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module cal_sph_divergence_of_force
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_spheric_rj_data
      use t_base_force_labels
!      use t_base_field_labels
!      use t_grad_field_labels
      use t_phys_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_MHD_boundaries
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_div_of_forces_sph_2                                &
     &         (sph_rj, r_2nd, MHD_prop, sph_MHD_bc, g_sph_rj,          &
     &          ipol_frc, ipol_div_frc, rj_fld)
!     &          ipol_base, ipol_grad, ipol_frc, ipol_div_frc, rj_fld)
!
      use t_control_parameter
      use const_sph_divergence
!      use div_self_buoyancies_sph
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!      type(base_field_address), intent(in) :: ipol_base
!      type(gradient_field_address), intent(in) :: ipol_grad
      type(base_force_address), intent(in) :: ipol_frc
      type(base_force_address), intent(in) :: ipol_div_frc
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(MHD_prop%fl_prop%iflag_4_inertia) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol_frc%i_m_advect, ipol_div_frc%i_m_advect, rj_fld)
      end if
!
      if(MHD_prop%fl_prop%iflag_4_lorentz) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol_frc%i_lorentz, ipol_div_frc%i_lorentz, rj_fld)
      end if
!
      if(MHD_prop%fl_prop%iflag_4_coriolis) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol_frc%i_coriolis, ipol_div_frc%i_Coriolis, rj_fld)
      end if
!
      call cal_div_of_buoyancies_sph_2                                  &
     &   (MHD_prop%fl_prop%iflag_4_gravity,                             &
     &    MHD_prop%fl_prop%iflag_4_composit_buo,                        &
     &    sph_rj, r_2nd, sph_MHD_bc, g_sph_rj,                          &
     &    ipol_frc, ipol_div_frc, rj_fld)
!
!      call sel_div_buoyancies_sph_MHD                                  &
!     &   (MHD_prop%fl_prop%iflag_4_gravity,                            &
!     &    MHD_prop%fl_prop%iflag_4_composit_buo,                       &
!     &    sph_rj, ipol_base, ipol_grad, ipol_div_frc,                  &
!     &    MHD_prop%fl_prop%coef_buo,  MHD_prop%fl_prop%coef_comp_buo,  &
!     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                  &
!     &    sph_MHD_bc%sph_bc_U, rj_fld)
!
      end subroutine cal_div_of_forces_sph_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_div_of_buoyancies_sph_2                            &
     &         (iflag_4_gravity, iflag_4_composit_buo,                  &
     &          sph_rj, r_2nd, sph_MHD_bc, g_sph_rj,                    &
     &          ipol_frc, ipol_div_frc, rj_fld)
!
      use t_control_parameter
      use const_sph_divergence
!
      logical, intent(in) :: iflag_4_gravity, iflag_4_composit_buo
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(base_force_address), intent(in) :: ipol_frc
      type(base_force_address), intent(in) :: ipol_div_frc
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(iflag_4_gravity) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol_frc%i_buoyancy, ipol_div_frc%i_buoyancy, rj_fld)
      end if
!
      if(iflag_4_composit_buo) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol_frc%i_comp_buo, ipol_div_frc%i_comp_buo, rj_fld)
      end if
!
      end subroutine cal_div_of_buoyancies_sph_2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_divergence_of_force
