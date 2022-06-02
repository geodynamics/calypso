!>@file   cal_sph_field_by_rotation.f90
!!@brief  module cal_sph_field_by_rotation
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate curl or divergence of forces
!!
!!@verbatim
!!      subroutine rot_momentum_eq_exp_sph                              &
!!     &         (sph_rj, r_2nd, sph_MHD_bc, leg,                       &
!!     &          ipol_frc, ipol_rot_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(base_force_address), intent(in) :: ipol_rot_frc
!!        type(base_force_address), intent(in) :: ipol_div_frc
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine s_cal_mag_induct_by_sym_rj                           &
!!       &         (sph_rj, r_2nd, sph_MHD_bc, leg, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine cal_rot_of_forces_sph_2(sph_rj, r_2nd,               &
!!     &          g_sph_rj, sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,     &
!!     &          ipol_frc, ipol_rot_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(base_force_address), intent(in) :: ipol_rot_frc
!!        type(sph_boundary_type), intent(in)  :: sph_bc_U
!!        type(fdm2_free_slip), intent(in) :: fdm2_free_ICB
!!        type(fdm2_free_slip), intent(in) :: fdm2_free_CMB
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine cal_rot_of_induction_sph                             &
!!     &         (sph_rj, r_2nd, g_sph_rj, sph_bc_B, ipol_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine cal_div_of_fluxes_sph(sph_rj, r_2nd, g_sph_rj,       &
!!     &          sph_bc_T, bcs_T, sph_bc_C, bcs_C, fdm2_center,        &
!!     &          ipol_frc, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(base_force_address), intent(in) :: ipol_frc
!!        type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T, bcs_C
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module cal_sph_field_by_rotation
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
      subroutine rot_momentum_eq_exp_sph                                &
     &         (sph_rj, r_2nd, sph_MHD_bc, leg,                         &
     &          ipol_frc, ipol_rot_frc, rj_fld)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(base_force_address), intent(in) :: ipol_frc
      type(base_force_address), intent(in) :: ipol_rot_frc
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_rot_of_forces_sph_2'
      call cal_rot_of_forces_sph_2                                      &
     &   (sph_rj, r_2nd, leg%g_sph_rj, sph_MHD_bc%sph_bc_U,             &
     &    sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,           &
     &    ipol_frc, ipol_rot_frc, rj_fld)
!
      call cal_rot_of_induction_sph                                     &
     &   (sph_rj, r_2nd, leg%g_sph_rj, sph_MHD_bc%sph_bc_B,             &
     &    ipol_frc, rj_fld)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_div_of_fluxes_sph'
      call cal_div_of_fluxes_sph(sph_rj, r_2nd, leg%g_sph_rj,           &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%bcs_T,                        &
     &    sph_MHD_bc%sph_bc_C, sph_MHD_bc%bcs_C,                        &
     &    sph_MHD_bc%fdm2_center, ipol_frc, rj_fld)
!
      end subroutine rot_momentum_eq_exp_sph
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_mag_induct_by_sym_rj                             &
     &         (sph_rj, r_2nd, sph_MHD_bc, leg, ipol, rj_fld)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_rot_of_induction_sph                                     &
     &   (sph_rj, r_2nd, leg%g_sph_rj, sph_MHD_bc%sph_bc_B,             &
     &    ipol%forces_by_sym_sym, rj_fld)
      call cal_rot_of_induction_sph                                     &
     &   (sph_rj, r_2nd, leg%g_sph_rj, sph_MHD_bc%sph_bc_B,             &
     &    ipol%forces_by_asym_asym, rj_fld)
      call cal_rot_of_induction_sph                                     &
     &   (sph_rj, r_2nd, leg%g_sph_rj, sph_MHD_bc%sph_bc_B,             &
     &    ipol%forces_by_sym_asym, rj_fld)
      call cal_rot_of_induction_sph                                     &
     &   (sph_rj, r_2nd, leg%g_sph_rj, sph_MHD_bc%sph_bc_B,             &
     &    ipol%forces_by_asym_sym, rj_fld)
!
      end subroutine s_cal_mag_induct_by_sym_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_rot_of_forces_sph_2(sph_rj, r_2nd,                 &
     &          g_sph_rj, sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,       &
     &          ipol_frc, ipol_rot_frc, rj_fld)
!
      use const_sph_radial_grad
      use const_sph_rotation
      use cal_inner_core_rotation
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(base_force_address), intent(in) :: ipol_frc
      type(base_force_address), intent(in) :: ipol_rot_frc
      type(sph_boundary_type), intent(in)  :: sph_bc_U
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB
      type(fdm2_free_slip), intent(in) :: fdm2_free_CMB
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol_rot_frc%i_m_advect .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take rotation of advection'
        call const_sph_force_rot2(sph_rj, r_2nd,                        &
     &      sph_bc_U, fdm2_free_ICB, fdm2_free_CMB, g_sph_rj,           &
     &      ipol_frc%i_m_advect, ipol_rot_frc%i_m_advect, rj_fld)
      end if
!
      if(ipol_rot_frc%i_lorentz .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take rotation of Lorentz'
        call const_sph_force_rot2(sph_rj, r_2nd,                        &
     &      sph_bc_U, fdm2_free_ICB, fdm2_free_CMB, g_sph_rj,           &
     &      ipol_frc%i_lorentz, ipol_rot_frc%i_lorentz, rj_fld)
!
        if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
          call int_icore_toroidal_lorentz                               &
     &       (sph_bc_U%kr_in, sph_rj, ipol_frc, ipol_rot_frc, rj_fld)
        end if
      end if
!
      end subroutine cal_rot_of_forces_sph_2
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rot_of_induction_sph                               &
     &         (sph_rj, r_2nd, g_sph_rj, sph_bc_B, ipol_frc, rj_fld)
!
      use const_sph_radial_grad
      use const_sph_rotation
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(base_force_address), intent(in) :: ipol_frc
      type(sph_boundary_type), intent(in) :: sph_bc_B
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol_frc%i_induction .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'obtain magnetic induction'
        call const_sph_rotation_uxb(sph_rj, r_2nd, sph_bc_B, g_sph_rj,  &
     &      ipol_frc%i_vp_induct, ipol_frc%i_induction, rj_fld)
      end if
!
      end subroutine cal_rot_of_induction_sph
!
! ----------------------------------------------------------------------
!
      subroutine cal_div_of_fluxes_sph(sph_rj, r_2nd, g_sph_rj,         &
     &          sph_bc_T, bcs_T, sph_bc_C, bcs_C, fdm2_center,          &
     &          ipol_frc, rj_fld)
!
      use calypso_mpi
      use const_sph_divergence
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(base_force_address), intent(in) :: ipol_frc
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(sph_scalar_boundary_data), intent(in) :: bcs_T, bcs_C
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol_frc%i_h_advect .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take div of heat flux'
        call const_sph_scalar_advect                                    &
     &     (sph_rj, r_2nd, sph_bc_T, bcs_T, fdm2_center, g_sph_rj,      &
     &      ipol_frc%i_h_flux, ipol_frc%i_h_advect, rj_fld)
      end if
!
      if(ipol_frc%i_c_advect .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take div  of composit flux'
        call const_sph_scalar_advect                                    &
     &     (sph_rj, r_2nd, sph_bc_C, bcs_C, fdm2_center, g_sph_rj,      &
     &      ipol_frc%i_c_flux, ipol_frc%i_c_advect, rj_fld)
      end if
!
      end subroutine cal_div_of_fluxes_sph
!
! -----------------------------------------------------------------------
!
      end module cal_sph_field_by_rotation
