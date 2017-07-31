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
!!     &         (sph_rj, r_2nd, sph_MHD_bc, leg, ipol, itor, rj_fld)
!!      subroutine cal_div_of_forces_sph_2(sph_rj, r_2nd,               &
!!     &          MHD_prop, sph_MHD_bc, g_sph_rj, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol, itor
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
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_MHD_boundaries
!
      implicit none
!
      private :: cal_rot_of_forces_sph_2
      private :: cal_rot_of_induction_sph, cal_div_of_fluxes_sph
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine rot_momentum_eq_exp_sph                                &
     &         (sph_rj, r_2nd, sph_MHD_bc, leg, ipol, itor, rj_fld)
!
      use calypso_mpi
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_rot_of_forces_sph_2'
      call cal_rot_of_forces_sph_2                                      &
     &   (sph_rj, r_2nd, leg%g_sph_rj, sph_MHD_bc%sph_bc_U,             &
     &    sph_MHD_bc%fdm2_free_ICB, sph_MHD_bc%fdm2_free_CMB,           &
     &    ipol, itor, rj_fld)
!
      call cal_rot_of_induction_sph(sph_rj, r_2nd, leg%g_sph_rj,        &
     &    sph_MHD_bc%sph_bc_B, ipol, rj_fld)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_div_of_fluxes_sph'
      call cal_div_of_fluxes_sph                                        &
     &   (sph_rj, r_2nd, leg%g_sph_rj, sph_MHD_bc%sph_bc_T,             &
     &    sph_MHD_bc%sph_bc_C, sph_MHD_bc%fdm2_center, ipol, rj_fld)
!
      end subroutine rot_momentum_eq_exp_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_rot_of_forces_sph_2(sph_rj, r_2nd, g_sph_rj,       &
     &          sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,                 &
     &          ipol, itor, rj_fld)
!
      use calypso_mpi
      use const_sph_radial_grad
      use const_sph_rotation
      use cal_inner_core_rotation
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(phys_address), intent(in) :: ipol, itor
      type(sph_boundary_type), intent(in)  :: sph_bc_U
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB, fdm2_free_CMB
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if( (ipol%i_m_advect*ipol%i_rot_inertia) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take rotation of advection'
        call const_sph_force_rot2(sph_rj, r_2nd,                        &
     &      sph_bc_U, fdm2_free_ICB, fdm2_free_CMB, g_sph_rj,           &
     &      ipol%i_m_advect, ipol%i_rot_inertia, rj_fld)
      end if
!
      if( (ipol%i_lorentz*ipol%i_rot_Lorentz) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take rotation of Lorentz'
        call const_sph_force_rot2(sph_rj, r_2nd,                        &
     &      sph_bc_U, fdm2_free_ICB, fdm2_free_CMB, g_sph_rj,           &
     &      ipol%i_lorentz, ipol%i_rot_Lorentz, rj_fld)
!
        if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
          call int_icore_toroidal_lorentz                               &
     &       (sph_bc_U%kr_in, sph_rj, ipol, itor, rj_fld)
        end if
      end if
!
      end subroutine cal_rot_of_forces_sph_2
!
! ----------------------------------------------------------------------
!
      subroutine cal_div_of_forces_sph_2(sph_rj, r_2nd,                 &
     &          MHD_prop, sph_MHD_bc, g_sph_rj, ipol, rj_fld)
!
      use t_control_parameter
      use cal_div_buoyancies_sph_MHD
      use const_sph_divergence
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call const_sph_div_force                                          &
     &   (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,                 &
     &    ipol%i_m_advect, ipol%i_div_inertia, rj_fld)
!
      if(MHD_prop%fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol%i_lorentz, ipol%i_div_Lorentz, rj_fld)
      end if
!
      if(MHD_prop%fl_prop%iflag_4_coriolis .gt. id_turn_OFF) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol%i_coriolis, ipol%i_div_Coriolis, rj_fld)
      end if
!
      if(MHD_prop%fl_prop%iflag_4_gravity .gt. id_turn_OFF) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol%i_buoyancy, ipol%i_div_buoyancy, rj_fld)
      end if
!
      if(MHD_prop%fl_prop%iflag_4_composit_buo .gt. id_turn_OFF) then
        call const_sph_div_force                                        &
     &     (sph_rj, r_2nd, sph_MHD_bc%sph_bc_U, g_sph_rj,               &
     &      ipol%i_comp_buo, ipol%i_div_comp_buo, rj_fld)
      end if
!
!      call sel_div_buoyancies_sph_MHD(sph_rj, ipol,                    &
!     &    MHD_prop%fl_prop, MHD_prop%ref_param_T, MHD_prop%ref_param_C,&
!     &    sph_MHD_bc%sph_bc_U, rj_fld)
!
      end subroutine cal_div_of_forces_sph_2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rot_of_induction_sph                               &
     &         (sph_rj, r_2nd, g_sph_rj, sph_bc_B, ipol, rj_fld)
!
      use calypso_mpi
      use const_sph_radial_grad
      use const_sph_rotation
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(phys_address), intent(in) :: ipol
      type(sph_boundary_type), intent(in) :: sph_bc_B
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if( (ipol%i_vp_induct*ipol%i_induction) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'obtain magnetic induction'
        call const_sph_rotation_uxb(sph_rj, r_2nd, sph_bc_B, g_sph_rj,  &
     &      ipol%i_vp_induct, ipol%i_induction, rj_fld)
      end if
!
      end subroutine cal_rot_of_induction_sph
!
! ----------------------------------------------------------------------
!
      subroutine cal_div_of_fluxes_sph(sph_rj, r_2nd, g_sph_rj,         &
     &          sph_bc_T, sph_bc_C, fdm2_center, ipol, rj_fld)
!
      use calypso_mpi
      use const_sph_divergence
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(phys_address), intent(in) :: ipol
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if( (ipol%i_h_flux*ipol%i_h_advect) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take div of heat flux'
        call const_sph_scalar_advect                                    &
     &     (sph_rj, r_2nd, sph_bc_T, fdm2_center, g_sph_rj,             &
     &      ipol%i_h_flux, ipol%i_h_advect, rj_fld)
      end if
!
      if( (ipol%i_c_flux*ipol%i_c_advect) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take div  of composit flux'
        call const_sph_scalar_advect                                    &
     &     (sph_rj, r_2nd, sph_bc_C, fdm2_center, g_sph_rj,             &
     &      ipol%i_c_flux, ipol%i_c_advect, rj_fld)
      end if
!
      end subroutine cal_div_of_fluxes_sph
!
! -----------------------------------------------------------------------
!
      end module cal_sph_field_by_rotation
