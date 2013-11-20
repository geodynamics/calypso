!>@file   cal_sph_field_by_rotation.f90
!!@brief  module cal_sph_field_by_rotation
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate curl or divergence of forces
!!
!!@verbatim
!!      subroutine cal_rot_of_forces_sph_2
!!      subroutine cal_div_of_forces_sph_2
!!      subroutine cal_rot_of_induction_sph
!!      subroutine cal_div_of_fluxes_sph
!!@endverbatim
!
      module cal_sph_field_by_rotation
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_control_parameter
      use m_sph_phys_address
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_rot_of_forces_sph_2
!
      use calypso_mpi
      use m_sph_spectr_data
      use m_boundary_params_sph_MHD
      use const_sph_radial_grad
      use const_sph_rotation
      use cal_inner_core_rotation
!
!
      if( (ipol%i_m_advect*ipol%i_rot_inertia) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take rotation of advection'
        call const_sph_force_rot2                                       &
     &     (sph_bc_U, ipol%i_m_advect, ipol%i_rot_inertia)
      end if
!
      if( (ipol%i_lorentz*ipol%i_rot_Lorentz) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take rotation of Lorentz'
        call const_sph_force_rot2(sph_bc_U,                             &
     &      ipol%i_lorentz, ipol%i_rot_Lorentz)
!
        if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
          call int_icore_toroidal_lorentz(sph_bc_U%kr_in)
        end if
      end if
!
      end subroutine cal_rot_of_forces_sph_2
!
! ----------------------------------------------------------------------
!
      subroutine cal_div_of_forces_sph_2
!
      use m_physical_property
      use m_boundary_params_sph_MHD
!      use sum_div_coriolis_rj_sph
      use cal_div_buoyancies_sph_MHD
      use const_sph_divergence
!
!
      call const_sph_div_force                                          &
     &    (sph_bc_U, ipol%i_m_advect, ipol%i_div_inertia)
!
      if(iflag_4_lorentz .gt. id_turn_OFF) then
        call const_sph_div_force                                        &
     &      (sph_bc_U, ipol%i_lorentz, ipol%i_div_Lorentz)
      end if
!
      if(iflag_4_coriolis .gt. id_turn_OFF) then
        call const_sph_div_force                                        &
     &      (sph_bc_U, ipol%i_coriolis, ipol%i_div_Coriolis)
!        call s_sum_div_coriolis_rj_sph(sph_bc_U%kr_in, sph_bc_U%kr_out,&
!     &      coef_cor, ipol%i_div_Coriolis)
      end if
!
      call sel_div_buoyancies_sph_MHD(sph_bc_U)
!
      end subroutine cal_div_of_forces_sph_2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rot_of_induction_sph
!
      use calypso_mpi
      use m_boundary_params_sph_MHD
      use const_sph_radial_grad
      use const_sph_rotation
      use m_sph_spectr_data
!
!
      if( (ipol%i_vp_induct*ipol%i_induction) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'obtain magnetic induction'
        call const_sph_rotation_uxb(sph_bc_B,                           &
     &      ipol%i_vp_induct, ipol%i_induction)
      end if
!
      end subroutine cal_rot_of_induction_sph
!
! ----------------------------------------------------------------------
!
      subroutine cal_div_of_fluxes_sph
!
      use calypso_mpi
      use m_sph_phys_address
      use m_boundary_params_sph_MHD
      use m_sph_spectr_data
      use const_sph_divergence
!
!
      if( (ipol%i_h_flux*ipol%i_h_advect) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take div of heat flux'
        call const_sph_scalar_advect                                    &
     &     (sph_bc_T, ipol%i_h_flux, ipol%i_h_advect)
      end if
!
      if( (ipol%i_c_flux*ipol%i_c_advect) .gt. 0) then
        if (iflag_debug .gt. 0) write(*,*) 'take div  of composit flux'
        call const_sph_scalar_advect                                    &
     &     (sph_bc_C, ipol%i_c_flux, ipol%i_c_advect)
      end if
!
      end subroutine cal_div_of_fluxes_sph
!
! -----------------------------------------------------------------------
!
      end module cal_sph_field_by_rotation
