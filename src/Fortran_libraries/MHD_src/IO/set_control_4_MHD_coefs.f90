!>@file   set_control_4_MHD_coefs.f90
!!@brief  module set_control_4_MHD_coefs
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2001
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set coeffcients for MHD simulation from control data
!!
!!@verbatim
!!      subroutine set_coefs_4_thermal_eq(heat_ctl,                     &
!!     &          coefs_termal, coefs_t_diffuse, coefs_h_source)
!!        type(heat_equation_control), intent(in) :: heat_ctl
!!        type(powers_4_coefficients), intent(inout) :: coefs_termal
!!        type(powers_4_coefficients), intent(inout) :: coefs_t_diffuse
!!        type(powers_4_coefficients), intent(inout) :: coefs_h_source
!!      subroutine set_coefs_4_momentum_eq(fl_prop, mom_ctl,            &
!!     &          coefs_momentum, coefs_pressure, coefs_v_diffuse,      &
!!     &          coefs_buoyancy, coefs_comp_buo, coefs_Coriolis,       &
!!     &          coefs_Lorentz)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(momentum_equation_control), intent(in) :: mom_ctl
!!        type(powers_4_coefficients), intent(inout) :: coefs_momentum
!!        type(powers_4_coefficients), intent(inout) :: coefs_pressure
!!        type(powers_4_coefficients), intent(inout) :: coefs_v_diffuse
!!        type(powers_4_coefficients), intent(inout) :: coefs_buoyancy
!!        type(powers_4_coefficients), intent(inout) :: coefs_comp_buo
!!        type(powers_4_coefficients), intent(inout) :: coefs_Coriolis
!!        type(powers_4_coefficients), intent(inout) :: coefs_Lorentz
!!      subroutine set_coefs_4_induction_eq(cd_prop, induct_ctl,        &
!!     &          coefs_magnetic, coefs_magne_p, coefs_m_diffuse,       &
!!     &          coefs_induction)
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(induction_equation_control), intent(in) :: induct_ctl
!!        type(powers_4_coefficients), intent(inout) :: coefs_magnetic
!!        type(powers_4_coefficients), intent(inout) :: coefs_magne_p
!!        type(powers_4_coefficients), intent(inout) :: coefs_m_diffuse
!!        type(powers_4_coefficients), intent(inout) :: coefs_induction
!!      subroutine set_coefs_4_composition_eq(comp_ctl,                 &
!!     &          coefs_composition, coefs_c_diffuse, coefs_c_source)
!!        type(heat_equation_control), intent(in) :: comp_ctl
!!        type(powers_4_coefficients), intent(inout) :: coefs_composition
!!        type(powers_4_coefficients), intent(inout) :: coefs_c_diffuse
!!        type(powers_4_coefficients), intent(inout) :: coefs_c_source
!!@endverbatim
!
      module set_control_4_MHD_coefs
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
      use m_error_IDs
!
      use t_physical_property
      use t_powers_4_coefficients
      use t_ctl_data_mhd_normalize
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_thermal_eq(heat_ctl,                       &
     &          coefs_termal, coefs_t_diffuse, coefs_h_source)
!
      use t_ctl_data_termal_norm
!
      type(heat_equation_control), intent(in) :: heat_ctl
      type(powers_4_coefficients), intent(inout) :: coefs_termal
      type(powers_4_coefficients), intent(inout) :: coefs_t_diffuse
      type(powers_4_coefficients), intent(inout) :: coefs_h_source
!
!
      if (heat_ctl%coef_4_adv_flux%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for temperature'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        coefs_termal%num = heat_ctl%coef_4_adv_flux%num
      end if
!
      if (heat_ctl%coef_4_diffuse%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for thermal diffusion'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        coefs_t_diffuse%num = heat_ctl%coef_4_diffuse%num
      end if
!
      if (heat_ctl%coef_4_source%icou .gt. 0) then
        coefs_h_source%num = heat_ctl%coef_4_source%num
      end if
!
      call copy_power_and_names_from_ctl                                &
     &   (heat_ctl%coef_4_adv_flux, coefs_termal)
      call copy_power_and_names_from_ctl                                &
     &   (heat_ctl%coef_4_diffuse, coefs_t_diffuse)
      call copy_power_and_names_from_ctl                                &
     &   (heat_ctl%coef_4_source, coefs_h_source)
!
      end subroutine set_coefs_4_thermal_eq
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_momentum_eq(fl_prop, mom_ctl,              &
     &          coefs_momentum, coefs_pressure, coefs_v_diffuse,        &
     &          coefs_buoyancy, coefs_comp_buo, coefs_Coriolis,         &
     &          coefs_Lorentz)
!
      use t_ctl_data_momentum_norm
      use t_physical_property
!
      type(fluid_property), intent(in) :: fl_prop
      type(momentum_equation_control), intent(in) :: mom_ctl
      type(powers_4_coefficients), intent(inout) :: coefs_momentum
      type(powers_4_coefficients), intent(inout) :: coefs_pressure
      type(powers_4_coefficients), intent(inout) :: coefs_v_diffuse
      type(powers_4_coefficients), intent(inout) :: coefs_buoyancy
      type(powers_4_coefficients), intent(inout) :: coefs_comp_buo
      type(powers_4_coefficients), intent(inout) :: coefs_Coriolis
      type(powers_4_coefficients), intent(inout) :: coefs_Lorentz
!
!
      if (mom_ctl%coef_4_intertia%icou .eq. 0) then
        e_message = 'Set coefficients for time stepping for velocity'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        coefs_momentum%num = mom_ctl%coef_4_intertia%num
      end if
!
      if (mom_ctl%coef_4_grad_p%icou .eq. 0) then
        e_message = 'Set coefficients for pressure gradient'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        coefs_pressure%num = mom_ctl%coef_4_grad_p%num
      end if
!
      if (mom_ctl%coef_4_viscous%icou .eq. 0) then
        e_message = 'Set coefficients for viscosity'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        coefs_v_diffuse%num = mom_ctl%coef_4_viscous%num
      end if
!
      if((fl_prop%iflag_4_gravity .eqv. .FALSE.)                        &
     &     .and. (fl_prop%iflag_4_filter_gravity .eqv. .FALSE.)) then
        coefs_buoyancy%num = 0
      else
        if (mom_ctl%coef_4_termal_buo%icou .eq. 0) then
          e_message = 'Set coefficients for buoyancy'
          call calypso_MPI_abort(ierr_dless, e_message)
        else
          coefs_buoyancy%num = mom_ctl%coef_4_termal_buo%num
        end if
      end if
!
      if((fl_prop%iflag_4_composit_buo .eqv. .FALSE.)                   &
     &     .and. (fl_prop%iflag_4_filter_comp_buo .eqv. .FALSE.)) then
        coefs_comp_buo%num = 0
      else
        if(mom_ctl%coef_4_comp_buo%icou .eq. 0) then
          e_message = 'Set coefficients for compiositional buoyancy'
          call calypso_MPI_abort(ierr_dless, e_message)
        else
          coefs_comp_buo%num = mom_ctl%coef_4_comp_buo%num
        end if
      end if
!
      if (fl_prop%iflag_4_coriolis .eqv. .FALSE.) then
        coefs_Coriolis%num = 0
      else
        if(mom_ctl%coef_4_Coriolis%icou .eq. 0) then
          e_message = 'Set coefficients for Coriolis force'
          call calypso_MPI_abort(ierr_dless, e_message)
        else
          coefs_Coriolis%num = mom_ctl%coef_4_Coriolis%num
        end if
      end if
!
      if (fl_prop%iflag_4_lorentz .eqv. .FALSE.) then
        coefs_Lorentz%num = 0
      else
        if(mom_ctl%coef_4_Lorentz%icou .eq. 0) then
          e_message = 'Set coefficients for Lorentz force'
          call calypso_MPI_abort(ierr_dless, e_message)
        else
          coefs_Lorentz%num = mom_ctl%coef_4_Lorentz%num
        end if
      end if
!
!
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_intertia, coefs_momentum)
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_grad_p, coefs_pressure)
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_viscous, coefs_v_diffuse)
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_termal_buo, coefs_buoyancy)
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_comp_buo, coefs_comp_buo)
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_Coriolis, coefs_Coriolis)
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_Lorentz, coefs_Lorentz)
!
      end subroutine set_coefs_4_momentum_eq
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_induction_eq(cd_prop, induct_ctl,          &
     &          coefs_magnetic, coefs_magne_p, coefs_m_diffuse,         &
     &          coefs_induction)
!
      use t_ctl_data_induct_norm
!
      type(conductive_property), intent(in)  :: cd_prop
      type(induction_equation_control), intent(in) :: induct_ctl
      type(powers_4_coefficients), intent(inout) :: coefs_magnetic
      type(powers_4_coefficients), intent(inout) :: coefs_magne_p
      type(powers_4_coefficients), intent(inout) :: coefs_m_diffuse
      type(powers_4_coefficients), intent(inout) :: coefs_induction
!
!
      if (induct_ctl%coef_4_magne_evo%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for integration for magnetic field'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        coefs_magnetic%num = induct_ctl%coef_4_magne_evo%num
      end if
!
      if (induct_ctl%coef_4_mag_potential%icou .eq. 0                   &
     &     .and. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        e_message =                                                     &
     &     'Set coefficients for integration for magnetic potential'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        coefs_magne_p%num = induct_ctl%coef_4_mag_potential%num
      end if
!
      if (induct_ctl%coef_4_mag_diffuse%icou .eq. 0) then
        e_message = 'Set coefficients for magnetic diffusion'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        coefs_m_diffuse%num = induct_ctl%coef_4_mag_diffuse%num
      end if
!
      if(induct_ctl%coef_4_induction%icou .eq. 0) then
        e_message = 'Set coefficients for induction term'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        coefs_induction%num = induct_ctl%coef_4_induction%num
      end if
!
      call copy_power_and_names_from_ctl                                &
     &   (induct_ctl%coef_4_magne_evo, coefs_magnetic)
      call copy_power_and_names_from_ctl                                &
     &   (induct_ctl%coef_4_mag_potential, coefs_magne_p)
      call copy_power_and_names_from_ctl                                &
     &   (induct_ctl%coef_4_mag_diffuse, coefs_m_diffuse)
      call copy_power_and_names_from_ctl                                &
     &   (induct_ctl%coef_4_induction, coefs_induction)
!
      end subroutine set_coefs_4_induction_eq
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_composition_eq(comp_ctl,                   &
     &          coefs_composition, coefs_c_diffuse, coefs_c_source)
!
      use t_ctl_data_termal_norm
!
      type(heat_equation_control), intent(in) :: comp_ctl
      type(powers_4_coefficients), intent(inout) :: coefs_composition
      type(powers_4_coefficients), intent(inout) :: coefs_c_diffuse
      type(powers_4_coefficients), intent(inout) :: coefs_c_source
!
!
      if (comp_ctl%coef_4_adv_flux%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for composition scalar'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        coefs_composition%num = comp_ctl%coef_4_adv_flux%num
      end if
!
      if (comp_ctl%coef_4_diffuse%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for scalar diffusion'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        coefs_c_diffuse%num = comp_ctl%coef_4_diffuse%num
      end if
!
      if (comp_ctl%coef_4_source%icou .gt. 0) then
        coefs_c_source%num = comp_ctl%coef_4_source%num
      end if
!
      call copy_power_and_names_from_ctl                                &
     &   (comp_ctl%coef_4_adv_flux, coefs_composition)
      call copy_power_and_names_from_ctl                                &
     &   (comp_ctl%coef_4_diffuse, coefs_c_diffuse)
      call copy_power_and_names_from_ctl                                &
     &   (comp_ctl%coef_4_source, coefs_c_source)
!
      end subroutine set_coefs_4_composition_eq
!
! -----------------------------------------------------------------------
!
      end module set_control_4_MHD_coefs
