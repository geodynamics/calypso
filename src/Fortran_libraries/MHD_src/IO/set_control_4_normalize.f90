!>@file   set_control_4_normalize.f90
!!@brief  module set_control_4_normalize
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2001
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set normalizatios for MHD simulation from control data
!!
!!@verbatim
!!      subroutine s_set_control_4_normalize                            &
!!     &        (fl_prop, cd_prop, ht_prop, cp_prop,                    &
!!     &         dless_ctl, eqs_ctl, MHD_coef_list)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(dimless_control), intent(in) :: dless_ctl
!!        type(equations_control), intent(in) :: eqs_ctl
!!        type(coef_parameters_list), intent(inout) :: MHD_coef_list
!!@endverbatim
!
      module set_control_4_normalize
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
      use m_error_IDs
!
      use t_physical_property
      use t_normalize_parameter
      use t_ctl_data_mhd_normalize
      use t_normalize_parameter
!
      implicit  none
!
      private :: set_dimensionless_numbers
      private :: set_coefs_4_thermal_eq, set_coefs_4_momentum_eq
      private :: set_coefs_4_induction_eq, set_coefs_4_composition_eq
      private :: copy_dimless_from_ctl, copy_power_and_names_from_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_normalize                              &
     &        (fl_prop, cd_prop, ht_prop, cp_prop,                      &
     &         dless_ctl, eqs_ctl, MHD_coef_list)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(dimless_control), intent(in) :: dless_ctl
      type(equations_control), intent(in) :: eqs_ctl
!
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
!
      integer (kind = kint) :: i
!
!
!   set dimensionless numbers
!
      call set_dimensionless_numbers(dless_ctl, MHD_coef_list)
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'num_dimless ', MHD_coef_list%dimless_list%num
        do i = 1, MHD_coef_list%dimless_list%num
          write(*,*) i, trim(MHD_coef_list%dimless_list%name(i)),       &
     &              ': ', MHD_coef_list%dimless_list%value(i)
        end do
      end if
!
!    set normalization for thermal
!
      if (ht_prop%iflag_scheme .eq. id_no_evolution) then
        MHD_coef_list%coefs_termal%num =    0
        MHD_coef_list%coefs_t_diffuse%num = 0
        MHD_coef_list%coefs_h_source%num =  0
      else
        call set_coefs_4_thermal_eq(eqs_ctl%heat_ctl, MHD_coef_list)
      end if
!
!    set coefficients for momentum equation
!
      if (fl_prop%iflag_scheme .eq. id_no_evolution) then
        MHD_coef_list%coefs_momentum%num =  0
        MHD_coef_list%coefs_pressure%num =  0
        MHD_coef_list%coefs_v_diffuse%num = 0
        MHD_coef_list%coefs_buoyancy%num =  0
        MHD_coef_list%coefs_comp_buo%num =  0
        MHD_coef_list%coefs_Coriolis%num =  0
        MHD_coef_list%coefs_Lorentz%num =   0
      else
        call set_coefs_4_momentum_eq                                    &
     &     (fl_prop, eqs_ctl%mom_ctl, MHD_coef_list)
      end if
!
!
!    coefficients for inducition equation
!
      if     (cd_prop%iflag_Bevo_scheme .eq. id_no_evolution            &
     &  .and. cd_prop%iflag_Aevo_scheme .eq. id_no_evolution) then
        MHD_coef_list%coefs_magnetic%num =  0
        MHD_coef_list%coefs_magne_p%num =   0
        MHD_coef_list%coefs_m_diffuse%num = 0
        MHD_coef_list%coefs_induction%num = 0
      else
        call set_coefs_4_induction_eq                                   &
     &     (cd_prop, eqs_ctl%induct_ctl, MHD_coef_list)
      end if
!
!    set normalization for composition
!
      if (cp_prop%iflag_scheme .eq. id_no_evolution) then
        MHD_coef_list%coefs_composition%num = 0
        MHD_coef_list%coefs_c_diffuse%num =   0
        MHD_coef_list%coefs_c_source%num =    0
      else
        call set_coefs_4_composition_eq                                 &
     &     (eqs_ctl%comp_ctl, MHD_coef_list)
      end if
!
      end subroutine s_set_control_4_normalize
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_dimensionless_numbers(dless_ctl, MHD_coef_list)
!
      type(dimless_control), intent(in) :: dless_ctl
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
!
!
      if (dless_ctl%dimless%icou .eq. 0) then
          e_message =                                                   &
     &     'Set dimensionless numbers'
          call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%dimless_list%num = dless_ctl%dimless%num
      end if
!
      call copy_dimless_from_ctl                                        &
     &   (dless_ctl%dimless, MHD_coef_list%dimless_list)
!
      end subroutine set_dimensionless_numbers
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_thermal_eq(heat_ctl, MHD_coef_list)
!
      use t_ctl_data_termal_norm
!
      type(heat_equation_control), intent(in) :: heat_ctl
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
!
!
      if (heat_ctl%coef_4_adv_flux%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for temperature'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_termal%num = heat_ctl%coef_4_adv_flux%num
      end if
!
      if (heat_ctl%coef_4_diffuse%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for thermal diffusion'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_t_diffuse%num = heat_ctl%coef_4_diffuse%num
      end if
!
      if (heat_ctl%coef_4_source%icou .gt. 0) then
        MHD_coef_list%coefs_h_source%num = heat_ctl%coef_4_source%num
      end if
!
      call copy_power_and_names_from_ctl                                &
     &   (heat_ctl%coef_4_adv_flux, MHD_coef_list%coefs_termal)
      call copy_power_and_names_from_ctl                                &
     &   (heat_ctl%coef_4_diffuse, MHD_coef_list%coefs_t_diffuse)
      call copy_power_and_names_from_ctl                                &
     &   (heat_ctl%coef_4_source, MHD_coef_list%coefs_h_source)
!
      end subroutine set_coefs_4_thermal_eq
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_momentum_eq                                &
     &         (fl_prop, mom_ctl, MHD_coef_list)
!
      use t_ctl_data_momentum_norm
      use t_physical_property
!
      type(fluid_property), intent(in) :: fl_prop
      type(momentum_equation_control), intent(in) :: mom_ctl
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
!
!
      if (mom_ctl%coef_4_intertia%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for velocity'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_momentum%num = mom_ctl%coef_4_intertia%num
      end if
!
      if (mom_ctl%coef_4_grad_p%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for pressure gradient'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_pressure%num = mom_ctl%coef_4_grad_p%num
      end if
!
      if (mom_ctl%coef_4_viscous%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for viscosity'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_v_diffuse%num = mom_ctl%coef_4_viscous%num
      end if
!
      if(fl_prop%iflag_4_gravity .eq. id_turn_OFF                       &
     &      .and. fl_prop%iflag_4_filter_gravity .eq. id_turn_OFF) then
        MHD_coef_list%coefs_buoyancy%num = 0
      else
        if (mom_ctl%coef_4_termal_buo%icou .eq. 0) then
          e_message = 'Set coefficients for buoyancy'
          call calypso_MPI_abort(ierr_dless, e_message)
        else
          MHD_coef_list%coefs_buoyancy%num                              &
     &              = mom_ctl%coef_4_termal_buo%num
        end if
      end if
!
      if (fl_prop%iflag_4_composit_buo .eq. id_turn_OFF) then
        MHD_coef_list%coefs_comp_buo%num = 0
      else
        if(mom_ctl%coef_4_comp_buo%icou .eq. 0) then
          e_message = 'Set coefficients for compiositional buoyancy'
          call calypso_MPI_abort(ierr_dless, e_message)
        else
          MHD_coef_list%coefs_comp_buo%num                              &
     &              = mom_ctl%coef_4_comp_buo%num
        end if
      end if
!
      if (fl_prop%iflag_4_coriolis .eq. id_turn_OFF) then
        MHD_coef_list%coefs_Coriolis%num = 0
      else
        if(mom_ctl%coef_4_Coriolis%icou .eq. 0) then
          e_message = 'Set coefficients for Coriolis force'
          call calypso_MPI_abort(ierr_dless, e_message)
        else
          MHD_coef_list%coefs_Coriolis%num                              &
     &              = mom_ctl%coef_4_Coriolis%num
        end if
      end if
!
      if (fl_prop%iflag_4_lorentz .eq. id_turn_OFF) then
        MHD_coef_list%coefs_Lorentz%num = 0
      else
        if(mom_ctl%coef_4_Lorentz%icou .eq. 0) then
          e_message = 'Set coefficients for Lorentz force'
          call calypso_MPI_abort(ierr_dless, e_message)
        else
          MHD_coef_list%coefs_Lorentz%num = mom_ctl%coef_4_Lorentz%num
        end if
      end if
!
!
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_intertia, MHD_coef_list%coefs_momentum)
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_grad_p, MHD_coef_list%coefs_pressure)
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_viscous, MHD_coef_list%coefs_v_diffuse)
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_termal_buo, MHD_coef_list%coefs_buoyancy)
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_comp_buo, MHD_coef_list%coefs_comp_buo)
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_Coriolis, MHD_coef_list%coefs_Coriolis)
      call copy_power_and_names_from_ctl                                &
     &   (mom_ctl%coef_4_Lorentz, MHD_coef_list%coefs_Lorentz)
!
      end subroutine set_coefs_4_momentum_eq
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_induction_eq                               &
     &         (cd_prop, induct_ctl, MHD_coef_list)
!
      use t_ctl_data_induct_norm
!
      type(conductive_property), intent(in)  :: cd_prop
      type(induction_equation_control), intent(in) :: induct_ctl
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
!
!
      if (induct_ctl%coef_4_magne_evo%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for integration for magnetic field'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_magnetic%num                                &
     &            = induct_ctl%coef_4_magne_evo%num
      end if
!
      if (induct_ctl%coef_4_mag_potential%icou .eq. 0                   &
     &     .and. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        e_message =                                                     &
     &     'Set coefficients for integration for magnetic potential'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_magne_p%num                                 &
     &            = induct_ctl%coef_4_mag_potential%num
      end if
!
      if (induct_ctl%coef_4_mag_diffuse%icou .eq. 0) then
        e_message = 'Set coefficients for magnetic diffusion'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_m_diffuse%num                               &
     &            = induct_ctl%coef_4_mag_diffuse%num
      end if
!
      if(induct_ctl%coef_4_induction%icou .eq. 0) then
        e_message = 'Set coefficients for induction term'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_induction%num                               &
     &            = induct_ctl%coef_4_induction%num
      end if
!
      call copy_power_and_names_from_ctl                                &
     &   (induct_ctl%coef_4_magne_evo, MHD_coef_list%coefs_magnetic)
      call copy_power_and_names_from_ctl                                &
     &   (induct_ctl%coef_4_mag_potential, MHD_coef_list%coefs_magne_p)
      call copy_power_and_names_from_ctl                                &
     &   (induct_ctl%coef_4_mag_diffuse, MHD_coef_list%coefs_m_diffuse)
      call copy_power_and_names_from_ctl                                &
     &   (induct_ctl%coef_4_induction, MHD_coef_list%coefs_induction)
!
      end subroutine set_coefs_4_induction_eq
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_composition_eq(comp_ctl, MHD_coef_list)
!
      use t_ctl_data_termal_norm
!
      type(heat_equation_control), intent(in) :: comp_ctl
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
!
!
      if (comp_ctl%coef_4_adv_flux%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for composition scalar'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_composition%num                             &
     &            = comp_ctl%coef_4_adv_flux%num
      end if
!
      if (comp_ctl%coef_4_diffuse%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for scalar diffusion'
        call calypso_MPI_abort(ierr_dless, e_message)
      else
        MHD_coef_list%coefs_c_diffuse%num = comp_ctl%coef_4_diffuse%num
      end if
!
      if (comp_ctl%coef_4_source%icou .gt. 0) then
        MHD_coef_list%coefs_c_source%num = comp_ctl%coef_4_source%num
      end if
!
      call copy_power_and_names_from_ctl                                &
     &   (comp_ctl%coef_4_adv_flux, MHD_coef_list%coefs_composition)
      call copy_power_and_names_from_ctl                                &
     &   (comp_ctl%coef_4_diffuse, MHD_coef_list%coefs_c_diffuse)
      call copy_power_and_names_from_ctl                                &
     &   (comp_ctl%coef_4_source, MHD_coef_list%coefs_c_source)
!
      end subroutine set_coefs_4_composition_eq
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_dimless_from_ctl(coef_ctl, dimless_list)
!
      use t_control_array_charareal
!
      type(ctl_array_cr), intent(in) :: coef_ctl
      type(list_of_dimless), intent(inout) :: dimless_list
!
!
      call alloc_dimless_list(dimless_list)
      if (dimless_list%num .le. 0) return
!
      dimless_list%name(1:dimless_list%num)                            &
     &             = coef_ctl%c_tbl(1:dimless_list%num)
      dimless_list%value(1:dimless_list%num)                           &
     &             = coef_ctl%vect(1:dimless_list%num)
!
      end subroutine copy_dimless_from_ctl
!
! -----------------------------------------------------------------------
!
      subroutine copy_power_and_names_from_ctl(coef_ctl, coef_list)
!
      use t_control_array_charareal
!
      type(ctl_array_cr), intent(in) :: coef_ctl
      type(powers_4_coefficients), intent(inout) :: coef_list
!
!
      call alloc_coef_power_list(coef_list)
      if (coef_list%num .le. 0) return
!
      coef_list%name(1:coef_list%num) = coef_ctl%c_tbl(1:coef_list%num)
      coef_list%power(1:coef_list%num) = coef_ctl%vect(1:coef_list%num)
!
      end subroutine copy_power_and_names_from_ctl
!
! -----------------------------------------------------------------------
!
      end module set_control_4_normalize
