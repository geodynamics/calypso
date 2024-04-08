!>@file   t_coef_parameters_list.f90
!!@brief  module t_coef_parameters_list
!!
!!@author H. Matsui
!!@date Programmed in 2005
!!@date Modified in Jan., 2007
!
!> @brief set normalizatios for MHD simulation from control data
!!
!!@verbatim
!!      subroutine set_control_4_normalize                              &
!!     &        (fl_prop, cd_prop, ht_prop, cp_prop,                    &
!!     &         dless_ctl, eqs_ctl, MHD_coef_list)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(dimless_control), intent(in) :: dless_ctl
!!        type(equations_control), intent(in) :: eqs_ctl
!!        type(coef_parameters_list), intent(inout) :: MHD_coef_list
!!      subroutine set_coefs_4_magnetic_scale                           &
!!     &         (bscale_ctl, MHD_coef_list)
!!        type(magnetic_field_scale_control), intent(in) :: bscale_ctl
!!        type(coef_parameters_list), intent(inout) :: MHD_coef_list
!!@endverbatim
!!
      module t_coef_parameters_list
!
      use m_precision
      use m_machine_parameter
      use m_error_IDs
!
      use t_physical_property
      use t_ctl_data_mhd_normalize
      use t_ctl_data_dimless_numbers
      use t_powers_4_coefficients
      use t_list_of_dimless_numbers
!
      implicit  none
!
!>       List of parameters to construct coefficients
      type coef_parameters_list
!>        Dimensionless numbers list
        type(list_of_dimless) :: dimless_list
!
!>        Dimensionless numbers for heat flux
        type(powers_4_coefficients) :: coefs_termal
!>        Dimensionless numbers for momentum flux
        type(powers_4_coefficients) :: coefs_momentum
!>        Dimensionless numbers for pressure gradient
        type(powers_4_coefficients) :: coefs_pressure
!>        Dimensionless numbers for heat evolution of magnetic field
        type(powers_4_coefficients) :: coefs_magnetic
!>        Dimensionless numbers for heat electric potential
        type(powers_4_coefficients) :: coefs_magne_p
!>        Dimensionless numbers for heat composition flux
        type(powers_4_coefficients) :: coefs_composition
!
!>        Dimensionless numbers for heat thermal diffusion
        type(powers_4_coefficients) :: coefs_t_diffuse
!>        Dimensionless numbers for heat viscous diffusion
        type(powers_4_coefficients) :: coefs_v_diffuse
!>        Dimensionless numbers for heat magnetic diffusion
        type(powers_4_coefficients) :: coefs_m_diffuse
!>        Dimensionless numbers for heat compositional diffusion
        type(powers_4_coefficients) :: coefs_c_diffuse
!
!>        Dimensionless numbers for heat thermal buoyancy flux
        type(powers_4_coefficients) :: coefs_buoyancy
!>        Dimensionless numbers for heat compositional buoyancy flux
        type(powers_4_coefficients) :: coefs_comp_buo
!>        Dimensionless numbers for heat Coriolis force
        type(powers_4_coefficients) :: coefs_Coriolis
!>        Dimensionless numbers for heat Lorengtz force
        type(powers_4_coefficients) :: coefs_Lorentz
!>        Dimensionless numbers for heat magnetic induction
        type(powers_4_coefficients) :: coefs_induction
!>        Dimensionless numbers for heat heat source
        type(powers_4_coefficients) :: coefs_h_source
!>        Dimensionless numbers for heat compositional source
        type(powers_4_coefficients) :: coefs_c_source
!
!>        Dimensionless numbers for magnetic energy scaling
        type(powers_4_coefficients) :: coefs_me_to_ke
      end type coef_parameters_list
!
      private :: set_dimensionless_numbers
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_normalize                                &
     &        (fl_prop, cd_prop, ht_prop, cp_prop,                      &
     &         dless_ctl, eqs_ctl, MHD_coef_list)
!
      use set_control_4_MHD_coefs
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(dimless_control), intent(in) :: dless_ctl
      type(equations_control), intent(in) :: eqs_ctl
!
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
!
!   set dimensionless numbers
!
      call set_dimensionless_numbers(dless_ctl, MHD_coef_list)
!
!    set normalization for thermal
!
      if (ht_prop%iflag_scheme .eq. id_no_evolution) then
        MHD_coef_list%coefs_termal%num =    0
        MHD_coef_list%coefs_t_diffuse%num = 0
        MHD_coef_list%coefs_h_source%num =  0
      else
        call set_coefs_4_thermal_eq(eqs_ctl%heat_ctl,                   &
     &      MHD_coef_list%coefs_termal, MHD_coef_list%coefs_t_diffuse,  &
     &      MHD_coef_list%coefs_h_source)
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
     &    (fl_prop, eqs_ctl%mom_ctl, MHD_coef_list%coefs_momentum,      &
     &     MHD_coef_list%coefs_pressure, MHD_coef_list%coefs_v_diffuse, &
     &     MHD_coef_list%coefs_buoyancy, MHD_coef_list%coefs_comp_buo,  &
     &     MHD_coef_list%coefs_Coriolis, MHD_coef_list%coefs_Lorentz)
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
        call set_coefs_4_induction_eq(cd_prop, eqs_ctl%induct_ctl,      &
     &      MHD_coef_list%coefs_magnetic, MHD_coef_list%coefs_magne_p,  &
     &      MHD_coef_list%coefs_m_diffuse,                              &
     &      MHD_coef_list%coefs_induction)
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
     &    (eqs_ctl%comp_ctl, MHD_coef_list%coefs_composition,           &
     &     MHD_coef_list%coefs_c_diffuse, MHD_coef_list%coefs_c_source)
      end if
!
      end subroutine set_control_4_normalize
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_magnetic_scale                             &
     &         (bscale_ctl, MHD_coef_list)
!
      use t_ctl_data_mhd_magne
      use t_ctl_data_magnetic_scale
!
      type(magnetic_field_scale_control), intent(in) :: bscale_ctl
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
!
!
      MHD_coef_list%coefs_me_to_ke%num = 0
      if(bscale_ctl%mag_to_kin_energy_ctl%icou .gt. 0) then
        MHD_coef_list%coefs_me_to_ke%num                                &
     &            = bscale_ctl%mag_to_kin_energy_ctl%num
      end if
!
      call copy_power_and_names_from_ctl                                &
     &   (bscale_ctl%mag_to_kin_energy_ctl,                             &
     &    MHD_coef_list%coefs_me_to_ke)
!
      end subroutine set_coefs_4_magnetic_scale
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_dimensionless_numbers(dless_ctl, MHD_coef_list)
!
      use calypso_mpi
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
      end module t_coef_parameters_list
