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
!!     subroutine s_set_control_4_normalize
!!@endverbatim
!
      module set_control_4_normalize
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
      private :: set_dimensionless_numbers
      private :: set_coefs_4_thermal_eq, set_coefs_4_momentum_eq
      private :: set_coefs_4_induction_eq, set_coefs_4_composition_eq
      private :: copy_coef_and_names_from_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_normalize
!
      use m_control_parameter
      use m_normalize_parameter
!
      integer (kind = kint) :: i
!
!
!   set dimensionless numbers
!
      call set_dimensionless_numbers
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'num_dimless ',num_dimless
        do i = 1, num_dimless
          write(*,*) i, trim(name_dimless(i)), ': ', dimless(i)
        end do
      end if
!
!    set normalization for thermal
!
      if (iflag_t_evo_4_temp .eq. id_no_evolution) then
        num_coef_4_termal =    0
        num_coef_4_t_diffuse = 0
        num_coef_4_h_source =  0
      else
        call set_coefs_4_thermal_eq
      end if
!
!    set coefficients for momentum equation
!
      if (iflag_t_evo_4_velo .eq. id_no_evolution) then
        num_coef_4_velocity =  0
        num_coef_4_press =     0
        num_coef_4_v_diffuse = 0
        num_coef_4_buoyancy =  0
        num_coef_4_Coriolis =  0
        num_coef_4_Lorentz =   0
      else
        call set_coefs_4_momentum_eq
      end if
!
!
!    coefficients for inducition equation
!
      if (iflag_t_evo_4_magne .eq. id_no_evolution                      &
     &  .and. iflag_t_evo_4_vect_p .eq. id_no_evolution) then
        num_coef_4_magnetic =  0
        num_coef_4_mag_p =     0
        num_coef_4_m_diffuse = 0
        num_coef_4_induction = 0
      else
        call set_coefs_4_induction_eq
      end if
!
!    set normalization for composition
!
      if (iflag_t_evo_4_composit .eq. id_no_evolution) then
        num_coef_4_composition =  0
        num_coef_4_c_diffuse =    0
        num_coef_4_c_source =     0
      else
        call set_coefs_4_composition_eq
      end if
!
      end subroutine s_set_control_4_normalize
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_dimensionless_numbers
!
      use m_control_parameter
      use m_ctl_data_mhd_normalize
!
!
      if (coef_4_dimless_ctl%icou .eq. 0) then
          e_message =                                                   &
     &     'Set dimensionless numbers'
          call calypso_MPI_abort(90, e_message)
      else
        num_dimless = coef_4_dimless_ctl%num
      end if
!
      call allocate_dimensionless_nums
      call copy_coef_and_names_from_ctl(coef_4_dimless_ctl,             &
     &    num_dimless, name_dimless, dimless)
      call deallocate_dimless_ctl
!
      end subroutine set_dimensionless_numbers
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_thermal_eq
!
      use m_control_parameter
      use m_normalize_parameter
      use m_ctl_data_termal_norm
!
!
      if (coef_4_heat_flux_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for temperature'
        call calypso_MPI_abort(90, e_message)
      else
        num_coef_4_termal = coef_4_heat_flux_ctl%num
      end if
!
      if (coef_4_t_diffuse_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for thermal diffusion'
        call calypso_MPI_abort(90, e_message)
      else
        num_coef_4_t_diffuse = coef_4_t_diffuse_ctl%num
      end if
!
      if (coef_4_heat_src_ctl%icou .eq. 0) then
        num_coef_4_h_source = coef_4_heat_src_ctl%num
      end if
!
      call allocate_coef_4_termal
      call copy_coef_and_names_from_ctl(coef_4_heat_flux_ctl,           &
     &    num_coef_4_termal, coef_4_termal_name, coef_4_termal_power)
      call deallocate_coef_4_termal_ctl
!
      call allocate_coef_4_t_diffuse
      call copy_coef_and_names_from_ctl(coef_4_t_diffuse_ctl,           &
     &    num_coef_4_t_diffuse, coef_4_t_diffuse_name,                  &
     &    coef_4_t_diffuse_power)
      call deallocate_coef_4_t_diffuse_ctl
!
      call allocate_coef_4_h_source
      call copy_coef_and_names_from_ctl(coef_4_heat_src_ctl,            &
     &    num_coef_4_h_source, coef_4_h_source_name,                    &
     &    coef_4_h_source_power)
      call deallocate_coef_4_h_source_ctl
!
      end subroutine set_coefs_4_thermal_eq
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_momentum_eq
!
      use m_control_parameter
      use m_normalize_parameter
      use m_ctl_data_momentum_norm
!
!
      if (coef_4_intertia_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for velocity'
        call calypso_MPI_abort(90, e_message)
      else
        num_coef_4_velocity = coef_4_intertia_ctl%num
      end if
!
      if (coef_4_grad_p_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for pressure gradient'
        call calypso_MPI_abort(90, e_message)
      else
        num_coef_4_press = coef_4_grad_p_ctl%num
      end if
!
      if (coef_4_viscous_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for viscosity'
        call calypso_MPI_abort(90, e_message)
      else
        num_coef_4_v_diffuse = coef_4_viscous_ctl%num
      end if
!
      if(iflag_4_gravity .eq. id_turn_OFF                               &
     &      .and. iflag_4_filter_gravity .eq. id_turn_OFF) then
        num_coef_4_buoyancy = 0
      else
        if (coef_4_termal_buo_ctl%icou .eq. 0) then
          e_message = 'Set coefficients for buoyancy'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_buoyancy = coef_4_termal_buo_ctl%num
        end if
      end if
!
      if (iflag_4_composit_buo .eq. id_turn_OFF) then
        num_coef_4_comp_buo = 0
      else
        if (coef_4_comp_buo_ctl%icou .eq. 0) then
          e_message = 'Set coefficients for compiositional buoyancy'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_comp_buo = coef_4_comp_buo_ctl%num
        end if
      end if
!
      if (iflag_4_coriolis .eq. id_turn_OFF) then
        num_coef_4_Coriolis = 0
      else
        if (coef_4_Coriolis_ctl%icou .eq. 0) then
          e_message = 'Set coefficients for Coriolis force'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_Coriolis = coef_4_Coriolis_ctl%num
        end if
      end if
!
      if (iflag_4_lorentz .eq. id_turn_OFF) then
        num_coef_4_Lorentz = 0
      else
        if (coef_4_Loreantz_ctl%icou .eq. 0) then
          e_message = 'Set coefficients for Lorentz force'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_Lorentz = coef_4_Loreantz_ctl%num
        end if
      end if
!
!
      call allocate_coef_4_velocity
      call copy_coef_and_names_from_ctl(coef_4_intertia_ctl,            &
     &    num_coef_4_velocity, coef_4_velocity_name,                    &
     &    coef_4_velocity_power)
      call deallocate_coef_4_velocity_ctl
!
      call allocate_coef_4_press
      call copy_coef_and_names_from_ctl(coef_4_grad_p_ctl,              &
     &    num_coef_4_press, coef_4_press_name, coef_4_press_power)
      call deallocate_coef_4_press_ctl
!
      call allocate_coef_4_v_diffuse
      call copy_coef_and_names_from_ctl(coef_4_viscous_ctl,             &
     &    num_coef_4_v_diffuse, coef_4_v_diffuse_name,                  &
     &    coef_4_v_diffuse_power)
      call deallocate_coef_4_v_diffuse_ctl
!
      call allocate_coef_4_buoyancy
      call copy_coef_and_names_from_ctl(coef_4_termal_buo_ctl,          &
     &    num_coef_4_buoyancy, coef_4_buoyancy_name,                    &
     &    coef_4_buoyancy_power)
      call deallocate_coef_4_buoyancy_ctl
!
      call allocate_coef_4_comp_buo
      call copy_coef_and_names_from_ctl(coef_4_comp_buo_ctl,            &
     &    num_coef_4_comp_buo, coef_4_comp_buo_name,                    &
     &    coef_4_comp_buo_power)
      call deallocate_coef_4_comp_buo_ctl
!
      call allocate_coef_4_coriolis
      call copy_coef_and_names_from_ctl(coef_4_Coriolis_ctl,            &
     &    num_coef_4_Coriolis, coef_4_Coriolis_name,                    &
     &    coef_4_Coriolis_power)
      call deallocate_coef_4_coriolis_ctl
!
      call allocate_coef_4_lorentz
      call copy_coef_and_names_from_ctl(coef_4_Loreantz_ctl,            &
     &    num_coef_4_Lorentz, coef_4_Lorentz_name,                      &
     &    coef_4_Lorentz_power)
      call deallocate_coef_4_lorentz_ctl
!
      end subroutine set_coefs_4_momentum_eq
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_induction_eq
!
      use m_control_parameter
      use m_normalize_parameter
      use m_ctl_data_induct_norm
!
!
      if (coef_4_magne_evo_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for integration for magnetic field'
        call calypso_MPI_abort(90, e_message)
      else
        num_coef_4_magnetic = coef_4_magne_evo_ctl%num
      end if
!
      if (coef_4_mag_potential_ctl%icou .eq. 0                          &
     &       .and. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        e_message =                                                     &
     &     'Set coefficients for integration for magnetic potential'
        call calypso_MPI_abort(90, e_message)
      else
        num_coef_4_mag_p = coef_4_mag_potential_ctl%num
      end if
!
      if (coef_4_mag_diffuse_ctl%icou .eq. 0) then
        e_message = 'Set coefficients for magnetic diffusion'
        call calypso_MPI_abort(90, e_message)
      else
        num_coef_4_m_diffuse = coef_4_mag_diffuse_ctl%num
      end if
!
      if (coef_4_induction_ctl%icou .eq. 0) then
        e_message = 'Set coefficients for induction term'
        call calypso_MPI_abort(90, e_message)
      else
        num_coef_4_induction = coef_4_induction_ctl%num
      end if
!
      call allocate_coef_4_magne
      call copy_coef_and_names_from_ctl(coef_4_magne_evo_ctl,           &
     &    num_coef_4_magnetic, coef_4_magnetic_name,                    &
     &    coef_4_magnetic_power)
      call deallocate_coef_4_magne_ctl
!
      call allocate_coef_4_mag_p
      call copy_coef_and_names_from_ctl(coef_4_mag_potential_ctl,       &
     &    num_coef_4_mag_p, coef_4_mag_p_name, coef_4_mag_p_power)
      call deallocate_coef_4_mag_p_ctl
!
      call allocate_coef_4_m_diffuse
      call copy_coef_and_names_from_ctl(coef_4_mag_diffuse_ctl,         &
     &    num_coef_4_m_diffuse, coef_4_m_diffuse_name,                  &
     &    coef_4_m_diffuse_power)
      call deallocate_coef_4_m_diffuse_ctl
!
      call allocate_coef_4_induction
      call copy_coef_and_names_from_ctl(coef_4_induction_ctl,           &
     &    num_coef_4_induction, coef_4_induction_name,                  &
     &    coef_4_induction_power)
      call deallocate_coef_4_induction_ctl
!
      end subroutine set_coefs_4_induction_eq
!
! -----------------------------------------------------------------------
!
      subroutine set_coefs_4_composition_eq
!
      use m_normalize_parameter
      use m_ctl_data_composite_norm
!
!
      if (coef_4_comp_flux_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for composition scalar'
        call calypso_MPI_abort(90, e_message)
      else
        num_coef_4_composition = coef_4_comp_flux_ctl%num
      end if
!
      if (coef_4_c_diffuse_ctl%icou .eq. 0) then
        e_message =                                                     &
     &     'Set coefficients for time stepping for scalar diffusion'
        call calypso_MPI_abort(90, e_message)
      else
        num_coef_4_c_diffuse = coef_4_c_diffuse_ctl%num
      end if
!
      if (coef_4_comp_src_ctl%icou .gt. 0) then
        num_coef_4_c_source = coef_4_comp_src_ctl%num
      end if
!
      call allocate_coef_4_composition
      call copy_coef_and_names_from_ctl(coef_4_comp_flux_ctl,           &
     &    num_coef_4_composition, coef_4_composit_name,                 &
     &    coef_4_composit_power)
      call deallocate_coef_4_dscalar_ctl
!
      call allocate_coef_4_c_diffuse
      call copy_coef_and_names_from_ctl(coef_4_c_diffuse_ctl,           &
     &    num_coef_4_c_diffuse, coef_4_c_diffuse_name,                  &
     &    coef_4_c_diffuse_power)
      call deallocate_coef_4_dsc_diff_ctl
!
      call allocate_coef_4_c_source
      call copy_coef_and_names_from_ctl(coef_4_comp_src_ctl,            &
     &    num_coef_4_c_source, coef_4_c_source_name,                    &
     &    coef_4_c_source_power)
      call deallocate_coef_4_dsc_src_ctl
!
      end subroutine set_coefs_4_composition_eq
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_coef_and_names_from_ctl(coefs_ctl, num_coefs,     &
     &         coef_name, value)
!
      use t_read_control_arrays
!
      type(ctl_array_cr), intent(inout) :: coefs_ctl
      integer(kind = kint), intent(in) :: num_coefs
      character(len = kchara), intent(inout) :: coef_name(num_coefs)
      real(kind = kreal), intent(inout) :: value(num_coefs)
!
!
      if (num_coefs .le. 0) return
!
      coef_name(1:num_coefs) =  coefs_ctl%c_tbl(1:num_coefs)
      value(1:num_coefs) = coefs_ctl%vect(1:num_coefs)
!
      end subroutine copy_coef_and_names_from_ctl
!
! -----------------------------------------------------------------------
!
      end module set_control_4_normalize
