!>@file   m_base_field_labels.f90
!!        module m_base_field_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels for basic fields
!!
!!@verbatim
!!      logical function check_base_vector(field_name)
!!      logical function check_base_scalar(field_name)
!!
!!      subroutine set_base_field_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!      subroutine time_evolution_list_array(array_c)
!!        type(ctl_array_chara), intent(inout) :: array_c
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   velocity        [i_velo]:     velocity    u
!!   pressure        [i_press]:     pressure    P
!!   vorticity       [i_vort]:     vorticity   \omega = \nabra \times v
!!   system_Rotation [i_omega]:    System rotation   \Omega
!!
!!   vector_potential [i_vecp] :   vector potential \nabla \times A = B
!!   magnetic_field [i_magne]:     magnetic field   B
!!   current_density [i_current]:    current density  J = \nabla \times B
!!   background_B [i_back_B]: background magnetic field 
!!                                                  B_{0}
!!   magnetic_potential [i_mag_p]:   potential       \phi
!!   scalar_potential [i_scalar_p]:  scalar potential   \phi
!!
!!   temperature [i_temp]:  temperature T
!!   composition [i_light]:  Composition anormally C
!!   density [i_density]:      density     \rho
!!   entropy [i_entropy]:      Entropy               S
!!
!!   perturbation_temp [i_per_temp]:         \Theta = T - T_0
!!   perturbation_composition [i_per_light]:  C - C_0
!!   perturbation_density [i_per_density]:      \rho - \rho_0
!!   perturbation_entropy [i_per_entropy]:      S - S_0
!!
!!   heat_source [i_heat_source]:            heat source          q_{T}
!!   composition_source [i_light_source]:     compositoin source  q_{C}
!!   entropy_source [i_entropy_source]:         entropy source    q_{S}
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_base_field_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!>        Field label for velocity
!!         @f$ u_{i} @f$
      type(field_def), parameter :: velocity                            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'velocity',                                &
     &                math = '$ u_{i} $')
!>        Field label for vorticity
!!         @f$ \omega_{i} = e_{ijk} \partial_{j} u_{k} @f$
      type(field_def), parameter :: vorticity                           &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'vorticity',                               &
     &                math = '$ \omega_{i}'                             &
     &                    // ' = e_{ijk} \partial_{j} u_{k} $')
!>        Field label for pressure @f$ p @f$
      type(field_def), parameter :: pressure                            &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'pressure',                                &
     &                math = '$ u_{i} $')
!!
!>        System rotation @f$ \Omega_{i} @f$
      type(field_def), parameter :: system_Rotation                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'system_Rotation',                         &
     &                math = '$ \Omega_{i} $')
!
!>        Field label for magnetic field
!!         @f$ B_{i} @f$
      type(field_def), parameter :: magnetic_field                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'magnetic_field',                          &
     &                math = '$ B_{i} $')
!>        Field label for magnetic vector potential
!!         @f$ B_{i} = e_{ijk} \partial_{j} A_{k} @f$
      type(field_def), parameter :: vector_potential                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'vector_potential',                        &
     &                math = '$ B_{i} = e_{ijk} \partial_{j} A_{k} $')
!>        Field label for current density
!!         @f$ J_{i} = e_{ijk} \partial_{j} B_{k} @f$
      type(field_def), parameter :: current_density                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'current_density',                         &
     &                math = '$ J_{i} = e_{ijk} \partial_{j} B_{k} $')
!>        Field label of backgroupnd  magnetic field
!!         @f$ B_{0} @f$
      type(field_def), parameter :: background_B                        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'background_B',                            &
     &                math = '$ B_{0} $')
!
!>        Field label for magnetic potential
!!         @f$ W @f$
      type(field_def), parameter :: magnetic_potential                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magnetic_potential',                      &
     &                math = '$ W $')
!>        Field label for electric potential
!!         @f$ \varphi @f$
      type(field_def), parameter :: scalar_potential                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'scalar_potential',                        &
     &                math = '$ \varphi $')
!!
!>        Field label for density
!!         @f$ \rho @f$
      type(field_def), parameter :: density                             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'density',                                 &
     &                math = '$ \rho $')
!>        Field label for perturbation of density
!!         @f$  \Theta_{\rho} = \rho - \rho_{0} @f$
      type(field_def), parameter :: perturbation_density                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'perturbation_density',                    &
     &                math = '$ \Theta_{\rho} = \rho - \rho_{0} $')
!
!>        Field label for temperature
!!         @f$ T @f$
      type(field_def), parameter :: temperature                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'temperature',                             &
     &                math = '$ T $')
!>        Field label for perturbation of temperature
!!         @f$ \Theta = T - T_{0} @f$
      type(field_def), parameter :: perturbation_temp                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'perturbation_temp',                       &
     &                math = '$ \Theta = T - T_{0} $')
!
!>        Field label for compostiion variation
!!         @f$ C @f$
      type(field_def), parameter :: composition                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'composition',                             &
     &                math = '$ C $')
!>        Field label for perturbation of composition
!!         @f$  C - C_{0} @f$
      type(field_def), parameter :: perturbation_composition            &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'perturbation_composition',                &
     &                math = '$ \Theta_{C} = C - C_{0} $')
!
!>        Field label for entropy
!!         @f$ S @f$
      type(field_def), parameter :: entropy                             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'entropy',                                 &
     &                math = '$ S $')
!>        Field label for perturbation of entropy
!!         @f$  S - S_{0} @f$
      type(field_def), parameter :: perturbation_entropy                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'perturbation_entropy',                    &
     &                math = '$ \Theta_{S} = S - S_{0} $')
!
!>        Field label for heat source
!!         @f$ q_{T} @f$
      type(field_def), parameter :: heat_source                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'heat_source',                             &
     &                math = '$ q_{T} $')
!>        Field label for composion source
!!         @f$ q_{C} @f$
      type(field_def), parameter :: composition_source                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'composition_source',                      &
     &                math = '$ q_{C} $')
!>        Field label for entropysource
!!         @f$ q_{S} @f$
      type(field_def), parameter :: entropy_source                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'entropy_source',                          &
     &                math = '$ q_{S} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_base_vector(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_base_vector                                                 &
     &   =    (field_name .eq. velocity%name)                           &
     &   .or. (field_name .eq. vorticity%name)                          &
     &   .or. (field_name .eq. magnetic_field%name)                     &
     &   .or. (field_name .eq. vector_potential%name)                   &
     &   .or. (field_name .eq. current_density%name)                    &
     &   .or. (field_name .eq. system_Rotation%name)                    &
     &   .or. (field_name .eq. background_B%name)
!
      end function check_base_vector
!
! ----------------------------------------------------------------------
!
      logical function check_base_scalar(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_base_scalar                                                 &
     &   =    (field_name .eq. pressure%name)                           &
     &   .or. (field_name .eq. magnetic_potential%name)                 &
     &   .or. (field_name .eq. scalar_potential%name)                   &
!
     &   .or. (field_name .eq. density%name)                            &
     &   .or. (field_name .eq. perturbation_density%name)               &
!
     &   .or. (field_name .eq. temperature%name)                        &
     &   .or. (field_name .eq. perturbation_temp%name)                  &
!
     &   .or. (field_name .eq. composition%name)                        &
     &   .or. (field_name .eq. perturbation_composition%name)           &
!
     &   .or. (field_name .eq. entropy%name)                            &
     &   .or. (field_name .eq. perturbation_entropy%name)               &
!
     &   .or. (field_name .eq. heat_source%name)                        &
     &   .or. (field_name .eq. composition_source%name)                 &
     &   .or. (field_name .eq. entropy_source%name)
!
      end function check_base_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_base_field_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(velocity,                 array_c2i)
      call set_field_label_to_ctl(vorticity,                array_c2i)
      call set_field_label_to_ctl(pressure,                 array_c2i)
      call set_field_label_to_ctl(system_Rotation,          array_c2i)
      call set_field_label_to_ctl(magnetic_field,           array_c2i)
      call set_field_label_to_ctl(vector_potential,         array_c2i)
      call set_field_label_to_ctl(current_density,          array_c2i)
      call set_field_label_to_ctl(background_B,             array_c2i)
      call set_field_label_to_ctl(magnetic_potential,       array_c2i)
      call set_field_label_to_ctl(scalar_potential,         array_c2i)
      call set_field_label_to_ctl(temperature,              array_c2i)
      call set_field_label_to_ctl(perturbation_temp,        array_c2i)
      call set_field_label_to_ctl(heat_source,              array_c2i)
      call set_field_label_to_ctl(composition,              array_c2i)
      call set_field_label_to_ctl(perturbation_composition, array_c2i)
      call set_field_label_to_ctl(composition_source,       array_c2i)
      call set_field_label_to_ctl(entropy,                  array_c2i)
      call set_field_label_to_ctl(perturbation_entropy,     array_c2i)
      call set_field_label_to_ctl(entropy_source,           array_c2i)
      call set_field_label_to_ctl(density,                  array_c2i)
      call set_field_label_to_ctl(perturbation_density,     array_c2i)
!
      end subroutine set_base_field_names
!
! ----------------------------------------------------------------------
!
      subroutine time_evolution_list_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(velocity%name, array_c)
      call append_c_to_ctl_array(magnetic_field%name, array_c)
      call append_c_to_ctl_array(temperature%name, array_c)
      call append_c_to_ctl_array(composition%name, array_c)
      call append_c_to_ctl_array(vector_potential%name, array_c)
!
      end subroutine time_evolution_list_array
!
! ----------------------------------------------------------------------
!
      end module m_base_field_labels
