!>@file  m_phys_labels.f90
!!       module m_phys_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields
!!
!!@verbatim
!! !!!!!  physical values!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   velocity:     velocity    u
!!   temperature:  temperature Tfilter
!!   pressure:     pressure    P
!!   density:      density     \rho
!!   vorticity:    vorticity   \omega = \nabra \times v
!!   vector_potential:  vector potential \nabla \times A = B
!!   magnetic_field:    magnetic field   B
!!   current_density:   current density  J = \nabla \times B
!!   magnetic_potential: potential       \phi
!!   composition:        Composition anormally C
!!   entropy:            Entropy               S
!!
!!   div_velocity: divergence of velocity          div u
!!   div_magnetic: divergence of magnetic field    div B
!!   div_vector_potential: divergence of vector potential  div A
!!
!!   heat_source:            heat source              q_{T}
!!   composition_source:     compositoin source       q_{T}
!!   entropy_source:         entropy source           q_{S}
!!
!!   electric_field:    electric field   E
!!   poynting_flux:     Poynting flux    S = E \times B
!!
!!   reference_temperature:   T_0
!!   reference_density:       \rho_0
!!   reference_entropy:       S_0
!!
!!   perturbation_temp:         \Theta = T - T_0
!!   perturbation_density:      \rho - \rho_0
!!   perturbation_composition:  C - C_0
!!   perturbation_entropy:      S - S_0
!!
!!   filter_velocity, filter_vorticity
!!   filter_temperature, filter_pert_temperature, filter_composition
!!   filter_vector_potential, filter_magne, filter_current
!!
!!   truncated_magnetic_field
!!
!!   kinetic_helicity, magnetic_helicity
!!   current_helicity, cross_helicity
!!
!!   buoyancy_flux, Lorentz_work, mag_tension_work,
!!   composite_buoyancy_flux, filtered_buoyancy_flux
!!   magnetic_ene_generation, work_against_Lorentz
!!   temp_generation, pert_temp_generation
!!   viscous_ene_diffusion, magnetic_ene_diffusion
!!
!!   thermal_diffusion, viscous_diffusion, vorticity_diffusion
!!   vector_potential_diffusion, magnetic_diffusion, composition_diffusion
!!   magnetic_tension, Lorentz_force, pressure_gradient
!!   Coriolis_force, buoyancy, composite_buoyancy, filtered_buoyancy
!!
!!   div_inertia, div_Lorentz_force, div_Coriolis_force
!!
!!   heat_flux,      pert_heat_flux
!!   composite_flux, pert_comp_flux
!!   momentum_flux, maxwell_tensor
!!   magnetic_induction, vecp_induction
!!   magnetic_stretch
!!
!!   heat_advect, pert_heat_advect
!!   inertia,  
!!   div_heat_flux, div_pert_heat_flux
!!   div_momentum_flux, div_maxwell_tensor
!!
!!   induction_tensor, div_induction_tensor
!!
!!   SGS_heat_flux, SGS_composit_flux
!!   SGS_momentum_flux, SGS_maxwell_tensor
!!   SGS_buoyancy,      SGS_composit_buoyancy
!!   SGS_induct_tensor, SGS_vecp_induction, SGS_inertia
!!
!!   rot_SGS_inertia, div_SGS_inertia
!!   rot_SGS_Lorentz, div_SGS_Lorentz
!!
!!   div_SGS_h_flux, div_SGS_m_flux, div_SGS_c_flux
!!   SGS_Lorentz
!!   SGS_induction
!!   temp_4_SGS, comp_4_SGS
!!
!!
!!   SGS_Lorentz_work      Reynolds_work
!!   SGS_temp_flux_gen     SGS_mag_induction_flux
!!   SGS_buoyancy_flux     SGS_comp_buoyancy_flux
!!
!!   rest_of_geostrophic
!!   heat_flux_w_SGS, comp_flux_w_SGS
!!   intertia_w_SGS, Lorentz_w_SGS
!!   momentum_flux_w_SGS, maxwell_tensor_w_SGS
!!   induction_w_SGS, vecp_induction_w_SGS
!!
!! termes for direct estimation
!!   SGS_div_h_flux_true
!!   SGS_div_m_flux_true, SGS_Lorentz_true, SGS_mag_induction_true
!!
!!   SGS_Lorentz_work_true   Reynolds_work_true
!!   SGS_temp_flux_gen_true  SGS_mag_induction_flux_true
!!
!!   velocity_scale   temperature_scale
!!   magnetic_scale   composition_scale
!!
!!
!!   viscosity  kinetic_viscosity
!!   magnetic_diffusivity
!!   thermal_conductivity thermal_diffusivity
!!   chemical_diffusivity
!!
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    name of terms
!!        heat flux:         heat
!!        advection:         inertia
!!        Lorentz force:     Lorentz
!!        Coriolis force:    Coriolis
!!        induction:         induction
!!        composition flux:  comp_flux
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_phys_labels
!
      use m_precision
!      use t_base_field_labels
!      use t_diffusion_term_labels
!      use t_base_force_labels
!      use t_energy_flux_labels
!      use t_grad_field_labels
!      use t_explicit_term_labels
!      use t_field_product_labels
!      use t_diff_vector_labels
!
!      use m_rot_force_labels
!      use m_div_force_labels
!
      implicit none
!
!>        Field label for time step
      character(len=kchara), parameter :: fhd_t_step = 't_step'
!>        Field label for time
!!         @f$ t @f$
      character(len=kchara), parameter :: fhd_time =   'time'
!
      end module m_phys_labels
