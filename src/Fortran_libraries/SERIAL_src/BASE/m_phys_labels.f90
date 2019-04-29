!>@file  m_phys_labels.f90
!!       module m_phys_labels
!!
!! @author H. Matsui
!! @date   Programmed on June, 2005
!
!> @brief Labels of fields
!!
!!@verbatim
!! !!!!!  physical values!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   velocity:     velocity    u
!!   temperature:  temperature T
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
!!   div_vector_p: divergence of vector potential  div A
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
!!   parturbation_composition:  C - C_0
!!   perturbation_entropy:      S - S_0
!!
!!   filter_velo, filter_vorticity
!!   filter_temp, filter_part_temp, filter_composition
!!   filter_vecp, filter_magne, filter_current
!!
!!   div_filter_velo, div_filter_magne, div_filter_vecp
!!
!!   wide_filter_velo, wide_filter_vorticity
!!   wide_filter_temp, wide_filter_composition
!!   wide_filter_grad_temp, wide_filter_grad_composition
!!   wide_filter_vecp, wide_filter_magne, wide_filter_current
!!
!!   kinetic_helicity, magnetic_helicity
!!   current_helicity, cross_helicity
!!
!!   buoyancy_flux, Lorentz_work, mag_tension_work,
!!   composite_buoyancy_flux, filtered_buoyancy_flux
!!   magnetic_ene_generation, work_against_Lorentz
!!   temp_generation, part_temp_gen
!!   vis_ene_diffuse, mag_ene_diffuse
!!
!!   thermal_diffusion, viscous_diffusion, vorticity_diffusion
!!   diffuse_vector_p, magnetic_diffusion, composition_diffusion
!!   magnetic_tension, Lorentz_force, pressure_gradient
!!   Coriolis_force, buoyancy, composite_buoyancy, filtered_buoyancy
!!
!!   div_inertia, div_Lorentz_force, div_Coriolis_force
!!
!!   heat_flux,      part_h_flux
!!   composite_flux, part_c_flux
!!   momentum_flux, maxwell_tensor
!!   magnetic_induction, vecp_induction
!!   magnetic_stretch
!!
!!   heat_advect, part_h_advect
!!   inertia,  
!!   div_h_flux, div_part_h_flux
!!   div_m_flux, div_maxwell_t
!!
!!   induction_tensor, div_induct_t
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
!!   SGS_temp_gen          SGS_m_ene_gen
!!   SGS_buoyancy_flux     SGS_comp_buoyancy_flux
!!
!!   geostrophic_balance
!!   heat_flux_w_SGS, comp_flux_w_SGS
!!   intertia_w_SGS, Lorentz_w_SGS
!!   momentum_flux_w_SGS, maxwell_tensor_w_sgs
!!   induction_w_SGS, vecp_induction_w_SGS
!!
!! termes for direct estimation
!!   SGS_div_h_flux_true
!!   SGS_div_m_flux_true, SGS_Lorentz_true, SGS_mag_induct_true
!!
!!   SGS_Lorentz_work_true   Reynolds_work_true
!!   SGS_temp_gen_true       SGS_m_ene_gen_true
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
!
      implicit none
!
!>        Field label for time step
      character(len=kchara), parameter :: fhd_t_step = 't_step'
!>        Field label for time
!!         @f$ t @f$
      character(len=kchara), parameter :: fhd_time =   'time'
!
!>        Field label for velocity
!!         @f$ u_{i} @f$
      character(len=kchara), parameter :: fhd_velo = 'velocity' 
!>        Field label for vorticity
!!         @f$ \omega_{i} = e_{ijk} \partial_{j} u_{k} @f$
      character(len=kchara), parameter :: fhd_vort = 'vorticity'
!>        Field label for magnetic field
!!         @f$ B_{i} @f$
      character(len=kchara), parameter :: fhd_magne = 'magnetic_field'
!>        Field label for magnetic vector potential
!!         @f$ B_{i} = e_{ijk} \partial_{j} A_{k} @f$
      character(len=kchara), parameter :: fhd_vecp = 'vector_potential'
!>        Field label for current density
!!         @f$ J_{i} = e_{ijk} \partial_{j} B_{k} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_current = 'current_density'
!>        Field label for electric field
!!         @f$ E_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_e_field = 'electric_field'
!>        Field label for poynting flux
!!         @f$  e_{ijk} E_{j} B_{k} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_poynting = 'poynting_flux'
!
!>        Divergence of velocity
!!         @f$ \partial_{i} u_{i} @f$
      character(len=kchara), parameter :: fhd_div_v = 'div_velocity'
!>        Divergence of magnetic field
!!         @f$ \partial_{i} B_{i} @f$
      character(len=kchara), parameter :: fhd_div_b = 'div_magnetic'
!>        Divergence of magnetic vector potential
!!         @f$ \partial_{i} A_{i} @f$
      character(len=kchara), parameter :: fhd_div_a = 'div_vector_p'
!
!>        Field label for filtered velocity
!!         @f$ \bar{u}_{i} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_filter_velo = 'filter_velo'
!>        Field label for filtered velocity
!!         @f$ \bar{\omega}_{i} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_filter_vort = 'filter_vorticity'
!>        Field label for filtered magnetic field
!!         @f$ \bar{B}_{i} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_filter_magne = 'filter_magne'
!>        Field label for filtered magnetic field
!!         @f$ \bar{B}_{i} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_filter_current = 'filter_current'
!>        Field label for filtered vetor potential
!!         @f$ \bar{A}_{i} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_filter_vecp = 'filter_vecp'
!
!>        Field label for filtered velocity
!!         @f$ \partial_{i} \bar{u}_{i} @f$
      character(len=kchara), parameter                                  &
     &                      :: fhd_div_filter_v = 'div_filter_velo'
!>        Field label for filtered magnetic field
!!         @f$ \partial_{i} \bar{B}_{i} @f$
      character(len=kchara), parameter                                  &
     &                      :: fhd_div_filter_b = 'div_filter_magne'
!>        Field label for filtered magnetic vector potential
!!         @f$ \partial_{i} \bar{A}_{i} @f$
      character(len=kchara), parameter                                  &
     &                      :: fhd_div_filter_a = 'div_filter_vecp'
!
!>        Field label for viscous diffusion
!!         @f$ \nu \partial_{j}\partial_{j} u_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_viscous = 'viscous_diffusion'
!>        Field label for diffusion of vorticity
!!         @f$ \nu \partial_{j}\partial_{j} \omega_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_w_viscous = 'vorticity_diffusion'
!>        Field label for diffusion of vetor potential
!!         @f$ -J_{i} = \eta \partial_{j}\partial_{j} A_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_vecp_diffuse = 'diffuse_vector_p'
!>        Field label for magnetic diffusion
!!         @f$ \nu \partial_{j}\partial_{j} B_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_diffuse = 'magnetic_diffusion'
!>        Field label for thermal diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} T @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_thermal_diffusion = 'thermal_diffusion'
!>        Field label for compositional diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} C @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_c_diffuse = 'composition_diffusion'
!
!>        Field label for magnetic tension
!!         @f$ B_{j} \partial_{j} B_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_press_grad = 'pressure_gradient'
!
!>        Field label for pressure gradient
!!         @f$ \partial_{i} p @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_tension = 'magnetic_tension'
!
!>        Field label for heat flux
!!         @f$ u_{i} T @f$
      character(len=kchara), parameter :: fhd_h_flux =  'heat_flux'
!>        Field label for perturbation of heat flux
!!         @f$ u_{i} \Theta @f$
      character(len=kchara), parameter :: fhd_ph_flux = 'part_h_flux'
!>        Field label for compositinoal flux
!!         @f$ u_{i} C @f$
      character(len=kchara), parameter :: fhd_c_flux =  'composite_flux'
!>        Field label for perturbation of composition flux
!!         @f$ u_{i} \Theta_C @f$
      character(len=kchara), parameter :: fhd_pc_flux = 'part_c_flux'
!
!>        Field label for advection for momentum
!!         @f$ u_{j} \partial_{j} u_{i} @f$
      character(len=kchara), parameter :: fhd_inertia = 'inertia'
!>        Field label for divergence of momentum flux
!!         @f$ \partial_{j} \left( u_{i} u_{j} \right) @f$
      character(len=kchara), parameter :: fhd_div_m_flux = 'div_m_flux'
!>        Field label for divergence of Maxwell stress
!!         @f$ \partial_{j} \left( B_{i} B_{j} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_maxwell_t = 'div_maxwell_t'
!>        Field label for divergence of magnetic induction
!!         @f$ \partial_{i} \left(e_{ijk} u_{j} B_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_induct_t =  'div_induct_t'
!>        Field label for magnetic induction
!!         @f$ e_{ijk} \partial_{j}\left(e_{klm}u_{l}B_{m} \right)@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_induct =    'magnetic_induction'
!>        Field label for inductino for vector potential
!!         @f$ e_{ijk} u_{j} B_{k} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_vp_induct =     'vecp_induction'
!>        Field label for magnetic stretch term
!!         @f$ B_{i} \partial_{k} u_{k} \right)@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_stretch =   'magnetic_stretch'
!>        Field label for Lorentz force
!!         @f$ e_{ijk} J_{j} B_{k} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Lorentz =       'Lorentz_force'
!>        Field label for Coriolis force
!!         @f$ -2 e_{ijk} \Omega_{j} u_{k} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Coriolis =      'Coriolis_force'
!>        Field label for buoyancy
!!         @f$ -\alpha_{T} g_{i} T @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_buoyancy =      'buoyancy'
!>        Field label for compositional buoyancy
!!         @f$ -\alpha_{C} g_{i} C @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_comp_buo =      'composite_buoyancy'
!>        Field label for filtered buoyancy
!!         @f$ -\alpha_{C} g_{i} \tilde{T} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_buo =    'filtered_buoyancy'
!
!>        Field label for divergence of SGS momentum flux
!!         @f$ \partial_{i} \left( \overline{u_{i}u_{j}}
!!             - \bar{u}_{i}\bar{u}_{j} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_SGS_m_flux =      'div_SGS_m_flux'
!>        Field label for SGS heat flux
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_h_flux =          'SGS_heat_flux'
!>        Field label for SGS compositional flux
!!         @f$ \overline{u_{i}C} - \bar{u}_{i}\bar{C} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_c_flux =          'SGS_composit_flux'
!>        Field label for divergence of SGS Maxwell tensor
!!         @f$ \partial_{i} \left( \overline{B_{i}B_{j}}
!!              - \bar{B}_{i}\bar{B}_{j} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz =         'SGS_Lorentz'
!>        Field label for divergence of SGS magnetic induction tensor
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_induction =       'SGS_induction'
!>        Field label for SGS induction for vector potential
!!         @f$ e_{ijk}\left(\overline{u_{j}B_{k}}
!!            - \bar{u}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_vp_induct =       'SGS_vecp_induction'
!>        Field label for true divergence of SGS momentum flux
!!         @f$ \partial_{i} \left( \overline{u_{i}u_{j}}
!!            - \bar{u}_{i}\bar{u}_{j} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_m_flux_true = 'SGS_div_m_flux_true'
!>        Field label for true divergence of SGS Maxwell tensor
!!         @f$ \partial_{i} \left( \overline{B_{i}B_{j}}
!!             - \bar{B}_{i}\bar{B}_{j} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz_true =    'SGS_Lorentz_true'
!>        Field label for true divergence
!>            of SGS magnetic induction tensor
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_mag_induct_true = 'SGS_mag_induct_true'
!
!>        Field label for SGS buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_buoyancy =  'SGS_buoyancy'
!>        Field label for SGS compositional buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_comp_buo = 'SGS_composit_buoyancy'
!
!
!>        Field label for model coefficient of SGS heat flux
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_h_flux = 'Csim_SGS_heat_flux'
!>        Field label for model coefficient of SGS composition flux
!!         @f$ \overline{u_{i}C} - \bar{u}_{i}\bar{C} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_c_flux = 'Csim_SGS_composit_flux'
!>        Field label for model coefficient of SGS inertia term
!!         @f$ e_{ijk}\left(\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_m_flux = 'Csim_SGS_inertia'
!>        Field label for model coefficient of SGS Lorentz force
!!         @f$ e_{ijk}\left(\overline{\J_{j}B_{k}}
!!            - \bar{J}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_Lorentz = 'Csim_SGS_Lorentz'
!>        Field label for model coefficient of SGS induction
!!         @f$ e_{ijk}\left(\overline{\u_{j}B_{k}}
!!            - \bar{u}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_induction = 'Csim_SGS_vp_induction'
!>        Field label for odel coefficient of SGS buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_buoyancy =  'Csim_SGS_buoyancy'
!>        Field label for odel coefficient of SGS compositional buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_Csim_SGS_comp_buo = 'Csim_SGS_composit_buo'
!
!      SGS terms by wider filter
!
!>        Field label for SGS heat flux with wider filter
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_wide_SGS_h_flux = 'wide_SGS_heat_flux'
!>        Field label for SGS composition flux with wider filter
!!         @f$ \overline{u_{i}C} - \bar{u}_{i}\bar{C} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_wide_SGS_c_flux = 'wide_SGS_composit_flux'
!>        Field label for SGS inertia term with wider filter
!!         @f$ e_{ijk}\left(\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_wide_SGS_inertia = 'wide_SGS_inertia'
!>        Field label for SGS Lorentz force with wider filter
!!         @f$ e_{ijk}\left(\overline{\J_{j}B_{k}}
!!            - \bar{J}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_wide_SGS_Lorentz = 'wide_SGS_Lorentz'
!>        Field label for SGS induction with wider filter
!!         @f$ e_{ijk}\left(\overline{\u_{j}B_{k}}
!!            - \bar{u}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_wide_SGS_vp_induct = 'wide_SGS_vp_induction'
!
!
!      SGS terms by double filtering
!
!>        Field label for SGS heat flux with wider filter
!!         @f$ \overline{\overline{u_{i}T}}
!!            - \bar{\bar{u}}_{i}\bar{\bar{T}} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_dbl_SGS_h_flux = 'double_SGS_heat_flux'
!>        Field label for SGS composition flux with wider filter
!!         @f$ \overline{\overline{u_{i}C}}
!!            - \bar{\bar{u}}_{i}\bar{\bar{C}} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_dbl_SGS_c_flux = 'double_SGS_composit_flux'
!>        Field label for SGS inertia term with wider filter
!!         @f$ e_{ijk}\left(\overline{\overline{\omega_{j}u_{k}}}
!!            - \bar{\bar{\omega}}_{j}\bar{\bar{u}}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_dbl_SGS_inertia = 'double_SGS_inertia'
!>        Field label for SGS Lorentz force with wider filter
!!         @f$ e_{ijk}\left(\overline{\overline{\J_{j}B_{k}}}
!!            - \bar{\bar{J}}_{j}\bar{\bar{B}}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_dbl_SGS_Lorentz = 'double_SGS_Lorentz'
!>        Field label for SGS induction with wider filter
!!         @f$ e_{ijk}\left(\overline{\overline{\u_{j}B_{k}}}
!!            - \bar{\bar{u}}_{j}\bar{\bar{B}}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_dbl_SGS_vp_induct = 'double_SGS_vp_induction'
!
!  scalars
!
!>        Field label for pressure @f$ p @f$
      character(len=kchara), parameter :: fhd_press = 'pressure'
!
!>        Field label for temperature
!!         @f$ T @f$
      character(len=kchara), parameter :: fhd_temp =  'temperature'
!>        Field label for perturbation of temperature
!!         @f$ \Theta = T - T_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_part_temp = 'perturbation_temp'
!>        Field label for reference temperature
!!         @f$  T_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_ref_temp =  'reference_temperature'
!
!>        Field label for compostiion variation
!!         @f$ C @f$
      character(len=kchara), parameter :: fhd_light = 'composition'
!>        Field label for perturbation of composition
!!         @f$  C - C_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_part_light = 'parturbation_composition'
!>        Field label for reference composition
!!         @f$  C_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_ref_light =  'reference_composition'
!
!>        Field label for entropy
!!         @f$ S @f$
      character(len=kchara), parameter :: fhd_entropy =  'entropy'
!>        Field label for perturbation of entropy
!!         @f$  S - S_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_per_entropy = 'perturbation_entropy'
!>        Field label for reference entropy
!!         @f$  S_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_ref_entropy =  'reference_entropy'
!
!>        Field label for heat source
!!         @f$ q_{T} @f$
      character(len=kchara), parameter                                  &
     &              :: fhd_heat_source =  'heat_source'
!>        Field label for composion source
!!         @f$ q_{C} @f$
      character(len=kchara), parameter                                  &
     &              :: fhd_light_source =  'composition_source'
!>        Field label for entropysource
!!         @f$ q_{S} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_entropy_source =  'entropy_source'
!
!>        Field label for density
!!         @f$ \rho @f$
      character(len=kchara), parameter :: fhd_density =  'density'
!>        Field label for perturbation of density
!!         @f$  \rho - \rho_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_per_density = 'perturbation_density'
!>        Field label for reference density
!!         @f$  \rho_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_ref_density =  'reference_density'
!
!>        Field label for magnetic potential
!!         @f$ W @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_potential =     'magnetic_potential'
!>        Field label for electric potential
!!         @f$ \varphi @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_scalar_potential =  'scalar_potential'
!
!>        Field label for filtered temperature
!!         @f$ \bar{T} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_temp =       'filter_temp'
!>        Field label for filtered perturbation of temperature
!!         @f$ \bar{\Theta} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_part_temp =  'filter_part_temp'
!>        Field label for filtered conposition
!!         @f$ \bar{C} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_comp =       'filter_composition'
!
!>        Field label for kinetic helicity
!!         @f$ H_{u} = u_{i} \omega_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_kinetic_helicity =  'kinetic_helicity'
!>        Field label for magnetic helicity
!!         @f$ H_{B} = B_{i} A_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_magnetic_helicity = 'magnetic_helicity'
!>        Field label for current helicity
!!         @f$ H_{J} = J_{i} B_{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_current_helicity =  'current_helicity'
!>        Field label for cross helicity
!!         @f$ H_{x} = u_{i} B{i} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_cross_helicity =    'cross_helicity'
!
!>        Field label for advection for temperature
!!         @f$ u_{i} \partial_{i} T @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_heat_advect =       'heat_advect'
!>        Field label for advection for perturbation of temperature
!!         @f$ u_{i} \partial_{i} \Theta @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_part_h_advect =     'part_h_advect'
!>        Field label for divergence of heat flux
!!         @f$ \partial_{i} \left( u_{i} T \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_h_flux =        'div_h_flux'
!>        Field label for divergence of perturbation of heat flux
!!         @f$ \partial_{i} \left( u_{i} \Theta \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_ph_flux =       'div_part_h_flux'
!>        Field label for advection for composition
!!         @f$ u_{i} \partial_{i} C @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_composit_advect =    'composition_advect'
!>        Field label for advection for perturbation of composition
!!         @f$ u_{i} \partial_{i} \Theta_C @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_part_c_advect =     'part_c_advect'
!>        Field label for divergence of composition flux
!!         @f$ \partial_{i} \left( u_{i} C \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_c_flux =        'div_c_flux'
!>        Field label for divergence of perturbation of compopstion flux
!!         @f$ \partial_{i} \left( u_{i} \Theta_C \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_pc_flux =       'div_part_c_flux'
!
!   Energy fluxes
!
!>        Field label for magnetic enerfy flux
!>       @f$ B_{i}e_{ijk} \partial_{j} \left(e_{klm}u_{l}B_{m}\right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_ene_gen =       'magnetic_ene_generation'
!>        Field label for work against Lorentz force
!!         @f$ - u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_work_agst_Lorentz = 'work_against_Lorentz'
!>        Field label for work of Lorentz force
!!         @f$ u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Lorentz_work =      'Lorentz_work'
!>        Field label for work of divergence of Maxwell tensor
!!         @f$ u_{i} \partial_{j} \left( B_{j} B_{i} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_tension_work =  'mag_tension_work'
!>        Field label for buoyancy flux
!!         @f$ -u_{i} \alpha_{T} g_{i} T @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_buoyancy_flux =     'buoyancy_flux'
!>        Field label for compositional buoyancy flux
!!         @f$ -u_{i} \alpha_{c} g_{i} C @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_comp_buo_flux =     'composite_buoyancy_flux'
!>        Field label for filtered buoyancy flux
!!         @f$ -u_{i} \alpha_{c} g_{i} \tilde{T} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_buo_flux =   'filtered_buoyancy_flux'
!
!>        Field label for divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}T} - \bar{u}_{i}\bar{T} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_SGS_h_flux =    'div_SGS_h_flux'
!>        Field label for divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}C} - \bar{u}_{i}\bar{C} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_div_SGS_c_flux =    'div_SGS_c_flux'
!>        Field label for energy flux of SGS induction
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_m_ene_gen =     'SGS_m_ene_gen'
!>        Field label for temperature generation by SGS heat flux
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_temp_gen =      'SGS_temp_gen'
!>        Field label for work of SGS Lorentz force
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz_work =  'SGS_Lorentz_work'
!>        Field label for work of SGS Reynolds stress
      character(len=kchara), parameter                                  &
     &             :: fhd_Reynolds_work =     'Reynolds_work'
!>        Field label for work of SGS buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_buo_flux =      'SGS_buoyancy_flux'
!>        Field label for work of SGS compositional buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_comp_buo_flux = 'SGS_comp_buoyancy_flux'
!
!
!>        Field label for geostrophic balance
!!         @f$ -2 e_{ijk} \Omega_{j} u_{k} + \partial_{i} p @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_geostrophic =  'geostrophic_balance'
!
!>        Field label for heat flux
!!         @f$ u_{i} T + (\overline{u_{i}T} - \bar{u}_{i}\bar{T}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_h_flux_w_sgs =  'heat_flux_w_SGS'
!>        Field label for compositinoal flux
!!         @f$ u_{i} C + (\overline{u_{i}C} - \bar{u}_{i}\bar{C}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_c_flux_w_sgs =  'comp_flux_w_SGS'
!
!>        Field label for advection for momentum
!!         @f$ u_{j} \partial_{j} u_{i}
!!           + e_{ijk}\left(\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_inertia_w_sgs = 'intertia_w_SGS'
!>        Field label for Lorentz force
!!         @f$ e_{ijk} J_{j} B_{k}
!!           + e_{ijk}\left(\overline{B{j}u_{k}}
!!            - \bar{J}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Lorentz_w_sgs = 'Lorentz_w_SGS'
!
!>        Field label for inductino for vector potential
!!         @f$ e_{ijk} u_{j} B_{k} @f$
!!           + e_{ijk}\left(\overline{u{j}B_{k}}
!!            - \bar{u}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_vp_induct_w_sgs = 'vecp_induction_w_SGS'
!>        Field label for magnetic induction
!!         @f$ e_{ijk} \partial_{j}\left(e_{klm}u_{l}B_{m} \right)@f$
!!           + e_{ijk} \partial_{j}(e_{klm}\left(\overline{u{l}B_{m}}
!!                              - \bar{u}_{l}\bar{B}_{m} \right)) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_induct_w_sgs = 'induction_w_SGS'
!
!>        Field label for momentum flux
!!         @f$ u_{i} u_{j}
!!            + (\overline{u_{i}u_{j}} - \bar{u}_{i}\bar{u}_{j})@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mom_flux_w_sgs = 'momentum_flux_w_SGS'
!>        Field label for momentum flux
!!         @f$ B_{i} B_{j}
!!            + (\overline{B_{i}B_{j}} - \bar{B}_{i}\bar{B}_{j})@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_maxwell_t_w_sgs = 'maxwell_tensor_w_sgs'
!
!>        Field label for temperature flux
      character(len=kchara), parameter                                  &
     &             :: fhd_temp_generation =   'temp_generation'
!>        Field label for perturbation temperature flux
      character(len=kchara), parameter                                  &
     &             :: fhd_part_temp_gen =     'part_temp_gen'
!>        Field label for perturbation composition flux
      character(len=kchara), parameter                                  &
     &             :: fhd_part_comp_gen =     'part_comp_gen'
!>        Field label for energy flux by viscous diffusion
!!         @f$ u_{i} \left( \partial_{j}\partial_{j} u_{i} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_vis_ene_diffuse =   'vis_ene_diffuse'
!>        Field label for energy flux by magnetic diffusion
!!         @f$ B_{i} \left( \partial_{j}\partial_{j} B_{i} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_ene_diffuse =   'mag_ene_diffuse'
!>        Field label for true divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}T} - \bar{u}_{i}\bar{T} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_h_flux_true = 'SGS_div_h_flux_true'
!>        Field label for true divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}C} - \bar{u}_{i}\bar{C} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_c_flux_true = 'SGS_div_c_flux_true'
!>        Field label for work of true SGS Lorentz force
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz_wk_true = 'SGS_Lorentz_work_true'
!>        Field label for work of true SGS Reynolds stress
      character(len=kchara), parameter                                  &
     &             :: fhd_Reynolds_work_true =  'Reynolds_work_true'
!>        Field label for temperature generation by true SGS heat flux
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_temp_gen_true =   'SGS_temp_gen_true'
!>        Field label for composition generation by true SGS compostion flux
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_comp_gen_true =   'SGS_comp_gen_true'
!>        Field label for energy flux of true SGS induction
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_m_ene_gen_true =  'SGS_m_ene_gen_true'
!
!  fluxes by resolved field
!
!>        Field label for momentum flux
!!         @f$ u_{i} u_{j} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mom_flux =      'momentum_flux'
!>        Field label for Maxwell tensor
!!         @f$ B_{i} B_{j} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_maxwell_t =     'maxwell_tensor'
!>        Field label for SGS momentum flux
!!         @f$ \overline{u_{i}u_{j}} - \bar{u}_{i}\bar{u}_{j} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_m_flux =    'SGS_momentum_flux'
!>        Field label for SGS inertia term
!!         @f$ e_{ijk}\left(\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_inertia =     'SGS_inertia'
!>        Field label for rotation of SGS inertia term
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_rot_inertia = 'rot_SGS_inertia'
!>        Field label for divergence of SGS inertia term
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_inertia = 'div_SGS_inertia'
!>        Field label for rotation of SGS Lorentz force
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_rot_Lorentz = 'rot_SGS_Lorentz'
!>        Field label for divergence of SGS Lorentz force
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_Lorentz = 'div_SGS_Lorentz'
!
!>        Field label for SGS Maxwell tensor
!!         @f$ \overline{B_{i}B_{j}} - \bar{B}_{i}\bar{B}_{j} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_maxwell_t = 'SGS_maxwell_tensor'
!
!>        Field label for Tensor for magnetic induction
!!         @f$ u_{i} B_{j}  - B_{i} u_{J} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_induct_t =      'induction_tensor'
!>        Field label for SGS magnetic induction tensor
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_induct_t =  'SGS_induct_tensor'
!
!   work fields
!
!>        Field label for potential in momentum euqaion
!!         @f$  \varphi @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_press_work =      'pressure_work'
!>        Field label for potential in induction euqaion
!!         @f$  \varphi @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_m_potential_work = 'm_potential_work'
!
!>        Field label for SGS term by scale similarity method
      character(len=kchara), parameter :: fhd_SGS_simi =   'SGS_simi'
!>        Field label for SGS term by nonlinear gradient method
      character(len=kchara), parameter :: fhd_SGS_grad =   'SGS_grad'
!>        Field label for SGS term by nonlinear gradient method
!>        using fileterd field
      character(len=kchara), parameter :: fhd_SGS_grad_f = 'SGS_grad_f'
!>        Field label for SGS term by turbulence diffusivity
      character(len=kchara), parameter                                  &
     &              :: fhd_SGS_diffuse = 'SGS_diffuse'
!
!>        Field label for temperature to obatin commutation error
      character(len=kchara), parameter :: fhd_SGS_temp =   'temp_4_SGS'
!>        Field label for composition variation
!!        to obatin commutation error
      character(len=kchara), parameter :: fhd_SGS_comp =   'comp_4_SGS'
!
!  Square of each component of fields
!
!>        Square of velocity @f$ u_{i}^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_v = 'square_velocity'
!>        Square of vorticity @f$ \omega_{i}^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_w = 'square_vorticity'
!>        Square of magnetic field @f$ B_{i}^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_b = 'square_magne'
!>        Square of magnetic vector potential @f$ A_{i}^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_a = 'square_vector_p'
!>        Square of current density @f$ J_{i}^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_j = 'square_current'
!>        Square of temperature @f$ T^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_t = 'square_temperature'
!>        Square of composition @f$ C^{2} @f$
      character(len=kchara), parameter                                  &
     &      :: fhd_square_c = 'square_composition'
!
!  difference of field
!>        Field label for gradient of @f$ u_{x} @f$
      character(len=kchara), parameter :: fhd_grad_v_1 =  'grad_v_1'
!>        Field label for gradient of @f$ u_{y} @f$
      character(len=kchara), parameter :: fhd_grad_v_2 =  'grad_v_2'
!>        Field label for gradient of @f$ u_{z} @f$
      character(len=kchara), parameter :: fhd_grad_v_3 =  'grad_v_3'
!
!>        Field label for gradient of @f$ \omega_{x} @f$
      character(len=kchara), parameter :: fhd_grad_w_1 =  'grad_w_1'
!>        Field label for gradient of @f$ \omega_{y} @f$
      character(len=kchara), parameter :: fhd_grad_w_2 =  'grad_w_2'
!>        Field label for gradient of @f$ \omega_{z} @f$
      character(len=kchara), parameter :: fhd_grad_w_3 =  'grad_w_3'
!
!>        Field label for gradient of @f$ A_{x} @f$
      character(len=kchara), parameter :: fhd_grad_a_1 =  'grad_a_1'
!>        Field label for gradient of @f$ A_{y} @f$
      character(len=kchara), parameter :: fhd_grad_a_2 =  'grad_a_2'
!>        Field label for gradient of @f$ A_{z} @f$
      character(len=kchara), parameter :: fhd_grad_a_3 =  'grad_a_3'
!
!>        Field label for gradient of @f$ B_{x} @f$
      character(len=kchara), parameter :: fhd_grad_b_1 =  'grad_b_1'
!>        Field label for gradient of @f$ B_{y} @f$
      character(len=kchara), parameter :: fhd_grad_b_2 =  'grad_b_2'
!>        Field label for gradient of @f$ B_{z} @f$
      character(len=kchara), parameter :: fhd_grad_b_3 =  'grad_b_3'
!
!>        Field label for gradient of @f$ J_{x} @f$
      character(len=kchara), parameter :: fhd_grad_j_1 =  'grad_j_1'
!>        Field label for gradient of @f$ J_{y} @f$
      character(len=kchara), parameter :: fhd_grad_j_2 =  'grad_j_2'
!>        Field label for gradient of @f$ J_{z} @f$
      character(len=kchara), parameter :: fhd_grad_j_3 =  'grad_j_3'
!
!>        Field label for gradient of @f$ T @f$
      character(len=kchara), parameter :: fhd_grad_temp = 'grad_temp'
!>        Field label for gradient of @f$ \Theta @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_par_temp = 'grad_part_temp'
!>        Field label for gradient of reference temperature
!!         @f$  \partial T_{0} / dz@f$
!>         or @f$  \partial T_{0} / dr@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_ref_temp = 'grad_reference_temp'
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_composit = 'grad_composition'
!>        Field label for gradient of perturbation of composition
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_par_light = 'grad_part_composition'
!>        Field label for gradient of reference temperature
!!         @f$  \partial C_{0} / dz@f$
!>         or @f$  \partial C_{0} / dr@f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_ref_light = 'grad_reference_composition'
!
!
!
!  difference of filtered field
!>        Field label for gradient of @f$ \tilde{u}_{x} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_v_1 =  'grad_filtered_v_1'
!>        Field label for gradient of @f$ \tilde{u}_{y} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_v_2 =  'grad_filtered_v_2'
!>        Field label for gradient of @f$ \tilde{u}_{z} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_v_3 =  'grad_filtered_v_3'
!
!>        Field label for gradient of @f$ \tilde{\omega}_{x} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_w_1 =  'grad_filtered_w_1'
!>        Field label for gradient of @f$ \tilde{\omega}_{y} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_w_2 =  'grad_filtered_w_2'
!>        Field label for gradient of @f$ \tilde{\omega}_{z} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_w_3 =  'grad_filtered_w_3'
!
!>        Field label for gradient of @f$ \tilde{A}_{x} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_a_1 =  'grad_filtered_a_1'
!>        Field label for gradient of @f$ \tilde{A}_{y} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_a_2 =  'grad_filtered_a_2'
!>        Field label for gradient of @f$ \tilde{A}_{z} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_a_3 =  'grad_filtered_a_3'
!
!>        Field label for gradient of @f$ \tilde{B}_{x} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_b_1 =  'grad_filtered_b_1'
!>        Field label for gradient of @f$ \tilde{B}_{y} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_b_2 =  'grad_filtered_b_2'
!>        Field label for gradient of @f$ \tilde{B}_{z} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_b_3 =  'grad_filtered_b_3'
!
!>        Field label for gradient of @f$ \tilde{J}_{x} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_j_1 =  'grad_filtered_j_1'
!>        Field label for gradient of @f$ \tilde{J}_{y} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_j_2 =  'grad_filtered_j_2'
!>        Field label for gradient of @f$ \tilde{J}_{z} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_j_3 =  'grad_filtered_j_3'
!
!>        Field label for gradient of @f$ \tilde{T} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_temp = 'grad_filtered_temp'
!>        Field label for gradient of @f$ C @f$
!>        Field label for gradient of @f$ \tilde{C} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_grad_filter_comp = 'grad_filtered_comp'
!
!  wider filtered field
!
!>        Field label for filtered velocity by wider filter
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_velo = 'wide_filter_velo'
!>        Field label for filtered vorticity by wider filter
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_vort = 'wide_filter_vorticity'
!>        Field label for filtered magnetic vector potential
!!        by wider filter
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_vecp = 'wide_filter_vecp'
!>        Field label for filtered magnetic field by wider filter
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_magne = 'wide_filter_magne'
!>        Field label for filtered current density by wider filter
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_current = 'wide_filter_current'
!
!>        Field label for filtered temperature by wider filter
      character(len=kchara), parameter                                  &
     &      :: fhd_w_filter_temp =      'wide_filter_temp'
!>        Field label for filtered grad. of temperature by wider filter
      character(len=kchara), parameter                                  &
     &      :: fhd_w_filter_grad_temp = 'wide_filter_grad_temp'
!
!>        Field label for filtered compostiion by wider filter
      character(len=kchara), parameter                                  &
     &      :: fhd_w_filter_comp =    'wide_filter_composition'
!>        Field label for filtered grad. of composition by wider filter
      character(len=kchara), parameter                                  &
     &      :: fhd_w_filter_grad_comp = 'wide_filter_grad_composition'
!
!  double filtered field
!
!>        Field label for filtered velocity by double filtering
      character(len=kchara), parameter                                  &
     &             :: fhd_d_filter_velo = 'double_filter_velo'
!>        Field label for filtered vorticity by double filtering
      character(len=kchara), parameter                                  &
     &             :: fhd_d_filter_vort = 'double_filter_vorticity'
!>        Field label for filtered magnetic vector potential
!!        by double filtering
      character(len=kchara), parameter                                  &
     &             :: fhd_d_filter_vecp = 'double_filter_vecp'
!>        Field label for filtered magnetic field by double filtering
      character(len=kchara), parameter                                  &
     &             :: fhd_d_filter_magne = 'double_filter_magne'
!>        Field label for filtered current density by double filtering
      character(len=kchara), parameter                                  &
     &             :: fhd_d_filter_current = 'double_filter_current'
!
!>        Field label for filtered temperature by double filtering
      character(len=kchara), parameter                                  &
     &      :: fhd_d_filter_temp =      'double_filter_temp'
!>        Field label for filtered grad. of temperature
!!        by double filtering
      character(len=kchara), parameter                                  &
     &      :: fhd_d_filter_grad_temp = 'double_filter_grad_temp'
!
!>        Field label for filtered compostiion by double filtering
      character(len=kchara), parameter                                  &
     &      :: fhd_d_filter_comp =    'double_filter_composition'
!>        Field label for filtered grad. of composition
!>        by double filtering
      character(len=kchara), parameter                                  &
     &      :: fhd_d_filter_grad_comp = 'double_filter_grad_comp'
!
!  divergence of momentum equations
!
!>        Field label for divergence of advection
      character(len=kchara), parameter                                  &
     &             :: fhd_div_inertia =    'div_inertia'
!>        Field label for divergence of Lorentz force
      character(len=kchara), parameter                                  &
     &             :: fhd_div_Lorentz =    'div_Lorentz_force'
!>        Field label for divergence of Coriolis force
      character(len=kchara), parameter                                  &
     &             :: fhd_div_Coriolis =   'div_Coriolis_force'
!>        Field label for divergence of buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_div_buoyancy =   'div_buoyancy'
!>        Field label for divergence of compositional buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_div_comp_buo =   'div_composite_buoyancy'
!>        Field label for divergence of filtered buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_div_filter_buo = 'div_filtered_buoyancy'
!>        Field label for divergence of viscousity
      character(len=kchara), parameter                                  &
     &             :: fhd_div_viscous =    'div_viscousity'
!
!  rotation of momentum equations
!
!>        Field label for curl of advection
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_inertia =  'rot_inertia'
!>        Field label for curl of Lorentz force
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_Lorentz =  'rot_Lorentz_force'
!>        Field label for curl of Coriolis term
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_Coriolis = 'rot_Coriolis_force'
!>        Field label for curl of buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_buoyancy = 'rot_buoyancy'
!>        Field label for curl of compositional buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_comp_buo = 'rot_composite_buoyancy'
!>        Field label for curl of filtered buoyancy
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_filter_buo = 'rot_filtered_buoyancy'
!
!  arrays for current forces
!
!>        Field label for total forces
      character(len=kchara), parameter                                  &
     &             :: fhd_forces =        'sum_forces'
!>        Field label for curl of total forces
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_forces =    'rot_sum_forces'
!>        Field label for divergence of total forces
      character(len=kchara), parameter                                  &
     &             :: fhd_div_forces =    'div_sum_forces'
!
!  arrays for previous evolution
!
!>        Field label for explicit term for momentum at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_mom =        'previous_momentum'
!>        Field label for explicit term for induction at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_uxb =        'previous_induction'
!>        Field label for explicit term for heat at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_heat =       'previous_heat'
!>        Field label for explicit term for composition
!!        at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_composit =   'previous_composition'
!>        Field label for explicit term for pressure at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_press =      'previous_pressure'
!
!  arrays for evolution check
!
!>        Field label for explicit term for momentum for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_mom =       'check_momentum'
!>        Field label for explicit term for induction for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_uxb =       'check_induction'
!>        Field label for explicit term for heat for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_heat =      'check_heat'
!>        Field label for explicit term for composition
!!        for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_composit =  'check_composition'
!>        Field label for explicit term for pressure for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_press =     'check_pressure'
!>        Field label for explicit term for electric potential
!!        for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_potential = 'check_potential'
!
!>        Field label for explicit term for momentum for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_mom_2 =       'check_momentum_2'
!>        Field label for explicit term for induction for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_uxb_2 =       'check_induction_2'
!>        Field label for explicit term for heat for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_heat_2 =      'check_heat_2'
!>        Field label for explicit term for composition
!!        for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_composit_2 =  'check_composition_2'
!>        Field label for explicit term for pressure for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_press_2 =     'check_pressure_2'
!>        Field label for explicit term for electric potential
!!        for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_potential_2 = 'check_potential_2'
!
!   --------------------------------------------------------------------
!
!>        Field label for velocity length scale
      character(len=kchara), parameter                                  &
     &             :: fhd_velocity_scale =    'velocity_scale'
!>        Field label for magnetic field length scale
      character(len=kchara), parameter                                  &
     &             :: fhd_magnetic_scale =    'magnetic_scale'
!>        Field label for temperature length scale
      character(len=kchara), parameter                                  &
     &             :: fhd_temp_scale =        'temperature_scale'
!>        Field label for composition length scale
      character(len=kchara), parameter                                  &
     &             :: fhd_composition_scale = 'composition_scale'
!
!
!>        Field label for buoyancy flux
!!         @f$ -u_{i} \alpha_{T} g_{i} T @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_buoyancy_work =     'buoyancy_work'
!
!   --------------------------------------------------------------------
!
!>        Field label for viscosity   @f$ \mu @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_viscosity =        'viscosity'
!>        Field label for thermal diffusivity @f$ k @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_T_conductivity =   'thermal_conductivity'
!
!
!>        Field label for kinetic viscosity
!>                               @f$ \nu = \mu / \bar{\rho} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_K_viscosity =      'kinetic_viscosity'
!>        Field label for thermal diffusivity 
!!                               @f$ \kappa_{T} = k / \bar{\rho} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_T_diffusivity =    'thermal_diffusivity'
!>        Field label for chemical diffusivity
      character(len=kchara), parameter                                  &
     &             :: fhd_C_diffusivity =    'chemical_diffusivity'
!>        Field label for magnetic diffusivity
      character(len=kchara), parameter                                  &
     &             :: fhd_B_diffusivity =    'magnetic_diffusivity'
!
!   --------------------------------------------------------------------
!
!>        Field label for rotation of ststem @f$ Omega @f$
      character(len=kchara), parameter :: fhd_omega = 'system_Rotation'
!
!>        Field label for background magnetic field @f$ B_{0} @f$
      character(len=kchara), parameter :: fhd_back_B = 'background_B'
!
!   --------------------------------------------------------------------
!
!>        Term label for heat equation
      character(len=kchara), parameter                                  &
     &             :: thd_heat_flux =  'heat'
!>        Term label for advection term
      character(len=kchara), parameter                                  &
     &             :: thd_advection =  'inertia'
!>        Term label for Lorentz term
      character(len=kchara), parameter                                  &
     &             :: thd_lorentz =    'Lorentz'
!>        Term label for Coriolis term
      character(len=kchara), parameter                                  &
     &             :: thd_coriolis =   'Coriolis'
!>        Term label for induction term
      character(len=kchara), parameter                                  &
     &             :: thd_induction =  'induction'
!>        Term label for cpmpositional flux term
      character(len=kchara), parameter                                  &
     &             :: thd_comp_flux =  'comp_flux'
!>        Term label for gravity
      character(len=kchara), parameter                                  &
     &             :: thd_gravity =    'gravity'
!
!
      end module m_phys_labels
