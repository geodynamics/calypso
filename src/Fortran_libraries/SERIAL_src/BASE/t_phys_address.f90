!>@file   t_phys_address.f90
!!        module t_phys_address
!!
!! @author H. Matsui
!! @date ...
!!
!
!> @brief Structure of field addresses
!!       These integer points adresses of fields.
!!
!
      module t_phys_address
!
      use m_precision
      use m_constants
!
      implicit  none
! 
!>       Structure for start address for fields
      type phys_address
!
!>        Start address for velocity
!!         @f$ u_{i} @f$
        integer (kind=kint) :: i_velo  =           izero
!>        Start address for pressure
!!         @f$ p @f$
        integer (kind=kint) :: i_press =           izero
!>        Start address for vorticity
!!         @f$ \omega_{i} = e_{ijk} \partial_{j} u_{k} @f$
        integer (kind=kint) :: i_vort  =           izero
!
!>        start address for temperature
!!         @f$ T @f$
        integer (kind=kint) :: i_temp  =           izero
!>        start address for compostiion variation
!!         @f$ C @f$
        integer (kind=kint) :: i_light =           izero
!>        start address for density
!!         @f$ \rho @f$
        integer (kind=kint) :: i_density =         izero
!>        start address for entropy
!!         @f$ S @f$
        integer (kind=kint) :: i_entropy =         izero
!
!>        start address for magnetic field
!!         @f$ B_{i} @f$
        integer (kind=kint) :: i_magne =           izero
!>        start address for magnetic vector potential
!!         @f$ B_{i} = e_{ijk} \partial_{j} A_{k} @f$
        integer (kind=kint) :: i_vecp =            izero
!>        start address for current density
!!         @f$ J_{i} = e_{ijk} \partial_{j} B_{k} @f$
        integer (kind=kint) :: i_current =         izero
!>        start address for electric field
!!         @f$ E_{i} @f$
        integer (kind=kint) :: i_electric =        izero
!>        start address for magnetic potential
!!         @f$ W @f$
        integer (kind=kint) :: i_mag_p =           izero
!>        start address for electric potential
!!         @f$ \varphi @f$
        integer (kind=kint) :: i_scalar_p =        izero
!
!>        start address for perturbation of temperature
!!         @f$ \Theta = T - T_{0} @f$
        integer (kind=kint) :: i_par_temp =        izero
!>        start address for perturbation of composition
!!         @f$  C - C_{0} @f$
        integer (kind=kint) :: i_par_light =       izero
!>        start address for perturbation of density
!!         @f$  \rho - \rho_{0} @f$
        integer (kind=kint) :: i_par_density =     izero
!>        start address for perturbation of entropy
!!         @f$  S - S_{0} @f$
        integer (kind=kint) :: i_par_entropy =     izero
!
!>        start address for heat source
!!         @f$ q_{T} @f$
        integer (kind=kint) :: i_heat_source =     izero
!>        start address for composion source
!!         @f$ q_{C} @f$
        integer (kind=kint) :: i_light_source =    izero
!>        start address for entropysource
!!         @f$ q_{S} @f$
        integer (kind=kint) :: i_entropy_source =  izero
!
!
!>        start address for momentum equation's potential
!!         @f$  \varphi @f$
        integer (kind=kint) :: i_p_phi =           izero
!>        start address for induction equation's potential
!!         @f$  \varphi @f$
        integer (kind=kint) :: i_m_phi =           izero
!>        start address for reference temperature
!!         @f$  T_{0} @f$
        integer (kind=kint) :: i_ref_t =           izero
!>        start address for gradient of reference temperature
!!         @f$  \partial T_{0} / dz@f$
!>         or @f$  \partial T_{0} / dr@f$
        integer (kind=kint) :: i_gref_t =          izero
!>        start address for reference temperature
!!         @f$  C_{0} @f$
        integer (kind=kint) :: i_ref_c =           izero
!>        start address for gradient of reference temperature
!!         @f$  \partial C_{0} / dz@f$
!>         or @f$  \partial C_{0} / dr@f$
        integer (kind=kint) :: i_gref_c =          izero
!>        start address for reference density
!!         @f$  \rho_{0} @f$
        integer (kind=kint) :: i_ref_density =     izero
!>        start address for reference entropy
!!         @f$  S_{0} @f$
        integer (kind=kint) :: i_ref_entropy =     izero
!>        start address for poynting flux
!!         @f$  e_{ijk} E_{j} B_{k} @f$
        integer (kind=kint) :: i_poynting =        izero
!
!>        start address for velocity
!!         @f$ \partial_{i} u_{i} @f$
        integer (kind=kint) :: i_div_v =           izero
!>        start address for magnetic field
!!         @f$ \partial_{i} B_{i} @f$
        integer (kind=kint) :: i_div_b =           izero
!>        start address for magnetic vector potential
!!         @f$ \partial_{i} A_{i} @f$
        integer (kind=kint) :: i_div_a =           izero
!
!>        Start address for filtered velocity
!!         @f$ \bar{u}_{i} @f$
        integer (kind=kint) :: i_filter_velo  =    izero
!>        Start address for filtered velocity
!!         @f$ \bar{\omega}_{i} @f$
        integer (kind=kint) :: i_filter_vort  =    izero
!>        Start address for filtered temperature
!!         @f$ \bar{T} @f$
        integer (kind=kint) :: i_filter_temp  =    izero
!>        Start address for filtered magnetic field
!!         @f$ \bar{B}_{i} @f$
        integer (kind=kint) :: i_filter_magne =    izero
!>        Start address for filtered current density
!!         @f$ \bar{B}_{i} @f$
        integer (kind=kint) :: i_filter_current =  izero
!>        Start address for filtered vetor potential
!!         @f$ \bar{A}_{i} @f$
        integer (kind=kint) :: i_filter_vecp =     izero
!>        Start address for filtered perturbation of temperature
!!         @f$ \bar{\Theta} @f$
        integer (kind=kint) :: i_filter_par_t =    izero
!>        Start address for filtered conposition
!!         @f$ \bar{C} @f$
        integer (kind=kint) :: i_filter_comp =     izero
!
!>        start address for filtered velocity
!!         @f$ \partial_{i} \bar{u}_{i} @f$
        integer (kind=kint) :: i_div_filter_v =    izero
!>        start address for filtered magnetic field
!!         @f$ \partial_{i} \bar{B}_{i} @f$
        integer (kind=kint) :: i_div_filter_b =    izero
!>        start address for filtered magnetic vector potential
!!         @f$ \partial_{i} \bar{A}_{i} @f$
        integer (kind=kint) :: i_div_filter_a =    izero
!
!>        start address for kinetic helicity
!!         @f$ H_{u} = u_{i} \omega_{i} @f$
        integer (kind=kint) :: i_k_heli =          izero
!>        start address for magnetic helicity
!!         @f$ H_{B} = B_{i} A_{i} @f$
        integer (kind=kint) :: i_m_heli =          izero
!>        start address for current helicity
!!         @f$ H_{J} = J_{i} B_{i} @f$
        integer (kind=kint) :: i_c_heli =          izero
!>        start address for cross helicity
!!         @f$ H_{x} = u_{i} B{i} @f$
        integer (kind=kint) :: i_x_heli =          izero
!
!>        start address for magnetic enerfy flux
!>       @f$ B_{i}e_{ijk} \partial_{j} \left(e_{klm}u_{l}B_{m}\right) @f$
        integer (kind=kint) :: i_me_gen =          izero
!>        start address for buoyancy flux
!!         @f$ -u_{i} \alpha_{T} g_{i} T @f$
        integer (kind=kint) :: i_buo_gen =         izero
!>        start address for compositional buoyancy flux
!!         @f$ -u_{i} \alpha_{c} g_{i} C @f$
        integer (kind=kint) :: i_c_buo_gen =       izero
!>        start address for filtered buoyancy flux
!!         @f$ -u_{i} \alpha_{c} g_{i} \tilde{T} @f$
        integer (kind=kint) :: i_f_buo_gen =       izero
!>        start address for temperature flux
        integer (kind=kint) :: i_temp_gen =        izero
!>        start address for perturbation temperature flux
        integer (kind=kint) :: i_par_t_gen =       izero
!>        start address for perturbation composition flux
        integer (kind=kint) :: i_par_c_gen =       izero
!>        start address for work of Lorentz force
!!         @f$ u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
        integer (kind=kint) :: i_ujb =             izero
!>        start address for work against Lorentz force
!!         @f$ - u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
        integer (kind=kint) :: i_nega_ujb =        izero
!>        start address for work of divergence of Maxwell tensor
!!         @f$ u_{i} \partial_{j} \left( B_{j} B_{i} \right) @f$
        integer (kind=kint) :: i_m_tension_wk  =   izero
!>        start address for energy flux by viscous diffusion
!!         @f$ u_{i} \left( \partial_{j}\partial_{j} u_{i} \right) @f$
        integer (kind=kint) :: i_vis_e_diffuse =   izero
!>        start address for energy flux by magnetic diffusion
!!         @f$ B_{i} \left( \partial_{j}\partial_{j} B_{i} \right) @f$
        integer (kind=kint) :: i_mag_e_diffuse =   izero
!
!>        start address for thermal diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} T @f$
        integer (kind=kint) :: i_t_diffuse =       izero
!>        start address for viscous diffusion
!!         @f$ \nu \partial_{j}\partial_{j} u_{i} @f$
        integer (kind=kint) :: i_v_diffuse =       izero
!>        start address for diffusion of vorticity
!!         @f$ \nu \partial_{j}\partial_{j} \omega_{i} @f$
        integer (kind=kint) :: i_w_diffuse =       izero
!>        start address for diffusion of vetor potential
!!         @f$ -J_{i} = \eta \partial_{j}\partial_{j} A_{i} @f$
        integer (kind=kint) :: i_vp_diffuse =      izero
!>        start address for magnetic diffusion
!!         @f$ \nu \partial_{j}\partial_{j} B_{i} @f$
        integer (kind=kint) :: i_b_diffuse =       izero
!>        start address for compositional diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} C @f$
        integer (kind=kint) :: i_c_diffuse =       izero
!
!>        start address for heat flux
!!         @f$ u_{i} T @f$
        integer (kind=kint) :: i_h_flux =          izero
!>        start address for perturbation of heat flux
!!         @f$ u_{i} \Theta @f$
        integer (kind=kint) :: i_ph_flux =         izero
!>        start address for compositinoal flux
!!         @f$ u_{i} C @f$
        integer (kind=kint) :: i_c_flux =          izero
!>        start address for compositinoal flux
!!         @f$ u_{i} \Theta_{C} @f$
        integer (kind=kint) :: i_pc_flux =          izero
!>        start address for momentum flux
!!         @f$ u_{i} u_{j} @f$
        integer (kind=kint) :: i_m_flux =          izero
!>        start address for Maxwell tensor
!!         @f$ B_{i} B_{j} @f$
        integer (kind=kint) :: i_maxwell =         izero
!>        start address for TEnsor for magnetic induction
!!         @f$ u_{i} B_{j}  - B_{i} u_{J} @f$
        integer (kind=kint) :: i_induct_t =        izero
!
!>        start address for advection for temperature
!!         @f$ u_{i} \partial_{i} T @f$
        integer (kind=kint) :: i_h_advect =        izero
!>        start address for advection for perturbation of temperature
!!         @f$ u_{i} \partial_{i} \Theta @f$
        integer (kind=kint) :: i_ph_advect =       izero
!>        start address for advection for momentum
!!         @f$ u_{j} \partial_{j} u_{i} @f$
        integer (kind=kint) :: i_m_advect =        izero
!>        start address for divergence of heat flux
!!         @f$ \partial_{i} \left( u_{i} T \right) @f$
        integer (kind=kint) :: i_h_flux_div =      izero
!>        start address for divergence of perturbation of heat flux
!!         @f$ \partial_{i} \left( u_{i} \Theta \right) @f$
        integer (kind=kint) :: i_ph_flux_div =     izero
!>        start address for divergence of heat flux
!!         @f$ \partial_{i} \left( u_{i} C \right) @f$
        integer (kind=kint) :: i_c_flux_div =      izero
!>        start address for divergence of perturbation of heat flux
!!         @f$ \partial_{i} \left( u_{i} \Theta_C \right) @f$
        integer (kind=kint) :: i_pc_flux_div =     izero
!>        start address for divergence of momentum flux
!!         @f$ \partial_{j} \left( u_{i} u_{j} \right) @f$
        integer (kind=kint) :: i_m_flux_div =      izero
!>        start address for divergence of Maxwell stress
!!         @f$ \partial_{j} \left( B_{i} B_{j} \right) @f$
        integer (kind=kint) :: i_maxwell_div =     izero
!>        start address for divergence of magnetic induction
!!         @f$ \partial_{i} \left(e_{ijk} u_{j} B_{k} \right) @f$
        integer (kind=kint) :: i_induct_div =      izero
!>        Start address for pressure gradient
!!         @f$ \partial_{i} p @f$
        integer (kind=kint) :: i_press_grad  =    izero
!>        start address for magnetic tension
!!         @f$ B_{j} \partial_{j} B_{i} @f$
        integer (kind=kint) :: i_m_tension =       izero
!>        start address for Lorentz force
!!         @f$ e_{ijk} J_{j} B_{k} @f$
        integer (kind=kint) :: i_lorentz =         izero
!>        start address for Coriolis force
!!         @f$ -2 e_{ijk} \Omega_{j} u_{k} @f$
        integer (kind=kint) :: i_coriolis =        izero
!>        start address for buoyancy
!!         @f$ -\alpha_{T} g_{i} T @f$
        integer (kind=kint) :: i_buoyancy =        izero
!>        start address for compositional buoyancy
!!         @f$ -\alpha_{C} g_{i} C @f$
        integer (kind=kint) :: i_comp_buo =        izero
!>        start address for filtered buoyancy
!!         @f$ -\alpha_{C} g_{i} \tilde{T} @f$
        integer (kind=kint) :: i_filter_buo =      izero
!>        start address for magnetic induction
!!         @f$ e_{ijk} \partial_{j}\left(e_{klm}u_{l}B_{m} \right)@f$
        integer (kind=kint) :: i_induction =       izero
!>        start address for inductino for vector potential
!!         @f$ e_{ijk} u_{j} B_{k} @f$
        integer (kind=kint) :: i_vp_induct =       izero
!>        start address for magnetic stretch term
!!         @f$ B_{i} \partial_{k} \u_{k} \right)@f$
        integer (kind=kint) :: i_mag_stretch =     izero
!>        start address for advection for composition
!!         @f$ u_{i} \partial_{i} C @f$
        integer (kind=kint) :: i_c_advect =        izero
!>        start address for advection for perturbation of composition
!!         @f$ u_{i} \partial_{i} \Theta_C @f$
        integer (kind=kint) :: i_pc_advect =       izero
!
!>        start address for SGS heat flux
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
        integer (kind=kint) :: i_SGS_h_flux =      izero
!>        start address for SGS compositional flux
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
        integer (kind=kint) :: i_SGS_c_flux =      izero
!>        start address for SGS momentum flux
!!         @f$ \overline{u_{i}u_{j}} - \bar{u}_{i}\bar{u}_{j} @f$
        integer (kind=kint) :: i_SGS_m_flux =      izero
!>        start address for SGS Maxwell tensor
!!         @f$ \overline{B_{i}B_{j}} - \bar{B}_{i}\bar{B}_{j} @f$
        integer (kind=kint) :: i_SGS_maxwell =     izero
!>        start address for SGS magnetic induction tensor
        integer (kind=kint) :: i_SGS_induct_t =    izero
!>        start address for SGS inertia term
        integer (kind=kint) :: i_SGS_inertia =    izero
!>        start address for divergence of SGS Maxwell tensor
!!         @f$ \partial_{i} \left( \overline{B_{i}B_{j}} - \bar{B}_{i}\bar{B}_{j} \right) @f$
        integer (kind=kint) :: i_SGS_Lorentz =     izero
!
!>        start address for curl of SGS inertia term
        integer (kind=kint) :: i_SGS_rot_inertia =    izero
!>        start address for divergence of SGS inertia term
        integer (kind=kint) :: i_SGS_div_inertia =    izero
!>        start address for curl of SGS inertia term
        integer (kind=kint) :: i_SGS_rot_Lorentz =    izero
!>        start address for divergence of SGS inertia term
        integer (kind=kint) :: i_SGS_div_Lorentz =    izero
!
!
!      SGS terms by wider filter
!
!>        start address for SGS heat flux by wider filter
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
        integer (kind=kint) :: i_wide_SGS_h_flux =      izero
!>        start address for SGS compositional flux by wider filter
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
        integer (kind=kint) :: i_wide_SGS_c_flux =      izero
!>        start address for SGS inertia term  by wider filter
        integer (kind=kint) :: i_wide_SGS_inertia =    izero
!>        start address for SGS Lorentz force wih wider filter
!!         @f$ \partial_{i} \left( \overline{B_{i}B_{j}}
!!            - \bar{B}_{i}\bar{B}_{j} \right) @f$
        integer (kind=kint) :: i_wide_SGS_Lorentz =     izero
!>        start address for SGS induction for vector potential by wider filter
!!         @f$e_{ijk}\left(\overline{u_{j}B_{k}} - \bar{u}_{j}\bar{B}_{k} \right) @f$
        integer (kind=kint) :: i_wide_SGS_vp_induct =   izero
!
!
!      SGS terms by double filtering
!
!>        start address for SGS heat flux with wider filter
!!         @f$ \overline{\overline{u_{i}T}}
!!            - \bar{\bar{u}}_{i}\bar{\bar{T}} @f$
        integer (kind=kint) :: i_dbl_SGS_h_flux =      izero
!>        start address for SGS composition flux with wider filter
!!         @f$ \overline{\overline{u_{i}C}}
!!            - \bar{\bar{u}}_{i}\bar{\bar{C}} @f$
        integer (kind=kint) :: i_dbl_SGS_c_flux =      izero
!>        start address for SGS inertia term with wider filter
!!         @f$ e_{ijk}\left(\overline{\overline{\omega_{j}u_{k}}}
!!            - \bar{\bar{\omega}}_{j}\bar{\bar{u}}_{k} \right) @f$
        integer (kind=kint) :: i_dbl_SGS_inertia =    izero
!>        start address for SGS Lorentz force with wider filter
!!         @f$ e_{ijk}\left(\overline{\overline{\J_{j}B_{k}}}
!!            - \bar{\bar{J}}_{j}\bar{\bar{B}}_{k} \right) @f$
        integer (kind=kint) :: i_dbl_SGS_Lorentz =     izero
!>        start address for SGS induction with wider filter
!!         @f$ e_{ijk}\left(\overline{\overline{\u_{j}B_{k}}}
!!            - \bar{\bar{u}}_{j}\bar{\bar{B}}_{k} \right) @f$
        integer (kind=kint) :: i_dbl_SGS_vp_induct =   izero
!
!   model coefficient
!
!>        start address for model coefficient of SGS heat flux
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
        integer (kind=kint) :: i_Csim_SGS_h_flux =      izero
!>        start address for model coefficient of SGS compositional flux
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
        integer (kind=kint) :: i_Csim_SGS_c_flux =      izero
!>        start address for model coefficient of SGS momentum flux
!!         @f$ \overline{u_{i}u_{j}} - \bar{u}_{i}\bar{u}_{j} @f$
        integer (kind=kint) :: i_Csim_SGS_m_flux =      izero
!>        start address for model coefficient of SGS Maxwell tensor
!!         @f$ \overline{B_{i}B_{j}} - \bar{B}_{i}\bar{B}_{j} @f$
        integer (kind=kint) :: i_Csim_SGS_Lorentz =     izero
!>        start address for model coefficient of SGS magnetic induction
        integer (kind=kint) :: i_Csim_SGS_induction =    izero
!>        start address for model coefficient of SGS buoyancy
        integer (kind=kint) :: i_Csim_SGS_buoyancy =    izero
!>        start address for model coefficient of SGS composition buoyancy
        integer (kind=kint) :: i_Csim_SGS_comp_buo =     izero
!
!
!>        start address for divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}T} - \bar{u}_{i}\bar{T} \right) @f$
        integer (kind=kint) :: i_SGS_div_h_flux=   izero
!>        start address for divergence of SGS composition flux
!!         @f$ \partial_{i} \left( \overline{u_{i}C} - \bar{u}_{i}\bar{C} \right) @f$
        integer (kind=kint) :: i_SGS_div_c_flux=   izero
!>        start address for divergence of SGS momentum flux
!!         @f$ \partial_{i} \left( \overline{u_{i}u_{j}} - \bar{u}_{i}\bar{u}_{j} \right) @f$
        integer (kind=kint) :: i_SGS_div_m_flux=   izero
!>        start address for divergence of SGS magnetic induction tensor
        integer (kind=kint) :: i_SGS_induction =   izero
!>        start address for SGS induction for vector potential
!!         @f$e_{ijk}\left(\overline{u_{j}B_{k}} - \bar{u}_{j}\bar{B}_{k} \right) @f$
        integer (kind=kint) :: i_SGS_vp_induct =   izero
!
!>        start address for SGS buoyancy
        integer (kind=kint) :: i_SGS_buoyancy =   izero
!>        start address for SGS compositional buoyancy
        integer (kind=kint) :: i_SGS_comp_buo =   izero
!
!
!>        start address for true divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}T} - \bar{u}_{i}\bar{T} \right) @f$
        integer (kind=kint) :: i_SGS_div_hf_true = izero
!>        start address for true divergence of SGS compostion flux
!!         @f$ \partial_{i} \left( \overline{u_{i}C} - \bar{u}_{i}\bar{C} \right) @f$
        integer (kind=kint) :: i_SGS_div_cf_true = izero
!>        start address for true divergence of SGS momentum flux
!!         @f$ \partial_{i} \left( \overline{u_{i}u_{j}} - \bar{u}_{i}\bar{u}_{j} \right) @f$
        integer (kind=kint) :: i_SGS_div_mf_true = izero
!>        start address for true divergence of SGS Maxwell tensor
!!         @f$ \partial_{i} \left( \overline{B_{i}B_{j}} - \bar{B}_{i}\bar{B}_{j} \right) @f$
        integer (kind=kint) :: i_SGS_Lor_true =    izero
!>        start address for true divergence
!>            of SGS magnetic induction tensor
        integer (kind=kint) :: i_SGS_idct_true =   izero
!
!>        start address for temperature generation by true SGS heat flux
        integer (kind=kint) :: i_SGS_t_gen_tr =    izero
!>        start address for temperature generation by true SGS compostion flux
        integer (kind=kint) :: i_SGS_c_gen_tr =    izero
!>        start address for energy flux of true SGS induction
        integer (kind=kint) :: i_SGS_me_gen_tr =   izero
!>        start address for work of true SGS Lorentz force
        integer (kind=kint) :: i_SGS_Lor_wk_tr =   izero
!>        start address for work of true SGS Reynolds stress
        integer (kind=kint) :: i_reynolds_wk_tr =  izero
!
!
!>        start address for temperature generation by SGS heat flux
        integer (kind=kint) :: i_SGS_temp_gen =    izero
!>        start address for energy flux of SGS induction
        integer (kind=kint) :: i_SGS_me_gen =      izero
!>        start address for work of SGS Lorentz force
        integer (kind=kint) :: i_SGS_Lor_wk =      izero
!>        start address for work of SGS Reynolds stress
        integer (kind=kint) :: i_reynolds_wk =     izero
!
!>        start address for work of SGS buoyancy
        integer (kind=kint) :: i_SGS_buo_wk =      izero
!>        start address for work of SGS compositional buoyancy
        integer (kind=kint) :: i_SGS_comp_buo_wk = izero
!
!
!>        Field address for geostrophic balance
!!         @f$ -2 e_{ijk} \Omega_{j} u_{k} + \partial_{i} p @f$
      integer (kind=kint) :: i_geostrophic = izero
!
!>        Field address for heat flux
!!         @f$ u_{i} T + (\overline{u_{i}T} - \bar{u}_{i}\bar{T}) @f$
      integer (kind=kint) :: i_h_flux_w_sgs = izero
!>        Field address for compositinoal flux
!!         @f$ u_{i} C + (\overline{u_{i}C} - \bar{u}_{i}\bar{C}) @f$
      integer (kind=kint) :: i_c_flux_w_sgs = izero
!
!>        Field address for advection for momentum
!!         @f$ u_{j} \partial_{j} u_{i}
!!           + e_{ijk}\left(\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k} \right) @f$
      integer (kind=kint) :: i_inertia_w_sgs = izero
!>        Field address for Lorentz force
!!         @f$ e_{ijk} J_{j} B_{k}
!!           + e_{ijk}\left(\overline{B{j}u_{k}}
!!            - \bar{J}_{j}\bar{B}_{k} \right) @f$
      integer (kind=kint) :: i_Lorentz_w_sgs = izero
!
!>        Field address for inductino for vector potential
!!         @f$ e_{ijk} u_{j} B_{k} @f$
!!           + e_{ijk}\left(\overline{u{j}B_{k}}
!!            - \bar{u}_{j}\bar{B}_{k} \right) @f$
      integer (kind=kint) :: i_vp_induct_w_sgs = izero
!>        Field address for magnetic induction
!!         @f$ e_{ijk} \partial_{j}\left(e_{klm}u_{l}B_{m} \right)@f$
!!           + e_{ijk} \partial_{j}(e_{klm}\left(\overline{u{l}B_{m}}
!!                              - \bar{u}_{l}\bar{B}_{m} \right)) @f$
      integer (kind=kint) :: i_mag_induct_w_sgs = izero
!
!>        Field address for momentum flux
!!         @f$ u_{i} u_{j}
!!            + (\overline{u_{i}u_{j}} - \bar{u}_{i}\bar{u}_{j})@f$
      integer (kind=kint) :: i_mom_flux_w_sgs = izero
!>        Field address for momentum flux
!!         @f$ B_{i} B_{j}
!!            + (\overline{B_{i}B_{j}} - \bar{B}_{i}\bar{B}_{j})@f$
      integer (kind=kint) :: i_maxwell_t_w_sgs = izero
!
!  Square of each component of fields
!
!>        Square of velocity @f$ u_{i}^{2} @f$
      integer (kind=kint) :: i_square_v = izero
!>        Square of vorticity @f$ \omega_{i}^{2} @f$
      integer (kind=kint) :: i_square_w = izero
!>        Square of magnetic field @f$ B_{i}^{2} @f$
      integer (kind=kint) :: i_square_b = izero
!>        Square of magnetic vector potential @f$ A_{i}^{2} @f$
      integer (kind=kint) :: i_square_a = izero
!>        Square of current density @f$ J_{i}^{2} @f$
      integer (kind=kint) :: i_square_j = izero
!>        Square of temperature @f$ T^{2} @f$
      integer (kind=kint) :: i_square_t = izero
!>        Square of composition @f$ C^{2} @f$
      integer (kind=kint) :: i_square_c = izero
!
!    Gradient of fields
!>        start address for gradient of @f$ u_{x} @f$
        integer (kind=kint) :: i_grad_vx = izero
!>        start address for gradient of @f$ u_{y} @f$
        integer (kind=kint) :: i_grad_vy = izero
!>        start address for gradient of @f$ u_{z} @f$
        integer (kind=kint) :: i_grad_vz = izero
!>        start address for gradient of @f$ \omega_{x} @f$
        integer (kind=kint) :: i_grad_wx = izero
!>        start address for gradient of @f$ \omega_{y} @f$
        integer (kind=kint) :: i_grad_wy = izero
!>        start address for gradient of @f$ \omega_{z} @f$
        integer (kind=kint) :: i_grad_wz = izero
!>        start address for gradient of @f$ A_{x} @f$
        integer (kind=kint) :: i_grad_ax = izero
!>        start address for gradient of @f$ A_{y} @f$
        integer (kind=kint) :: i_grad_ay = izero
!>        start address for gradient of @f$ A_{z} @f$
        integer (kind=kint) :: i_grad_az = izero
!>        start address for gradient of @f$ B_{x} @f$
        integer (kind=kint) :: i_grad_bx = izero
!>        start address for gradient of @f$ B_{y} @f$
        integer (kind=kint) :: i_grad_by = izero
!>        start address for gradient of @f$ B_{z} @f$
        integer (kind=kint) :: i_grad_bz = izero
!>        start address for gradient of @f$ J_{x} @f$
        integer (kind=kint) :: i_grad_jx = izero
!>        start address for gradient of @f$ J_{y} @f$
        integer (kind=kint) :: i_grad_jy = izero
!>        start address for gradient of @f$ J_{z} @f$
        integer (kind=kint) :: i_grad_jz = izero
!
!>        start address for gradient of @f$ T @f$
        integer (kind=kint) :: i_grad_t =           izero
!>        start address for gradient of @f$ \Theta @f$
        integer (kind=kint) :: i_grad_part_t =      izero
!>        start address for gradient of @f$ C @f$
        integer (kind=kint) :: i_grad_composit =    izero
!>        start address for gradient of perturbation of composition
        integer (kind=kint) :: i_grad_part_c =      izero
!
!
!    Gradient of filtered fields
!>        start address for gradient of @f$ \tilde{u}_{x} @f$
        integer (kind=kint) :: i_grad_filter_vx = izero
!>        start address for gradient of @f$ \tilde{u}_{y} @f$
        integer (kind=kint) :: i_grad_filter_vy = izero
!>        start address for gradient of @f$ \tilde{u}_{z} @f$
        integer (kind=kint) :: i_grad_filter_vz = izero
!>        start address for gradient of @f$ \tilde{\omega}_{x} @f$
        integer (kind=kint) :: i_grad_filter_wx = izero
!>        start address for gradient of @f$ \tilde{\omega}_{y} @f$
        integer (kind=kint) :: i_grad_filter_wy = izero
!>        start address for gradient of @f$ \tilde{\omega}_{z} @f$
        integer (kind=kint) :: i_grad_filter_wz = izero
!>        start address for gradient of @f$ \tilde{A}_{x} @f$
        integer (kind=kint) :: i_grad_filter_ax = izero
!>        start address for gradient of @f$ \tilde{A}_{y} @f$
        integer (kind=kint) :: i_grad_filter_ay = izero
!>        start address for gradient of @f$ \tilde{A}_{z} @f$
        integer (kind=kint) :: i_grad_filter_az = izero
!>        start address for gradient of @f$ \tilde{B}_{x} @f$
        integer (kind=kint) :: i_grad_filter_bx = izero
!>        start address for gradient of @f$ \tilde{B}_{y} @f$
        integer (kind=kint) :: i_grad_filter_by = izero
!>        start address for gradient of @f$ \tilde{B}_{z} @f$
        integer (kind=kint) :: i_grad_filter_bz = izero
!>        start address for gradient of @f$ \tilde{J}_{x} @f$
        integer (kind=kint) :: i_grad_filter_jx = izero
!>        start address for gradient of @f$ \tilde{J}_{y} @f$
        integer (kind=kint) :: i_grad_filter_jy = izero
!>        start address for gradient of @f$ \tilde{J}_{z} @f$
        integer (kind=kint) :: i_grad_filter_jz = izero
!
!>        start address for gradient of @f$ \tilde{T} @f$
        integer (kind=kint) :: i_grad_filter_temp = izero
!>        start address for gradient of @f$ \tilde{C} @f$
        integer (kind=kint) :: i_grad_filter_comp = izero
!
!
!
!>        start address for SGS term by scale similarity method
        integer (kind=kint) :: i_sgs_simi =        izero
!>        start address for SGS term by nonlinear gradient method
        integer (kind=kint) :: i_sgs_grad =        izero
!>        start address for SGS term by nonlinear gradient method
!!        using fileterd field
        integer (kind=kint) :: i_sgs_grad_f =      izero
!>        start address for SGS term by turbulence diffusivity
        integer (kind=kint) :: i_sgs_diffuse =     izero
!
!>        start address for temperature to obatin commutation error
        integer (kind=kint) :: i_sgs_temp =        izero
!>        start address for composition variation
!!        to obatin commutation error
        integer (kind=kint) :: i_sgs_composit =    izero
!
!  wider filtered field
!
!>        start address for filtered velocity by wider filter
        integer (kind=kint) :: i_wide_fil_velo  =  izero
!>        start address for filtered vorticity by wider filter
        integer (kind=kint) :: i_wide_fil_vort  =  izero
!>        start address for filtered magnetic field by wider filter
        integer (kind=kint) :: i_wide_fil_magne =  izero
!>        start address for filtered magnetic vector potential
!!        by wider filter
        integer (kind=kint) :: i_wide_fil_vecp =     izero
!>        start address for filtered current density by wider filter
        integer (kind=kint) :: i_wide_fil_current =  izero
!
!>        start address for filtered temperature by wider filter
        integer (kind=kint) :: i_wide_fil_temp  =  izero
!>        start address for filtered grad. of temperature
!!        by wider filter
        integer (kind=kint) :: i_wide_fil_grad_t  =  izero
!
!>        start address for filtered composition by wider filter
        integer (kind=kint) :: i_wide_fil_comp  =  izero
!>        start address for filtered grad. of composition
!!        by wider filter
        integer (kind=kint) :: i_wide_fil_grad_c  =  izero
!
!  double filtered field
!
!>        start address for filtered velocity by double filtering
        integer (kind=kint) :: i_dbl_fil_velo  =  izero
!>        start address for filtered vorticity by double filtering
        integer (kind=kint) :: i_dbl_fil_vort  =  izero
!>        start address for filtered magnetic vector potential
!!        by double filtering
        integer (kind=kint) :: i_dbl_fil_vecp =     izero
!>        start address for filtered magnetic field by double filtering
        integer (kind=kint) :: i_dbl_fil_magne =  izero
!>        start address for filtered current density by double filtering
        integer (kind=kint) :: i_dbl_fil_current =  izero
!
!>        start address for filtered temperature by double filtering
        integer (kind=kint) :: i_dbl_fil_temp  =  izero
!>        start address for filtered grad. of temperature
!!        by double filtering
        integer (kind=kint) :: i_dbl_fil_grad_t  =  izero
!
!>        start address for filtered compostiion by double filtering
        integer (kind=kint) :: i_dbl_fil_comp  =  izero
!>        start address for filtered grad. of composition
!>        by double filtering
        integer (kind=kint) :: i_dbl_fil_grad_c  =  izero
!
!
!  divergence of momentum equations
!
!>        start address for divergence of advection
        integer (kind=kint) :: i_div_inertia =    izero
!>        start address for divergence of Lorentz force
        integer (kind=kint) :: i_div_Lorentz =    izero
!>        start address for divergence of Coriolis force
        integer (kind=kint) :: i_div_Coriolis =   izero
!>        start address for divergence of buoyancy
        integer (kind=kint) :: i_div_buoyancy =   izero
!>        start address for divergence of compositional buoyancy
        integer (kind=kint) :: i_div_comp_buo =   izero
!>        start address for divergence of filtered buoyancy
        integer (kind=kint) :: i_div_filter_buo = izero
!>        start address for divergence of viscousity
        integer (kind=kint) :: i_div_viscous =    izero
!
!  curl of momentum equations
!
!>        start address for curl of advection
        integer (kind=kint) :: i_rot_inertia =  izero
!>        start address for curl of Lorentz force
        integer (kind=kint) :: i_rot_Lorentz =  izero
!>        start address for curl of Coriolis term
        integer (kind=kint) :: i_rot_Coriolis = izero
!>        start address for curl of buoyancy
        integer (kind=kint) :: i_rot_buoyancy = izero
!>        start address for curl of compositional buoyancy
        integer (kind=kint) :: i_rot_comp_buo = izero
!>        start address for curl of filtered buoyancy
        integer (kind=kint) :: i_rot_filter_buo = izero
!
!  arrays for current forces
!
!>        start address for total forces
        integer (kind=kint) :: i_forces =       izero
!>        start address for curl of total forces
        integer (kind=kint) :: i_rot_forces =   izero
!>        start address for divergence of total forces
        integer (kind=kint) :: i_div_forces =   izero
!
!  arrays for previous evolution
!
!>        start address for explicit term for momentum at previous step
        integer (kind=kint) :: i_pre_mom =      izero
!>        start address for explicit term for induction at previous step
        integer (kind=kint) :: i_pre_uxb =      izero
!>        start address for explicit term for heat at previous step
        integer (kind=kint) :: i_pre_heat =     izero
!>        start address for explicit term for composition
!!        at previous step
        integer (kind=kint) :: i_pre_composit = izero
!>        start address for explicit term for pressure at previous step
        integer (kind=kint) :: i_pre_press =    izero
!
!  arrays for evolution check
!
!>        start address for explicit term for momentum for check
        integer (kind=kint) :: i_chk_mom =       izero
!>        start address for explicit term for induction for check
        integer (kind=kint) :: i_chk_uxb =       izero
!>        start address for explicit term for heat for check
        integer (kind=kint) :: i_chk_heat =      izero
!>        start address for explicit term for composition
!!        for check
        integer (kind=kint) :: i_chk_composit =  izero
!>        start address for explicit term for pressure for check
        integer (kind=kint) :: i_chk_press =     izero
!>        start address for explicit term for electric potential
!!        for check
        integer (kind=kint) :: i_chk_potential = izero
!
!>        start address for explicit term for momentum for 2nd check
        integer (kind=kint) :: i_chk_mom_2 =       izero
!>        start address for explicit term for induction for 2nd check
        integer (kind=kint) :: i_chk_uxb_2 =       izero
!>        start address for explicit term for heat for 2nd check
        integer (kind=kint) :: i_chk_heat_2 =      izero
!>        start address for explicit term for composition
!!        for 2nd check
        integer (kind=kint) :: i_chk_composit_2 =  izero
!>        start address for explicit term for pressure for 2nd check
        integer (kind=kint) :: i_chk_press_2 =     izero
!>        start address for explicit term for electric potential
!!        for 2nd check
        integer (kind=kint) :: i_chk_potential_2 = izero
!
!
!>        start address for velocity length scale
        integer (kind=kint) :: i_velo_scale =       izero
!>        start address for magnetic field length scale
        integer (kind=kint) :: i_magne_scale =      izero
!>        start address for temperature length scale
        integer (kind=kint) :: i_temp_scale =       izero
!>        start address for composition length scale
        integer (kind=kint) :: i_comp_scale =       izero
!
!
!>        start address for viscosity   @f$ \mu @f$
        integer (kind=kint) :: i_viscosity =   izero
!>        start address for thermal diffusivity @f$ k @f$
        integer (kind=kint) :: i_T_conductivity =   izero
!
!>        start address for kinetic viscosity
        integer (kind=kint) :: i_K_viscosity =   izero
!>        start address for thermal diffusivity
        integer (kind=kint) :: i_T_diffusivity = izero
!>        start address for chemical diffusivity
        integer (kind=kint) :: i_C_diffusivity = izero
!>        start address for magnetic diffusivity
        integer (kind=kint) :: i_B_diffusivity = izero
!
!>        start address for rotation of ststem @f$ Omega @f$
!!          i_omega:   poloidal component
!!          i_omega+1: radial derivative of poloidal component
!!          i_omega+2: 2nd radial derivative of poloidal component
        integer (kind=kint) :: i_omega = izero
!
!>        start address for background magnetic field @f$ B_{0} @f$
!!          i_back_B:   poloidal component
!!          i_back_B+1: radial derivative of poloidal component
!!          i_back_B+2: 2nd radial derivative of poloidal component
        integer (kind=kint) :: i_back_B = izero
      end type phys_address
!
!
      end module t_phys_address
