!>@file   t_energy_flux_labels.f90
!!        module t_energy_flux_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      subroutine set_enegy_fluxes_addresses                           &
!!     &         (i_phys, field_name, base_force, flag)
!!        type(energy_flux_address), intent(inout) :: ene_flux
!! !!!!!  energy flux names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names  [address]
!!
!!   inertia_work             [i_m_advect_work]:  Work of Reynolds stress
!!                                           -u \cdot (\omega \times u)
!!   Lorentz_work             [i_ujb]:  Work of Lorentz force
!!                                            u \cdot (J \times B)
!!   work_against_Lorentz     [i_nega_ujb]:  Work against Lorentz force
!!                                           -u \cdot (J \times B)
!!   mag_tension_work         [i_m_tension_wk]: Work of magnetic tension
!!                                            u \cdot( (B \nabla) B)
!!   buoyancy_flux            [i_buo_gen]:       Thermal buoyancy flux
!!                                           -u \cdot (\alpha_{T} g T)
!!   composite_buoyancy_flux  [i_c_buo_gen]:  Compositional buoyancy flux
!!                                           -u \cdot (\alpha_{C} g C)
!!
!!   magnetic_ene_generation  [i_me_gen]:
!!           energy flux by magneitic induction
!!                              B \cdot (\nabla \times (u \times B))
!!   magnetic_stretch_flux  [i_mag_stretch_flux]:
!!           energy flux by magneitic streatch  B \cdot ((B \nabla) u)
!!
!!   temp_generation       [i_temp_gen]: heat advection flux
!!                                     T (u \cdot \nabla) T
!!   pert_temp_generation  [i_par_t_gen]:
!!           perturbation of heat advection flux
!!                                     \Theta (u \cdot \nabla) \Theta
!!   comp_generation       [i_comp_gen]:    composition advection flux 
!!                                     C (u \cdot \nabla) C
!!   pert_comp_generation  [i_par_c_gen]:
!!          perturbation of composition advection flux
!!                                     C (u \cdot \nabla) (C-C_0)
!!
!!   viscous_ene_diffusion   [i_vis_e_diffuse]:
!!         Energy dissipation by Viscousity   u ( \nabla^{2} u)
!!   magnetic_ene_diffusion  [i_mag_e_diffuse]:
!!         Energy dissipation by Ohmic dissipation  B ( \nabla^{2} B)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_energy_flux_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!>       Structure of start address of base forces
      type energy_flux_address
!>        Field address of work of inertia
!!         @f$ u_{i} (u_{j} \partial_{j} u_{i}) @f$
        integer (kind=kint) :: i_m_advect_work =   izero
!>        Field address of work against Lorentz force
!!         @f$ - u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
        integer (kind=kint) :: i_nega_ujb =        izero
!>        Field address of work of Lorentz force
!!         @f$ u_{i} \left( e_{ijk} J_{j} B_{k} \right) @f$
        integer (kind=kint) :: i_ujb =             izero
!>        Field address of work of magnetic tension
!!         @f$ u_{i} (B_{j} \partial_{j}) B_{i} @f$
        integer (kind=kint) :: i_m_tension_wk  =   izero
!>        Field address of buoyancy flux
!!         @f$ -u_{i} \alpha_{T} g_{i} T @f$
        integer (kind=kint) :: i_buo_gen =         izero
!>        Field address of compositional buoyancy flux
!!         @f$ -u_{i} \alpha_{c} g_{i} C @f$
        integer (kind=kint) :: i_c_buo_gen =       izero
!!
!>        Field address of magnetic energy flux
!>       @f$ B_{i}e_{ijk} \partial_{j} \left(e_{klm}u_{l}B_{m}\right) @f$
        integer (kind=kint) :: i_me_gen =           izero
!>        Field label of energy flux of magnetic stretch term
!!       @f$ B_{i} \left(B_{j} \partial_{j} u_{i} \right)@f$
        integer (kind=kint) :: i_mag_stretch_flux = izero
!
!>        Field address of temperature flux
!!         @f$ T (u_{i} \partial_{i}) T @f$
        integer (kind=kint) :: i_temp_gen =          izero
!>        Field address of perturbation temperature flux
!!         @f$ \Theta (u_{i} \partial_{i}) \Theta @f$
        integer (kind=kint) :: i_par_t_gen =         izero
!>        Field address of composition flux
!!         @f$ C (u_{i} \partial_{i}) @f$
        integer (kind=kint) :: i_comp_gen =          izero
!>        Field address of perturbation composition flux
!!         @f$ (C - C_0) (u_{i} \partial_{i}) (C - C_0) @f$
        integer (kind=kint) :: i_par_c_gen =         izero
!
!>        Field address of energy flux by viscous diffusion
!!         @f$ u_{i} \left( \partial_{j}\partial_{j} u_{i} \right) @f$
        integer (kind=kint) :: i_vis_e_diffuse =   izero
!>        Field address of energy flux by magnetic diffusion
!!         @f$ B_{i} \left( \partial_{j}\partial_{j} B_{i} \right) @f$
        integer (kind=kint) :: i_mag_e_diffuse =   izero
      end type energy_flux_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_enegy_fluxes_addresses                             &
     &         (i_phys, field_name, ene_flux, flag)
!
      use m_energy_flux_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: ene_flux
      logical, intent(inout) :: flag
!
!
      flag = check_enegy_fluxes(field_name)
      if(flag) then
        if (field_name .eq. inertia_work%name) then
          ene_flux%i_m_advect_work = i_phys
        else if (field_name .eq. work_against_Lorentz%name) then
          ene_flux%i_nega_ujb =      i_phys
        else if (field_name .eq. Lorentz_work%name) then
          ene_flux%i_ujb =           i_phys
        else if (field_name .eq. mag_tension_work%name) then
          ene_flux%i_m_tension_wk =  i_phys
!
        else if (field_name .eq. buoyancy_flux%name) then
          ene_flux%i_buo_gen =       i_phys
        else if (field_name .eq. composite_buoyancy_flux%name) then
          ene_flux%i_c_buo_gen =     i_phys
!
        else if (field_name .eq. magnetic_ene_generation%name) then
          ene_flux%i_me_gen =           i_phys
        else if (field_name .eq. magnetic_stretch_flux%name) then
          ene_flux%i_mag_stretch_flux = i_phys
!
        else if (field_name .eq. temp_generation%name) then
          ene_flux%i_temp_gen =  i_phys
        else if (field_name .eq. pert_temp_generation%name) then
          ene_flux%i_par_t_gen = i_phys
!
        else if (field_name .eq. comp_generation%name) then
          ene_flux%i_comp_gen =  i_phys
        else if (field_name .eq. pert_comp_generation%name) then
          ene_flux%i_par_c_gen = i_phys
!
        else if (field_name .eq. viscous_ene_diffusion%name) then
          ene_flux%i_vis_e_diffuse = i_phys
        else if (field_name .eq. magnetic_ene_diffusion%name) then
          ene_flux%i_mag_e_diffuse = i_phys
        end if
      end if
!
      end subroutine set_enegy_fluxes_addresses
!
! ----------------------------------------------------------------------
!
      end module t_energy_flux_labels
