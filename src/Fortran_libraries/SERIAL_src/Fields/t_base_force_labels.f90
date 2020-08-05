!>@file   t_base_force_labels.f90
!!        module t_base_force_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief  Addresses for basic forces
!!
!!@verbatim
!!      subroutine set_base_force_addresses                             &
!!     &         (i_phys, field_name, forces, flag)
!!        type(base_force_address), intent(inout) :: forces
!!      subroutine set_rot_force_addresses                              &
!!     &         (i_phys, field_name, rot_forces, flag)
!!        type(base_force_address), intent(inout) :: rot_forces
!!      subroutine set_div_force_addresses                              &
!!     &         (i_phys, field_name, div_force, flag)
!!        type(base_force_address), intent(inout) :: div_force
!!
!! !!!!!  Base force names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   pressure_gradient  [i_press_grad]:     pressure gradient    u
!!   inertia    [i_m_advect]:        inertia (\omega \times u)
!!   Lorentz_force   [i_lorentz]:  Lorentz force     J \times B
!!   magnetic_tension  [i_m_tension]:  magnetic tension   (B \nabla) B
!!   Coriolis_force   [i_coriolis]:  Coriolis force     2 \Omega \times u
!!   buoyancy   [i_buoyancy]:   Thermal buoyancy       - \alpha_{T} g T
!!   composite_buoyancy   [i_comp_buo]:
!!                       compositional buoyancy  - \alpha_{C} g C
!!
!!   vecp_induction   [i_vp_induct]:     induction         u \times B
!!   magnetic_induction   [i_induction]:
!!                         magneitic induction \nabla \times (u \times B)
!!   magnetic_stretch    [i_mag_stretch]:
!!                         magneitic streatch         (B \nabla) u
!!
!!   heat_advect         [i_h_advect]:  heat advection
!!                                     (u \cdot \nabla) T
!!   pert_heat_advect    [i_ph_advect]:  perturbation of heat advection
!!                                      (u \cdot \nabla) \Theta
!!   composition_advect [i_c_advect]:    composition advection
!!                                      (u \cdot \nabla) C
!!   pert_comp_advect   [i_pc_advect]:
!!                     perturbation of composition advection
!!                                      (u \cdot \nabla) (C-C_0)
!!
!!   momentum_flux   [i_m_flux]:  momentum flux     u_{i} u_{j}
!!   maxwell_tensor    [i_maxwell]:  maxwell tensor       B_{i} B_{j}
!!   induction_tensor    [i_induct_t]:    induction induction tensor
!!                                 u_{i} B_{j}  - B_{i} u_{J}
!!   heat_flux           [i_h_flux]:    heat flux          uT
!!   pert_heat_flux      [i_ph_flux]:  perturbation of heat flux 
!!                                    u\Theta
!!   composite_flux     [i_c_flux]:    composition flux         uC
!!   part_c_flux        [i_pc_flux]:  perturbation of composition flux
!!                                      u(C-C_0)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_base_force_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
!>       Structure for start address for base forces
      type base_force_address
!>        Start address for pressure gradient
!!         @f$ \partial_{i} p @f$
        integer (kind=kint) :: i_press_grad  =     izero
!>        start address for advection for momentum
!!         @f$ u_{j} \partial_{j} u_{i} @f$
        integer (kind=kint) :: i_m_advect =        izero
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
!
!>        start address for magnetic induction
!!         @f$ e_{ijk} \partial_{j}\left(e_{klm}u_{l}B_{m} \right)@f$
        integer (kind=kint) :: i_induction =       izero
!>        start address for induction for vector potential
!!         @f$ e_{ijk} u_{j} B_{k} @f$
        integer (kind=kint) :: i_vp_induct =       izero
!>        start address for magnetic stretch term
!!         @f$ B_{j} \partial_{j} u_{i} @f$
        integer (kind=kint) :: i_mag_stretch =     izero
!
!>        start address for advection for temperature
!!         @f$ u_{i} \partial_{i} T @f$
        integer (kind=kint) :: i_h_advect =        izero
!>        start address for advection for perturbation of temperature
!!         @f$ u_{i} \partial_{i} \Theta @f$
        integer (kind=kint) :: i_ph_advect =       izero
!>        start address for advection for composition
!!         @f$ u_{i} \partial_{i} C @f$
        integer (kind=kint) :: i_c_advect =        izero
!>        start address for advection for perturbation of composition
!!         @f$ u_{i} \partial_{i} \Theta_{C} @f$
        integer (kind=kint) :: i_pc_advect =       izero
!!
!>        start address for momentum flux
!!         @f$ u_{i} u_{j} @f$
        integer (kind=kint) :: i_m_flux =          izero
!>        start address for Maxwell tensor
!!         @f$ B_{i} B_{j} @f$
        integer (kind=kint) :: i_maxwell =         izero
!>        start address for TEnsor for magnetic induction
!!         @f$ u_{i} B_{j}  - B_{i} u_{J} @f$
        integer (kind=kint) :: i_induct_t =        izero
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
      end type base_force_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_base_force_addresses                               &
     &         (i_phys, field_name, forces, flag)
!
      use m_base_force_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: forces
      logical, intent(inout) :: flag
!
!
      flag = check_force_vectors(field_name)                            &
     &      .or. check_scalar_advection(field_name)                     &
     &      .or. check_flux_tensors(field_name)                         &
     &      .or. check_asym_flux_tensors(field_name)
      if(flag) then
        if (field_name .eq. pressure_gradient%name) then
          forces%i_press_grad = i_phys
        else if (field_name .eq. inertia%name) then
          forces%i_m_advect =   i_phys
        else if (field_name .eq. Coriolis_force%name) then
          forces%i_coriolis =   i_phys
        else if (field_name .eq. Lorentz_force%name) then
          forces%i_lorentz =    i_phys
        else if (field_name .eq. magnetic_tension%name) then
          forces%i_m_tension =  i_phys
!
        else if (field_name .eq. buoyancy%name) then
          forces%i_buoyancy =   i_phys
        else if (field_name .eq. composite_buoyancy%name) then
          forces%i_comp_buo =   i_phys
!
        else if (field_name .eq. vecp_induction%name) then
          forces%i_vp_induct =   i_phys
        else if (field_name .eq. magnetic_induction%name) then
          forces%i_induction =   i_phys
        else if (field_name .eq. magnetic_stretch%name) then
          forces%i_mag_stretch = i_phys
!
        else if (field_name .eq. heat_advect%name) then
          forces%i_h_advect =  i_phys
        else if (field_name .eq. pert_heat_advect%name) then
          forces%i_ph_advect = i_phys
!
        else if (field_name .eq. composition_advect%name) then
          forces%i_c_advect =  i_phys
        else if (field_name .eq. pert_comp_advect%name) then
          forces%i_pc_advect = i_phys
!
        else if (field_name .eq. momentum_flux%name ) then
          forces%i_m_flux =     i_phys
        else if (field_name .eq. maxwell_tensor%name ) then
          forces%i_maxwell =    i_phys
        else if (field_name .eq. induction_tensor%name ) then
          forces%i_induct_t =    i_phys
!
        else if (field_name .eq. heat_flux%name) then
          forces%i_h_flux =    i_phys
        else if (field_name .eq. pert_heat_flux%name) then
          forces%i_ph_flux =   i_phys
        else if (field_name .eq. composite_flux%name) then
          forces%i_c_flux =    i_phys
        else if (field_name .eq. pert_comp_flux%name) then
          forces%i_pc_flux =   i_phys
        end if
      end if
!
      end subroutine set_base_force_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_rot_force_addresses                                &
     &         (i_phys, field_name, rot_forces, flag)
!
      use m_rot_force_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: rot_forces
      logical, intent(inout) :: flag
!
!
      flag = check_rot_force(field_name)
      if(flag) then
        if (field_name .eq. rot_inertia%name) then
          rot_forces%i_m_advect =   i_phys
        else if (field_name .eq. rot_Coriolis_force%name) then
          rot_forces%i_Coriolis =   i_phys
        else if (field_name .eq. rot_Lorentz_force%name) then
          rot_forces%i_lorentz =    i_phys
!
        else if (field_name .eq. rot_buoyancy%name) then
          rot_forces%i_buoyancy =   i_phys
        else if (field_name .eq. rot_composite_buoyancy%name) then
          rot_forces%i_comp_buo =   i_phys
        end if
      end if
!
      end subroutine set_rot_force_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_div_force_addresses                                &
     &         (i_phys, field_name, div_forces, flag)
!
      use m_div_force_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: div_forces
      logical, intent(inout) :: flag
!
!
      flag = check_div_force(field_name)                                &
     &      .or. check_div_flux_tensor(field_name)                      &
     &      .or. check_div_scalar_flux(field_name)
      if(flag) then
        if (field_name .eq. div_inertia%name) then
          div_forces%i_m_advect =   i_phys
        else if (field_name .eq. div_Coriolis_force%name) then
          div_forces%i_Coriolis =   i_phys
        else if (field_name .eq. div_Lorentz_force%name) then
          div_forces%i_lorentz =    i_phys
!
        else if (field_name .eq. div_buoyancy%name) then
          div_forces%i_buoyancy =   i_phys
        else if (field_name .eq. div_composite_buoyancy%name) then
          div_forces%i_comp_buo =   i_phys
!
        else if (field_name .eq. div_heat_flux%name) then
          div_forces%i_h_flux =    i_phys
        else if (field_name .eq. div_pert_heat_flux%name) then
          div_forces%i_ph_flux =   i_phys
!
        else if (field_name .eq. div_composition_flux%name) then
          div_forces%i_c_flux =    i_phys
        else if (field_name .eq. div_pert_composition_flux%name) then
          div_forces%i_pc_flux =   i_phys
!
        else if (field_name .eq. div_momentum_flux%name) then
          div_forces%i_m_flux =   i_phys
        else if (field_name .eq. div_maxwell_tensor%name) then
          div_forces%i_maxwell =  i_phys
        else if (field_name .eq. div_induction_tensor%name) then
          div_forces%i_induct_t = i_phys
        end if
      end if
!
      end subroutine set_div_force_addresses
!
! ----------------------------------------------------------------------
!
      end module t_base_force_labels
