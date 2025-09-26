!>@file   set_sym_force_labels.f90
!!        module set_sym_force_labels
!!
!!@author T, Kera
!!@date   Programmed in July, 2021 by T. Kera (Tohoku Univ.)
!!
!!
!> @brief Labels and addresses for forces by sym field
!!
!!@verbatim
!!      subroutine set_sym_sym_force_addresses                          &
!!     &         (i_phys, field_name, force_by_sym, flag)
!!        integer(kind = kint), intent(in) :: i_phys
!!        character(len = kchara), intent(in) :: field_name
!!        type(base_force_address), intent(inout) :: force_by_sym_sym
!!      subroutine set_asym_asym_force_addresses                        &
!!     &         (i_phys, field_name, force_by_asym_asym, flag)
!!        integer(kind = kint), intent(in) :: i_phys
!!        character(len = kchara), intent(in) :: field_name
!!        type(base_force_address), intent(inout) :: force_by_asym_asym
!!      subroutine set_sym_asym_force_addresses                         &
!!     &         (i_phys, field_name, force_by_sym_asym, flag)
!!        integer(kind = kint), intent(in) :: i_phys
!!        character(len = kchara), intent(in) :: field_name
!!        type(base_force_address), intent(inout) :: force_by_sym_asym
!!      subroutine set_asym_sym_force_addresses                         &
!!     &         (i_phys, field_name, force_by_asym_sym, flag)
!!        integer(kind = kint), intent(in) :: i_phys
!!        character(len = kchara), intent(in) :: field_name
!!        type(base_force_address), intent(inout) :: force_by_asym_sym
!!
!!      subroutine set_sym_eflux_address_sym_asym                       &
!!     &         (i_phys, field_name, eflux_s_sxa, flag)
!!        integer(kind = kint), intent(in) :: i_phys
!!        character(len = kchara), intent(in) :: field_name
!!        type(energy_flux_address), intent(inout) :: eflux_s_sxa
!!      subroutine set_sym_eflux_address_asym_sym                       &
!!     &         (i_phys, field_name, eflux_s_axs, flag)
!!        integer(kind = kint), intent(in) :: i_phys
!!        character(len = kchara), intent(in) :: field_name
!!        type(energy_flux_address), intent(inout) :: eflux_s_axs
!!      subroutine set_asym_eflux_address_sym_sym                       &
!!     &         (i_phys, field_name, eflux_a_sxs, flag)
!!        integer(kind = kint), intent(in) :: i_phys
!!        character(len = kchara), intent(in) :: field_name
!!        type(energy_flux_address), intent(inout) :: eflux_a_sxs
!!      subroutine set_asym_eflux_address_asm_asm                       &
!!     &         (i_phys, field_name, eflux_a_axa, flag)
!!        integer(kind = kint), intent(in) :: i_phys
!!        character(len = kchara), intent(in) :: field_name
!!        type(energy_flux_address), intent(inout) :: eflux_a_axa
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   wsym_x_usym  wasym_x_uasym  wsym_x_uasym  wasym_x_usym
!!                 :        inertia (\omega \times u)
!!   m_flux_sym_sym  m_flux_asym_asym  m_flux_sym_asym
!!                 :  momentum flux     u_{i} u_{j}
!!   Jsym_x_Bsym  Jasym_x_Basym  Jsym_x_Basym  Jasym_x_Bsym
!!                 :  Lorentz force     J \times B
!!   Bsym_nabla_Bsym  Basym_nabla_Basym
!!   Bsym_nabla_Basym  Basym_nabla_Bsym
!!                 :  magnetic tension   (B \nabla) B
!!   maxwell_tensor_sym_sym, maxwell_tensor_asym_asym,
!!   maxwell_tensor_sym_asym
!!                 :  maxwell tensor       B_{i} B_{j}
!!
!!   sym_thermal_buoyancy, asym_thermal_buoyancy
!!                 :   Thermal buoyancy       - \alpha_{T} T g
!!   sym_composite_buoyancy, asym_composite_buoyancy
!!                 :   Compositional buoyancy  - \alpha_{C} C g
!!   sym_buoyancy, asym_buoyancy
!!                 :   Total buoyancy  - (\alpha_{T} T + \alpha_{C} C) g
!!
!!   usym_x_Bsym, uasym_x_Basym, usym_x_Basym, uasym_x_Bsym
!!                 :     induction                 u \times B
!!   Bsym_nabla_usym, Basym_nabla_uasym,
!!   Bsym_nabla_uasym, Basym_nabla_usym
!!                 :    magneitic streatch         (B \nabla) u
!!   usym_Bsym, uasym_Basym, usym_Basym
!!                 :    induction induction tensor
!!                                 u_{i} B_{j}  - B_{i} u_{J}
!!
!!   usym_nabla_Tsym, uasym_nabla_Tasym,
!!   usym_nabla_Tasym, uasym_nabla_Tsym
!!                 :    heat advection     (u \cdot \nabla) T
!!   usym_nabla_pTsym, uasym_nabla_pTasym,
!!   usym_nabla_pTasym, uasym_nabla_pTsym
!!                 :  perturbation of heat advection
!!                                      (u \cdot \nabla) \Theta
!!   heat_flux_sym_sym, heat_flux_asym_asym,
!!   heat_flux_sym_asym, heat_flux_asym_sym
!!                 :    heat flux                   uT
!!   pert_h_flux_sym_sym, pert_h_flux_asym_asym
!!   pert_h_flux_sym_asym, pert_h_flux_asym_sym
!!                 :  perturbation of heat flux   u\Theta
!!
!!   usym_nabla_Csym, uasym_nabla_Casym
!!   usym_nabla_Casym, uasym_nabla_Csym
!!                 :    composition advection     (u \cdot \nabla) C
!!   usym_nabla_pCsym, uasym_nabla_pCasym,
!!   usym_nabla_pCasym, uasym_nabla_pCsym
!!                 :  perturbation of composition advection
!!                                      (u \cdot \nabla) (C-C_0)
!!   composite_flux_sym_sym, composite_flux_asym_asym, 
!!   composite_flux_sym_asym, composite_flux_asym_sym
!!                 :    composition flux                   uC
!!   pert_c_flux_sym_sym, pert_c_flux_asym_asym,
!!   pert_c_flux_sym_asym, pert_c_flux_asym_sym
!!                 :  perturbation of composition flux   u(C-C_0)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module set_sym_force_labels
!
      use m_precision
      use m_constants
      use t_base_force_labels
      use t_energy_flux_labels
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sym_sym_force_addresses                           &
     &         (i_phys, field_name, force_by_sym_sym, flag)
!
      use m_force_w_sym_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: force_by_sym_sym
      logical, intent(inout) :: flag
!
!
      flag = check_forces_w_sym(field_name)                           &
      &      .or. check_flux_tensors_w_sym(field_name)                &
      &      .or. check_scalar_advection_w_sym(field_name)            &
      &      .or. check_flux_asym_tensors_w_sym(field_name)
      if(flag) then
            if     (field_name .eq. wsym_x_usym%name) then
            force_by_sym_sym%i_m_advect =   i_phys
            else if(field_name .eq. Jsym_x_Bsym%name) then
            force_by_sym_sym%i_lorentz =    i_phys
            else if(field_name .eq. Bsym_nabla_Bsym%name) then
            force_by_sym_sym%i_m_tension =  i_phys
!
            else if(field_name .eq. asym_thermal_buoyancy%name) then
            force_by_sym_sym%i_thrm_buo =   i_phys
            else if(field_name .eq. asym_composite_buoyancy%name) then
            force_by_sym_sym%i_comp_buo =   i_phys
!
            else if(field_name .eq. usym_x_Bsym%name) then
            force_by_sym_sym%i_vp_induct =    i_phys
            else if(field_name .eq. rot_usym_x_Bsym%name) then
            force_by_sym_sym%i_induction =  i_phys
            else if(field_name .eq. Bsym_nabla_usym%name) then
            force_by_sym_sym%i_mag_stretch =  i_phys
!
            else if (field_name .eq. usym_nabla_Tsym%name) then
            force_by_sym_sym%i_h_advect =    i_phys
            else if (field_name .eq. usym_nabla_pTsym%name) then
            force_by_sym_sym%i_ph_advect =   i_phys
            else if (field_name .eq. usym_nabla_Csym%name) then
            force_by_sym_sym%i_c_advect =    i_phys
            else if (field_name .eq. usym_nabla_pCsym%name) then
            force_by_sym_sym%i_pc_advect =   i_phys
!
            else if (field_name .eq. heat_flux_sym_sym%name) then
            force_by_sym_sym%i_h_flux =    i_phys
            else if (field_name .eq. pert_h_flux_sym_sym%name) then
            force_by_sym_sym%i_ph_flux =   i_phys
            else if (field_name .eq. composite_flux_sym_sym%name) then
            force_by_sym_sym%i_c_flux =    i_phys
            else if (field_name .eq. pert_c_flux_sym_sym%name) then
            force_by_sym_sym%i_pc_flux =   i_phys
!
            else if(field_name .eq. m_flux_sym_sym%name) then
            force_by_sym_sym%i_m_flux =   i_phys
            else if(field_name .eq. maxwell_tensor_sym_sym%name) then
            force_by_sym_sym%i_maxwell =  i_phys
            else if(field_name .eq. usym_Bsym%name) then
            force_by_sym_sym%i_induct_t = i_phys
            end if
      end if
!
      end subroutine set_sym_sym_force_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_asym_asym_force_addresses                          &
     &         (i_phys, field_name, force_by_asym_asym, flag)
!
      use m_force_w_sym_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: force_by_asym_asym
      logical, intent(inout) :: flag
!
!
      flag = check_forces_w_sym(field_name)                           &
      &      .or. check_flux_tensors_w_sym(field_name)                &
      &      .or. check_scalar_advection_w_sym(field_name)            &
      &      .or. check_flux_asym_tensors_w_sym(field_name)
      if(flag) then
            if     (field_name .eq. wasym_x_uasym%name) then
            force_by_asym_asym%i_m_advect =   i_phys
            else if(field_name .eq. Jasym_x_Basym%name) then
            force_by_asym_asym%i_lorentz =    i_phys
            else if(field_name .eq. Basym_nabla_Basym%name) then
            force_by_asym_asym%i_m_tension =  i_phys
!
            else if(field_name .eq. uasym_x_Basym%name) then
            force_by_asym_asym%i_vp_induct =    i_phys
            else if(field_name .eq. rot_uasym_x_Basym%name)    &
      &   then
            force_by_asym_asym%i_induction =  i_phys
            else if(field_name .eq. Basym_nabla_uasym%name) then
            force_by_asym_asym%i_mag_stretch =  i_phys
!
            else if (field_name .eq. uasym_nabla_Tasym%name) then
            force_by_asym_asym%i_h_advect =    i_phys
            else if (field_name .eq. uasym_nabla_pTasym%name) then
            force_by_asym_asym%i_ph_advect =   i_phys
            else if (field_name .eq. uasym_nabla_Casym%name) then
            force_by_asym_asym%i_c_advect =    i_phys
            else if (field_name .eq. uasym_nabla_pCasym%name) then
            force_by_asym_asym%i_pc_advect =   i_phys
!
            else if (field_name .eq. heat_flux_asym_asym%name) then
            force_by_asym_asym%i_h_flux =    i_phys
            else if (field_name .eq. pert_h_flux_asym_asym%name) then
            force_by_asym_asym%i_ph_flux =   i_phys
            else if (field_name .eq. composite_flux_asym_asym%name) then
            force_by_asym_asym%i_c_flux =    i_phys
            else if (field_name .eq. pert_c_flux_asym_asym%name) then
            force_by_asym_asym%i_pc_flux =   i_phys
!
            else if(field_name .eq. m_flux_asym_asym%name) then
            force_by_asym_asym%i_m_flux =   i_phys
            else if(field_name .eq. maxwell_tensor_asym_asym%name) then
            force_by_asym_asym%i_maxwell =  i_phys
            else if(field_name .eq. uasym_Basym%name) then
            force_by_asym_asym%i_induct_t = i_phys
            end if
      end if
!
      end subroutine set_asym_asym_force_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_sym_asym_force_addresses                           &
     &         (i_phys, field_name, force_by_sym_asym, flag)
!
      use m_force_w_sym_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: force_by_sym_asym
      logical, intent(inout) :: flag
!
!
      flag = check_forces_w_sym(field_name)                           &
      &      .or. check_flux_tensors_w_sym(field_name)                &
      &      .or. check_scalar_advection_w_sym(field_name)            &
      &      .or. check_flux_asym_tensors_w_sym(field_name)
      if(flag) then
            if     (field_name .eq. wsym_x_uasym%name) then
            force_by_sym_asym%i_m_advect =   i_phys
            else if(field_name .eq. Jsym_x_Basym%name) then
            force_by_sym_asym%i_lorentz =    i_phys
            else if(field_name .eq. Bsym_nabla_Basym%name) then
            force_by_sym_asym%i_m_tension =  i_phys
!
            else if(field_name .eq. sym_thermal_buoyancy%name) then
            force_by_sym_asym%i_thrm_buo =   i_phys
            else if(field_name .eq. sym_composite_buoyancy%name) then
            force_by_sym_asym%i_comp_buo =   i_phys
!
            else if(field_name .eq. usym_x_Basym%name) then
            force_by_sym_asym%i_vp_induct =   i_phys
            else if(field_name .eq. rot_usym_x_Basym%name) then
            force_by_sym_asym%i_induction =   i_phys
            else if(field_name .eq. Bsym_nabla_uasym%name) then
            force_by_sym_asym%i_mag_stretch = i_phys
!
            else if (field_name .eq. usym_nabla_Tasym%name) then
            force_by_sym_asym%i_h_advect =    i_phys
            else if (field_name .eq. usym_nabla_pTasym%name) then
            force_by_sym_asym%i_ph_advect =   i_phys
            else if (field_name .eq. usym_nabla_Casym%name) then
            force_by_sym_asym%i_c_advect =    i_phys
            else if (field_name .eq. usym_nabla_pCasym%name) then
            force_by_sym_asym%i_pc_advect =   i_phys
!
            else if (field_name .eq. heat_flux_sym_asym%name) then
            force_by_sym_asym%i_h_flux =    i_phys
            else if (field_name .eq. pert_h_flux_sym_asym%name) then
            force_by_sym_asym%i_ph_flux =   i_phys
            else if (field_name .eq. composite_flux_sym_asym%name) then
            force_by_sym_asym%i_c_flux =    i_phys
            else if (field_name .eq. pert_c_flux_sym_asym%name) then
            force_by_sym_asym%i_pc_flux =   i_phys
!
            else if(field_name .eq. m_flux_sym_asym%name) then
            force_by_sym_asym%i_m_flux =   i_phys
            else if(field_name .eq. maxwell_tensor_sym_asym%name) then
            force_by_sym_asym%i_maxwell =  i_phys
            else if(field_name .eq. usym_Basym%name) then
            force_by_sym_asym%i_induct_t = i_phys
            end if
      end if
!
      end subroutine set_sym_asym_force_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_asym_sym_force_addresses                           &
     &         (i_phys, field_name, force_by_asym_sym, flag)
!
      use m_force_w_sym_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: force_by_asym_sym
      logical, intent(inout) :: flag
!
!
      flag = check_forces_w_sym(field_name)                           &
      &      .or. check_flux_tensors_w_sym(field_name)                &
      &      .or. check_scalar_advection_w_sym(field_name)            &
      &      .or. check_flux_asym_tensors_w_sym(field_name)
      if(flag) then
            if     (field_name .eq. wasym_x_usym%name) then
            force_by_asym_sym%i_m_advect =   i_phys
            else if(field_name .eq. Jasym_x_Bsym%name) then
            force_by_asym_sym%i_lorentz =    i_phys
            else if(field_name .eq. Basym_nabla_Bsym%name) then
            force_by_asym_sym%i_m_tension =  i_phys
!
            else if(field_name .eq. uasym_x_Bsym%name) then
            force_by_asym_sym%i_vp_induct =    i_phys
            else if(field_name .eq. rot_uasym_x_Bsym%name)    &
      &   then
            force_by_asym_sym%i_induction =  i_phys
            else if(field_name .eq. Basym_nabla_usym%name) then
            force_by_asym_sym%i_mag_stretch =  i_phys
!
            else if (field_name .eq. uasym_nabla_Tsym%name) then
            force_by_asym_sym%i_h_advect =    i_phys
            else if (field_name .eq. uasym_nabla_pTsym%name) then
            force_by_asym_sym%i_ph_advect =   i_phys
            else if (field_name .eq. uasym_nabla_Csym%name) then
            force_by_asym_sym%i_c_advect =    i_phys
            else if (field_name .eq. uasym_nabla_pCsym%name) then
            force_by_asym_sym%i_pc_advect =   i_phys
!
            else if (field_name .eq. heat_flux_asym_sym%name) then
            force_by_asym_sym%i_h_flux =    i_phys
            else if (field_name .eq. pert_h_flux_asym_sym%name) then
            force_by_asym_sym%i_ph_flux =   i_phys
            else if (field_name .eq. composite_flux_asym_sym%name) then
            force_by_asym_sym%i_c_flux =    i_phys
            else if (field_name .eq. pert_c_flux_asym_sym%name) then
            force_by_asym_sym%i_pc_flux =   i_phys
!
            else if(field_name .eq. uasym_Bsym%name) then
            force_by_asym_sym%i_induct_t = i_phys
            end if
      end if
!
      end subroutine set_asym_sym_force_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_sym_eflux_address_sym_asym                         &
     &         (i_phys, field_name, eflux_s_sxa, flag)
!
      use m_sym_ene_flux_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: eflux_s_sxa
      logical, intent(inout) :: flag
!
!
      flag = check_enegy_fluxes_w_sym(field_name)
      if(flag) then
            if (field_name .eq. us_d_js_x_ba%name) then
            eflux_s_sxa%i_ujb =           i_phys
            else if (field_name .eq. mns_us_d_ws_x_ua%name) then
            eflux_s_sxa%i_m_advect_work =       i_phys
            else if (field_name .eq. sym_buoyancy_flux%name) then
            eflux_s_sxa%i_buo_gen =       i_phys
            end if
      end if
!
      end subroutine set_sym_eflux_address_sym_asym
!
! ----------------------------------------------------------------------
!
      subroutine set_sym_eflux_address_asym_sym                         &
     &         (i_phys, field_name, eflux_s_axs, flag)
!
      use m_sym_ene_flux_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: eflux_s_axs
      logical, intent(inout) :: flag
!
!
      flag = check_enegy_fluxes_w_sym(field_name)
      if(flag) then
            if (field_name .eq. us_d_ja_x_bs%name) then
            eflux_s_axs%i_ujb =           i_phys
            else if (field_name .eq. mns_us_d_wa_x_us%name) then
            eflux_s_axs%i_m_advect_work =       i_phys
            end if
      end if
!
      end subroutine set_sym_eflux_address_asym_sym
!
! ----------------------------------------------------------------------
!
      subroutine set_asym_eflux_address_sym_sym                         &
     &         (i_phys, field_name, eflux_a_sxs, flag)
!
      use m_sym_ene_flux_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: eflux_a_sxs
      logical, intent(inout) :: flag
!
!
      flag = check_enegy_fluxes_w_sym(field_name)
      if(flag) then
            if (field_name .eq. ua_d_js_x_bs%name) then
            eflux_a_sxs%i_ujb =           i_phys
            else if (field_name .eq. mns_ua_d_ws_x_us%name) then
            eflux_a_sxs%i_m_advect_work =       i_phys
            else if (field_name .eq. asym_buoyancy_flux%name) then
            eflux_a_sxs%i_buo_gen =       i_phys
            end if
      end if
!
      end subroutine set_asym_eflux_address_sym_sym
!
! ----------------------------------------------------------------------
!
      subroutine set_asym_eflux_address_asm_asm                         &
     &         (i_phys, field_name, eflux_a_axa, flag)
!
      use m_sym_ene_flux_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: eflux_a_axa
      logical, intent(inout) :: flag
!
!
      flag = check_enegy_fluxes_w_sym(field_name)
      if(flag) then
            if (field_name .eq. ua_d_ja_x_ba%name) then
            eflux_a_axa%i_ujb =           i_phys
            else if (field_name .eq. mns_ua_d_wa_x_ua%name) then
            eflux_a_axa%i_m_advect_work = i_phys
            end if
      end if
!
      end subroutine set_asym_eflux_address_asm_asm
!
! ----------------------------------------------------------------------
!
      end module set_sym_force_labels
