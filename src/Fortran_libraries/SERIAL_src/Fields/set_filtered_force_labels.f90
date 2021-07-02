!>@file   set_filtered_force_labels.f90
!!        module set_filtered_force_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for forces by filtered field
!!
!!@verbatim
!!      subroutine set_filtered_force_addresses                         &
!!     &         (i_phys, field_name, force_by_filter, flag)
!!        type(base_force_address), intent(inout) :: force_by_filter
!!
!!      subroutine set_filter_ene_flux_addresses                        &
!!     &         (i_phys, field_name, eflux_by_filter, flag)
!!        type(energy_flux_address), intent(inout) :: eflux_by_filter
!!
!! !!!!!  divergence of forces by filtered field !!!!!!!!!!!!!!!!!!
!!
!!      Field label  [Address]
!!
!!   inertia_by_filtered             [force_by_filter%i_m_advect]
!!   Lorentz_force_by_filtered       [force_by_filter%i_lorentz]
!!   magnetic_tension_by_filtered    [force_by_filter%i_m_tension]
!!
!!   filtered_buoyancy               [force_by_filter%i_buoyancy]
!!   filtered_comp_buoyancy          [force_by_filter%i_comp_buo]
!!
!!   vecp_induction_by_filtered      [force_by_filter%i_vp_induct]
!!   magnetic_induction_by_filtered  [force_by_filter%i_induction]
!!   magnetic_stretch_by_filtered    [force_by_filter%i_mag_stretch]
!!
!!   heat_advect_by_filtered         [force_by_filter%i_h_advect]
!!   pert_h_advect_by_filtered       [force_by_filter%i_ph_advect]
!!   comp_advect_by_filtered         [force_by_filter%i_c_advect]
!!   pert_c_advect_by_filtered       [force_by_filter%i_pc_advect]
!!
!!   momentum_flux_by_filtered       [force_by_filter%i_m_flux]
!!   maxwell_tensor_by_filtered      [force_by_filter%i_maxwell]
!!   induction_tensor_by_filtered    [force_by_filter%i_induct_t]
!!
!!   heat_flux_by_filtered           [force_by_filter%i_h_flux]
!!   pert_h_flux_by_filtered         [force_by_filter%i_ph_flux]
!!   composite_flux_by_filtered      [force_by_filter%i_c_flux]
!!   pert_c_flux_by_filtered         [force_by_filter%i_pc_flux]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! !!!!!  List of energy flux by SGS terms  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    Field name [Address]
!!
!!   inertia_work_by_filtered          [eflux_by_filter%i_m_advect_work]
!!   wk_against_Lorentz_by_filtered    [eflux_by_filter%i_nega_ujb]
!!   Lorentz_work_by_filtered          [eflux_by_filter%i_ujb]
!!   mag_tension_work_by_filtered      [eflux_by_filter%i_m_tension_wk]
!!
!!   filtered_buoyancy_flux            [eflux_by_filter%i_buo_gen]
!!   filtered_comp_buoyancy_flux       [eflux_by_filter%i_c_buo_gen]
!!
!!   mag_ene_generation_by_filtered    [eflux_by_filter%i_me_gen]
!!   mag_stretch_flux_by_filtered
!!                              [eflux_by_filter%i_mag_stretch_flux]
!!
!!   temp_generation_by_filtered       [eflux_by_filter%i_temp_gen]
!!   part_temp_gen_by_filtered         [eflux_by_filter%i_par_t_gen]
!!   comp_generation_by_filtered       [eflux_by_filter%i_comp_gen]
!!   part_comp_gen_by_filtered         [eflux_by_filter%i_par_c_gen]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module set_filtered_force_labels
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
      subroutine set_filtered_force_addresses                           &
     &         (i_phys, field_name, force_by_filter, flag)
!
      use m_filtered_force_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: force_by_filter
      logical, intent(inout) :: flag
!
!
      flag = check_filtered_force(field_name)                           &
     &      .or. check_filtered_flux_tensor(field_name)                 &
     &      .or. check_filtered_scalar_flux(field_name)
      if(flag) then
        if     (field_name .eq. inertia_by_filtered%name) then
          force_by_filter%i_m_advect =   i_phys
        else if(field_name .eq. Lorentz_force_by_filtered%name) then
          force_by_filter%i_lorentz =    i_phys
        else if(field_name .eq. magnetic_tension_by_filtered%name) then
          force_by_filter%i_m_tension =  i_phys
!
        else if(field_name .eq. filtered_buoyancy%name) then
          force_by_filter%i_buoyancy =   i_phys
        else if(field_name .eq. filtered_comp_buoyancy%name) then
          force_by_filter%i_comp_buo =   i_phys
!
        else if(field_name .eq. vecp_induction_by_filtered%name) then
          force_by_filter%i_vp_induct =    i_phys
        else if(field_name .eq. magnetic_induction_by_filtered%name)    &
     &   then
          force_by_filter%i_induction =  i_phys
        else if(field_name .eq. magnetic_stretch_by_filtered%name) then
          force_by_filter%i_mag_stretch =  i_phys
!
        else if (field_name .eq. heat_advect_by_filtered%name) then
          force_by_filter%i_h_advect =    i_phys
        else if (field_name .eq. pert_h_advect_by_filtered%name) then
          force_by_filter%i_ph_advect =   i_phys
        else if (field_name .eq. comp_advect_by_filtered%name) then
          force_by_filter%i_c_advect =    i_phys
        else if (field_name .eq. pert_c_advect_by_filtered%name) then
          force_by_filter%i_pc_advect =   i_phys
!
        else if (field_name .eq. heat_flux_by_filtered%name) then
          force_by_filter%i_h_flux =    i_phys
        else if (field_name .eq. pert_h_flux_by_filtered%name) then
          force_by_filter%i_ph_flux =   i_phys
        else if (field_name .eq. composite_flux_by_filtered%name) then
          force_by_filter%i_c_flux =    i_phys
        else if (field_name .eq. pert_c_flux_by_filtered%name) then
          force_by_filter%i_pc_flux =   i_phys
!
        else if(field_name .eq. momentum_flux_by_filtered%name) then
          force_by_filter%i_m_flux =   i_phys
        else if(field_name .eq. maxwell_tensor_by_filtered%name) then
          force_by_filter%i_maxwell =  i_phys
        else if(field_name .eq. induction_tensor_by_filtered%name) then
          force_by_filter%i_induct_t = i_phys
        end if
      end if
!
      end subroutine set_filtered_force_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_filter_ene_flux_addresses                          &
     &         (i_phys, field_name, eflux_by_filter, flag)
!
      use m_filtered_ene_flux_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(energy_flux_address), intent(inout) :: eflux_by_filter
      logical, intent(inout) :: flag
!
!
      flag = check_filter_enegy_fluxes(field_name)
      if(flag) then
        if (field_name .eq. inertia_work_by_filtered%name) then
          eflux_by_filter%i_m_advect_work = i_phys
        else if (field_name .eq. wk_against_Lorentz_by_filtered%name)   &
     &   then
          eflux_by_filter%i_nega_ujb =      i_phys
        else if (field_name .eq. Lorentz_work_by_filtered%name) then
          eflux_by_filter%i_ujb =           i_phys
        else if (field_name .eq. mag_tension_work_by_filtered%name)     &
     &   then
          eflux_by_filter%i_m_tension_wk =  i_phys
!
        else if (field_name .eq. filtered_buoyancy_flux%name) then
          eflux_by_filter%i_buo_gen =       i_phys
        else if (field_name .eq. filtered_comp_buoyancy_flux%name) then
          eflux_by_filter%i_c_buo_gen =     i_phys
!
        else if (field_name .eq. mag_ene_generation_by_filtered%name)   &
     &   then
          eflux_by_filter%i_me_gen =           i_phys
        else if (field_name .eq. mag_stretch_flux_by_filtered%name)     &
     &   then
          eflux_by_filter%i_mag_stretch_flux = i_phys
!
        else if (field_name .eq. temp_generation_by_filtered%name) then
          eflux_by_filter%i_temp_gen =  i_phys
        else if (field_name .eq. part_temp_gen_by_filtered%name) then
          eflux_by_filter%i_par_t_gen = i_phys
!
        else if (field_name .eq. comp_generation_by_filtered%name) then
          eflux_by_filter%i_comp_gen =  i_phys
        else if (field_name .eq. part_comp_gen_by_filtered%name) then
          eflux_by_filter%i_par_c_gen = i_phys
        end if
      end if
!
      end subroutine set_filter_ene_flux_addresses
!
! ----------------------------------------------------------------------
!
      end module set_filtered_force_labels
