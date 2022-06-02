!>@file   set_sym_field_labels.f90
!!        module set_sym_field_labels
!!
!!@author T. kera
!!@date   Programmed in July, 2021 by T. Kera (Tohoku Univ.)
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine set_sym_field_addresses                           &
!!     &         (i_phys, field_name, sym_fld, flag)
!!        type(base_field_address), intent(inout) :: sym_fld
!! !!!!!  sym field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names (Single sym, wide sym, double sym)
!!
!!   sym_velocity    [sym_fld%i_velo]:     sym velocity    u
!!   sym_vorticity   [sym_fld%i_vort]: 
!!            sym vorticity   \omega = \nabra \times v
!!
!!   sym_vector_potential  [sym_fld%i_vecp]: 
!!            sym vector potential \nabla \times A = B
!!   sym_magne      [sym_fld%i_magne]: sym magnetic field   B
!!   sym_current    [sym_fld%i_current]: 
!!            sym current density  J = \nabla \times B
!!
!!   sym_temperature  [sym_fld%i_temp]:  sym temperature              T
!!   sym_composition  [sym_fld%i_light]:
!!                              sym Composition anormally   C
!!   sym_density      [sym_fld%i_density]: sym density  \rho
!!   sym_entropy      [sym_fld%i_entropy]: sym Entropy  S
!!
!!   sym_pert_temperature   [sym_fld%i_per_temp]: \Theta = T - T_0
!!   sym_pert_composition   [sym_fld%i_per_light]:    C - C_0
!!   sym_pert_density       [sym_fld%i_per_density]: \rho - \rho_0
!!   sym_pert_entropy       [sym_fld%i_per_entropy]:  S - S_0
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module set_sym_field_labels
!
      use m_precision
      use m_phys_constants
      use t_base_field_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sym_field_addresses                             &
      &         (i_phys, field_name, sym_fld, flag)
!
      use m_field_w_symmetry_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: sym_fld
      logical, intent(inout) :: flag
!
!
      flag =    check_base_vector_symmetry(field_name)                         &
      &     .or. check_base_scalar_w_symmetry(field_name)
      if(flag) then
            if (field_name .eq. sym_velocity%name) then
            sym_fld%i_velo = i_phys
            else if (field_name .eq. sym_vorticity%name) then
            sym_fld%i_vort = i_phys
!
            else if (field_name .eq. sym_magnetic_field%name) then
            sym_fld%i_magne =    i_phys
            else if (field_name .eq. sym_vector_potential%name) then
            sym_fld%i_vecp =     i_phys
            else if (field_name .eq. sym_current_density%name) then
            sym_fld%i_current =  i_phys
!
            else if (field_name .eq. sym_temperature%name) then
            sym_fld%i_temp =            i_phys
            else if (field_name .eq. sym_perturbation_temp%name) then
            sym_fld%i_per_temp =        i_phys
!
            else if (field_name .eq. sym_composition%name) then
            sym_fld%i_light =          i_phys
            else if (field_name .eq. sym_perturbation_composition%name) then
            sym_fld%i_per_light =      i_phys
!
            else if (field_name .eq. sym_density%name) then
            sym_fld%i_density =        i_phys
            else if (field_name .eq. sym_perturbation_density%name) then
            sym_fld%i_per_density =    i_phys
!
            else if (field_name .eq. sym_entropy%name) then
            sym_fld%i_entropy =        i_phys
            else if (field_name .eq. sym_perturbation_entropy%name) then
            sym_fld%i_per_entropy =    i_phys
!       
            end if
      end if  
!
      end subroutine set_sym_field_addresses
! 
! ----------------------------------------------------------------------
!
      subroutine set_asym_field_addresses                             &
      &         (i_phys, field_name, asym_fld, flag)
!
      use m_field_w_symmetry_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: asym_fld
      logical, intent(inout) :: flag
!
!
      flag =    check_base_vector_symmetry(field_name)                         &
      &     .or. check_base_scalar_w_symmetry(field_name)
      if(flag) then
            if (field_name .eq. asym_velocity%name) then
            asym_fld%i_velo = i_phys
            else if (field_name .eq. asym_vorticity%name) then
            asym_fld%i_vort = i_phys
!
            else if (field_name .eq. asym_magnetic_field%name) then
            asym_fld%i_magne =    i_phys
            else if (field_name .eq. asym_vector_potential%name) then
            asym_fld%i_vecp =     i_phys
            else if (field_name .eq. asym_current_density%name) then
            asym_fld%i_current =  i_phys
!
! 
            else if (field_name .eq. asym_temperature%name) then
            asym_fld%i_temp =            i_phys
            else if (field_name .eq. asym_perturbation_temp%name) then
            asym_fld%i_per_temp =        i_phys
!
            else if (field_name .eq. asym_composition%name) then
            asym_fld%i_light =          i_phys
            else if (field_name .eq. asym_perturbation_composition%name) then
            asym_fld%i_per_light =      i_phys
!
            else if (field_name .eq. asym_density%name) then
            asym_fld%i_density =        i_phys
            else if (field_name .eq. asym_perturbation_density%name) then
            asym_fld%i_per_density =    i_phys
!
            else if (field_name .eq. asym_entropy%name) then
            asym_fld%i_entropy =        i_phys
            else if (field_name .eq. asym_perturbation_entropy%name) then
            asym_fld%i_per_entropy =    i_phys
            end if
      end if  
!
      end subroutine set_asym_field_addresses
!
! ----------------------------------------------------------------------
      end module set_sym_field_labels
