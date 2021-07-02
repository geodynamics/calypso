!>@file   set_filtered_field_labels.f90
!!        module set_filtered_field_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine set_filter_field_addresses                           &
!!     &         (i_phys, field_name, filter_fld, flag)
!!        type(base_field_address), intent(inout) :: filter_fld
!! !!!!!  Filtered field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names (Single filtered, wide filtered, double filtered)
!!
!!   filter_velocity    [filter_fld%i_velo]:     filtered velocity    u
!!   filter_vorticity   [filter_fld%i_vort]: 
!!            filtered vorticity   \omega = \nabra \times v
!!
!!   filter_vector_potential  [filter_fld%i_vecp]: 
!!            filtered vector potential \nabla \times A = B
!!   filter_magne      [filter_fld%i_magne]: filtered magnetic field   B
!!   filter_current    [filter_fld%i_current]: 
!!            filtered current density  J = \nabla \times B
!!
!!   filter_temperature  [filter_fld%i_temp]:  filtered temperature              T
!!   filter_composition  [filter_fld%i_light]:
!!                              filtered Composition anormally   C
!!   filter_density      [filter_fld%i_density]: filtered density  \rho
!!   filter_entropy      [filter_fld%i_entropy]: filtered Entropy  S
!!
!!   filter_pert_temperature   [filter_fld%i_per_temp]: \Theta = T - T_0
!!   filter_pert_composition   [filter_fld%i_per_light]:    C - C_0
!!   filter_pert_density       [filter_fld%i_per_density]: \rho - \rho_0
!!   filter_pert_entropy       [filter_fld%i_per_entropy]:  S - S_0
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module set_filtered_field_labels
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
      subroutine set_filter_field_addresses                             &
     &         (i_phys, field_name, filter_fld, flag)
!
      use m_filtered_field_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: filter_fld
      logical, intent(inout) :: flag
!
!
      flag =    check_filter_vector(field_name)                         &
     &     .or. check_filter_scalar(field_name)
      if(flag) then
        if (field_name .eq. filter_velocity%name) then
          filter_fld%i_velo = i_phys
        else if (field_name .eq. filter_vorticity%name) then
          filter_fld%i_vort = i_phys
!
        else if (field_name .eq. filter_magne%name) then
          filter_fld%i_magne =    i_phys
        else if (field_name .eq. filter_vector_potential%name) then
          filter_fld%i_vecp =     i_phys
        else if (field_name .eq. filter_current%name) then
          filter_fld%i_current =  i_phys
!
        else if (field_name .eq. filter_temperature%name) then
          filter_fld%i_temp =            i_phys
        else if (field_name .eq. filter_pert_temperature%name) then
          filter_fld%i_per_temp =        i_phys
!
        else if (field_name .eq. filter_composition%name) then
          filter_fld%i_light =          i_phys
        else if (field_name .eq. filter_pert_composition%name) then
          filter_fld%i_per_light =      i_phys
!
        else if (field_name .eq. filter_density%name) then
          filter_fld%i_density =        i_phys
        else if (field_name .eq. filter_pert_density%name) then
          filter_fld%i_per_density =    i_phys
!
        else if (field_name .eq. filter_entropy%name) then
          filter_fld%i_entropy =        i_phys
        else if (field_name .eq. filter_pert_entropy%name) then
          filter_fld%i_per_entropy =    i_phys
        end if
      end if  
!
      end subroutine set_filter_field_addresses
!
! ----------------------------------------------------------------------
!
      end module set_filtered_field_labels
